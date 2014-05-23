open Params
open Utils
open Unix
open Sys
open Format

let make_addr serv port =
	let host = (gethostbyname serv).h_addr_list.(0) in
	ADDR_INET (host, port)

let master_addr = make_addr "localhost" 4455
let max_slaves = 4

module type S = sig
  type 'a process
  type 'a in_port
  type 'a out_port

  val new_channel: unit -> 'a in_port * 'a out_port
  val put: 'a -> 'a out_port -> unit process
  val get: 'a in_port -> 'a process

  val doco: unit process list -> unit process

  val return: 'a -> 'a process
  val bind: 'a process -> ('a -> 'b process) -> 'b process

  val run: 'a process -> 'a
end

module Lib (K : S) = struct

  let ( >>= ) x f = K.bind x f

  let delay f x =
    K.bind (K.return ()) (fun () -> K.return (f x))

  let par_map f l =
    let rec build_workers l (ports, workers) =
      match l with
      | [] -> (ports, workers)
      | x :: l ->
          let qi, qo = K.new_channel () in
          build_workers
            l
            (qi :: ports,
             ((delay f x) >>= (fun v -> K.put v qo)) :: workers)
    in
    let ports, workers = build_workers l ([], []) in
    let rec collect l acc qo =
      match l with
      | [] -> K.put acc qo
      | qi :: l -> (K.get qi) >>= (fun v -> collect l (v :: acc) qo)
    in
    let qi, qo = K.new_channel () in
    K.run
      ((K.doco ((collect ports [] qo) :: workers)) >>= (fun _ -> K.get qi))

end


module Network: S = struct
	type env = {
		addr : sockaddr;
		id   : int
	}
	type 'a process = (env -> 'a)

	type 'a in_port = int
	type 'a out_port = int

	type packet =
		| Send of (int * bool * Marshal.extern_flags list * string)
		(* int: channel id ; bool: force forwarding ; string: serialized data *)
		| Listen of int (* Listen to incoming packet on a given channel id — 0 for unspecified *)
		| Wait of int (* Wait for someone who listen *)
		| Ask (* Ask for free channel ids *)
		| Spawn of unit process
		| Alloc of (int * int) (* Allocate a range of channel ids *)
		| Ack (* Acknowledgment *)

	let wait_ack cin =
		while Marshal.from_channel cin <> Ack do () done

	let send_ack cout =
		Marshal.to_channel cout Ack [];
		flush cout

	let send_wait srvin srvout channel callback packet =
		eprintf "Wait for %d@." channel;
		Marshal.to_channel srvin (Wait channel) [];
		flush srvin;
		eprintf "Delay for %d@." channel;
		Utils.pause wait_delay;
		callback packet

	let spawn pids local_addr process =
		match fork () with
		| 0 -> (
			let id = Random.int 99999 in
			eprintf "Start process %d@." id;
			process {
				addr = local_addr;
				id = id
			};
			eprintf "End of %d@." id;
			exit 0
			)
		| pid -> pids := pid :: !pids;eprintf "TEST@."

	let handle_all srvin srvout pids local_addr lut node =
		let cin  = in_channel_of_descr node in
		let cout  = out_channel_of_descr node in
		let rec aux = function
			| Send (channel, force, flags, data) as packet -> (
				try
					eprintf "Attempt to find %d@." channel;
					let dest = out_channel_of_descr (Hashtbl.find lut channel) in
						eprintf "Found.@.";
						Hashtbl.remove lut channel;
						Marshal.to_channel dest packet flags;
						flush dest;
						send_ack cout;
				with Not_found -> (
					eprintf "Not found.@.";
					if force then send_wait srvin srvout channel aux packet
					)
				)
			| Listen channel ->
				eprintf "Listen at %d@." channel;
				Hashtbl.add lut channel node;
				Marshal.to_channel cout Ack [];
				flush cout
			| Wait channel ->
				if Hashtbl.mem lut channel
				then (Marshal.to_channel cout Ack []; flush cout)
			| Spawn process ->
				eprintf "Will spawn !@.";
				spawn pids local_addr process;
				send_ack cout
			| Ask -> assert (not implemented)
			| Alloc (a, b) -> assert (not implemented)
			| Ack -> ()
		in
			try
				while true do
					match select [node] [] [] timeout with
					| [node], [], [] -> aux (Marshal.from_channel cin)
					| _ -> ()
				done
			with _ -> ()

	let rec accepter (sock, handler) =
		let node, addr = accept sock in
			eprintf "Node input from %s@." (print_sockaddr addr);
			let th = Thread.create handler node in
				accepter (sock, handler);
				Thread.join th

	let init () =
		eprintf "Starting node...@.";
		let lut : (int, file_descr) Hashtbl.t = Hashtbl.create 17 in
		let pids = ref [] in

		let serv = socket PF_INET SOCK_STREAM 0 in
		let srvin = out_channel_of_descr serv in
		let srvout = in_channel_of_descr serv in
		eprintf "Waiting for master...@.";
		let rec try_loop () =
			try connect serv master_addr
			with Unix_error (ECONNREFUSED, "connect", "") -> try_loop ()
		in
		try_loop ();
		eprintf "Connection established.@.";

		let interface_inet = socket PF_INET SOCK_STREAM 0 in
		eprintf "Starting inet interface...@.";
		let rec aux () =
			let port = random_port () in
			eprintf "Trying port %d...@." port;
			(
				try bind interface_inet (make_addr "localhost" port)
				with _ -> aux ()
			)
		in aux ();
		listen interface_inet max_chans;
		eprintf "Inet interface runing.@.";


		let interface_local	= socket PF_UNIX SOCK_STREAM 0 in
		eprintf "Starting local interface...@.";
		let rec aux i =
			let path = sprintf "%s%d" local_base_filename i in
			eprintf "Trying path %s...@." path;
			(
				try
					let addr = ADDR_UNIX path in
						bind interface_local addr;
						addr
				with _ -> aux (i + 1)
			)
		in
		let local_addr = aux 0 in
		listen interface_local max_chans;
		eprintf "Local interface runing.@.";

		let handler = handle_all srvin srvout pids local_addr lut in
		let th_inet = Thread.create accepter (interface_inet, handler) in
		let th_local = Thread.create accepter (interface_local, handler) in
		eprintf "Node running.@.";

		{ addr = local_addr ; id = 0 },
		fun () ->
		Thread.join th_inet;
		Thread.join th_local;
		List.iter (fun pid -> ignore (waitpid [] pid)) !pids;

		shutdown serv SHUTDOWN_ALL;
		shutdown interface_inet SHUTDOWN_ALL;
		shutdown interface_local SHUTDOWN_ALL;

		eprintf "Node stoped.@."

	let new_channel () =
		let chan = Random.int 99999 in
			chan, chan
	
	let put v c env =
		eprintf "-- put on %d@." c;
		let sock = socket PF_UNIX SOCK_STREAM 0 in
		let cout = out_channel_of_descr sock in
		let cin = in_channel_of_descr sock in
		let packet = Send (c, true, [Marshal.Closures], Marshal.to_string v [Marshal.Closures]) in
			eprintf "connect...@.";
			connect sock env.addr;
			eprintf "send...@.";
			Marshal.to_channel cout packet [];
			flush cout;
			eprintf "wait ack...@.";
			wait_ack cin;
			shutdown sock SHUTDOWN_ALL;
			eprintf "Done.@."

	let rec get c env =
		eprintf "-- get from %d@." c;
		let sock = socket PF_UNIX SOCK_STREAM 0 in
		let cout = out_channel_of_descr sock in
		let cin = in_channel_of_descr sock in
			connect sock env.addr;
			Marshal.to_channel cout (Listen c) [];
			flush cout;
			wait_ack cin;
			let v = match Marshal.from_channel cin with
				| Send (channel, force, flags, data) ->
					Marshal.from_string data 0
				| _ -> assert false
			in
				shutdown sock SHUTDOWN_ALL;
				v
	
	let doco l env =
		eprintf "-- doco @@%d@." env.id;
		let sock = socket PF_UNIX SOCK_STREAM 0 in
		let cout = out_channel_of_descr sock in
		let cin = in_channel_of_descr sock in
			connect sock env.addr;
			List.iter (fun process ->
				eprintf "* Spawn a new process@.";
				Marshal.to_channel cout (Spawn process) [Marshal.Closures];
				flush cout;
				eprintf "* Sent@.";
				wait_ack cin;
				eprintf "* Acked@."
			) l
	
	let return v env =
		(* eprintf "-- return @@%d@." env.id; *)
		v
	
	let bind e e' env =
		eprintf "-- bind @@%d@." env.id;
		let v = e env in
		e' v env
	
	let run e =
		let env, callback = init () in
		let v = e env in
			callback ();
			v

end
