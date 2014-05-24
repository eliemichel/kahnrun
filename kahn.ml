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
		| Send of (int * bool * string)
		(* int: channel id ; bool: force forwarding ; string: serialized data *)
		| Listen of int (* Listen to incoming packet on a given channel id — 0 for unspecified *)
		| Wait of int (* Wait for someone who listen *)
		| Ask (* Ask for free channel ids *)
		| Spawn of (int * string)
		| Alloc of (int * int) (* Allocate a range of channel ids *)
		| Ack (* Acknowledgment *)

	let open_nodes = ref []

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


	let new_channel () =
		let chan = Random.int 99999 in
			chan, chan
	
	let put v c env =
		eprintf "-- put on %d@." c;
		let sock = socket PF_UNIX SOCK_STREAM 0 in
		let cout = out_channel_of_descr sock in
		let cin = in_channel_of_descr sock in
		let packet = Send (c, true, Marshal.to_string v [Marshal.Closures]) in
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
				| Send (channel, force, data) ->
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
				let c, _ = new_channel () in
				let process_str = Marshal.to_string process [Marshal.Closures] in
				eprintf "* Spawn a new process@.";
				Marshal.to_channel cout (Spawn (c, process_str)) [];
				flush cout;
				eprintf "* Sent@.";
				wait_ack cin;
				eprintf "* Acked@."
			) l;
			List.iter (fun _ ->
				eprintf "* Wait for process end@.";
				match Marshal.from_channel cin with
				| Send _ -> ()
				| _ -> assert false
			) l

	
	let return v env =
		eprintf "-- return @@%d@." env.id;
		v
	
	let bind e e' env =
		eprintf "-- bind @@%d@." env.id;
		let v = e env in
		e' v env

	(* ===== Init dependencies =====*)

	let spawn pids local_addr channel process =
		match fork () with
		| 0 -> (
			let id = Random.int 99999 in
			eprintf "Start process %d@." id;
			let env = {
				addr = local_addr;
				id = id
				}
			in
			let v = process env in
			eprintf "End of %d@." id;
			put v channel env;
			exit 0
			)
		| pid -> pids := pid :: !pids;eprintf "TEST@."

	let handle_all srvin srvout pids local_addr lut node =
		let cin  = in_channel_of_descr node in
		let cout  = out_channel_of_descr node in
		let listen_to channel =
			Hashtbl.add lut channel node
			(* XXX broadcast *)
		in
		let rec aux = function
			| Send (channel, force, data) as packet -> (
				try
					eprintf "Attempt to find %d@." channel;
					let dest = out_channel_of_descr (Hashtbl.find lut channel) in
						eprintf "Found.@.";
						Hashtbl.remove lut channel;
						Marshal.to_channel dest packet [];
						flush dest;
						send_ack cout;
				with Not_found -> (
					eprintf "Not found.@.";
					if force then send_wait srvin srvout channel aux packet
					)
				)
			| Listen channel ->
				eprintf "Listen at %d@." channel;
				listen_to channel;
				send_ack cout
			| Wait channel ->
				if Hashtbl.mem lut channel
				then send_ack cout
			| Spawn (channel, process_str) ->
				eprintf "Will spawn !@.";
				listen_to channel;
				let process = Marshal.from_string process_str 0 in
				spawn pids local_addr channel process;
				send_ack cout
			| Ask -> assert (not implemented)
			| Alloc (a, b) -> assert (not implemented)
			| Ack -> ()
		in aux (Marshal.from_channel cin)

	let rec accepter sock handler =
		let node, addr = accept sock in
			eprintf "Node input from %s@." (print_sockaddr addr);
			open_nodes := node :: !open_nodes;
			handler node;
			eprintf "## Handled@."

	let run process =
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
				try Unix.bind interface_inet (make_addr "localhost" port)
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
						Unix.bind interface_local addr;
						addr
				with _ -> aux (i + 1)
			)
		in
		let local_addr = aux 0 in
		listen interface_local max_chans;
		eprintf "Local interface runing.@.";

		eprintf "Node running.@.";


		eprintf "Launch root process...@.";
		let sock_root = socket PF_UNIX SOCK_STREAM 0 in
		let rootout = out_channel_of_descr sock_root in
		let rootin = in_channel_of_descr sock_root in
		let c, _ = new_channel () in
		let process_str = Marshal.to_string process [Marshal.Closures] in

		let th = Thread.create (fun () ->
			connect sock_root local_addr;
			Marshal.to_channel rootout (Spawn (c, process_str)) [Marshal.Closures];
			flush rootout;
			wait_ack rootin;
			eprintf "Root process launched.@.";
		) ()
		in
		
		eprintf "Start main listening loop.@.";

		let handler = handle_all srvin srvout pids local_addr lut in
		let v = (
			try
				(while true do
					let connections = serv :: interface_inet :: interface_local :: sock_root :: !open_nodes in
					let l, _, _ = select connections [] [] (-1.) in
					eprintf "selected@.";
					List.iter (fun sock ->
						if sock = serv then (
							()
						) else if sock = sock_root then (
							match Marshal.from_channel rootin with
							| Send (_, _, str) -> raise (Return str)
							| _ -> assert false
						) else if sock = interface_inet || sock = interface_local then (
							eprintf "# accepted?@.";
							accepter sock handler;
							eprintf "# accepted.@."
						) else (
							try handler sock
							with End_of_file -> open_nodes := List.filter ((<>) sock) !open_nodes
						)
					) l
				done;
				assert false)
			with Return str -> Marshal.from_string str 0
		) in
		eprintf "Process ending.@.";

		Thread.join th;
		List.iter (fun pid -> ignore (waitpid [] pid)) !pids;

		shutdown serv SHUTDOWN_ALL;
		shutdown interface_inet SHUTDOWN_ALL;
		shutdown interface_local SHUTDOWN_ALL;
		eprintf "Node stopped.@.";
		v

end
