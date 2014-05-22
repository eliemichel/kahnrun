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
		addr : sockaddr
	}
	type 'a process = (env -> 'a)

	type 'a in_port = int
	type 'a out_port = int

	type packet =
	| Send of int * bool * Marshal.extern_flags list * string (* int: channel id ; bool: force forwarding ; string: serialized data *)
	| Listen of int (* Listen to incoming packet on a given channel id — 0 for unspecified *)
	| Wait of int (* Wait for someone who listen *)
	| Ask (* Ask for free channel ids *)
	| Spawn of unit process
	| Alloc of int * int (* Allocate a range of channel ids *)
	| Ack (* Acknowledgment *)

	let wait_ack cin =
		while Marshal.from_channel cin <> Ack do () done

	let send_wait srvin srvout channel packet callback =
		Marshal.to_channel srvin (Wait channel);
		flush srvin;
		wait_ack srvout;
		eprintf "Delay for %d@." channel;
		pause wait_delay;
		callback packet

	let spawn pids local_addr process =
		match fork () with
		| 0 -> (
			process {
				addr = local_addr
			};
			exit 0
			)
		| pid -> pids := pid :: !pids

	let handle_all srvin srvout pids local_addr lut node =
		let cin  = in_channel_of_descr node in
		let cout  = out_channel_of_descr node in
		let aux = function
			| Send ((channel, force, flags, data) as packet) -> (
				try
					let dest = out_channel_of_descr (Hashtbl.find lut channel) in
						Hashtbl.remove lut channel;
						Marshal.to_channel dest packet flags;
						flush dest;
						Marshal.to_channel cout Ack [];
						flush cout;
				with Not_found ->
					if force then send_wait srvin srvout channel packet aux
				)
			| Listen channel ->
				Hashtbl.add lut channel node;
				Marshal.to_channel cout Ack [];
				fluch cout
			| Wait channel ->
				if Hashtbl.mem lut channel
				then (Marshal.to_channel cout Ack []; fluch cout)
			| Spawn process -> spawn pids local_addr process
			| Ask -> assert (not implemented)
			| Alloc (a, b) -> assert (not implemented)
		in
			aux (Marshal.from_channel cin)


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

		let interface_inet = sock PF_INET SOCK_STREAM 0 in
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


		let interface_local	= sock PF_UNIX SOCK_STREAM 0 in
		eprintf "Starting local interface...@.";
		let rec aux i =
			let path = sprintf "%s%d" local_base_filename i in
			eprintf "Trying path %s...@." path;
			(
				try bind interface_local (ADDR_UNIX path)
				with _ -> aux ()
			);
			ADDR_UNIX path
		in
		let local_addr = aux 0 in
		listen interface_local max_chans;
		eprintf "Local interface runing.@.";

		let handler = handle_all srvin srvout pids local_addr lut in
		let th_inet = Thread.create accepter (interface_inet, handler) in
		let th_inet = Thread.create accepter (interface_local, handler) in
		eprintf "Node running.@.";

		fun () ->
		Thread.join th_inet;
		Thread.join th_local;
		List.iter (fun pid -> ignore (waitpid [] pid)) !pids;

		shutdown serv SHUTDOWN_ALL;
		shutdown interface_inet SHUTDOWN_ALL;
		shutdown interface_local SHUTDOWN_ALL;

		eprintf "Node stoped.@."



	let init () =
		eprintf "Waiting for master...@.";
		let rec try_loop () =
			try connect serv master_addr
			with Unix_error (ECONNREFUSED, "connect", "") -> try_loop ()
		in
		try_loop ();
		eprintf "Connection established.@.";
		
		eprintf "Starting process server...@.";
		let rec aux () =
			let port = random_port () in
			eprintf "Trying port %d...@." port;
			(
				try bind sock (make_addr "localhost" port)
				with _ -> aux ()
			)
		in aux ();
		listen sock max_chans;
		eprintf "Process server runing.@.";
		
		let listen_in_th = Thread.create listen_in () in
		
		(* let close = *) fun () ->
		shutdown sock SHUTDOWN_ALL;
		shutdown serv SHUTDOWN_ALL;
		Thread.join listen_in_th

	let rec next_channel_id () =
		incr last_channel_id;
		if Hashtbl.mem lut_in !last_channel_id || Hashtbl.mem lut_out !last_channel_id
		then next_channel_id ()
		else !last_channel_id

	let new_channel () =
		let chan = Random.int max_int in
			chan, chan
	
	let put v c env =
		let sock = socket PF_UNIX SOCK_STREAM 0 in
		let cout = out_channel_of_descr sock in
		let cin = in_channel_of_descr sock in
		let packet = Send (c, true, [Marshal.Closures], Marshal.to_string v 0 [Marshal.Closures]) in
			connect sock env.addr;
			Marshal.to_channel cout packet [];
			flush cout;
			wait_ack cin


	let rec get c env =
		let sock = socket PF_UNIX SOCK_STREAM 0 in
		let cout = out_channel_of_descr sock in
		let cin = in_channel_of_descr sock in
			connect sock env.addr;
			Marshal.to_channel cout (Listen c) [];
			flush cout;
			wait_ack cin;
			Marshal.from_channel cin
	
	let doco l env =
		let sock = socket PF_UNIX SOCK_STREAM 0 in
		let cout = out_channel_of_descr sock in
		let cin = in_channel_of_descr sock in
			connect sock env.addr;
			let rec aux pids = function
				| [] -> List.iter (fun pid -> ignore (waitpid [] pid)) pids
				| f :: q -> (
					Marshal.to_channel cout (Spawn f) [Marshal.Closures];
					flush cout;
					wait_ack cin
					)
			in aux [] l
	
	let return v = (fun env -> v)
	
	let bind e e' env =
		let v = e () in
		e' v ()
	
	let run e =
		let callback = init () in
		let v = e () in
			callback ();
			v

end
