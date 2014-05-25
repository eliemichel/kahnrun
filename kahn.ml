open Params
open Utils
open Unix
open Sys
open Format

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
		| Listen of int * sockaddr option
			(* Listen to incoming packet on a given channel id — 0 for unspecified *)
		| Wait of int * sockaddr option (* Wait for someone who listen *)
		| Ask (* Ask for free channel ids *)
		| Spawn of (int * string)
		| Register of sockaddr
		| Alloc of (int * int) (* Allocate a range of channel ids *)
		| Ack (* Acknowledgment *)

	let peers = ref []

	let wait_ack cin =
		while Marshal.from_channel cin <> Ack do () done

	let send_ack cout =
		Marshal.to_channel cout Ack [];
		flush cout


	let send_packet domain addr packet =
		eprintf "--- Send packet@.";
		let sock = socket domain SOCK_STREAM 0 in
		let cout = out_channel_of_descr sock in
		let cin = in_channel_of_descr sock in
			(* eprintf "connect...@."; *)
			connect sock addr;
			(* eprintf "send...@."; *)
			Marshal.to_channel cout packet [];
			flush cout;
			(* eprintf "wait ack...@."; *)
			wait_ack cin;
			shutdown sock SHUTDOWN_ALL;
			eprintf "Done.@."

	let broadcast packet =
		List.iter (fun addr -> send_packet PF_INET addr packet) !peers

	let send_wait channel return_addr callback packet =
		eprintf "Wait for %d@." channel;
		broadcast (Wait (channel, return_addr));
		eprintf "Delay for %d@." channel;
		Utils.pause wait_delay;
		callback packet

	let new_channel () =
		let chan = Random.int 99999 in
			chan, chan
	
	let put v c env =
		eprintf "-- put on %d@." c;
		let packet = Send (c, true, Marshal.to_string v [Marshal.Closures]) in
			send_packet PF_UNIX env.addr packet

	let rec get c env =
		eprintf "-- get from %d@." c;
		let sock = socket PF_UNIX SOCK_STREAM 0 in
		let cout = out_channel_of_descr sock in
		let cin = in_channel_of_descr sock in
			connect sock env.addr;
			Marshal.to_channel cout (Listen (c, None)) [];
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

	let get_peer addr =
		let rec aux = function
			| [] -> raise Not_found
			| ADDR_INET (addr_inet, _) as addr :: q -> addr
			| _ :: q -> aux q
		in aux !peers

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
		| pid -> pids := pid :: !pids

	let handler_serv srvout inet_addr =
		eprintf "Listening to the master server@.";
		while true do
			eprintf "Listening to the master server (try again)@.";
			let addr = Marshal.from_channel srvout in
			eprintf "New node registered from %s@." (print_sockaddr addr);
			send_packet PF_INET addr (Register inet_addr);
			peers := addr :: !peers
		done

	let handle_all srvin srvout pids local_addr lut node addr =
		let cin  = in_channel_of_descr node in
		let cout  = out_channel_of_descr node in
		let sock_of_option = function
			| Some addr ->
				let s = socket PF_INET SOCK_STREAM 0 in
				connect s addr;
				s
			| None -> node
		in
		let lut_item_of_option = function
			| Some addr -> Address addr
			| None -> Socket node
		in
		let listen_to channel return_addr =
			Hashtbl.add lut channel (lut_item_of_option return_addr)
		in
		let rec aux = function
			| Send (channel, force, data) as packet -> (
				eprintf "[packet]Send to %d@." channel;
				try
					eprintf "Attempt to find %d@." channel;
					let dest = out_channel_of_descr (Hashtbl.find lut channel) in
						eprintf "Found : %s.@." (print_sockaddr (getsockname (Hashtbl.find lut channel)));
						Hashtbl.remove lut channel;
						Marshal.to_channel dest packet [];
						flush dest;
						send_ack cout;
				with Not_found -> (
					eprintf "Not found.@.";
					if force then send_wait channel (Some local_addr) aux packet
					)
				)
			| Listen (channel, return_addr) ->
				eprintf "[packet]Listen at %d@." channel;
				listen_to channel return_addr;
				send_ack cout
			| Wait (channel, return_addr) ->
				eprintf "[packet]Wait for %d@." channel;
				if Hashtbl.mem lut channel
				then (
					eprintf "################@.";
					(match return_addr with
					| Some addr -> send_packet PF_INET addr (Listen (channel, Some local_addr))
					| None -> ()
					);
					eprintf "################ack@.";
				);
				send_ack cout
			| Spawn (channel, process_str) ->
				if Random.int 2 = 0 (* Distribution aléatoire pour le moment *)
				then (
					eprintf "Redirect spawn !@.";
					send_packet PF_INET (List.hd !peers) (Spawn (channel, process_str))
				)
				else (
					eprintf "Will spawn !@.";
					listen_to channel (Some local_addr);
					eprintf "Will spawn !2 le retour@.";
					let process = Marshal.from_string process_str 0 in
					spawn pids local_addr channel process;
					send_ack cout
				)
			| Register addr ->
				eprintf "[packet]Node registered from %s@." (print_sockaddr addr);
				peers := addr :: !peers;
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

	let rec accepter sock handler =
		let node, addr = accept sock in
			eprintf "Process input from %s@." (print_sockaddr addr);
			let th = Thread.create (handler node) addr in
				accepter sock handler;
				Thread.join th

	let run process =
		eprintf "Starting node...@.";
		let lut : (int, file_descr) Hashtbl.t = Hashtbl.create 17 in
		let pids = ref [] in

		let interface_inet = socket PF_INET SOCK_STREAM 0 in
		eprintf "Starting inet interface...@.";
		let rec aux () =
			let port = random_port () in
			eprintf "Trying port %d...@." port;
			(
				try
					let addr = make_addr "localhost" port in
						Unix.bind interface_inet addr;
						addr
				with _ -> aux ()
			)
		in
		let inet_addr = aux () in
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
		Marshal.to_channel srvin inet_addr [];
		flush srvin;

		let handler = handle_all srvin srvout pids local_addr lut in
		let th_inet = Thread.create (accepter interface_inet) handler in
		let th_local = Thread.create (accepter interface_local) handler in
		let th_serv = Thread.create (handler_serv srvout) inet_addr in
		eprintf "Node running.@.";

		let close () =
			Thread.join th_inet;
			eprintf "inet interface ended.@.";
			Thread.join th_local;
			eprintf "local interface ended.@.";
			Thread.join th_serv;
			eprintf "master interface ended.@.";
			List.iter (fun pid -> ignore (waitpid [] pid)) !pids;

			shutdown serv SHUTDOWN_ALL;
			shutdown interface_inet SHUTDOWN_ALL;
			shutdown interface_local SHUTDOWN_ALL
		in
		
		if Array.length Sys.argv > 1 && Sys.argv.(1) = "--root" then (

			eprintf "Launch root process.@.";
			let sock_root = socket PF_UNIX SOCK_STREAM 0 in
			let cout = out_channel_of_descr sock_root in
			let cin = in_channel_of_descr sock_root in
			let c, _ = new_channel () in
			let process_str = Marshal.to_string process [Marshal.Closures] in
			connect sock_root local_addr;
			Marshal.to_channel cout (Spawn (c, process_str)) [Marshal.Closures];
			flush cout;
			wait_ack cin;
			eprintf "Wait for root process end@.";

			match Marshal.from_channel cin with
			| Send (_, _, str) ->
				close();
				Marshal.from_string str 0
			| _ -> assert false

		)
		else (
			eprintf "Run as child node !@.";
			close();
			raise End_of_file
		)

end
