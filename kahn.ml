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
	type 'a process = (unit -> 'a)
	
	type 'a in_port = int
	type 'a out_port = int
	
	let sock = socket PF_INET SOCK_STREAM 0
	let serv = socket PF_INET SOCK_STREAM 0
	let local = socket PF_UNIX SOCK_STREAM 0
	let srvin = out_channel_of_descr serv
	let srvout = in_channel_of_descr serv
	let initialized = ref false
	let lut_in : (int, file_descr) Hashtbl.t = Hashtbl.create 5 (* comes into the Node *)
	let lut_out : (int, file_descr) Hashtbl.t = Hashtbl.create 5 (* goes out of the Node *)
	let last_channel_id = ref 0
	let main_pipe_o, main_pipe_i = pipe ()
	let cmanag = out_channel_of_descr main_pipe_i

	let handle_in node =
		let attached_chan = Protocol.read node Protocol.In_port in
			if Hashtbl.mem lut_in attached_chan
			then Protocol.error node "Channel already attached"
			else
				Hashtbl.add lut_in attached_chan node;
				Protocol.ack node

	let rec listen_in () =
		let node, addr = accept sock in
			eprintf "Node input from %s@." (print_sockaddr addr);
			let th = Thread.create handle_in node in
				listen_in ();
				Thread.join th

	let rec channel_manager cin =
		let (is_in, id, cout) = Marshal.from_channel cin in
		eprintf "request@.";
		let lut = if is_in then lut_in else lut_out in
		let ch =
			try Hashtbl.find lut id
			with Not_found -> failwith "Channel not found"
		in
			Marshal.to_channel cout ch [];
			flush cout;
			channel_manager cin

	let init () =
		eprintf "Waiting for master...@.";
		let rec try_loop () =
			try connect serv master_addr
			with Unix_error (ECONNREFUSED, "connect", "") -> try_loop ()
		in
		try_loop ();
		eprintf "Connection established.@.";
		
		eprintf "Starting node server...@.";
		let rec aux () =
			let port = random_port () in
			eprintf "Trying port %d...@." port;
			(
				try bind sock (make_addr "localhost" port)
				with _ -> aux ()
			)
		in aux ();
		listen sock max_chans;
		eprintf "Node server runing.@.";
		
		let listen_in_th = Thread.create listen_in () in
		let channel_manager_th = Thread.create channel_manager (in_channel_of_descr main_pipe_o) in
		
		(* let close = *) fun () ->
		shutdown sock SHUTDOWN_ALL;
		shutdown serv SHUTDOWN_ALL;
		shutdown local SHUTDOWN_ALL;
		Thread.join listen_in_th;
		Thread.join channel_manager_th

	let rec next_channel_id () =
		incr last_channel_id;
		if Hashtbl.mem lut_in !last_channel_id || Hashtbl.mem lut_out !last_channel_id
		then next_channel_id ()
		else !last_channel_id

	let new_channel () =
		let o, i = pipe () in
			let i_id = next_channel_id () in
			let o_id = next_channel_id () in
				Hashtbl.add lut_out i_id i;
				Hashtbl.add lut_in o_id o;
				o_id, i_id
	
	let put v c () =
		eprintf "put in %d@." c;
		let cm_out, cm_in = pipe () in
		Marshal.to_channel cmanag (false, c, cm_in) [];
		flush cmanag;
		let out_descr = Marshal.from_channel (in_channel_of_descr cm_out) in
		eprintf "test@.";
		let cout = out_channel_of_descr out_descr in
			Marshal.to_channel cout v []
	
	let rec get c () =
		eprintf "get from %d@." c;
		let cm_out, cm_in = pipe () in
		Marshal.to_channel cmanag (true, c, cm_in) [];
		flush cmanag;
		let in_descr = Marshal.from_channel (in_channel_of_descr cm_out) in
		let cin = in_channel_of_descr in_descr in
			eprintf "test: %d@." (input_binary_int cin);
			Marshal.from_channel cin
	
	let doco l () =
		let rec aux pids = function
			| [] -> List.iter (fun pid -> ignore (waitpid [] pid)) pids
			| f :: q ->
				match fork () with
				| 0 -> f ()
				| pid -> aux (pid :: pids) q
		in aux [] l
	
	let return v = (fun () -> v)
	
	let bind e e' () =
		let v = e () in
		e' v ()
	
	let run e =
		let callback = init () in
		let v = e () in
			callback ();
			v

end
