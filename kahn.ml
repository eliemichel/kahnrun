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


module Th: S = struct
  type 'a process = (unit -> 'a)

  type 'a channel = { q: 'a Queue.t ; m: Mutex.t; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let new_channel () =
    let q = { q = Queue.create (); m = Mutex.create (); } in
    q, q

  let put v c () =
    Mutex.lock c.m;
    Queue.push v c.q;
    Mutex.unlock c.m;
    Thread.yield ()

  let rec get c () =
    try
      Mutex.lock c.m;
      let v = Queue.pop c.q in
      Mutex.unlock c.m;
      v
    with Queue.Empty ->
      Mutex.unlock c.m;
      Thread.yield ();
      get c ()

  let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

  let return v = (fun () -> v)

  let bind e e' () =
    let v = e () in
    Thread.yield ();
    e' v ()

  let run e = e ()
end



module Mono: S = struct
	let q : (unit -> unit) Queue.t = Queue.create ()
	exception Delay (* Pour repporter un processus si une fifo est vide *)
	
	type 'a process = ('a -> unit) -> unit

	type 'a channel = 'a Queue.t
	type 'a in_port = 'a channel
	type 'a out_port = 'a channel

	let new_channel () =
		let c = Queue.create () in
			c, c
	
	let put a c cb =
		Queue.push a c;
		cb ()
	
	let rec get c cb =
		try cb (Queue.pop c)
		with Queue.Empty ->
			Queue.push (fun () -> get c cb) q (* On procrastine… *)
	
	let doco l =
		(* À cause de `get` on ne peut se contenter d'ajouter `cb` à `q`
			après les éléments de `l` *)
		let n = List.length l in
		fun cb ->
			let k = ref 0 in
			let aux () =
				incr k;
				if !k = n (* Si tous les processi de la liste ont été exécuté *)
				then cb ()
			in
			List.iter (fun p -> Queue.push (fun () -> p aux) q) l
	
	
	let return a =
		fun cb -> Queue.push (fun () -> cb a) q

	let bind p f =
			fun cb ->
				let e () = p (fun a -> f a cb) in
					Queue.push e q

	let run p =
		let r = ref None in
		let cb = fun a -> r := Some a in
		let e () = p cb in
			Queue.push e q;
			while not (Queue.is_empty q) do
				Queue.pop q ()
			done;
			match !r with
				| None -> assert false
				| Some a -> a
end






module Proc: S = struct
	type 'a process = (unit -> 'a)
	
	type 'a in_port = in_channel
	type 'a out_port = out_channel
	type 'a channel = 'a in_port * 'a out_port
	
	let new_channel () =
		let o, i = pipe () in
			in_channel_of_descr o, out_channel_of_descr i
	
	let put v c () =
		Marshal.to_channel c v []
	
	let rec get c () =
		Marshal.from_channel c
	
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
	
	let run e = e ()
end


module Sock: S = struct
	type 'a process = (unit -> 'a)
	
	type 'a in_port = in_channel
	type 'a out_port = out_channel
	type 'a channel = 'a in_port * 'a out_port

	let new_channel_addr addr =
		let sock_in = socket PF_INET SOCK_STREAM 0 in
		let sock_serv = socket PF_INET SOCK_STREAM 0 in
			bind sock_serv addr;
			listen sock_serv 1;
			connect sock_in addr;
			let sock_out, _ = accept sock_serv in
		
			in_channel_of_descr sock_in, out_channel_of_descr sock_out
	
	let rec new_channel () =
		let port = 1024 + Random.int 64611 in
		eprintf "Attempt to create a socket pipe on port %d...@." port;
		try new_channel_addr (make_addr "localhost" port)
		with _ -> new_channel ()

	let put v c () =
		Marshal.to_channel c v []
	
	let rec get c () =
		Marshal.from_channel c
	
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
	
	let run e = e ()
end



module Network: S = struct
	type 'a process = (unit -> 'a)
	
	type 'a in_port = int
	type 'a out_port = int
	
	let sock = socket PF_INET SOCK_STREAM 0
	let serv = socket PF_INET SOCK_STREAM 0
	let local = socket PF_INET SOCK_STREAM 0 (* J'ai voulu utiliser une socket UNIX mais sans succès (Invalid argument lors de attach) *)
	let srvin = out_channel_of_descr sock
	let srvout = in_channel_of_descr sock
	let initialized = ref false
	let lut_in : (int, file_descr) Hashtbl.t = Hashtbl.create 5 (* comes into the Node *)
	let lut_out : (int, file_descr) Hashtbl.t = Hashtbl.create 5 (* goes out of the Node *)
	let last_channel_id = ref 0

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

	let handle_channel_request req =
		match Protocol.read_header req with
			| Protocol.In_port ->
				let id = Protocol.read_int req in (
					try Marshal.to_channel (out_channel_of_descr req) (Hashtbl.find lut_in id) []
					with Not_found -> Protocol.error req "In port not found"
				)
			| Protocol.Out_port ->
				let id = Protocol.read_int req in (
					try Marshal.to_channel (out_channel_of_descr req) (Hashtbl.find lut_in id) []
					with Not_found -> Protocol.error req "Out port not found"
				)
			| _ -> Protocol.error req "Port type expected"

	let rec channel_manager () =
		let req, _ = accept local in
			let th = Thread.create handle_channel_request req in
				channel_manager ();
				Thread.join th

	let init () = 
		if not !initialized then (
			eprintf "Waiting for master...@.";
			let rec try_loop () =
				try connect sock master_addr
				with Unix_error (ECONNREFUSED, "connect", "") -> try_loop ()
			in
			try_loop ();
			eprintf "Connection established.@.";
			
			eprintf "Starting node server...@.";
			let rec aux () =
				let port = random_port () in
				eprintf "Trying port %d...@." port;
				(
					try bind serv (make_addr "localhost" port)
					with _ -> aux ()
				)
			in aux ();
			listen serv max_chans;
			eprintf "Node server runing.@.";
			
			eprintf "Starting channel manager...@.";
			let rec aux () =
				let port = random_port () in
				eprintf "Trying port %d...@." port;
				(
					try bind local (make_addr "localhost" port)
					with _ -> aux ()
				)
			in
			let local_addr = aux () in
			listen local max_chans;
			eprintf "Channel manager runing.@.";

			let listen_in_th = Thread.create listen_in () in
			let channel_manager_th = Thread.create channel_manager () in
			initialized := true;

			(* let close = *) fun () ->
			if !initialized
			then (
				shutdown sock SHUTDOWN_ALL;
				shutdown serv SHUTDOWN_ALL;
				shutdown local SHUTDOWN_ALL;
				Thread.join listen_in_th;
				Thread.join channel_manager_th
			)
		
		)
		else fun () -> ()
	

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
		let out_descr = Hashtbl.find lut_out c in
		let cout = out_channel_of_descr out_descr in
			Marshal.to_channel cout v []
	
	let rec get c () =
		let in_descr = Hashtbl.find lut_in c in
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
