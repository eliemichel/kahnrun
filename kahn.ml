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



  (** The scheduler communicates with the workers with messages *)

  type 'a request = 
  | PingRequest
  | KillSig
  | NewTasks of 'a list

  type 'a answer = int * ('a answer_body)
  and  'a answer_body =
  | PingAnswer
  | TasksDone of 'a list  (* workerId, answers *)


  (** Creates a process aimed at computing some parallels tasks

      [nWorkers] : number of processes working in parallel, not counting
      the scheduler
      [groupSize] : the number of tasks delivered at each message from the scheduler
 
      For each task, all happens as if [handler (computation task)] was called
      but handler is called by the scheduler and computation by a worker process
  *)

  let make_scheduler nWorkers groupSize computation handler tasks =

    let sch_in, sch_out = K.new_channel () in 
    let workersPorts = Array.init nWorkers (fun i -> K.new_channel ()) in
    let send i m = K.put m (snd workersPorts.(i)) in

    let rec watch nLeft notSent = match nLeft with
      | 0 -> killWorkersFrom 0
      | _ ->

        begin
          K.get sch_in >>= fun (workerId, msg) -> 
          match msg with
          (* The worker is ready, we send a new task *)
          | PingAnswer -> 
            (* Printf.printf "Ping %d\n" workerId; *)
            assignTasks nLeft notSent workerId

          | TasksDone answers ->
            (* Printf.printf "Received %d (size : %d)\n" workerId (List.length answers); *)
            List.iter handler answers ;
            assignTasks (nLeft - List.length answers) notSent workerId
        end

    and assignTasks nLeft notSent workerId = 

      let sending, notSent' = splitAt groupSize notSent in
      (
        if sending <> [] then
          send workerId (NewTasks sending) 
        else
          K.return ()
      ) >>= fun () ->
      watch nLeft notSent'

    and killWorkersFrom i = 
      if i >= nWorkers then K.return ()
      else
        send i KillSig >>= fun () -> 
        killWorkersFrom (i + 1)
    in

    let rec init i =
      if i >= nWorkers then 
        watch (List.length tasks) tasks
      else
        send i PingRequest >>= fun () -> init (i + 1)
    in


    let rec workerLoop id worker_in sch_out =
      K.get worker_in >>= fun msg -> match msg with
      | KillSig -> K.return ()

      | PingRequest -> 
        K.put (id, PingAnswer) sch_out >>= fun () -> 
        workerLoop id worker_in sch_out

      | NewTasks tasks ->
        (* Printf.printf "Task received (worker : %d, size : %d)\n" id (List.length tasks); *)
        let answers = List.map computation tasks in
        K.put (id, TasksDone answers) sch_out >>= fun () -> 
        workerLoop id worker_in sch_out

    in 

    let workers = List.map (fun i -> 
      workerLoop i (fst workersPorts.(i)) sch_out
    ) (range 0 (nWorkers - 1))

    in

    K.doco ((init 0) :: workers)
    
    

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
		| Listen of int
			(* Listen to incoming packet on a given channel id — 0 for unspecified *)
		| Wait of int
			(* Wait for someone who listen *)
		| Ask
			(* Ask for free channel ids *)
		| Spawn of (int * string)
			(* Send a process to spawn *)
		| Register of sockaddr
			(* Add to lists of peers *)
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

	let send_wait channel callback packet =
		eprintf "Wait for %d@." channel;
		broadcast (Wait channel);
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

	let get_peer = function
		(* Un peu foireux et empêche de faire tourner plusieurs nœuds sur la
		même machine mais pour faire plus propre il fallait ajouter une info
		de retour aux paquets Listen et Wait et donc modifier la LUT pour
		pouvoir gérer à la fois des adresses et des sockets. *)
		| ADDR_INET (addr_inet, _) ->
			let rec aux = function
				| [] -> raise Not_found
				| ADDR_INET (addr_inet', _) as addr :: q when addr_inet' = addr_inet -> addr
				| _ :: q -> aux q
			in aux !peers
		| _ -> raise Not_found

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
		let sock_of_option addr =
			try
				(* TODO : trouver un moyen de fermer ce socket *)
				let s = socket PF_INET SOCK_STREAM 0 in
				connect s (get_peer addr);
				s
			with Not_found -> node
		in
		let listen_to channel =
			Hashtbl.add lut channel (sock_of_option addr)
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
					if force then send_wait channel aux packet
					)
				)
			| Listen channel ->
				eprintf "[packet]Listen at %d@." channel;
				listen_to channel;
				send_ack cout
			| Wait channel ->
				eprintf "[packet]Wait for %d@." channel;
				if Hashtbl.mem lut channel
				then (
					eprintf "################@.";
					(try send_packet PF_INET (get_peer addr) (Listen channel)
					with Not_found -> ());
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
					listen_to channel;
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
