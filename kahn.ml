open Unix

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



let make_addr serv port =
	let host = (gethostbyname serv).h_addr_list.(0) in
	ADDR_INET (host, port)


(*
module Sock: S = struct
	type 'a process = (unit -> 'a)
	
	type 'a in_port = in_channel
	type 'a out_port = out_channel
	type 'a channel = 'a in_port * 'a out_port
	
	let new_channel () =
		let sock = socket PF_INET SOCK_STREAM 0 in
			bind sock addr;
			listen sock 1;
			let client_sock, client_addr = accept sock in
				service client_sock client_addr
				(*connect sock2 addr*)
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

*)





