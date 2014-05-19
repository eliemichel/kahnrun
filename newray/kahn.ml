open Utils

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
