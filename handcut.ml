open Unix
open Sys

let make_addr serv port =
	let host = (gethostbyname serv).h_addr_list.(0) in
	ADDR_INET (host, port)


let print_sockaddr = function
	| ADDR_INET (addr, port) -> Format.sprintf "%s (port %d)" (string_of_inet_addr addr) port
	| _ -> "[unix]"


let output_line output line =
	output_string output line;
	output_string output "\n";
	flush output

let master_addr = make_addr "localhost" 4455
let max_nodes = 4

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

  val export: string * 'a out_port
  val import: string * 'a in_port
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




module Master = struct
	


	let run () =
		Format.eprintf "Searching for nodes...@.";
		(*let sock_serv = socket PF_INET SOCK_STREAM 0 in
		bind sock_serv master_addr;
		listen sock_serv max_nodes;*)
		
		let handler input output =
			while true do
				let l = input_line input in
					print_endline l;
					output_line output l
			done

		in establish_server handler master_addr

		(*
		let research () =
			while List.length !nodes < max_nodes do
				let node = accept sock_serv in
					Format.eprintf "Node found at %s@." (print_sockaddr (snd node));
					nodes := node :: !nodes
			done
		in

		let loop () =


		match fork () with
		| 0 -> research ()
		| pid -> (
			for i = 5 downto 1 do
				Format.eprintf "%d@." i;
				sleep 1
			done;
			kill pid sigint;
			while true do loop () done
		)
		*)



	
end

(*)

module Node: S = struct
	
	let root_sock = socket PF_INET SOCK_STREAM 0
	let initialized = ref false

	let init () =
		if not !initialized then (
			Format.eprintf "Waiting for master...@.";
			let rec try_loop () =
				try connect root_sock master_addr
				with Unix_error (ECONNREFUSED, "connect", "") -> try_loop ()
			in
				try_loop ();
				Format.eprintf "Connection established.@.";
				initialized := true
		)
	

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
		Format.eprintf "Attempt to create a socket pipe on port %d...@." port;
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
	
	let run e =
		init ();
		e ()


	let export (hyperchan, out) = return ()

	let improt (hyperchan, in) = return ()
end



*)
