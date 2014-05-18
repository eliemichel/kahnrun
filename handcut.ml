open Unix
open Sys
open Utils
open Params

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

  val export: string * 'a in_port -> unit process
  val import: string * 'a out_port -> unit process
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


module Node: S = struct
	
	let sock = socket PF_INET SOCK_STREAM 0
	let srvin = out_channel_of_descr sock
	let srvout = in_channel_of_descr sock
	let initialized = ref false

	let init () = 
		if not !initialized then (
			Format.eprintf "Waiting for master...@.";
			let rec try_loop () =
				try connect sock master_addr
				with Unix_error (ECONNREFUSED, "connect", "") -> try_loop ()
			in
				try_loop ();
				Format.eprintf "Connection established.@.";
				initialized := true
		)

	let close () =
		if !initialized then shutdown sock SHUTDOWN_ALL
	

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
	
	let run e =
		init ();
		let v = e () in
		close ();
		v

	let import (hyperchan, cin) =
		bind (return ()) (return (fun () ->
			let mysrvout = tee srvout in
			while true do
				let hl = input_line mysrvout in
				let l = hl in (*(escape_hyperchan hyperchan) *)
					output_line cin l
			done
		))

	let export (hyperchan, cout) =
		bind (return ()) (return (fun () -> (* delay expansé *)
			while true do
				let l = input_line cout in
					output_line srvin (Format.sprintf "%s!%s" (escape_hyperchan hyperchan) l)
			done
		))
end


