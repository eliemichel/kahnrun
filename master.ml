open Unix
open Utils
open Params
open Format


(** Master est juste un serveur d'écho pour servir aux différents paires à s'identifier. *)
let run () =
	eprintf "Searching for nodes...@.";
	let nodes = ref [] in
	let sock_serv = socket PF_INET SOCK_STREAM 0 in
	bind sock_serv (ADDR_INET (inet_addr_any, master_port));
	listen sock_serv max_nodes;
	

	let broadcast fdin =
		let buffer = String.create buffer_size in
		let rec copy() =
			match read fdin buffer 0 buffer_size with
			| 0 -> ()
			| n ->
				eprintf "broadcast: %s@." buffer;
				List.iter (fun node ->
					if node != fdin || echo_on
					then ignore (write node buffer 0 n)
				) !nodes;
				copy ()
		in
		copy ()
	in


	let rec research () =
		let node, addr = accept sock_serv in
			eprintf "Node found at %s@." (print_sockaddr addr);
			nodes := node :: !nodes;
			let th = Thread.create broadcast node in
				research ();
				Thread.join th
	in

	research ()

let () = run ()
