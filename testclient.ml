open Unix
open Params
open Utils

let () =
	let servout, servin = open_connection master_addr in
		print_endline "Connected.";
		let run = ref true in

		let rec from_server () =
			print_endline (input_line servout);
			if !run then from_server ()
		in

		let rec from_user () =
			print_string "> ";
			let l = read_line () in
				output_line servin l;
				if String.compare l "quit" != 0
				then from_user ()
				else run := false
		in

		let th = Thread.create from_server () in
			from_user ();
			Thread.join th;
			shutdown_connection servout;
			print_endline "Disconnected."


