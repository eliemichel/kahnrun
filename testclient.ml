open Unix
open Handcut

let () =
	let servout, servin = open_connection master_addr in
		print_endline "Connected.";
		let rec repl () =
			print_string "> ";
			let l = read_line () in
				output_line servin l;
				print_endline (input_line servout);
				if String.compare l "quit" != 0 then repl ()
		in repl ();
		shutdown_connection servout;
		print_endline "Disconnected."


