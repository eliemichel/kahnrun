open Format



let save_bmp filename buff =
	(**
		`save_bmp filename buff`
		saves the raw image contained in `buff` as a matrix of 24bit integers
		in the file named `filename` under Windows Bitmap Format.
	*)
	let w, h = Array.length buff, Array.length buff.(0) in
	let f = open_out filename in
	let rec write n = function
		| 0 -> ()
		| s -> output_byte f (n mod 256); write (n lsr 8) (s - 1)
	in
		output_char f 'B';
		output_char f 'M';
		write (w * h * 3 + 26) 8;
		write 26 4;
		write 12 4;
		write w 2;
		write h 2;
		write 1 2;
		write 24 2;
		for j = 0 to h - 1 do
			for i = 0 to w - 1 do
				write buff.(i).(j) 3
			done
		done;
		close_out f




let get_pos start_pos end_pos =
	end_pos.Lexing.pos_lnum,
	start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol,
	end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol

let print_error filename msg start_pos end_pos =
	let line, sp, ep = get_pos start_pos end_pos in
	eprintf
		"File \"%s\", line %d, caracters %d-%d:\n%s@."
		filename
		line
		sp
		ep
		msg


let print_syntax_error filename buf msg =
	print_error
		filename
		msg
		(buf.Lexing.lex_start_p)
		(buf.Lexing.lex_curr_p)

let main filename = 
	let f = open_in filename in
	let buf = Lexing.from_channel f in
	let print_syntax_error = print_syntax_error filename buf in
	(*let print_error = print_error filename in*)
	
	let img =
		try
			let pAst = Parser.scene Lexer.token buf in
			eprintf "Parsing done.@.";
			let scene = Typer.process pAst in
			eprintf "Typing done.@.";
			let s = Raycast.render scene in
              eprintf "!!!@." ; s
		with
		| Lexer.Error err -> (
			print_syntax_error ("Lexing error: " ^ err);
			exit 1
			)
		| Parser.Error -> (
			print_syntax_error
				("Parse error: unexpected token " ^ (Lexing.lexeme buf));
			exit 1
			)
		| Typer.Error err -> (
			print_syntax_error
				("Render error: " ^ err);
			exit 1
			)
	(*	| _ -> exit 2 *)
	in
	
	save_bmp "test.bmp" img;
	exit 0

let () = Arg.parse
	["--root", Arg.Unit (fun () -> ()) , "Run node as root (for Network interface only)"]
	main
	"Usage : ray filename.\n\
	Render a scene.\n\
	The available options are :"



