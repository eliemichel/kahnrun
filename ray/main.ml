open Format

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
	
	let pAst =
		try
			Parser.scene Lexer.token buf
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
		| _ -> exit 2
	in
	eprintf "Parsing done.@.";
	ignore pAst;
	exit 0

let () = Arg.parse
	[]
	main
	"Usage : ray filename.\n\
	Render a scene.\n\
	The available options are :"



