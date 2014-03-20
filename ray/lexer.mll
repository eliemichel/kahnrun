{
	open Lexing
	open Parser
	
	exception Error of string
	
	
	let keywords_assoc = [
		"box",          BOX;
		"cone",         CONE;
		"cylinder",     CYLINDER;
		"sphere",       SPHERE;
		"torus",        TORUS;
		"triangle",     TRIANGLE;
		
		"union",        UNION;
		"intersection", INTERSECTION;
		"difference",   DIFFERENCE;
		"merge",        MERGE;
		
		"camera",       CAMERA;
	]
	
	
	let id_or_keyword =
		let h = Hashtbl.create 97 in
			List.iter (fun (s, t) -> Hashtbl.add h s t) keywords_assoc;
			fun s ->
				try
					Hashtbl.find h (String.lowercase s)
				with Not_found -> raise (Error "Invalid bloc identifier")
	
	
	let convert_hexa c = match Char.lowercase c with
		| '0' -> 0
		| '1' -> 1
		| '2' -> 2
		| '3' -> 3
		| '4' -> 4
		| '5' -> 5
		| '6' -> 6
		| '7' -> 7
		| '8' -> 8
		| '9' -> 9
		| 'a' -> 10
		| 'b' -> 11
		| 'c' -> 12
		| 'd' -> 13
		| 'e' -> 14
		| 'f' -> 15
		|  c  -> raise (Invalid_argument ("convert_hexa : " ^ (String.make 1 c)))
	
	let fromAscii a b =
		let c = (convert_hexa a) lsl 4 + (convert_hexa b) in
			String.make 1 (Char.chr c)
	
	
	let newline lexbuf =
		let pos = lexbuf.lex_curr_p in
			lexbuf.lex_curr_p <- {
					pos with
					pos_lnum = pos.pos_lnum + 1;
					pos_bol = pos.pos_cnum
				}
	
	let convert_float = float_of_string (* TODO: octal and binary *)	
}

let chiffre = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_')(alpha | chiffre | '_')*

let chiffre_octal = ['0'-'7']
let chiffre_hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let flottant_positif =
	  '.' chiffre*
	| '0' ('.' chiffre*)?
	| ['1'-'9'] chiffre* ('.' chiffre*)?
	| '0' chiffre_octal+
	| "0x" chiffre_hexa+
let flottant = '-'? flottant_positif

let whitespace = [' ' '\t']+
let commentaire_simple = "//" [^'\n']* '\n'

rule token = parse
	| '\n'                      { newline lexbuf ; token lexbuf}
	| whitespace                { token lexbuf }
	| ident as id               { id_or_keyword id }
	| flottant as n             { FLOAT (convert_float n) }
	| '{'    { LBRACE }
	| '}'    { RBRACE }
	| ','    { COMMA }
	| '<'    { LCHEVRON }
	| '>'    { RCHEVRON }
	| "/*"   { comment lexbuf }
	| commentaire_simple { newline lexbuf ; token lexbuf }
	| eof    { EOF }
	| _ as c { raise (Error ("unexpected character : " ^ String.make 1 c)) }

and comment = parse
	| '\n'  { newline lexbuf ; comment lexbuf }
	| "*/"  { token lexbuf }
	| _     { comment lexbuf }
	| eof   { raise (Error "unexpected end of file (unterminated commentary)")}


