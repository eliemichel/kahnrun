open Unix
open Utils
open Format

type header =
	| In_port
	| Out_port
	| Unknown
	| Error
	| Ack

let int_of_header = function
	| In_port -> 0
	| Out_port -> 1
	| Error -> 2
	| Ack -> 3
	| Unknown -> 999

let header_of_int = function
	| 0 -> In_port
	| 1 -> Out_port
	| 2 -> Error
	| 3 -> Ack
	| _ -> Unknown




let confirm sock hd =
	let cout = out_channel_of_descr sock in
		output_binary_int cout (int_of_header hd);
		flush cout

let write sock hd value =
	let cout = out_channel_of_descr sock in
		output_binary_int cout (int_of_header hd);
		output_binary_int cout value;
		flush cout

let write_line sock hd line =
	let cout = out_channel_of_descr sock in
		output_binary_int cout (int_of_header hd);
		output_line cout line;
		flush cout


let error sock err =
	write_line sock Error err;
	shutdown sock SHUTDOWN_ALL;
	printf "Protocole Error: %s@." err

let check sock hd =
	let expected_header = int_of_header hd in
	let cin = in_channel_of_descr sock in
	let header = input_binary_int cin in
		if header != expected_header
		then error sock (sprintf "Wrong header: %d received but %d expected" header expected_header)
		else ()


let read sock hd =
	check sock hd;
	let cin = in_channel_of_descr sock in
		input_binary_int cin

let read_int sock =
	let cin = in_channel_of_descr sock in
		input_binary_int cin


let read_header sock =
	header_of_int (read_int sock)



let ack sock = confirm sock Ack

