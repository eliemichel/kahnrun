open Unix
open Utils
open Format

type header =
	| Channel_id
	| Unknown
	| Error
	| Ack

let int_of_header = function
	| Channel_id -> 0
	| Unknown -> 1
	| Error -> 2
	| Ack -> 3



let write0 sock hd =
	let cout = out_channel_of_descr sock in
		output_binary_int cout (int_of_header hd);
		flush cout

let write1 sock hd value =
	let cout = out_channel_of_descr sock in
		output_binary_int cout (int_of_header hd);
		output_binary_int cout value;
		flush cout

let write1line sock hd line =
	let cout = out_channel_of_descr sock in
		output_binary_int cout (int_of_header hd);
		output_line cout line;
		flush cout


let error sock err =
	write1line sock Error err;
	shutdown sock SHUTDOWN_ALL;
	printf "Protocole Error: %s@." err

let read0 sock hd =
	let expected_header = int_of_header hd in
	let cin = in_channel_of_descr sock in
	let header = input_binary_int cin in
		if header != expected_header
		then error sock (sprintf "Wrong header: %d received but %d expected" header expected_header)
		else ()


let read1 sock hd =
	read0 sock hd;
	let cin = in_channel_of_descr sock in
		input_binary_int cin




let ack sock = write0 sock Ack

