open Unix
open Utils
open Params
open Format

type packet =
| Send of int * string (* int: channel id ; string: serialized data *)
| Listen of int (* Listen to incoming packet on a given channel id — 0 for unspecified *)
| Wait of int (* Wait for someone who listen *)
| Ask (* Ask for free channel ids *)
| Alloc of int * int (* Allocate a range of channel ids *)



let handle_inet node =
	()

let rec accepter (sock, handler) =
	let node, addr = accept sock in
		eprintf "Node input from %s@." (print_sockaddr addr);
		let th = Thread.create handler node in
			accepter (sock, handler);
			Thread.join th


let router () =
	eprintf "Starting router...@.";
	let lut : (int, file_descr) Hashtbl.t = Hashtbl.create 17 in

	let interface_inet = sock PF_INET SOCK_STREAM 0 in
	eprintf "Starting inet interface...@.";
	let rec aux () =
		let port = random_port () in
		eprintf "Trying port %d...@." port;
		(
			try bind interface_inet (make_addr "localhost" port)
			with _ -> aux ()
		)
	in aux ();
	listen interface_inet max_chans;
	eprintf "Inet interface runing.@.";


	let interface_local	= sock PF_UNIX SOCK_STREAM 0 in
	eprintf "Starting local interface...@.";
	let rec aux i =
		let path = sprintf "/tmp/kahn%d" i in
		eprintf "Trying path %s...@." path;
		(
			try bind interface_local (ADDR_UNIX path)
			with _ -> aux ()
		)
	in aux 0;
	listen interface_local max_chans;
	eprintf "Local interface runing.@.";


	let th_inet = Thread.create accepter (interface_inet, handle_inet) in
	let th_inet = Thread.create accepter (interface_local, handle_local) in
	eprintf "Router running.@.";
	Thread.join th_inet;
	Thread.join th_local;

	eprintf "Router stoped.@."


