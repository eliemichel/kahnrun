open Unix

let make_addr serv port =
	let host = (gethostbyname serv).h_addr_list.(0) in
	ADDR_INET (host, port)


let print_sockaddr = function
	| ADDR_INET (addr, port) -> Format.sprintf "%s (port %d)" (string_of_inet_addr addr) port
	| _ -> "[unix]"


let output_line output line =
	output_string output line;
	output_string output "\n";
	flush output

let pause delay =
	try ignore(select [] [] [] delay) with _ -> ()

let random_port () =
	1024 + Random.int 64611

let escape_hyperchan str =
	try
		ignore (String.index str '!');
		raise (Invalid_argument "hyperchan should not contain exclamation mark ('!')")
	with Not_found -> str

let dummy_address = make_addr "localhost" 0


let close_all_fd () =
	Format.eprintf "close all fd@.";
	let dir = opendir "/proc/self/fd" in
	try
		while true do
			let sfd = readdir dir in
			if sfd <> "." && sfd <> ".."
			then (
				let fd : file_descr = Obj.magic (int_of_string sfd) in (* You shall never do that *)
				if fd <> stdin && fd <> stdout && fd <> stderr
				then (
					close fd;
					Format.eprintf "close fd %s@." sfd
				)
			)
		done
	with Not_found -> ()

let implemented = true
