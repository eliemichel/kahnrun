
open Format
open Unix

let o, i = pipe ()
let ci = in_channel_of_descr o
let co = out_channel_of_descr i


let o', i' = pipe ()
let ci' = in_channel_of_descr o'
let co' = out_channel_of_descr i'

let srv cin =
	let i, cout = Marshal.from_channel cin in
	Marshal.to_channel co' (i + cout) []


let () =
	let th = Thread.create srv ci in
	Marshal.to_channel co' 41 [];
	flush co';
	printf "test@.";
	printf "Résultat : %d@." (Marshal.from_channel ci');
	Thread.join th


