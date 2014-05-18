open Unix

type header =
	| Channel_id
	| Unknown
	| Error

let int_of_header = function
	| Channel_id -> 0
	| Unknown -> 1
	| Error -> 2

let read sock hd =
	


