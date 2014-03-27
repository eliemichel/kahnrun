(*
	Utils fournit des petites fonctions utilitaires qui auraient pu être dans la
	bibliothèque standard.
*)

let rec autofilter = function
	| [] -> []
	| None :: q -> autofilter q
	| Some t :: q -> t :: autofilter q


