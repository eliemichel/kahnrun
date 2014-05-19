(*
	Utils fournit des petites fonctions utilitaires qui auraient pu être dans la
	bibliothèque standard.
*)

let rec autofilter = function
	| [] -> []
	| None :: q -> autofilter q
	| Some t :: q -> t :: autofilter q


let rec replicate n a = match n with
  | 0 -> []
  | n -> a :: replicate (n - 1) a 


(* Computes the list [a ... b] *)
let rec range a b = if a <= b then a :: range (a + 1) b else []


let rec matIdx w h = 
  let rec aux i j acc = 
    if j >= h then acc
    else if i >= w then aux 0 (j + 1) acc
    else aux (i + 1) j ((i, j) :: acc)
  in
  aux 0 0 []


let uncurry f (i, j) = f i j


let rec splitAt n = function
  | [] -> ([], [])
  | x :: xs -> 
    if n <= 0 then 
      ([], x :: xs) 
    else
      let (r, s) = splitAt (n - 1) xs in
      (x :: r, s)


(* Splits fairly a list [l] in [n] sublists *)
let rec split n l = 

  let rec aux before after l = match l, after with
    | [], _ -> before @ after
    | x :: xs, [] -> aux [] before l
    | x :: xs, g :: gs -> aux ((x :: g) :: before) gs xs

  in aux [] (replicate n []) l
