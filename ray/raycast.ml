
open Ast


type ray = vector * vector (* origin, direction *)
(* Important note: We assume that the direction vector is always unitary *)



let (++) (xa, ya, za) (xb, yb, zb) =
	(xa +. xb, ya +. yb, za +. zb)

let (--) (xa, ya, za) (xb, yb, zb) =
	(xa -. xb, ya -. yb, za -. zb)

let (//) (x, y, z) alpha =
	(alpha /. x, alpha /. y, alpha /. z)

let ( **) (x, y, z) alpha =
	(alpha *. x, alpha *. y, alpha *. z)


let scal (xa, ya, za) (xb, yb, zb) =
	xa *. xb +. ya *. yb +. za *. zb

let norm2 v = scal v v
let norm  v = sqrt (norm2 v)

let normalize v =
	v // (norm v)

let intersection_ray_primitive (orig, dir) = function
	| Box (c1, c2) ->
		let toobj = c -- orig in
			dir ** (scal toobj dir)
	| Cone (p1, r1, p2, r2) ->
	| Sphere (c, r) ->
	| Torus (r1, r2) ->
	| Triangle (p1, p2, p3) ->





