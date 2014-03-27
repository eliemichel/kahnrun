

(* Canonical R^3 base *)
let e0 = (1., 0., 0.)
let e1 = (0., 1., 0.)
let e2 = (0., 0., 1.)

(* Common operations on 3d vectors *)
let (++) (xa, ya, za) (xb, yb, zb) =
	(xa +. xb, ya +. yb, za +. zb)

let (--) (xa, ya, za) (xb, yb, zb) =
	(xa -. xb, ya -. yb, za -. zb)

let (//) (x, y, z) alpha =
	(x /. alpha, y /. alpha, z /. alpha)

let ( ** ) (x, y, z) alpha =
	(alpha *. x, alpha *. y, alpha *. z)


let scal (xa, ya, za) (xb, yb, zb) =
	xa *. xb +. ya *. yb +. za *. zb

let (^) (xa, ya, za) (xb, yb, zb) =
	ya *. zb -. yb *. za,
	za *. xb -. zb *. xa,
	xa *. yb -. xb *. ya


let norm2 v = scal v v
let norm  v = sqrt (norm2 v)

let normalize v =
	v // (norm v)

let identity x = x

let symetry_axis axis v =
	let axis = normalize axis in
	let along = axis ** (scal axis v) in
		along ** 2. -- v

let interp_cos u v t =
	(u ** t) ++ (v ** (1. -. t *. t))

let complete_base v =
	if v = e2 then e0, e1
	else
		let b1 = normalize (v ^ e2) in
			b1, normalize (v ^ b1)

let debug_vect s (x, y, z) = Format.printf "%s = (%f, %f, %f)@." s x y z



