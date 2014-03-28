(*
	Vecutils définit quelques fonctions de manipulation de vecteurs
	tridimensionnels.
*)

type length = float
type vector = length * length * length


type ray = vector * vector (* origin, direction *)
(* ray direction should always be unitary *)

type color = float * float * float


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

let complete_base z =
	(**
		`complete_base Z`
		computes vectors X and Y such as (X, Y, Z) is an orthonormal base and
		Y is as similar as global Z (e2) as possible.
		returns (X, Y).
	*)
	if z = e2 then e0, e1
	else
		let b1 = normalize (e2 ^ z) in
			b1, normalize (z ^ b1)

let debug_vect s (x, y, z) = Format.printf "%s = (%f, %f, %f)@." s x y z

let opp (x, y, z) =
	(-.x, -.y, -.z)

let det2 a b c d = a *. c -. b *. d

let det3 ((a00, a10, a20), (a01, a11, a21), (a02, a12, a22)) =
	a00 *. a11 *. a22 +.
	a01 *. a12 *. a20 +.
	a10 *. a21 *. a02
	-. a02 *. a11 *. a20
	-. a01 *. a10 *. a22
	-. a12 *. a21 *. a00

let solve33 ((c0, c1, c2) as m) x =

	let det = det3 m in
	if det = 0.
	then None
	else
		let a = det3 (x, c1, c2) in
		let b = det3 (c0, x, c2) in
		let c = det3 (c0, c1, x) in
			Some ((a, b, c) // det)



