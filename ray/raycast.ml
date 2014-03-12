
open Ast


type ray = vector * vector (* origin, direction *)
(* Important note: We assume that the direction vector is always unitary *)

let e0 = (1., 0., 0.)
let e1 = (0., 1., 0.)
let e2 = (0., 0., 1.)

let (++) (xa, ya, za) (xb, yb, zb) =
	(xa +. xb, ya +. yb, za +. zb)

let (--) (xa, ya, za) (xb, yb, zb) =
	(xa -. xb, ya -. yb, za -. zb)

let (//) (x, y, z) alpha =
	(alpha /. x, alpha /. y, alpha /. z)

let ( ** ) (x, y, z) alpha =
	(alpha *. x, alpha *. y, alpha *. z)


let scal (xa, ya, za) (xb, yb, zb) =
	xa *. xb +. ya *. yb +. za *. zb

let (/\) (xa, ya, za) (xb, yb, zb) =
	ya *. zb -. yb *. za,
	za *. xb -. zb *. xa,
	xa *. yb -. xb *. ya


let norm2 v = scal v v
let norm  v = sqrt (norm2 v)

let normalize v =
	v // (norm v)

let identity x = x

let simetry_axis axis v =
	let axis = normalize axis in
	let along = axis ** (scal axis v) in
		along ** 2 -- v

let interp_cos u v t =
	(u ** t) ++ (v ** (1. -. t *. t))

let full_base v =
	if v = e0 then e1, e2
	else
		let b1 = v /\ e0 in
			b1, v /\ b1

let intersection_ray_primitive (orig, dir) = function
	| Box (c1, c2) -> assert false

	| Cone (p1, r1, p2, r2) -> assert false

	| Sphere (c, r) ->
		let r2 = r *. r in
		let toobj = c -- orig in
		let rad = toobj -- dir ** (scal toobj dir) in
			if norm2(rad) > r2
			then fun _ -> (origin, dir), identity
			else
				let hitPoint = c -- rad -- (dir ** (r2 - norm2 rad)) in
				let newDir = symetry_axis (hitPoint -- c) dir  in
					let b1, b2 = full_base newDir in
					fun p ->
						let p1 = p() in
						let orthDir = interp_cos b1 b2 p() in
						(
							hitPoint,
							interp_cos newDir orthDir p()
						),
						identity 

	| Torus (r1, r2) -> assert false

	| Triangle (p1, p2, p3) -> assert false





