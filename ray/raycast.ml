
open Ast

exception Error of string

type ray = vector * vector * float (* origin, direction, age *)


let e0 = (1., 0., 0.)
let e1 = (0., 1., 0.)
let e2 = (0., 0., 1.)

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

let full_base v =
	if v = e2 then e0, e1
	else
		let b1 = v ^ e2 in
			b1, v ^ b1

let debug_vect s (x, y, z) = Format.printf "%s = (%f, %f, %f)@." s x y z

let intersection_ray_primitive (origin, dir, len) =
	let dir = normalize dir in
	function
	| Box (c1, c2) -> assert false

	| Cone (p1, r1, p2, r2) -> assert false

	| Sphere (c, r) ->
		let r2 = r *. r in
		let toobj = c -- origin in
		let rad = toobj -- (dir ** (scal toobj dir)) in
		let hitPoint = c -- rad -- (dir ** (r2 -. norm2 rad)) in
		let newlen = len +. norm (hitPoint -- origin) in
			if norm2(rad) > r2
			then fun _ -> (origin, dir, 1.), identity
			else
				let newDir = symetry_axis (hitPoint -- c) dir  in
					let b1, b2 = full_base newDir in
					fun p ->
						let orthDir = interp_cos b1 b2 (p ()) in
						(
							hitPoint,
							interp_cos newDir orthDir (p ()),
							newlen
						),
						identity 

	| Torus (r1, r2) -> assert false

	| Triangle (p1, p2, p3) -> assert false


let rec find_cam = function
		| [] -> raise (Error "Camera not found")
		| (Camera c) :: _ -> c
		| _ :: q -> find_cam q



let save_bmp filename buff =
	let w, h = Array.length buff, Array.length buff.(0) in
	let f = open_out filename in
	let rec write n = function
		| 0 -> ()
		| s -> output_byte f (n mod 256); write (n lsr 8) (s - 1)
	in
		output_char f 'B';
		output_char f 'M';
		write (w * h * 3 + 26) 8;
		write 26 4;
		write 12 4;
		write w 2;
		write h 2;
		write 1 2;
		write 24 2;
		for j = 0 to h - 1 do
			for i = 0 to w - 1 do
				write buff.(i).(j) 3
			done
		done;
		close_out f

let render scene =
	let campos, camla = find_cam scene in
	let camdir = normalize (campos-- camla) ** 10. in
	let w, h = 800, 600 in
	let cast prim x y =
		let b1, b2 = full_base camdir in
		let diff =
			b1 ** ((float_of_int x) /. (float_of_int w)) ++
			b2 ** ((float_of_int y) /. (float_of_int h))
		in

		let (hitpos, color, len), f =
			intersection_ray_primitive
				(campos, camdir ++ diff, 0.)
				prim
				(fun () -> 0.5)
		in
			len
	in		
	let aux x y =
		let iter a = function
			| Object (Primitive p, _) -> a +. (cast p x y)
			| _ -> a
		in
			int_of_float (List.fold_left iter 0. scene *. 10.)
	in
		save_bmp "test.bmp" (Array.init w (fun x -> Array.init h (aux x)))






