
open Ast

exception Error of string

type ray = vector * vector (* origin, direction *)
(* ray direction should always be unitary *)

type color = float * float * float

let infty = max_float




let save_bmp filename buff =
	(**
		`save_bmp filename buff`
		saves the raw image contained in `buff` as a matrix of 24bit integers
		in the file named `filename` under Windows Bitmap Format.
	*)
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


let collision_primitive (origin, dir) =
	(**
		`collision_primitive ray primitive`
		computes the first intersection between a ray and a primitive mesh.
		returns an optionnal hit point, normale couple.
	*)
	let dir = normalize dir in
	function
	| Box (c1, c2) -> assert false

	| Cone (p1, r1, p2, r2) -> assert false

	| Sphere (c, r) ->
		let r2 = r *. r in
		let toobj = c -- origin in
		let rad = toobj -- (dir ** (scal toobj dir)) in
			if norm2(rad) > r2
			then None
			else
				let hitPoint = c -- rad -- (dir ** (r2 -. norm2 rad)) in
				let normale = normalize (hitPoint -- c) in
					Some (hitPoint, normale)

	| Torus (r1, r2) -> assert false

	| Triangle (p1, p2, p3) -> assert false


let rec collision_scene ray = function
	(**
		`collision_scene ray scene`
		computes the first intersection of between ray and a scene.
		returns an optionnal hit point, normale couple.
	*)
	| [] -> None
	| (Object (Primitive p, m)) :: q -> (
		match collision_primitive ray p, collision_scene ray q with
			| None, None -> None
			| Some (hp, n), None -> Some (hp, n, m)
			| None, Some r -> Some r
			| Some (hp, n), Some (hp', n', m') -> (* On voit l'objet le plus proche *)
				let o = fst ray in
					if norm2 (o -- hp) < norm2 (o -- hp')
					then Some (hp, n, m)
					else Some (hp', n', m')
		)
	| _ :: q -> collision_scene ray q



let rec find_cam = function
		| [] -> raise (Error "Camera not found")
		| (Camera c) :: _ -> c
		| _ :: q -> find_cam q



let diffuse color alpha (origin, dir) hitPoint normale =
	let t = sqrt (abs_float (scal dir normale)) in
		color ** (t *. alpha)

let ambiant color alpha (origin, dir) hitPoint normale =
	color ** alpha

let specular color lightDir alpha (origin, dir) hitPoint normale =
	let lightDir = normalize lightDir in
	let x = max (-. scal lightDir normale) 0. in
	let t = exp (5. *. (log (x *. x))) in
		color ** (t *. alpha)


(* to be inserted into scene object *)
let w, h = 800, 800
let background_color = 0., 0., 0.
let mat = [
	specular (1., 1., 1.) (-1., -1., -1.) (1. /. 2.1);
	diffuse (0., 0., 1.) (0.9 /. 2.1);
	ambiant (0., 0., 1.) (0.2 /. 2.1)
]

let render_ray scene ray =
	(**
		`render_ray scene ray`
		renders the color of a given ray.
		returns a color.
	*)
	match collision_scene ray scene with
		| None -> background_color
		| Some (hitPoint, normale, material) ->
			let material = mat in
			List.fold_left
				(fun c f -> c ++ f ray hitPoint normale)
				(0., 0., 0.)
				material


let int_of_color (r, g, b) =
	(**
		`color_to_int color`
		converts color from rgb float format to 24bit int.
		returns an int.
	*)
	let aux c =
		let x = int_of_float (c *. 255.) in
			min 255 (max 0 x)
	in
		((aux r) * 256 + (aux g)) * 256 + (aux b)


let render_pixel scene x y =
	(**
		`render_pixel scene x y`
		renders the color of a given pixel.
		returns a color.
	*)
	let alpha = 0.4 /. (float_of_int w) in
	let campos, camlookat = find_cam scene in
	let camdir = normalize (campos -- camlookat) in
	let b1, b2 = complete_base camdir in
	let diff =
		b1 ** ((float_of_int (x - w / 2)) *. alpha) ++
		b2 ** ((float_of_int (y - h / 2)) *. alpha)
	in
		int_of_color (render_ray scene (campos, camdir ++ diff))




let render scene =
	(**
		`render scene`
		is the main function of that module. It renders a scene and saves it
		into "test.bmp".
	*)		
	save_bmp
		"test.bmp"
		(Array.init w (fun x -> Array.init h (render_pixel scene x)))







