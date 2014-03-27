
open Ast
open Vecutils

exception Error of string

let infty = max_float



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


(* to be inserted into scene object *)
let w, h = 800, 800
let background_color = 0., 0., 0.


let render_ray scene ray =
	(**
		`render_ray scene ray`
		renders the color of a given ray.
		returns a color.
	*)
	match collision_scene ray scene with
		| None -> background_color
		| Some (hitPoint, normale, material) ->
			let material = Material.example in
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
		into a matrix.
	*)		
	Array.init w (fun x -> Array.init h (render_pixel scene x))







