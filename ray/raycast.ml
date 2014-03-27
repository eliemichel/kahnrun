(*
	Raycast est le cœur du moteur de rendu. Il définit les procédures de
	calcul de collision entre rayons et objets et décompose les différentes
	étapes du rendu (rendu par pixel, par rayon, etc).
*)

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

	| Triangle (p1, p2, p3) ->
		let v1 = p2 -- p1 in
		let v2 = p3 -- p1 in
		match solve33 (v1, v2, dir) (origin -- p1) with
			| None -> None
			| Some (alpha, beta, gamma) ->
				if
					alpha < 0. || alpha > 1.
					|| beta < 0. || beta > 1.
					|| gamma > 0.
				then None
				else
					let hitPoint = origin -- (dir ** gamma) in
					let normale = normalize (v1 ^ v2) in
						Some (hitPoint, normale)


let rec collision_scene ray = function
	(**
		`collision_scene ray scene`
		computes the first intersection of between ray and a scene.
		returns an optionnal hit point, normale couple.
	*)
	| [] -> None
	| (Primitive p, m) :: q -> (
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

let render_ray scene ray =
	(**
		`render_ray scene ray`
		renders the color of a given ray.
		returns a color.
	*)
	match collision_scene ray scene.objects with
		| None -> scene.background_color
		| Some (hitPoint, normale, material) ->
			Material.render material ray hitPoint normale


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
	let w, h = scene.output_size in
	let alpha = scene.ratio /. (float_of_int w) /. 2. in
	let campos, camlookat = scene.camera in
	let camdir = normalize (campos -- camlookat) in
	let b1, b2 = complete_base camdir in
	let diff i j =
		b1 ** ((float_of_int (2 * x + i - w)) *. alpha) ++
		b2 ** ((float_of_int (2 * y + j - h)) *. alpha)
	in
		int_of_color ((
			(render_ray scene (campos, camdir ++ (diff 0 0))) ++
			(render_ray scene (campos, camdir ++ (diff 0 1))) ++
			(render_ray scene (campos, camdir ++ (diff 1 0))) ++
			(render_ray scene (campos, camdir ++ (diff 1 1)))
		) // 4.)



let render scene =
	(**
		`render scene`
		is the main function of that module. It renders a scene and saves it
		into a matrix.
	*)
	let w, h = scene.output_size in
		Array.init w (fun x -> Array.init h (render_pixel scene x))







