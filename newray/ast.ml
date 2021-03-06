
open Vecutils
open Material

(* ------ Parsed AST ------ *)

type position = Lexing.position * Lexing.position

type 'a pos_node = 'a * position

type pigment =
	| RGB of vector
	| Spec of (vector * float)


type primitive =
	| Box of (vector * vector) (* corner 1, corner 2 *)
	| Cone of (vector * length * vector * length) (* base point, base radius, cap point, cap radius *)
	| Sphere of (vector * length) (* center, radius *)
	| Torus of (length * length) (* major, minor *)
	| Triangle of (vector * vector * vector) (* point 1, point 2, point 3 *)
	| Smooth_triangle of (vector * vector * vector * vector * vector * vector)
		(* point 1, normale 1, point 2, normale 2, point 3, normale 3 *)

type combination =
	| Union of obj list
	| Difference of obj list
	| Intersection of obj list
	| Merge of obj list

and obj =
	| Combination of combination
	| Primitive of primitive


type decl =
	| Object of (obj * pigment list)
	| Camera of (vector * vector)

type scene = {
	objects : (obj * material) list;
	camera : vector * vector;
	background_color : color;
	output_size : int * int;
	ratio : float
}




