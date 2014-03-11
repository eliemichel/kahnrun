
(* ------ Parsed AST ------ *)

type position = Lexing.position * Lexing.position

type 'a pos_node = 'a * position

type length = float
type vector = length * length * length

type pigment =
	| RGB of vect
	| Spec of vect * float


type primitive =
	| Box of (vector * vector) (* corner 1, corner 2 *)
	| Cone of (vector * length * vector * length) (* base point, base radius, cap point, cap radius *)
	| Sphere of (vector * length) (* center, radius *)
	| Torus of (length * length) (* major, minor *)

type combination =
	| Union of obj * obj
	| Difference of obj * obj
	| Intersection of obj * obj
	| Merge of obj * obj

and obj =
	| Combination of combination
	| Primitive of primitive

type decl = obj * pigment list

type pAst = decl list

