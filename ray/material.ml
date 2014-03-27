(*
	Material fournit des constructeurs de shaders de base et des moyens de les
	fusionner pour construire les matériaux.
	Un shader est une clôture retournant à partir d'un coeficient (utilisé pour
	pondérer les différents shaders), du rayon venant de la caméra, du point
	où celui-ci touche l'objet et de la normale en ce point une couleur.
	
	 ray -> \   ^ <- normale
	         \  |  *
	          \ | /
	           \|/
	  ----------.------------- <- Surface de l'objet
	            ^ hitPoint
	
	Un matériau est un assemblage de shaders.
	Pour le moment, on ne peut que les sommer, donc il est représenté par
	une liste de shaders.
*)


open Vecutils

type shader = float -> ray -> vector -> vector -> color

let diffuse color alpha ((origin, dir) : ray) (hitPoint : vector) normale =
	let t = sqrt (abs_float (scal dir normale)) in
		color ** (t *. alpha)

let ambiant color alpha (origin, dir) hitPoint normale =
	color ** alpha

let specular color lightDir alpha (origin, dir) hitPoint normale =
	let lightDir = normalize lightDir in
	let x = max (-. scal lightDir normale) 0. in
	let t = exp (5. *. (log (x *. x))) in
		color ** (t *. alpha)

let example = [
	specular (1., 1., 1.) (-1., -1., -1.) (1. /. 2.1);
	diffuse (0., 0., 1.) (0.9 /. 2.1);
	ambiant (0., 0., 1.) (0.2 /. 2.1)
]


let render material ray hitPoint normale =
	List.fold_left
		(fun c f -> c ++ f ray hitPoint normale)
		(0., 0., 0.)
		material



