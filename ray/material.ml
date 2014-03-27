
open Vecutils

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



