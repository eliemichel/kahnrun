(*
	Typer n'est pas à proprement parler un typeur, mais simplement un module
	adaptant l'arbre de syntaxe abstraite donné par le parseur pour simplifier
	et optimiser la phase de rendu.
*)

open Ast

exception Error of string


let rec find_cam = function
	| [] -> raise (Error "Camera not found")
	| (Camera c) :: _ -> c
	| _ :: q -> find_cam q

let material_of_pigment p =
	(**
		`material_of_pigment p`
		builds a material from raw pigment information.
	*)
	Material.example


let process ast =
	let objects = Utils.autofilter
		(List.map (function
			| Object (o, p) -> Some (o, material_of_pigment p)
			| _ -> None) ast
		)
	in
	let camera = find_cam ast in
	let background_color = 0., 0., 0. in
	let output_size = 800, 600 in
	let ratio = 0.4 in
	{
		objects = objects;
		camera = camera;
		background_color = background_color;
		output_size = output_size;
		ratio = ratio
	}



