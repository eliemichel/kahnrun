%{
	open Ast
%}

%token BOX CONE CYLINDER SPHERE TORUS TRIANGLE SMOOTH_TRIANGLE
%token UNION INTERSECTION DIFFERENCE MERGE
%token CAMERA
%token LBRACE RBRACE LCHEVRON RCHEVRON
%token COMMA EOF

%token <float> FLOAT

%start <Ast.decl list> scene

%%

position(X):
	x = X { x, ($startpos, $endpos) }

scene:
	decls = decl* EOF { decls }


decl:
	| o = obj    { Object (o, []) } (* Ajouter les pigments *)
	| c = camera { c              }

obj:
	| c = combination { Combination c }
	| p = primitive   { Primitive   p }


combination:
	| UNION LBRACE l = separated_nonempty_list(COMMA, obj) RBRACE
		{ Union l }
	| DIFFERENCE LBRACE l = separated_nonempty_list(COMMA, obj) RBRACE
		{ Difference l }
	| INTERSECTION LBRACE l = separated_nonempty_list(COMMA, obj) RBRACE
		{ Intersection l }
	| MERGE LBRACE l = separated_nonempty_list(COMMA, obj) RBRACE
		{ Merge l }

vector:
	| LCHEVRON x = FLOAT COMMA y = FLOAT COMMA z = FLOAT RCHEVRON { (x, y, z) }

primitive:
	|
	BOX LBRACE
		p1 = vector COMMA
		p2 = vector
	RBRACE
		{ Box (p1, p2) }
	|
	CONE LBRACE
		p1 = vector COMMA r1 = FLOAT COMMA
		p2 = vector COMMA r2 = FLOAT
	RBRACE
		{ Cone (p1, r1, p2, r2) }
	|
	CYLINDER LBRACE
		p1 = vector COMMA
		p2 = vector COMMA
		r = FLOAT
	RBRACE
		{ Cone (p1, r, p2, r) }
	|
	SPHERE LBRACE
		c = vector COMMA
		r = FLOAT
	RBRACE
		{ Sphere (c, r) }
	|
	TORUS LBRACE
		r1 = FLOAT COMMA
		r2 = FLOAT
	RBRACE
		{ Torus (r1, r2) }
	|
	TRIANGLE LBRACE
		p1 = vector COMMA
		p2 = vector COMMA
		p3 = vector
	RBRACE
		{ Triangle (p1, p2, p3) }
	|
	SMOOTH_TRIANGLE LBRACE
		p1 = vector COMMA n1 = vector COMMA
		p2 = vector COMMA n2 = vector COMMA
		p3 = vector COMMA n3 = vector
	RBRACE
		{ Smooth_triangle (p1, n1, p2, n2, p3, n3) }


camera:
	CAMERA LBRACE
		pos = vector COMMA
		lookat = vector
	RBRACE
		{ Camera (pos, lookat) }



