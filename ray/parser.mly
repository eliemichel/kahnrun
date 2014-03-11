%{
	open Ast
%}

%token BOX CONE CYLINDER SPHERE TORUS
%token UNION INTERSECTION DIFFERENCE MERGE
%token LBRACE RBRACE LCHEVRON RCHEVRON
%token COMMA EOF

%token <float> FLOAT

%start <Ast.pAst> scene

%%

position(X):
	x = X { x, ($startpos, $endpos) }

scene:
	decls = decl* EOF { decls }


decl:
	o = obj { (o, []) } (* Ajouter les pigments *)

obj:
	| c = combination { Combination c     }
	| p = primitive   { Primitive (p, []) }

repeat(X, Name):
	| x = X                         { x        }
	| x = X COMMA y = repeat(X, f)  { Combination (Name (x, y)) }

bloc(NAME, Name):
	NAME LBRACE
		x = obj COMMA
		y = repeat(obj, Name)
	RBRACE
		{ Name (x, y) }

combination:
	| b = bloc(UNION, Union)              { b }
	| b = bloc(DIFFERENCE, Difference)    { b }
	| b = bloc(INTERSECION, Ast.Intersection) { b }
	| b = bloc(MERGE, Merge)              { b }

vector:
	| RCHEVRON x = FLOAT COMMA y = FLOAT COMMA z = FLOAT RCHEVRON { (x, y, z) }

primitive:
	|
	BOX LBRACE
		p1 = point COMMA
		p2 = point
	RBRACE
		{ Box (p1, p2) }
	|
	CONE LBRACE
		p1 = point COMMA r1 = FLOAT COMMA
		p2 = point COMMA r2 = FLOAT
	RBRACE
		{ Cone (p1, r1, p2, r2) }
	|
	CYLINDER LBRACE
		p1 = point COMMA
		p2 = point COMMA
		r = FLOAT
	RBRACE
		{ Cone (p1, r, p2, r) }
	|
	SPHERE LBRACE
		c = point COMMA
		r = FLOAT
	RBRACE
		{ Sphere (c, r) }
	|
	TORUS LBRACE
		r1 = FLOAT COMMA
		r2 = FLOAT
	RBRACE
		{ Box (r1, r2) }





