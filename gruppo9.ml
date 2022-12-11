type 'a ntree = Tr of 'a * 'a ntree list

type multi_expr =
	MultiInt of int
	| MultiVar of string
	| MultiDiff of multi_expr * multi_expr
	| MultiDiv of multi_expr * multi_expr
	| MultiSum of multi_expr list
	| MultiMult of multi_expr list

let rec subexpr e1 e2=
	e1=e2 ||
	match e1 with
		MultiDiff(e1a,e1b) -> subexpr e1a e2 || subexpr e1b e2
		| MultiDiv(e1a,e1b) -> subexpr e1a e2 || subexpr e1b e2
		| MultiSum(lst) ->List.exists (fun e-> subexpr e e2) lst
		| MultiMult(lst) ->List.exists (fun e-> subexpr e e2) lst
		|_->false

let rec subst e1 x e2=
	match e1 with
		MultiVar(c) when c=x -> e2
		| MultiDiff(e1a,e1b) -> MultiDiff(subst e1a x e2,subst e1b x e2)
		| MultiDiv(e1a,e1b) -> MultiDiv(subst e1a x e2,subst e1b x e2)
		| MultiSum(lst) -> MultiSum(List.map (fun e -> subst e x e2) lst)
		| MultiMult(lst) -> MultiMult(List.map (fun e -> subst e x e2) lst)
		| _ -> e1

let rec postorder (Tr(v,lst))=
	(List.concat_map (postorder) lst)@[v]

let rec inorder (Tr(v,lst))=
	match lst with
		[]->[v]
		|a::rest-> inorder a @ v :: List.concat_map (inorder) rest

let rec foglie_in_lista lst (Tr(v,trees))=
	match trees with
		[] -> List.mem v lst
		| _ -> List.for_all (foglie_in_lista lst) trees