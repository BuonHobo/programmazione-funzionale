type 'a ntree = Tr of 'a * 'a ntree list

type multi_expr =
  | MultiInt of int
  | MultiVar of string
  | MultiDiff of multi_expr * multi_expr
  | MultiDiv of multi_expr * multi_expr
  | MultiSum of multi_expr list
  | MultiMult of multi_expr list

let rec subexpr e1 e2 =
  e1 = e2
  ||
  match e1 with
  | MultiDiff (e1a, e1b) -> subexpr e1a e2 || subexpr e1b e2
  | MultiDiv (e1a, e1b) -> subexpr e1a e2 || subexpr e1b e2
  | MultiSum lst -> List.exists (fun e -> subexpr e e2) lst
  | MultiMult lst -> List.exists (fun e -> subexpr e e2) lst
  | _ -> false

let rec subst e1 x e2 =
  match e1 with
  | MultiVar c when c = x -> e2
  | MultiDiff (e1a, e1b) -> MultiDiff (subst e1a x e2, subst e1b x e2)
  | MultiDiv (e1a, e1b) -> MultiDiv (subst e1a x e2, subst e1b x e2)
  | MultiSum lst -> MultiSum (List.map (fun e -> subst e x e2) lst)
  | MultiMult lst -> MultiMult (List.map (fun e -> subst e x e2) lst)
  | _ -> e1

let rec postorder (Tr (v, lst)) = List.concat_map postorder lst @ [ v ]

let rec inorder (Tr (v, lst)) =
  match lst with
  | [] -> [ v ]
  | a :: rest -> inorder a @ (v :: List.concat_map inorder rest)

let rec foglie_in_lista lst (Tr (v, trees)) =
  match trees with
  | [] -> List.mem v lst
  | _ -> List.for_all (foglie_in_lista lst) trees

let rec num_di_foglie (Tr (_, lst)) =
  match lst with
  | [] -> 1
  | a :: rest -> List.fold_left ( + ) 0 (List.map num_di_foglie lst)

let rec listaGuida lst (Tr (x, trees)) =
  match lst with
  | [] -> Tr (x, trees)
  | a :: rest -> listaGuida rest (List.nth trees a)

let rec maxl = function
  | [] -> failwith "empty list"
  | [ a ] -> a
  | first :: second :: rest ->
      let other = maxl (second :: rest) in
      if snd other > snd first then other else first

let rec foglia_costo (Tr (x, lst)) =
  match lst with
  | [] -> (x, x)
  | _ ->
      maxl
        (List.map
           (fun nodo ->
             let label, cost = foglia_costo nodo in
             (label, cost + x))
           lst)

let rec tutte_foglie_costi (Tr (x, lst)) =
  match lst with
  | [] -> [ (x, x) ]
  | _ ->
      List.concat_map
        (fun nodo ->
          List.map (function l, c -> (l, c + x)) (tutte_foglie_costi nodo))
        lst

let rec contiene_duplicati = function
  | [] -> false
  | a :: rest -> List.mem a rest || contiene_duplicati rest

let stessi_elementi lst1 lst2 =
  List.for_all (fun x -> List.mem x lst2) lst1
  && List.for_all (fun x -> List.mem x lst1) lst2

let rec cammini_verso_foglia (Tr (x, trees)) k =
  match trees with
  | [] -> if x = k then [ [ x ] ] else []
  | _ ->
      List.map (List.cons x)
        (List.concat_map (fun nodo -> cammini_verso_foglia nodo k) trees)

(*
Versione bovina

let ramo_da_lista tr lst k =
  List.find
    (fun x -> (not (contiene_duplicati x)) && is_permutazione x lst)
    (cammini_verso_foglia tr k)
*)

let ramo_da_lista tr lst k =
	let rec aux1 cammino (Tr (x, trees)) =
		match trees with
		| [] -> if x = k then cammino @ [ x ] else failwith "Not_found"
		| _ -> aux2 (cammino @ [ x ]) trees

	and aux2 cammino = function
		| [] -> failwith "Not_found"
		| Tr (x, trees) :: rest -> 
			try
				if List.mem x lst && not (List.mem x cammino) 
				then aux1 cammino (Tr (x, trees))
				else failwith "Not_found"
			with _ -> aux2 cammino rest

	in aux1 [] tr

let rec tutti_cammini_foglie (Tr (x, trees)) =
  match trees with
  | [] -> [ [ x ] ]
  | _ -> List.map (List.cons x) (List.concat_map tutti_cammini_foglie trees)

let is_primo n =
  let rec aux j = if j >= n then true else (not (n mod j = 0)) && aux (j + 1) in
  aux 2

let rec last = function
  | [] -> failwith "empty"
  | [ a ] -> a
  | a :: rest -> last rest

(*
Versione bovina

let ramo_di_primi tr =
  last (List.find (List.for_all is_primo) (tutti_cammini_foglie tr))
*)

let ramo_di_primi tr=
	let rec aux1 (Tr(x,trees))=
		if not (is_primo x) 
		then failwith "Not_found"
		else match trees with
			[]-> x
			|_-> aux2 trees
		 
	and aux2 = function
		[]->failwith "Not_found"
		| a::rest -> 
			try aux1 a
			with _-> aux2 rest
	in aux1 tr

(*
Versione bovina

let path_non_pred p tr =
  List.find (List.for_all (fun x -> not (p x))) (tutti_cammini_foglie tr)
*)

let path_non_pred p tr=
	let rec aux1 cammino (Tr(x,trees))=
		if p x
		then failwith "Not_found"
		else match trees with
			[]-> cammino@[x]
			|_-> aux2 (cammino@[x]) trees
	and aux2 cammino = function
		[]->failwith "Not_found"
		| a::rest -> 
			try aux1 cammino a
			with _-> aux2 cammino rest
	in aux1 [] tr

let rec same_structure (Tr (_, lst1)) (Tr (_, lst2)) =
  List.for_all (function a, b -> same_structure a b) (List.combine lst1 lst2)

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

let colore x colass =
  fst (List.find (function _, lst -> List.mem x lst) colass)

let rec is_alternato colass = function
  | [] -> true
  | [ a ] -> true
  | a :: b :: rest ->
      colore a colass <> colore b colass && is_alternato colass (b :: rest)

(*
Versione bovina

let ramo_colorato x colass tr =
  List.find (is_alternato colass) (cammini_verso_foglia tr x)
*)

let ramo_colorato x colass (Tr(y,trees))=

	let rec aux1 cammino ultimo (Tr(y,trees))=
		let col= colore y colass in
		if ultimo = col 
		then failwith "Not_found"
		else match trees with 
			[]-> if y=x then cammino@[y] else failwith "Not_found"
			| _->aux2 cammino col trees
	
	and aux2 cammino ultimo = function
		[]->failwith "Not_found"
		| a::rest->
			try aux1 cammino ultimo a
			with _-> aux2 cammino ultimo rest

	in aux2 [y] (colore y colass) trees 

let leaf x = Tr (x, [])

let t =
  Tr
    ( 1,
      [
        Tr (2, [ Tr (3, [ leaf 4; leaf 5 ]); Tr (6, [ leaf 7 ]); leaf 8 ]);
        leaf 9;
        Tr
          ( 10,
            [
              Tr (11, [ leaf 12; leaf 13; leaf 14 ]);
              leaf 15;
              Tr (16, [ leaf 17; Tr (18, [ leaf 19; leaf 20 ]) ]);
            ] );
      ] )

let t2 =
  Tr
    ( 3,
      [
        Tr (2, [ Tr (4, [ leaf 4; leaf 5 ]); Tr (6, [ leaf 6 ]); leaf 8 ]);
        leaf 9;
        Tr (10, [ Tr (11, [ leaf 12; leaf 1; leaf 14 ]); leaf 15; Tr (16, []) ]);
      ] )
