type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec take n = function
  | [] -> []
  | a :: rest -> if n = 0 then [] else a :: take (n - 1) rest

let rec drop n = function
  | [] -> []
  | a :: rest -> if n = 0 then a :: rest else drop (n - 1) rest

let rec get n = function
  | [] -> failwith "out of bounds"
  | a :: rest -> if n = 0 then a else get (n - 1) rest

(*
	balpreorder e balinorder, entrambe di tipo ’a list -> ’a tree. Data
	una lista lst, costruiscono un albero bilanciato con nodi etichettati da
	elementi di lst, in modo tale che
	preorder (balpreorder lst) = lst
	inorder (balinorder lst) = lst
*)

let rec balpreorder = function
  | [] -> Empty
  | a :: rest ->
      let n = List.length rest / 2 in
      Tr (a, balpreorder (take n rest), balpreorder (drop n rest))

let rec balinorder = function
  | [] -> Empty
  | lst ->
      let n = List.length lst / 2 in
      Tr (List.nth lst n, balinorder (take n lst), balinorder (drop (n + 1) lst))

let rec balinorder lst =
  match lst with
  | [] -> Empty
  | lst ->
      Tr
        ( get (List.length lst / 2) lst,
          balinorder (take (List.length lst / 2) lst),
          balinorder (drop ((List.length lst / 2) + 1) lst) )

let rec foglie_in_lista lst = function
  | Empty -> true
  | Tr (x, Empty, Empty) -> List.mem x lst
  | Tr (_, l, r) -> foglie_in_lista lst l && foglie_in_lista lst r

let rec num_foglie = function
  | Empty -> 0
  | Tr (_, Empty, Empty) -> 1
  | Tr (_, l, r) -> num_foglie l + num_foglie r

let rec segui_bool lst = function
  | Empty -> failwith "empty tree"
  | Tr (x, l, r) -> (
      match lst with
      | [] -> Tr (x, l, r)
      | true :: rest -> segui_bool rest l
      | false :: rest -> segui_bool rest r)

let rec foglia_costo = function
  | Empty -> failwith "no leaves"
  | Tr (x, Empty, Empty) -> (x, x)
  | Tr (x, l, Empty) ->
      let el, cl = foglia_costo l in
      (el, cl + x)
  | Tr (x, Empty, r) ->
      let er, cr = foglia_costo r in
      (er, cr + x)
  | Tr (x, l, r) ->
      let el, cl = foglia_costo l in
      let er, cr = foglia_costo r in
      if cl > cr then (el, cl + x) else (er, cr + x)

let rec foglie_costi = function
  | Empty -> []
  | Tr (x, Empty, Empty) -> [ (x, x) ]
  | Tr (x, l, r) ->
		List.map (function (y,c) -> (y,x+c)) ((foglie_costi l) @ (foglie_costi r))

type expr =
Jolly
| Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr

let rec pattern_matching e p= 
	match e,p with
		_, Jolly -> true
		| Sum(e1,e2),Sum(p1,p2)->
			pattern_matching e1 p1 && pattern_matching e2 p2
		| Diff(e1,e2),Diff(p1,p2)->
			pattern_matching e1 p1 && pattern_matching e2 p2
		| Mult(e1,e2),Mult(p1,p2)->
			pattern_matching e1 p1 && pattern_matching e2 p2
		| Div(e1,e2),Div(p1,p2)->
			pattern_matching e1 p1 && pattern_matching e2 p2
		| _-> e=p

let rec max_common_subtree t1 t2=
	match t1,t2 with
		Empty,_ | _,Empty -> 
			Tr(0,Empty,Empty)
		| Tr(a,al,ar), Tr(b,bl,br) ->
			if a<>b 
			then Tr(0,Empty,Empty)
			else Tr(a,max_common_subtree al bl, max_common_subtree ar br)

let rec stessa_struttura t1 t2=
	match t1,t2 with
	Empty,Tr(_) | Tr(_),Empty-> false
	| Tr(_,al,ar), Tr(_,bl,br) ->
		stessa_struttura al bl && stessa_struttura ar br
	| _-> true

let rec preordine= function
	Empty -> []
	| Tr(x,l,r) -> x::((preordine l) @ (preordine r))

let rec zip l1 l2=
	match l1,l2 with
		[],[]->[]
		| a::arest,b::brest -> (a,b)::zip arest brest 
		|_ -> failwith "lists have different length"

let rec is_func = function
	[]-> true
	| (a,b)::rest ->
		try b= List.assoc a rest && is_func rest 
		with _ -> is_func rest

let esiste_mapping t1 t2=
	if not (stessa_struttura t1 t2)
	then false
	else 
		is_func (List.combine (preordine t1) (preordine t2))

let rec path p= function
  Empty-> []
  | Tr(x,l,r)-> 
    if p x 
    then failwith "not found"
    else
      x::
      try path p l
      with _ -> path p r

type 'a sostituzione = ('a*'a tree) list

let rec applica sost = function
  Empty->Empty
  | Tr(x,Empty,Empty)->
    (try List.assoc x sost
    with _ -> Tr(x,Empty,Empty))
  | Tr(x,l,r) -> Tr(x,applica sost l, applica sost r)

let rec is_sottoinsieme lst1 lst2=
  List.for_all (fun x -> List.mem x lst2) lst1

let rec percorsi_foglie t=
  match t with 
  Empty -> []
  | Tr(x,Empty,Empty) -> [[x]]
  | Tr(x,l,r) ->
    List.map (List.cons x) (percorsi_foglie l @ percorsi_foglie r)

let rec path_coprente t lst=
  List.find (is_sottoinsieme lst) (percorsi_foglie t)

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

(*Versione meno efficiente fatta a scopo didattico*)
let colore x colass=
  List.assoc x (
    List.concat_map (
      function color,lst -> List.map (fun x-> (x,color)) lst
    ) colass
  )

let colore x colass=
  fst (List.find (function _,lst -> List.mem x lst) colass)

let rec percorsi_foglia x = function
  Empty  -> failwith "Not_found"
  | Tr(y,Empty,Empty) ->
    if x=y
    then [[x]]
    else failwith "Not_found"
  | Tr(y,l,r) ->
    List.map (List.cons y) ((percorsi_foglia x l) @ (percorsi_foglia x r))

let rec is_alternato colass = function
  |a::b::rest -> 
    (colore a colass <> colore b colass) && is_alternato colass (b::rest) 
  | _-> true

let rec path_to x colass t=
  List.find (is_alternato colass) (percorsi_foglia x t)

type 'a* 'b btree= Empty | Btr of ('a*'b) * ('a*'b) btree * ('a*'b) btree

let key = function
  (Btr((k,_),_,_)) -> k
  | _ -> failwith ""

let rec abr_check = function
  Empty -> true
  | Btr((k,_),l,r) -> k>key l && k<label r && abr_check r && abr_check l