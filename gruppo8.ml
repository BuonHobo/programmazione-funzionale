type expr =
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

let rec subexpr e1 e2 =
  e1 = e2
  ||
  match e1 with
  | Sum (e1a, e1b) ->
      subexpr e1a e2 || subexpr e1b e2
  | Diff (e1a, e1b) ->
      subexpr e1a e2 || subexpr e1b e2
  | Mult (e1a, e1b) ->
      subexpr e1a e2 || subexpr e1b e2
  | Div (e1a, e1b) ->
      subexpr e1a e2 || subexpr e1b e2
  | _ ->
      false

let rec subst_in_expr e1 x e2 =
  match e1 with
  | Var y when x = y ->
      e2
  | Sum (e1a, e1b) ->
      Sum (subst_in_expr e1a x e2, subst_in_expr e1b x e2)
  | Diff (e1a, e1b) ->
      Diff (subst_in_expr e1a x e2, subst_in_expr e1b x e2)
  | Mult (e1a, e1b) ->
      Mult (subst_in_expr e1a x e2, subst_in_expr e1b x e2)
  | Div (e1a, e1b) ->
      Div (subst_in_expr e1a x e2, subst_in_expr e1b x e2)
  | _ ->
      e1

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec reflect = function Empty -> Empty | Tr (x, l, r) -> Tr (x, r, l)

let fulltree n =
  let rec aux piano x =
    if piano < n
    then Tr (x, aux (piano + 1) (2 * x), aux (piano + 1) ((2 * x) + 1))
    else Empty
  in
  aux 0 1

let rec take n = function
  | [] ->
      []
  | a :: rest ->
      if n = 0 then [] else a :: take (n - 1) rest

let rec drop n = function
  | [] ->
      []
  | a :: rest ->
      if n = 0 then a :: rest else drop (n - 1) rest

let rec get n = function
  | [] ->
      failwith "out of bounds"
  | a :: rest ->
      if n = 0 then a else get (n - 1) rest

(*
	balpreorder e balinorder, entrambe di tipo ’a list -> ’a tree. Data
	una lista lst, costruiscono un albero bilanciato con nodi etichettati da
	elementi di lst, in modo tale che
	preorder (balpreorder lst) = lst
	inorder (balinorder lst) = lst
*)

let rec balpreorder = function
  | [] ->
      Empty
  | a :: rest ->
      let n = List.length rest / 2 in
      Tr (a, balpreorder (take n rest), balpreorder (drop n rest))

let rec balinorder = function
  | [] ->
      Empty
  | lst ->
      let n = List.length lst / 2 in
      Tr (List.nth lst n, balinorder (take n lst), balinorder (drop (n + 1) lst))

let rec balinorder lst =
  match lst with
  | [] ->
      Empty
  | lst ->
      Tr
        ( get (List.length lst / 2) lst,
          balinorder (take (List.length lst / 2) lst),
          balinorder (drop ((List.length lst / 2) + 1) lst) )

let rec foglie_in_lista lst = function
  | Empty ->
      true
  | Tr (x, Empty, Empty) ->
      List.mem x lst
  | Tr (_, l, r) ->
      foglie_in_lista lst l && foglie_in_lista lst r

let rec num_foglie = function
  | Empty ->
      0
  | Tr (_, Empty, Empty) ->
      1
  | Tr (_, l, r) ->
      num_foglie l + num_foglie r

let rec segui_bool lst = function
  | Empty ->
      failwith "empty tree"
  | Tr (x, l, r) -> (
      match lst with
      | [] ->
          Tr (x, l, r)
      | true :: rest ->
          segui_bool rest l
      | false :: rest ->
          segui_bool rest r)

let rec foglia_costo = function
  | Empty ->
      failwith "no leaves"
  | Tr (x, Empty, Empty) ->
      (x, x)
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
  | Empty ->
      []
  | Tr (x, Empty, Empty) ->
      [ (x, x) ]
  | Tr (x, l, r) ->
      List.map (function y, c -> (y, x + c)) (foglie_costi l @ foglie_costi r)

(*
type expr =
  | Jolly
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

let rec pattern_matching e p =
  match (e, p) with
  | _, Jolly -> true
  | Sum (e1, e2), Sum (p1, p2) ->
      pattern_matching e1 p1 && pattern_matching e2 p2
  | Diff (e1, e2), Diff (p1, p2) ->
      pattern_matching e1 p1 && pattern_matching e2 p2
  | Mult (e1, e2), Mult (p1, p2) ->
      pattern_matching e1 p1 && pattern_matching e2 p2
  | Div (e1, e2), Div (p1, p2) ->
      pattern_matching e1 p1 && pattern_matching e2 p2
  | _ -> e = p
*)

let rec max_common_subtree t1 t2 =
  match (t1, t2) with
  | Empty, _ | _, Empty ->
      Tr (0, Empty, Empty)
  | Tr (a, al, ar), Tr (b, bl, br) ->
      if a <> b
      then Tr (0, Empty, Empty)
      else Tr (a, max_common_subtree al bl, max_common_subtree ar br)

let rec stessa_struttura t1 t2 =
  match (t1, t2) with
  | Empty, Tr _ | Tr _, Empty ->
      false
  | Tr (_, al, ar), Tr (_, bl, br) ->
      stessa_struttura al bl && stessa_struttura ar br
  | _ ->
      true

let rec preordine = function
  | Empty ->
      []
  | Tr (x, l, r) ->
      x :: (preordine l @ preordine r)

let rec zip l1 l2 =
  match (l1, l2) with
  | [], [] ->
      []
  | a :: arest, b :: brest ->
      (a, b) :: zip arest brest
  | _ ->
      failwith "lists have different length"

let rec is_func = function
  | [] ->
      true
  | (a, b) :: rest -> (
      try b = List.assoc a rest && is_func rest with _ -> is_func rest)

let esiste_mapping t1 t2 =
  stessa_struttura t1 t2 && is_func (List.combine (preordine t1) (preordine t2))

let rec path p = function
  | Empty ->
      []
  | Tr (x, l, r) ->
      if p x
      then failwith "not found"
      else x :: (try path p l with _ -> path p r)

type 'a sostituzione = ('a * 'a tree) list

let rec applica sost = function
  | Empty ->
      Empty
  | Tr (x, Empty, Empty) -> (
      try List.assoc x sost with _ -> Tr (x, Empty, Empty))
  | Tr (x, l, r) ->
      Tr (x, applica sost l, applica sost r)

let rec is_sottoinsieme lst1 lst2 = List.for_all (fun x -> List.mem x lst2) lst1

let rec percorsi_foglie t =
  match t with
  | Empty ->
      []
  | Tr (x, Empty, Empty) ->
      [ [ x ] ]
  | Tr (x, l, r) ->
      List.map (List.cons x) (percorsi_foglie l @ percorsi_foglie r)

let rec path_coprente t lst =
  List.find (is_sottoinsieme lst) (percorsi_foglie t)

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

(*Versione meno efficiente fatta a scopo didattico*)
let colore x colass =
  List.assoc x
    (List.concat_map
       (function color, lst -> List.map (fun x -> (x, color)) lst)
       colass)

let colore x colass =
  fst (List.find (function _, lst -> List.mem x lst) colass)

let rec percorsi_foglia x = function
  | Empty ->
      failwith "Not_found"
  | Tr (y, Empty, Empty) ->
      if x = y then [ [ x ] ] else failwith "Not_found"
  | Tr (y, l, r) ->
      List.map (List.cons y) (percorsi_foglia x l @ percorsi_foglia x r)

let rec is_alternato colass = function
  | a :: b :: rest ->
      colore a colass <> colore b colass && is_alternato colass (b :: rest)
  | _ ->
      true

let path_to x colass t = List.find (is_alternato colass) (percorsi_foglia x t)

let path_to x colass = function
  | Empty ->
      failwith "Empty_tree"
  | Tr (y, l, r) -> (
      let rec aux col_prec = function
        | Empty ->
            failwith "Not_found"
        | Tr (z, Empty, Empty) ->
            let z_col = colore z colass in
            if z_col <> col_prec && x = z
            then [ z ]
            else failwith "Invalid_path"
        | Tr (z, l, r) ->
            let z_col = colore z colass in
            if z_col = col_prec
            then failwith "Invalid_path"
            else z :: (try aux z_col l with _ -> aux z_col r)
      in

      let x_col = colore x colass in
      try aux x_col l with _ -> aux x_col r)

let rec get_rightmost = function
  | Empty ->
      failwith "empty tree"
  | Tr (x, l, r) -> (
      try get_rightmost r with _ -> Tr (x, l, r))

let rec get_leftmost = function
  | Empty ->
      failwith "empty tree"
  | Tr (x, l, r) -> (
      try get_leftmost l with _ -> Tr (x, l, r))

let key = function Tr ((k, _), _, _) -> k | _ -> failwith "empty node"
let value = function Tr ((_, v), _, _) -> v | _ -> failwith "empty node"

let rec abr_check = function
  | Empty ->
      true
  | Tr ((k, _), Empty, Empty) ->
      true
  | Tr ((k, _), l, Empty) ->
      k > key l && k > key (get_rightmost l) && abr_check l
  | Tr ((k, _), Empty, r) ->
      k < key r && k < key (get_leftmost r) && abr_check r
  | Tr ((k, _), l, r) ->
      k > key l
      && k < key r
      && k > key (get_rightmost l)
      && k < key (get_leftmost r)
      && abr_check l && abr_check r

let rec abr_search t chiave =
  match t with
  | Empty ->
      failwith "not found"
  | Tr ((k, v), l, r) ->
      if k = chiave
      then v
      else if chiave > k
      then abr_search r chiave
      else abr_search l chiave

let rec abr_update t (chiave, new_v) =
  match t with
  | Empty ->
      Tr ((chiave, new_v), Empty, Empty)
  | Tr ((k, v), l, r) ->
      if k = chiave
      then Tr ((k, new_v), l, r)
      else if chiave > k
      then Tr ((k, v), l, abr_update r (chiave, new_v))
      else Tr ((k, v), abr_update l (chiave, new_v), r)

let rec abr_delmin = function
  | Empty ->
      failwith "empty tree"
  | Tr ((k, v), l, r) -> (
      try
        let min_k, new_l = abr_delmin l in
        (min_k, Tr ((k, v), new_l, r))
      with _ -> (k, r))

let rec abr_delete t chiave =
  match t with
  | Empty ->
      Empty
  | Tr ((k, _), Empty, Empty) ->
      Empty
  | Tr ((k, v), l, Empty) ->
      if chiave = k then l else Tr ((k, v), abr_delete l chiave, Empty)
  | Tr ((k, v), Empty, r) ->
      if chiave = k then r else Tr ((k, v), Empty, abr_delete r chiave)
  | Tr ((k, v), l, r) ->
      if chiave = k
      then
        let child = get_leftmost r in
        Tr ((key child, value child), l, abr_delete r (key child))
      else if chiave > k
      then Tr ((k, v), l, abr_delete r chiave)
      else Tr ((k, v), abr_delete l chiave, r)

let rec abr_insert tr element =
  match tr with
  | Empty ->
      Tr (element, Empty, Empty)
  | Tr (x, l, r) when element < x ->
      Tr (x, abr_insert l element, r)
  | Tr (x, l, r) ->
      Tr (x, l, abr_insert r element)

let build_abr lst =
  let rec aux tr = function
    | [] ->
        tr
    | a :: rest ->
        aux (abr_insert tr a) rest
  in
  aux Empty lst

let rec inorder = function
  | Empty ->
      []
  | Tr (x, l, r) ->
      inorder l @ (x :: inorder r)

let tree_sort lst = inorder (build_abr lst)
