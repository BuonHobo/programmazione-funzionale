let rec find p=function
	[] -> failwith "EmptyList"
	| a::rest -> 
		if p a then a 
		else 
			try find p rest
		  with _ -> failwith "NotFound"

let find_applicata lst=
	find (fun x->x*x<30) lst

let rec takewhile p= function
	[] -> []
	| a::rest ->
		if p a
		then a::(takewhile p rest)
		else []

let rec dropwhile p= function
	[]->[]
	| a::rest-> 
		if p a
		then (dropwhile p rest)
		else a::rest
	
let partition p lst=

 (* 'a list -> 'a list -> 'a list -> ('a list, 'a list) 
 		aux yes no lst restituisce una coppia di liste.
		La prima contiene tutti gli elementi di yes
		più quelli di lst che rispettano il predicato p.
		La seconda contiene tutti gli elementi di no
		più quelli di lst che non rispettano il predicato p *)
	let rec aux yes no = function
	[] -> (yes,no)
	| a::rest ->
		if p a
		then aux (a::yes) no rest
		else aux yes (a::no) rest
	in aux [] [] lst

let pairwith y lst=
	List.map (fun a-> (y,a)) lst

let verifica_matrice n matrice =
	List.exists (List.for_all ((>) n)) matrice

let setdiff lst1 lst2=
	List.filter (fun x->not (List.mem x lst2)) lst1

let subset set1 set2=
	List.for_all (fun x -> List.mem x set2) set1

let duplica lst=
	List.map (( * )2) lst

let mapcons lst x=
	List.map (function (a,b) -> (a,x::b)) lst

let rec tutte_liste_con num x y=
	if num = 0
	then [[]]
	else 
		let result= tutte_liste_con (num-1) x y in
		(List.map (List.cons x) result) @ (List.map (List.cons y) result);;

let rec interleave x = function 
	[] -> [[x]]
	| a::rest -> 
		(x::a::rest)::List.map (List.cons a) (interleave x rest);;

let rec permut = function
	[]-> [[]]
	| a::rest ->   (*  [[a;b];[b;a]]  *)
		List.flatten (List.map (interleave a) (permut rest))

let in_riga (_,labirinto) riga valore=
	List.exists (function ((_,y),value) -> y=riga && value=valore) labirinto

let trova_colonna (_,labirinto) riga valore =
	let ((col,_),_) = List.find (function ((x,y),value)->y=riga &&value=valore) labirinto
	in col

let in_tutte (dim,labirinto) valore=
	let rec upto num=
		if num<dim
		then num::upto (num+1)
		else []
	in List.for_all (fun x->in_riga (dim,labirinto) x valore) (upto 0)

let find_2 x lst=
	(* 'a list -> 'a list -> ('a list * 'a list)
		 aux lst1 lst2 restituisce una coppia di liste,
		 di cui la prima contiene gli elementi di lst2 fino alla prima occorrenza di x
		 e la seconda contiene gli elementi di ls2 dopo la prima occorrenza di x (x inclusa) *)
	let rec aux prima= function
		[]-> failwith "NotFound"
		| a::rest -> 
			if a <> x
			then aux (a::prima) rest
			else (prima,x::rest)
	in aux [] lst

let spezza x lst=
	let (a,b) = find_2 x (List.tl (snd (find_2 x lst)))
	in (a, List.tl b)

let prendi p lst=
	let x= List.find (p) lst in
	let (a,b) = find_2 x lst in
	(x, a@ (List.tl b))
