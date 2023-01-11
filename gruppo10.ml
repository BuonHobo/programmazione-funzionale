type 'a graph = ('a * 'a) list

let prossimi grafo nodo =
  List.filter_map (function f, t -> if f = nodo then Some t else None) grafo

let vicini grafo nodo =
  List.filter_map
    (function
      | f, t when f = nodo ->
          Some t
      | f, t when t = nodo ->
          Some f
      | _, _ ->
          None)
    grafo

let test_connessi grafo n m =
  let rec aux_nodo nodo visitati =
    nodo = m
    || (not (List.mem nodo visitati))
       && aux_prossimi (prossimi grafo nodo) (nodo :: visitati)
  and aux_prossimi nodi visitati =
    match nodi with
    | [] ->
        false
    | a :: rest ->
        aux_nodo a visitati || aux_prossimi rest visitati in
  aux_prossimi (prossimi grafo n) [ n ]

let esiste_ciclo grafo n =
  let rec aux_nodo nodo visitati =
    nodo = n
    || (not (List.mem nodo visitati))
       && aux_prossimi (prossimi grafo nodo) (nodo :: visitati)
  and aux_prossimi nodi visitati =
    match nodi with
    | [] ->
        false
    | a :: rest ->
        aux_nodo a visitati || aux_prossimi rest visitati in
  aux_prossimi (prossimi grafo n) [ n ]

let ciclo grafo n =
  let rec aux_nodo nodo visitati =
    if nodo = n
    then List.rev (nodo :: visitati)
    else if not (List.mem nodo visitati)
    then aux_prossimi (prossimi grafo nodo) (nodo :: visitati)
    else failwith "Not found"
  and aux_prossimi nodi visitati =
    match nodi with
    | [] ->
        failwith "Not found"
    | a :: rest -> (
        try aux_nodo a visitati with _ -> aux_prossimi rest visitati) in
  aux_prossimi (prossimi grafo n) [ n ]

(* Per grafo_connesso basta far partire una visita da un nodo qualsiasi
   e verificare che trova tutti gli altri nodi *)

type 'a graph_lista = 'a list * ('a * 'a) list

let visita_vicini grafo n =
  let rec aux_nodo nodo visitati =
    if List.mem nodo visitati
    then visitati
    else aux_vicini (vicini grafo nodo) (nodo :: visitati)
  and aux_vicini nodi visitati =
    match nodi with
    | [] ->
        visitati
    | a :: rest ->
        aux_vicini rest (aux_nodo a visitati) in
  aux_nodo n []

let grafo_connesso (nodi, archi) =
  match nodi with
  | [] ->
      true
  | _ ->
      List.for_all
        (fun nodo -> List.mem nodo (visita_vicini archi (List.hd nodi)))
        nodi

let rec rimuovi n = function
  | [] ->
      []
  | a :: rest when a = n ->
      rest
  | a :: rest ->
      a :: rimuovi n rest

let cammino grafo lst n m =
  let rec aux_nodo nodo ammessi =
    if not (List.mem nodo ammessi)
    then failwith "nodo non ammesso"
    else if nodo = m
    then if [ nodo ] = ammessi then [ nodo ] else failwith "fail"
    else nodo :: aux_prossimi (prossimi (snd grafo) nodo) (rimuovi nodo ammessi)
  and aux_prossimi nodi ammessi =
    match nodi with
    | [] ->
        failwith "non trovato"
    | a :: rest -> (
        try aux_nodo a ammessi with _ -> aux_prossimi rest ammessi) in
  aux_nodo n lst

let hamiltoniano grafo =
  let lista_nodi = fst grafo in
  if lista_nodi = []
  then failwith "empty"
  else
    let rec aux_nodo nodo non_visitati =
      if not (List.mem nodo non_visitati)
      then failwith "already visited"
      else if [ nodo ] = non_visitati
      then [ nodo ]
      else
        nodo
        :: aux_prossimi (prossimi (snd grafo) nodo) (rimuovi nodo non_visitati)
    and aux_prossimi nodi non_visitati =
      match nodi with
      | [] ->
          failwith "not found"
      | a :: rest -> (
          try aux_nodo a non_visitati with _ -> aux_prossimi rest non_visitati)
    in

    aux_nodo (List.hd lista_nodi) lista_nodi

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

let colore assoc n = fst (List.find (function _, lst -> List.mem n lst) assoc)

let colori_alterni grafo assoc start goal =
  let colore = colore assoc in
  let prossimi = prossimi grafo in

  let rec aux_nodo nodo visitati ultimo =
    let col = colore nodo in
    if col = ultimo || List.mem nodo visitati
    then failwith "non valido"
    else if nodo = goal
    then List.rev (nodo :: visitati)
    else aux_prossimi (prossimi nodo) (nodo :: visitati) col
  and aux_prossimi nodi visitati ultimo =
    match nodi with
    | [] ->
        failwith "not found"
    | a :: rest -> (
        try aux_nodo a visitati ultimo
        with _ -> aux_prossimi rest visitati ultimo) in
  aux_prossimi (prossimi start) [ start ] (colore start)

let rec connessi_in_glist grafi b c =
  b <> c
  &&
  match grafi with
  | [] ->
      false
  | grafo :: rest ->
      test_connessi grafo b c || connessi_in_glist rest b c

let cammino_con_nodi grafo n lst =
  let rec aux_nodo nodo visitati da_visitare =
    if List.mem nodo visitati
    then failwith "già visitato"
    else if [ nodo ] = da_visitare
    then List.rev (nodo :: visitati)
    else
      aux_prossimi (prossimi grafo nodo) (nodo :: visitati)
        (rimuovi nodo da_visitare)
  and aux_prossimi nodi visitati da_visitare =
    match nodi with
    | [] ->
        failwith "non trovato"
    | a :: rest -> (
        try aux_nodo a visitati da_visitare
        with _ -> aux_prossimi rest visitati da_visitare) in
  aux_nodo n [] lst

let is_primo n =
  let rec aux k = k > n / 2 || (n mod k <> 0 && aux (k + 1)) in
  aux 2

let cammino_di_primi g start goal =
  let rec aux_nodo nodo visitati =
    if (not (is_primo nodo)) || List.mem nodo visitati
    then failwith "non valido"
    else if nodo = goal
    then List.rev (nodo :: visitati)
    else aux_prossimi (prossimi g nodo) (nodo :: visitati)
  and aux_prossimi nodi visitati =
    match nodi with
    | [] ->
        failwith "not found"
    | a :: rest -> (
        try aux_nodo a visitati with _ -> aux_prossimi rest visitati) in
  aux_nodo start []

type form =
  | Prop of string
  | Not of form
  | And of form * form
  | Or of form * form

let is_negazione f1 f2 = f1 = Not f2 || f2 = Not f1

let rec contiene_contraddizione f = function
  | [] ->
      false
  | a :: rest ->
      is_negazione a f || contiene_contraddizione f rest

let non_contradictory_path grafo start goal =
  let rec aux_nodo nodo visitati =
    if List.mem nodo visitati || contiene_contraddizione nodo visitati
    then failwith "non valido"
    else if nodo = goal
    then List.rev (nodo :: visitati)
    else aux_prossimi (prossimi grafo nodo) (nodo :: visitati)
  and aux_prossimi nodi visitati =
    match nodi with
    | [] ->
        failwith "non trovato"
    | a :: rest -> (
        try aux_nodo a visitati with _ -> aux_prossimi rest visitati) in

  aux_nodo start []

let path_n_p grafo p n start =
  let rec aux_nodo nodo visitati k =
    if List.mem nodo visitati
    then failwith "non valido"
    else if k = 1
    then List.rev (nodo :: visitati)
    else
      aux_prossimi (prossimi grafo nodo) (nodo :: visitati)
        (if p nodo then k - 1 else k)
  and aux_prossimi nodi visitati k =
    match nodi with
    | [] ->
        failwith "non trovato"
    | a :: rest -> (
        try aux_nodo a visitati k with _ -> aux_prossimi rest visitati k) in
  aux_nodo start [] n
