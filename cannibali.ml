type obj = Miss | Cann | Barca
type situazione = obj list * obj list
type azione = From_left of obj list | From_right of obj list

let initial = ([ Miss; Miss; Miss; Cann; Cann; Cann; Barca ], [])

let actions =
  let elems =
    [ [ Miss ]; [ Cann ]; [ Miss; Cann ]; [ Miss; Miss ]; [ Cann; Cann ] ] in
  List.map (function x -> From_left x) elems
  @ List.map (function x -> From_right x) elems

let conta_oggetti sponda =
  let rec aux miss cann barca = function
    | [] ->
        (miss, cann, barca)
    | Miss :: rest ->
        aux (miss + 1) cann barca rest
    | Cann :: rest ->
        aux miss (cann + 1) barca rest
    | Barca :: rest ->
        aux miss cann (barca + 1) rest in
  aux 0 0 0 sponda

let safe_sponda sponda =
  let miss, cann, _ = conta_oggetti sponda in
  miss >= cann

let safe (sx, dx) = safe_sponda sx && safe_sponda dx

let rec crea_lista_n n obj =
  if n = 0 then [] else obj :: crea_lista_n (n - 1) obj

let crea_sponda (miss, cann, barca) =
  if miss < 0 || cann < 0 || barca < 0 || (miss > 0 && cann > miss)
  then failwith "Sponda non valida"
  else
    crea_lista_n barca Barca @ crea_lista_n miss Miss @ crea_lista_n cann Cann

let modifica lst dest op =
  let lst = Barca :: lst in
  let lstmiss, lstcann, lstbarca = conta_oggetti lst in
  let destmiss, destcann, destbarca = conta_oggetti dest in
  crea_sponda (op destmiss lstmiss, op destcann lstcann, op destbarca lstbarca)

let applica act (sx, dx) =
  match act with
  | From_left lst ->
      (modifica lst sx ( - ), modifica lst dx ( + ))
  | From_right lst ->
      (modifica lst sx ( + ), modifica lst dx ( - ))

let from_sit sit =
  List.filter_map
    (fun act -> try Some (applica act sit) with _ -> None)
    actions

let flip (a, b) = (b, a)

let stessa_situazione sit1 sit2 =
  conta_oggetti (fst sit1) = conta_oggetti (fst sit2)
  && conta_oggetti (snd sit1) = conta_oggetti (snd sit2)

let nodo_visitato nodo visitati = List.exists (stessa_situazione nodo) visitati
let goal sit = conta_oggetti (fst initial) = conta_oggetti (snd sit)

let miss_cann () =
  let rec aux_nodo nodo visitati =
    if nodo_visitato nodo visitati
    then failwith "already visited"
    else if goal nodo
    then List.rev (nodo :: visitati)
    else aux_prossimi (from_sit nodo) (nodo :: visitati)
  and aux_prossimi nodi visitati =
    match nodi with
    | [] ->
        failwith "Not found"
    | a :: rest -> (
        try aux_nodo a visitati with _ -> aux_prossimi rest visitati) in
  aux_nodo initial []

let rec mosse n =
  if n = 1 then from_sit initial else List.concat_map from_sit (mosse (n - 1))

let from_sit2 sit =
  List.filter_map
    (fun act -> try Some (act, applica act sit) with _ -> None)
    actions

let miss_cann2 () =
  let rec aux_nodo (act, nodo) visitati =
    if nodo_visitato nodo visitati
    then failwith "already visited"
    else if goal nodo
    then [ act ]
    else act :: aux_prossimi (from_sit2 nodo) (nodo :: visitati)
  and aux_prossimi nodi visitati =
    match nodi with
    | [] ->
        failwith "Not found"
    | a :: rest -> (
        try aux_nodo a visitati with _ -> aux_prossimi rest visitati) in
  aux_prossimi (from_sit2 initial) [ initial ]
