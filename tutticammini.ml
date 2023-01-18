let prossimi grafo nodo =
  List.filter_map (function f, t -> if f = nodo then Some t else None) grafo

let prossimi grafo nodo =
  List.map snd (List.filter (function f, t -> f = nodo) grafo)

let cammino grafo start goal =
  let rec aux visitati = function
    | [] ->
        []
    | nodo :: rest when List.mem nodo visitati ->
        aux visitati rest
    | nodo :: rest when nodo = goal ->
        [ [ nodo ] ]
    | nodo :: rest ->
        List.map (List.cons nodo) (aux (nodo :: visitati) (prossimi grafo nodo))
        @ aux visitati rest
  in
  aux [] [ start ]

let cammino grafo start goal =
  let rec aux_nodo visitati nodo =
    if List.mem nodo visitati
    then []
    else if nodo = goal
    then [ [ nodo ] ]
    else
      List.map (List.cons nodo)
        (aux_prossimi (nodo :: visitati) (prossimi grafo nodo))
  and aux_prossimi visitati = function
    | [] ->
        []
    | nodo :: rest ->
        aux_nodo visitati nodo @ aux_prossimi visitati rest
  in
  aux_nodo [] start

let tutti_cammini grafo start goal =
  let rec aux visitati nodo =
    if List.mem nodo visitati
    then []
    else if nodo = goal
    then [ List.rev (nodo :: visitati) ]
    else aux2 (nodo :: visitati) (prossimi grafo nodo)
  and aux2 l2 = function [] -> [] | x :: rest -> aux l2 x @ aux2 l2 rest in
  aux [] start
