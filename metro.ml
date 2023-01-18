type metro = (int * int * string) list

let vicini stazione m =
  List.filter (function a, b, _ -> a = stazione || b = stazione) m
  |> List.map (function a, b, c -> if a = stazione then (b, c) else (a, c))

let raggiungi m maxc start goal =
  let rec aux visitati cambi linea = function
    | (stazione, _) :: rest when List.mem stazione visitati ->
        aux visitati cambi linea rest
    | (stazione, _) :: rest when stazione = goal ->
        List.rev (stazione :: visitati)
    | (stazione, nuova_linea) :: rest when cambi > 0 || linea = nuova_linea -> (
        try
          aux (stazione :: visitati)
            (if linea = nuova_linea then cambi else cambi - 1)
            nuova_linea (vicini stazione m)
        with _ -> aux visitati cambi linea rest)
    | _ ->
        failwith "fail"
  in

  aux [] maxc "" [ (start, "") ]

type color = Rosso | Verde | Neutro

let colore x col_assoc =
  try List.find (function num, col -> num = x) col_assoc |> snd
  with _ -> Neutro

let vicini nodo g =
  List.filter (function a, b -> a = nodo || b = nodo) g
  |> List.map (function a, b -> if a = nodo then b else a)

let path g colors lst start =
  if lst = []
  then [ start ]
  else
    let rec aux visitati (prox :: prossimi) = function
      | [] ->
          failwith "fail"
      | nodo :: rest when List.mem nodo visitati ->
          aux visitati (prox :: prossimi) rest
      | nodo :: rest when prossimi = [] && colore nodo colors = prox ->
          List.rev (nodo :: visitati)
      | nodo :: rest -> (
          let colori =
            if colore nodo colors = prox then prossimi else prox :: prossimi
          in
          try aux (nodo :: visitati) colori (vicini nodo g)
          with _ -> aux visitati colori rest)
    in
    aux [] lst [ start ]

let grafo_6 =
  [
    (1, 2);
    (1, 3);
    (2, 3);
    (3, 4);
    (3, 5);
    (2, 5);
    (5, 6);
    (5, 7);
    (6, 7);
    (7, 8);
  ]

let colors = [ (2, Rosso); (3, Verde); (4, Verde); (6, Verde); (7, Rosso) ]

let vicini nodo grafo =
  grafo
  |> List.filter (function a, b -> a = nodo || b = nodo)
  |> List.map (function a, b -> if a = nodo then b else a)

let esci (g, mostri) k ingresso uscita =
  let rec aux k visitati = function
    | [] ->
        failwith "fail"
    | nodo :: rest when List.mem nodo visitati || (k < 1 && List.mem nodo mostri) ->
        aux k visitati rest
    | nodo :: rest when nodo = uscita ->
        List.rev (nodo :: visitati)
    | nodo :: rest ->
        try aux (if List.mem nodo mostri then k - 1 else k) (nodo :: visitati) (vicini nodo g)
        with _ -> aux k visitati rest
  in aux k [] [ ingresso ]

let archi= [1,2;2,3;3,4;4,5;2,7;4,7;1,6;6,7;7,8;8,5;]
let mostri = [1;3;4;7]