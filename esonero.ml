type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let bin k tr =
  let rec aux peso cammino = function
    | Empty ->
        failwith "bin"
    | Tr (x, _, _) when x + peso > k ->
        failwith "bin"
    | Tr (x, Empty, Empty) ->
        (List.rev (x :: cammino), x, peso + x)
    | Tr (x, lc, rc) -> (
        try aux (peso + x) (x :: cammino) lc
        with _ -> aux (peso + x) (x :: cammino) rc) in
  aux 0 [] tr

let t =
  Tr
    ( 1,
      Tr (2, Tr (4, Tr (5, Empty, Empty), Empty), Tr (2, Empty, Empty)),
      Tr (9, Empty, Empty) )

type 'a ntree = Tr of 'a * 'a ntree list

let ntr k tr =
  let rec aux_nodo peso cammino (Tr (x, tlst)) =
    if peso + x > k
    then failwith "ntr"
    else
      match tlst with
      | [] ->
          (List.rev (x :: cammino), x, x + peso)
      | _ ->
          aux_lista (peso + x) (x :: cammino) tlst
  and aux_lista peso cammino = function
    | [] ->
        failwith "ntr"
    | a :: rest -> (
        try aux_nodo peso cammino a with _ -> aux_lista peso cammino rest) in
  aux_nodo 0 [] tr

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
