type chiave = Aperta | Chiusa
type cassaforte = chiave list

let gira = function
    Aperta->Chiusa
    |Chiusa->Aperta

let giraPrima =function
    []-> failwith "giraPrima: lista vuota"
    | a::rest -> gira a ::rest

let rec giraDopoChiusa = function
    Chiusa::a::rest-> Chiusa:: gira a::rest
    | a::rest-> a:: giraDopoChiusa rest
    |_-> failwith "giraDopoChiusa: impossibile"

let successori cassa=
    (try [giraPrima cassa] with _-> [])@ (try [giraDopoChiusa cassa] with _-> []);;

let rec nodi n =
    let prec= if n=1 then [[]] else nodi (n-1)
    in (List.map (List.cons Aperta) prec) @ (List.map (List.cons Chiusa) prec)

let archi n=
    List.map (fun cassa-> cassa,successori cassa) (nodi n)

let start n=
    List.init n (fun _-> Chiusa)

let aperta cassa=
    List.for_all ((=)Aperta) cassa

let apri n=
    let rec aux_nodo nodo visitati=
        if List.mem nodo visitati 
        then failwith "combinazione già provata"
        else 
            if aperta nodo 
            then List.rev (nodo::visitati)
            else aux_prossimi (successori nodo) (nodo::visitati)
    and aux_prossimi nodi visitati=
        match nodi with
            []->failwith "non trovato"
            | a::rest -> try aux_nodo a visitati with _-> aux_prossimi rest visitati

    in aux_nodo (start n) []