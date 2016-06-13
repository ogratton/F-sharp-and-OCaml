(*mr [R;R;O;L;L] 1 [];;*)

type llama = L | R | O
type move = ML | MR | JL | JR

(*let rec mr ls n acc =
    match ls with
    |[]                               -> []
    |x::y::xys when n=0 && x=R && y=O -> (List.rev acc)@(y::x::xys)
    |x::y::xys                        -> mr (y::xys) (n-1) (x::acc)
    |_                                -> failwith "mr"
;;*)

let rec mr ls n acc =
    match ls with
    |[]                -> []
    |R::O::ls when n=0 -> (List.rev acc)@(O::R::ls)
    |x::y::xys         -> mr (y::xys) (n-1) (x::acc)
    |_                 -> failwith "mr"
;;

let rec ml ls n acc =
    match ls with
    |[]                 -> []
    |O::L::xys when n=1 -> (List.rev acc)@(L::O::xys)
    |x::y::xys          -> ml (y::xys) (n-1) (x::acc)
    |_                  -> failwith "ml"
;;

let rec jr ls n acc =
    match ls with
    |[]                     -> []
    |R::L::O::xyzs when n=0 -> (List.rev acc)@(O::L::R::xyzs)
    |R::L::[] when n=0      -> (List.rev acc)@(O::L::R::[])
    |x::y::z::xyzs          -> jr (y::z::xyzs)(n-1)(x::acc)
    |_                      -> failwith "jr"
;;

let rec jl ls n acc =
    match ls with
    |[]                     -> []
    |O::R::L::xyzs when n=2 -> (List.rev acc)@(L::R::O::xyzs)
    |R::L::xyzs when n=2    -> (List.rev acc)@(L::R::O::xyzs)
    |x::y::z::xyzs          -> jl (y::z::xyzs)(n-1)(x::acc)
    |_                      -> failwith "jl"
;;

let rec play ls moves =
    match moves with
    |[]                 -> Some ls
    |(MR,index)::moves  -> play (mr ls index []) moves
    |(ML,index)::moves  -> play (ml ls index []) moves
    |(JR,index)::moves  -> play (jr ls index []) moves
    |(JL,index)::moves  -> play (jl ls index []) moves
    |moves when ls = [] -> None
    |_                  -> failwith "play"
;;