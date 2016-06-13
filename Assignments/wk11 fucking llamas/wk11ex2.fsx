type llama = L | R | O
type move = ML | MR | JL | JR

(*
play [R;R;O;L;L] [(3,ML);(1,JR);(2,ML);(0,JR);(2,JR);(3,ML);(1,JR)];;
play [R;O;R;L] [(3,JL);(1,JL);(0,ML);(2,MR)];;
play [R;O;L;L;O;O;O;L;O;O;R;L;L;L;R;O;R;R;L;R] [(0,MR);(6,ML);(13,MR);(10,JL);(5,ML)];;
play [R;O;O;L;R;L;R;R;R] [(3,ML);(5,JL);(0,MR);(3,MR)];;
play [R;L;O;O;R;O;R;L;R;L;R;R;L;L;R;R;L;O;L;O;R;R;R;L;O;R;L;R;L;O;O;R;L;R;O;R] [(16,ML);(13,ML);(6,MR)]



*)

let rec mr ls n acc =
    match ls with
    |[]                -> []
    |R::O::ls when n=0 -> (List.rev acc)@(O::R::ls)
    |R::[] when n=0    -> (List.rev acc)@(O::R::[])
    |x::y::xys         -> mr (y::xys) (n-1) (x::acc)
    |_                 -> []
;;

let rec ml ls n acc =
    match ls with
    |[]                 -> []
    |O::L::xys when n=1 -> (List.rev acc)@(L::O::xys)
    |L::xys when n=0    -> (List.rev acc)@(L::O::xys)
    |x::y::xys          -> ml (y::xys) (n-1) (x::acc)
    |_                  -> []
;;

let rec jr ls n acc =
    match ls with
    |[]                     -> []
    |R::L::O::xyzs when n=0 -> (List.rev acc)@(O::L::R::xyzs)
    |R::L::[] when n=0      -> (List.rev acc)@(O::L::R::[])
    |x::y::z::xyzs          -> jr (y::z::xyzs)(n-1)(x::acc)
    |_                      -> []
;;

let rec jl ls n acc =
    match ls with
    |[]                     -> []
    |O::R::L::xyzs when n=2 -> (List.rev acc)@(L::R::O::xyzs)
    |R::L::xyzs when n=1    -> (List.rev acc)@(L::R::O::xyzs)
    |x::y::z::xyzs          -> jl (y::z::xyzs)(n-1)(x::acc)
    |_                      -> []
;;

(* strip trailing Os *)
let rec lstrip ls = if List.head ls = O then lstrip (List.tail ls) else ls 
;;
let rec rstrip ls = if List.head (List.rev ls) = O then rstrip (List.rev (List.tail (List.rev ls))) else ls 
;;

let rec play ls moves = 
    if ls = [] 
    then None 
    else
    match moves with
    |[]                -> Some (rstrip (lstrip ls))
    |(index,MR)::moves -> play (mr (rstrip (lstrip ls)) index []) moves
    |(index,ML)::moves -> play (ml (rstrip (lstrip ls)) index []) moves
    |(index,JR)::moves -> play (jr (rstrip (lstrip ls)) index []) moves
    |(index,JL)::moves -> play (jl (rstrip (lstrip ls)) index []) moves
;;