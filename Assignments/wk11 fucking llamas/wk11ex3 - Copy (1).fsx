type llama = L | R | O
type move = ML | MR | JL | JR


(* QUESTION 1 *)

let ls = [L;L;L;O;R;R;R];;

let remove e l =
    let rec go l acc =
        match l with 
        | [] -> List.rev acc 
        | x::xs when e = x -> go xs acc 
        | x::xs -> go xs (x::acc) 
    in go l []
;;

let remove_duplicates l = 
    let rec go l acc =
        match l with 
        | []      -> List.rev acc 
        | x :: xs -> go (remove x xs) (x::acc) 
    in go l []
;;

let sublister xs =
    let rec packer current acc xs =
        match xs with
        |[]                 -> []
        |[x]                -> (x::current)::acc
        |x::(y::_ as xys) when y=x -> packer (x::current) acc xys
        |x::(y::_ as xys)          -> packer [] ((x::current)::acc) xys
    in List.rev (packer [] [] xs)
;;

let aux ls = sublister (remove O ls);;

let solved2 ls = if ls = [] then true else if List.length (aux ls) = 2 && (List.head (List.head (aux ls))) = L then true else false;;

let solved ls = if List.length (remove O (remove_duplicates ls)) < 2 then true else solved2 ls;;

(* QUESTION 2 *)

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
let strip ls = rstrip (lstrip ls);;

let rec play ls moves = 
    if ls = [] 
    then None 
    else
    match moves with
    |[]                -> Some (strip ls)
    |(index,MR)::moves -> play (mr (strip ls) index []) moves
    |(index,ML)::moves -> play (ml (strip ls) index []) moves
    |(index,JR)::moves -> play (jr (strip ls) index []) moves
    |(index,JL)::moves -> play (jl (strip ls) index []) moves
;;

(* QUESTION 3 *)

let rec solver ls acc n =
    if solved ls 
        then Some (List.rev acc)
    else
    match ls with
    |[]    ->   None
    (* priority of moves: JR, JL, MR, ML *)
    |x::xs ->   if (jr ls n []) = [] 
                    then 
                    if (jl ls n []) = []
                        then
                        if (mr ls n []) = []
                            then
                            if (ml ls n []) = []
                                then
                                solver xs acc (n+1)
                            else solver (ml (x::xs) 0 []) ((n,ML)::acc) 0
                        else solver (mr (x::xs) 0 []) ((n,MR)::acc) 0
                    else solver (jl (x::xs) 0 []) ((n,JL)::acc) 0 
                else solver (jr (x::xs) 0 []) ((n,JR)::acc) 0
;;

let solve ls = solver ls [] 0;;