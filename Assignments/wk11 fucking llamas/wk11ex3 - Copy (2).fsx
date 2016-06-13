type llama = L | R | O
type move = ML | MR | JL | JR


(* QUESTION 1 *)
(* if shorter way is needed, assume solved unless there is an L in the tail after an R *)


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

(* strip trailing Os *)
let rec lstrip ls = if List.head ls = O then lstrip (List.tail ls) else ls;;
let rec rstrip ls = if List.head (List.rev ls) = O then rstrip (List.rev (List.tail (List.rev ls))) else ls;;
let strip ls = rstrip (lstrip ls);;

let rec mr ls n acc =
    match ls with
    |[]                -> []
    |R::O::ls when n=0 -> strip ((List.rev acc)@(O::R::ls))
    |R::[] when n=0    -> strip ((List.rev acc)@(O::R::[]))
    |x::xys         -> mr xys (n-1) (x::acc)
    |_                 -> []
;;

let rec ml ls n acc =
    match ls with
    |[]                 -> []
    |O::L::xys when n=1 -> strip ((List.rev acc)@(L::O::xys))
    |L::xys when n=0    -> strip ((List.rev acc)@(L::O::xys))
    |x::y::xys          -> ml (y::xys) (n-1) (x::acc)
    |_                  -> []
;;

(*let rec ml ls n acc =
    match ls with
    |[]                                         -> []
    |L::xys when n=0                            -> strip ((List.rev (List.tail acc))@(L::O::xys))
    |L::xys when (n=0) && ((List.head acc) = O) -> strip ((List.rev (List.tail acc))@(L::O::xys))
    |x::xys                                     -> ml xys (n-1) (x::acc)
    |_                                          -> []
;;*)

let rec jr ls n acc =
    match ls with
    |[]                     -> []
    |R::L::O::xyzs when n=0 -> strip ((List.rev acc)@(O::L::R::xyzs))
    |R::L::[] when n=0      -> strip ((List.rev acc)@(O::L::R::[]))
    |x::y::z::xyzs          -> jr (y::z::xyzs)(n-1)(x::acc)
    |_                      -> []
;;

let rec jl ls n acc =
    match ls with
    |[]                     -> []
    |L::xyzs when (n=0) && (List.head acc = R)                                    -> strip ((List.rev (List.tail (List.tail acc)))@(L::R::O::xyzs))
    |L::xyzs when (n=0) && (List.head acc = R) && (List.head (List.tail acc) = O) -> strip ((List.rev (List.tail (List.tail acc)))@(L::R::O::xyzs))
    |x::xyzs                -> jl xyzs (n-1) (x::acc)
    |_                      -> []
;;

(*let rec jl ls n acc =
    match ls with
    |[]                     -> []
    |O::R::L::xyzs when n=2 -> strip ((List.rev acc)@(L::R::O::xyzs))
    |R::L::xyzs when n=1    -> strip ((List.rev acc)@(L::R::O::xyzs))
    |x::y::z::xyzs          -> jl (y::z::xyzs)(n-1)(x::acc)
    |_                      -> []
;;*)

let rec play ls moves = 
    if ls = [] 
    then None 
    else
    match moves with
    |[]                -> Some ls
    |(index,MR)::moves -> play (mr ls index []) moves
    |(index,ML)::moves -> play (ml ls index []) moves
    |(index,JR)::moves -> play (jr ls index []) moves
    |(index,JL)::moves -> play (jl ls index []) moves
;;

(* QUESTION 3 *)

(*let rec test move n ls = 
    match ls with
    |[]    -> (false,-1)
    |h::tail -> if (move ls n [] = []) 
                then test move (n+1) tail 
                else (true,n)

;; why does this not work pls halp*)

let rec test move n ls =
    if n < (List.length ls) 
    then
        if (move ls n [] = []) 
        then test move (n+1) ls 
        else (true,n)
    else (false,-1)
;;

let rec solver ls moves =
    if solved ls then Some (List.rev moves)

    else
    let tup = test jr 0 ls in
    let lsd = jr ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),JR)::moves)

    else
    let tup = test jl 0 ls in
    let lsd = jl ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),JL)::moves)

    else
    let tup = test mr 0 ls in
    let lsd = mr ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),MR)::moves)

    else
    let tup = test ml 0 ls in
    let lsd = ml ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),ML)::moves)

    else
    None
;;

let solve ls = solver ls [];;

(*  
    test jr on all positions of the list
    test jl on all positions of the list
    test mr on all positions of the list
    test ml on all positions of the list
*)