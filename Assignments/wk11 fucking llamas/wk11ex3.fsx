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
        |[]                        -> []
        |[x]                       -> (x::current)::acc
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

let rec jr ls n acc =
    match ls with
    |[]                     -> []
    |R::L::O::xyzs when n=0 -> strip ((List.rev acc)@(O::L::R::xyzs))
    |R::L::[] when n=0      -> strip ((List.rev acc)@(O::L::R::[]))
    |x::xyzs                -> jr xyzs (n-1) (x::acc)
    |_                      -> []
;;

let jl ls n acc =
    let rec jl_aux ls n acc =
        match ls with
        |[]                     -> []
        |L::R::O::xyzs when n=0 -> List.rev (strip ((List.rev acc)@(O::R::L::xyzs)))
        |L::R::[] when n=0      -> List.rev (strip ((List.rev acc)@(O::R::L::[])))
        |x::xyzs                -> jl_aux xyzs (n-1)(x::acc)
        |_                      -> []
    in jl_aux (List.rev ls) ((List.length ls)-(n+1)) []
;;

let rec mr ls n acc =
    match ls with
    |[]                -> []
    |R::O::ls when n=0 -> strip ((List.rev acc)@(O::R::ls))
    |x::xs             -> mr xs (n-1) (x::acc)
    |_                 -> []
;;

let rec mr_end ls n acc =
    match ls with
    |[]                -> []
    |R::[] when n=0    -> strip ((List.rev acc)@(O::R::[]))
    |x::xs             -> mr_end xs (n-1) (x::acc)
    |_                 -> []
;;

let ml ls n acc =
    let rec ml_aux ls n acc =
        match ls with
        |[]                -> []
        |L::O::ls when n=0 -> List.rev (strip ((List.rev acc)@(O::L::ls)))
        |x::xs             -> ml_aux xs (n-1) (x::acc)
        |_                 -> []
    in ml_aux (List.rev ls) ((List.length ls)-(n+1)) []
;;

(*let ml_end ls n acc =
    let rec ml_aux ls n acc =
        match ls with
        |[]                -> []
        |L::[] when n=0    -> List.rev (strip ((List.rev acc)@(O::L::[])))
        |x::xs             -> ml_aux xs (n-1) (x::acc)
        |_                 -> []
    in ml_aux (List.rev ls) ((List.length ls)-(n+1)) []
;;*)

let ml_end ls = if List.head ls = L then L::O::(List.tail ls) else [];;

(* QUESTION 3 *)

let rec test move n ls =
    (* if can do move, true, else carry on until end reached *)
    if n < (List.length ls) 
    then
        if (move ls n [] = [])
        then test move (n+1) ls
        else (true,n)
    else (false,-1)
;;

let rec solver2 ls moves =
    if solved ls then Some (List.rev moves)

    else
    (*JR*)
    let tup = test jr 0 ls in
    let lsd = jr ls (snd tup) [] in
    if fst tup then solver2 lsd (((snd tup),JR)::moves)

    else
    (*JL*)
    let tup = test jl 0 ls in
    let lsd = jl ls (snd tup) [] in
    if fst tup then solver2 lsd (((snd tup),JL)::moves)

    else
    (*MR mid*)
    let tup = test mr 0 ls in
    let lsd = mr ls (snd tup) [] in
    if fst tup then solver2 lsd (((snd tup),MR)::moves)
    
    else
    (*ML mid*)
    let tup = test ml 0 ls in
    let lsd = ml ls (snd tup) [] in
    if fst tup then solver2 lsd (((snd tup),ML)::moves)

    else
    (*MR end*)
    let len = (List.length ls) - 1 in
    (* if penultimate item is L and last is R then do the move *)
    if (List.head (List.tail (List.rev ls)) = L) && (List.head (List.rev ls) = R) then solver2 (mr_end ls len []) ((len,MR)::moves)
    
    else
    (*ML end*)
    (* if second item is R and first is L then do the move *)
    if List.head (List.tail ls) = L && List.head ls = R then solver2 (ml_end ls) ((0,ML)::moves)

    else
    None
;;

let rec solver ls moves =
    if solved ls then Some (List.rev moves)

    else
    (*JR*)
    let tup = test jr 0 ls in
    let lsd = jr ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),JR)::moves)

    else
    (*JL*)
    let tup = test jl 0 ls in
    let lsd = jl ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),JL)::moves)

    else
    (*ML mid*)
    let tup = test ml 0 ls in
    let lsd = ml ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),ML)::moves)

    else
    (*MR mid*)
    let tup = test mr 0 ls in
    let lsd = mr ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),MR)::moves)

    else
    (*MR end*)
    let len = (List.length ls) - 1 in
    (* if penultimate item is L and last is R then do the move *)
    if (List.head (List.tail (List.rev ls)) = L) && (List.head (List.rev ls) = R) then solver (mr_end ls len []) ((len,MR)::moves)
    
    else
    (*ML end*)
    (* if second item is R and first is L then do the move *)
    if List.head (List.tail ls) = L && List.head ls = R then solver (ml_end ls) ((0,ML)::moves)

    else
    solver2 ls []
;;

let solver3 ls =
    (*MR end*)
    let tup = test mr_end 0 ls in
    if fst tup then Some [((snd tup),MR)]
    
    else
    (*ML end*)
    if List.head ls = L then Some [(0,ML)]

    else
    None

let solve ls = if ls = [] then None else if solved ls then solver3 ls else solver ls [];;

(*  
    STRATEGY
    test jr on all positions of the list
    test jl on all positions of the list
    test mr_middle on all positions of the list
    test ml_middle on all positions of the list

    if (penultimate item = L && last = R) then
    test mr_end on last element of list

    if (first item = L && second item = R) then
    test ml_end on first element of list
*)

(*
solve [R; R; R; R; R; R; O; L; L; L; L; L; L];;
[(5, MR); (7, JL); (8, ML); (6, JR); (4, JR); (3, MR); (5, JL); 
(7, JL); (9, JL); (10, ML); (8, JR); (6, JR); (4, JR); (2, JR);
(1, MR); (3, JL); (5, JL); (7, JL); (9, JL); (11, JL); (12, ML); 
(1, JL); (4, JL); (6, JL); (8, JL); (10, JL); (12, JL); (0, ML);
(3, JL); (5, JL); (7, JL); (9, JL); (11, JL); (10, MR); (8, JR);
(6, JR); (4, JR); (2, JR); (3, ML); (5, JL); (7, JL); (9, JL);
(8, MR); (6, JR); (4, JR); (5, ML); (7, JL)]


solve [R; L; O; L; L; L; R; R; R; L; O; O; R; L];;
[(0, JR); (12, JL); (7, JR); (6, MR); (8, JL); (10, JL); (9, MR);
(7, JR); (5, JR); (6, ML); (8, JL); (7, MR); (0, ML); (3, JL);
(2, MR); (4, JL); (3, MR); (5, JL); (4, MR); (6, JL); (5, MR); (6, JR)]



*)