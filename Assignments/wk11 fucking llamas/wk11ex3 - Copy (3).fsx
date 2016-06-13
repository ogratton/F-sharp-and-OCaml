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
    |x::xs             -> mr xs (n-1) (x::acc)
    |_                 -> []
;;

let ml ls n acc =
    let rec ml_aux ls n acc =
        match ls with
        |[]                -> []
        |L::O::ls when n=0 -> List.rev (strip ((List.rev acc)@(O::L::ls)))
        |L::[] when n=0    -> List.rev (strip ((List.rev acc)@(O::L::[])))
        |x::xs             -> ml_aux xs (n-1) (x::acc)
        |_                 -> []
    in ml_aux (List.rev ls) ((List.length ls)-(n+1)) []
;;

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
(* currently infin loops because left most can always move left and same for right *)
(* if there is an O behind something mr'ing or ml'ing and it is on the end, don't do it, as it will cause this loop. This needs to be in the test function *)

(*let rec test move n ls = 
    match ls with
    |[]    -> (false,-1)
    |h::tail -> if (move ls n [] = []) 
                then test move (n+1) tail 
                else (true,n)

;; need extra argument "orig"*)

let rec testj move n ls =
    if n < (List.length ls) 
    then
        if (move ls n [] = [])
        then testj move (n+1) ls
        (* if can do move, TEST FIRST IF MOVE IS MR OR ML AND IS ON THE END AND MOVING AWAY FROM AN EMPTY SPACE. IF SO, SAY FALSE *)
        else (true,n)
    else (false,-1)
;;

let rec testmr n xs ls =
    match xs with
    |[]       ->  (false,-1)
    |O::R::[] ->  testmr (n+1) (List.tail xs) ls
    |x::xs    -> if (mr ls n [] = []) 
                 then testmr (n+1) xs ls 
                 else (true,n)
;;

let testml n ls =
    let r = (List.rev ls) in
    let rec testml_aux n xs ls =
        match xs with
        |[]       ->  (false,-1)
        |O::L::[] ->  testml_aux (n+1) (List.tail xs) ls
        |x::xs    ->  if (ml ls n [] = []) 
                      then testml_aux (n+1) xs ls 
                      else (true,n)
    in testml_aux n r r
;;
    

let rec solver ls moves =
    if solved ls then Some (List.rev moves)

    else
    let tup = testj jr 0 ls in
    let lsd = jr ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),JR)::moves)

    else
    let tup = testj jl 0 ls in
    let lsd = jl ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),JL)::moves)

    else
    let tup = testmr 0 ls ls in
    let lsd = mr ls (snd tup) [] in
    if fst tup then solver lsd (((snd tup),MR)::moves)

    else
    let tup = testml 0 ls in
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