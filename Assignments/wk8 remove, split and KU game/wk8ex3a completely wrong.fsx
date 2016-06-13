(* 
[U 'x'; U 'y'; U 'x'] [K 3; K 1; K 3];;
zips to [(U 'x', K 3); (U 'y', K 1); (U 'x', K 3)]
*)

type puzzle =
    | K of int
    | U of char
;;

(* zips the two lists into a tuple list *)
let rec zip xs ys =
    match xs,ys with
    |[], [] -> []
    |[], _
    |_ , [] -> failwith "zip"
    |x::xs,y::ys -> (x,y)::(zip xs ys)
;;

(* This will go through all the tuples and return the list, having normalised the tuple order *)
let rec p2 kulist =
    match kulist with
    | []         -> []
    | ((K x),(U y))::kus -> [(U y),(K x)] @ p2 kus
    | ((U y),(K x))::kus -> [(U y),(K x)] @ p2 kus
    | _                  -> failwith "p2"
;;

(* remove a specific element from a list (used in remove_duplicates)*)
let remove_elt e l =
    let rec go l acc =
        match l with 
        | [] -> List.rev acc 
        | x::xs when e = x -> go xs acc 
        | x::xs -> go xs (x::acc) 
    in go l []
;;

(* remove the duplicates in a list *)
let remove_duplicates l = 
    let rec go l acc =
        match l with 
        | []      -> List.rev acc 
        | x :: xs -> go (remove_elt x xs) (x::acc) 
    in go l []
;;

let normalise xs ys = remove_duplicates (p2 (zip xs ys));;

let rec unzipHead xs =
    match xs with
    |[]         -> []
    |(x,y)::xys ->
        let (xs) = unzipHead xys in
        (x::xs)
;;

let unchanged xs =
    if xs = remove_duplicates xs then true
    else false
;;

let solvable xs ys = unchanged (unzipHead (normalise xs ys));;




(*
solvable [K 1; K 5; K 8; K 9; U 'a'] [U 'c'; U 'e'; K 8; U 'd' ; U 'c'];; should be true
*)