﻿(* Remove all duplicates from a list *)

let remove_elt e l =
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
        | x :: xs -> go (remove_elt x xs) (x::acc) 
    in go l []
;;
        (* Alternative remove function - probably not tail recursive, idk *)
let rec remove x ys =
    match ys with
    |[]             -> []
    |y::ys when y=x -> remove x ys
    |y::ys          -> [y]@(remove x ys)
;;
