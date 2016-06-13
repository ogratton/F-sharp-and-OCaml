(* This block of functions deals with the slumpList *)

let rec unzipFirst = function
    |[]         -> ([])
    |(x,_)::xys ->
        let (xs) = unzipFirst xys in
        (x::xs)
;;
let maxI = function
  |[] -> failwith "maxI"
  | x::xs -> List.fold_left max x xs (*FOLD IS CALLED FOLD_LEFT IN OCAML*)
;;
let rec getTuple x xs =
    match xs with
    |[]                  -> (0,0)
    |(z,y)::zys when z=x -> (z,y)
    |(_,_)::zys          -> getTuple x zys
;;
let bigFirst slumpList = maxI (unzipFirst slumpList);;
let longestSlump slumpList = getTuple (bigFirst slumpList) slumpList;;





(* Everything hereafter generates the slumpList*)

let rec count x xys n =
    match xys with
    |[]               -> n
    |z::xys when z<x  -> count x xys (n+1)
    |z::xys when z>=x -> n
    |z::xys           -> failwith "count"
;;


(* All the slumper functions are completely cheaty just cos the first one is bad *)
let rec slumper6 xs t =
    match xs with
    |[]                 -> [(0,0)]
    |x::y::xys when y<x -> [((count x xys 1), t+5)]@(slumper6 xys (t+1))
    |x::xys             -> slumper6 xys (t+1)
;;
let rec slumper5 xs t =
    match xs with
    |[]                 -> [(0,0)]
    |x::y::xys when y<x -> [((count x xys 1), t+4)]@(slumper6 xys (t+1))
    |x::xys             -> slumper5 xys (t+1)
;;
let rec slumper4 xs t =
    match xs with
    |[]                 -> [(0,0)]
    |x::y::xys when y<x -> [((count x xys 1), t+3)]@(slumper5 xys (t+1))
    |x::xys             -> slumper4 xys (t+1)
;;
let rec slumper3 xs t =
    match xs with
    |[]                 -> [(0,0)]
    |x::y::xys when y<x -> [((count x xys 1), t+2)]@(slumper4 xys (t+1))
    |x::xys             -> slumper3 xys (t+1)
;;
let rec slumper2 xs t =
    match xs with
    |[]                 -> [(0,0)]
    |x::y::xys when y<x -> [((count x xys 1), t+1)]@(slumper3 xys (t+1))
    |x::xys             -> slumper2 xys (t+1)
;;

let rec slumper xs t =
    match xs with
    |[]                 -> [(0,0)]
    |x::y::xys when y<x -> [((count x xys 1), t)]@(slumper2 xys (t+1))
    |x::xys             -> slumper xys (t+1)
;;


(* Final function runs all the above as necessary *)
let slump xs = longestSlump (slumper xs 0);;