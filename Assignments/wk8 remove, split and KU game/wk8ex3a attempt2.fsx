type puzzle =
    | K of int
    | U of char
;;

(*
solvable [K 1; K 5; K 8; K 9; U 'a'] [U 'c'; U 'e'; K 8; U 'd' ; U 'c'];; out: true
solvable [U 'b'; K 6; K 9; U 'd'; U 'd'] [U 'e'; K 6; K 9; K 0; U 'd'];; out: true
solvable [c; c; b; b; 1] [e; 5; b; a; 1];; out: true


*)

let rec replace (U y) (K x) xs =
    match xs with
    |[]                 -> xs
    |z::zs when z=(U y) -> [(K x)] @ replace (U y) (K x) (zs)
    |z::zs              -> [z] @ replace (U y) (K x) (zs)
    |_                  -> failwith "shit-for-brains"
;;

let rec scanner bs cs =
    match bs,cs with
    |[],[]                -> []
    |(K x)::xs, (U y)::ys
    |(U y)::xs, (K x)::ys -> [(K x)]@(scanner (replace (U y) (K x) xs) (replace (U y) (K x) ys))
    |(K x)::xs, (K m)::ys -> [(K x)]@(scanner xs ys)
    |(U y)::xs, (U n)::ys -> if (U y) <= (U n) then [(U y)]@(scanner (replace (U n) (U y) xs) (replace (U n) (U y) ys)) else [(U n)]@(scanner (replace (U y) (U n) xs) (replace (U y) (U n) ys))
    | _                   -> failwith "shithead"
;;


let solvable xs ys = if scanner xs ys = scanner ys xs then true else false;;

(*
scanner [U 'x';U 'x';K 0] [K 1;K 2;U 'z'];;
*)