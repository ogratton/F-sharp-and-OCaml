(*
add [m; -1; -m; 2; -m; 8; 9; m];;

add [-48; -12; -27; 38; -2; -3; -10; -38; 43; -8; -42; -m; 43; -6; 35; -m; -m; -34; -44; -15; -41; 7; -44; 0; -28; 22; 25; -15; -28; 17];;
*)

let m = Microsoft.FSharp.Core.int.MaxValue;;
(* for ocaml:
let m = max_int;;
*)

(* HELPER FUNCTIONS THAT ACTUALLY WORK *)
let rec weave xs ys = 
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, y :: ys -> x :: y :: weave xs ys
;;

let rec rev sx = function
    |[]    -> sx
    |x::xs -> rev (x::sx) xs
;;

let rec quicksort = function
    | [] -> []
    | x::xs -> let smaller, larger = List.partition (fun y -> y < x) xs
               in quicksort smaller @ (x::quicksort larger)
;;

let sumTuple xs =
    match xs with
    |(x,y) -> Some (x+y)
;;


(* NOT SO SURE ABOUT THESE ONES THOUGH *)

(* this one needs to catch whether it will go over m, which is kinda the whole point of the question *)
let add' xs =
    let rec addSimple xs acc =
        match xs with
        |[]    -> acc
        |x::xs -> addSimple xs acc+x
    in addSimple xs 0
;;

(* this one splits the numbers into 3 size groups and then alternates the biggest with the smallest *)
let sep xs = 
    let rec separate xs small medium large =
        match xs with
        |x::xs when x < (-m/2) -> separate xs (x::small) medium large 
        |x::xs when x > (m/2)  -> separate xs small medium (x::large)
        |x::xs                 -> separate xs small (x::medium) large
        |[]                    -> ((add' (weave (List.rev small) (List.rev large))), add' (List.rev medium))
    in separate xs [] [] []
;;



let add xs = sumTuple (sep (quicksort xs));;