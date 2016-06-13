(*

let rec dif0 (xs: int list) (ys: int list) =
    match xs with
    |[] -> xs
    |x::xs when isIn ys x -> dif0 xs ys
    |x::xs                -> dif0 xs ys;;

let rec dif1 (xs:int list) (ys:int list) =
    match xs, ys with
    |[], ys                -> ys
    |x::xs, y::ys when x=y -> dif1 xs ys
    |x::xs, y::ys          -> dif1 xs [y]@ys
    |_, _                  -> failwith "dif1";;
*)

let rec runenc ys xs =
    match xs, ys with
    | []     , ys                      -> ys
    | x :: xs, (y, n) :: ys when x = y -> runenc ((y, n+1) :: ys) xs
    | x :: xs, ys                      -> runenc ((x, 1) :: ys) xs

let rec rev (sx:int list) = function
    |[] -> sx
    |x::xs -> rev (x::sx) xs;;

let rec remove x xs = 
    match xs with
    |[] -> []
    |z::xs when z = x -> remove x xs
    |z::xs            -> remove x xs@[z];;

let rem' xs = rev [] xs;; 
(* rem' (remove 2 [1;2;3;4;5]);; *)

let rec isIn ys x = 
    match ys with
    |[]               -> false
    |y::ys when x = y -> true
    |y::ys            -> isIn ys x;;

let rec notIn ys x = 
    match ys with
    |[]               -> true
    |y::ys when x = y -> false
    |y::ys            -> isIn ys x;;


let rec dif xs ys = 
    match xs with
    |[]                    -> ys@xs
    |x::xs when isIn ys x  -> dif xs (rem' (remove x ys))
    |x::xs when notIn ys x -> dif xs (xs@[x])
    |_                     -> failwith "dif"
    ;;


(*dif [1;2;3;2;3] [3;4;3];;*)