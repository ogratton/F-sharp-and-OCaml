(* for a better solution (tail-recursive), see the revision file *)

let rec isIn x ys = 
    match ys with
    |[]             -> false
    |y::ys when y=x -> true
    |y::ys          -> isIn x ys;;

let rec remove x ys =
    match ys with
    |[]             -> []
    |y::ys when y=x -> remove x ys
    |y::ys          -> [y]@(remove x ys);;


let rec dif (xs:int list) (ys:int list) = 
    match xs,ys with
    |[],[]                    -> []
    |[],ys                    -> ys
    |x::xs, ys when isIn x ys -> dif (remove x xs) (remove x ys)
    |x::xs, ys                -> [x]@(dif xs ys);;



dif [1;2;3;2;3] [3;4;3]   =   [1; 2; 2; 4]

 (* Randomly populate a list of size size *)
let mr size =
    let rnd = System.Random()
    List.init size (fun _ -> rnd.Next (0,10))
;;


dif [8; 8; 5; 6; 5; 9; 8; 3; 6; 9] [1; 2; 2; 5; 0; 8; 5; 4; 3; 5]   =   [6; 9; 6; 9; 1; 2; 2; 0; 4]