(* val histo : 'a list -> ('a * int) list *)


let rec count x xs n = 
    match xs with
    |[] -> n
    |z::xs -> if z = x then count x xs (n+1) else count x xs n;;

let rec remove x xs = 
    match xs with
    |[] -> []
    |z::xs -> if z = x then (remove x xs) else z::(remove x xs);;

let rec histo = function
    |[] -> []
    |x::xs -> (histo (remove x xs))@[(x,(count x xs 1))];;
    

let mr size =
    let rnd = System.Random()
    List.init size (fun _ -> rnd.Next (0,10))
;;


(* 

# histo [1;2;3;4;3;2;1;3;4;5;6;7;5];;
-> [(7,1);(6,1);(5,2);(4,2);(3,3);(2,2);(1,2)]

let rec histo = function
    |[] -> []
    |x::xs -> (x,1)::histo xs;; (*obviously change from 1 to the number*)



let rec unzip = function
    |[] -> ([],[])
    |(x,y)::xys ->
        let (xs,ys) = unzip xys in
        (x::xs,y::ys);;


*)

