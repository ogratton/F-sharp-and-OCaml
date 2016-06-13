let rec isIn x ys = 
    match ys with
    |[]             -> false
    |y::ys when y=x -> true
    |y::ys          -> isIn x ys
;;
let rec notEmpty ys = 
    match ys with
    |[]    -> false
    |y::ys -> true
;;



let rec rev sx = function
    |[]    -> sx
    |x::xs -> rev (x::sx) xs
;;
let remove e l =
    let rec remover l acc =
        match l with 
        | []             -> acc 
        | x::xs when e=x -> remover xs acc 
        | x::xs          -> remover xs (x::acc) 
    in remover l []
;;
(* benchmarking remove *)
let _ =
  let l  = mkrlist [] 1000000 in 
  let t0 = Sys.time () in 
  let _ = remove 0 l in 
  let t1 = Sys.time () in
  print_float (t1 -. t0); print_newline ();; (* Remove is a very well optimised function *)





(* non tail recursive version *)
let rec differ' xs ys = match xs with
| []                         -> []
| x :: xs when isIn x ys     -> differ' xs ys
| x :: xs                    -> x :: (differ' xs ys);;

let dif xs ys = (differ' xs ys) @ (differ' ys xs);;


(* tail recursive(?) version  but takes 50 seconds for 20000....*)
let sim_dif xs ys =
    let rec differ xs ys acc =
        match xs with
        | []                   -> acc
        | x::xs when isIn x ys -> differ xs ys acc
        | x::xs                -> differ xs ys (x::acc)
    in (differ xs ys []) @ (differ ys xs [])
;;
(* tail recursive(?) version  but takes 25 seconds for 20000....*)
let sim_dif xs ys =
    let rec differ' xs ys acc =
        match xs,ys with
        |[]   , []                -> acc
        |[]   , ys                -> ys@acc
        |x::xs, ys when isIn x ys -> differ' (remove x xs) (remove x ys) acc
        |x::xs, ys                -> differ' xs ys (x::acc)
    in differ' xs ys []
;;
(* this is just a copy of the one above but hopefully eventually it will metamorphosise into a beautiful answer *)
let sim_dif xs ys =
    let rec differ' xs ys acc =
        match xs with
        |[] when notEmpty ys  -> ys@acc
        |[]                   -> acc
        |x::xs when isIn x ys -> differ' (remove x xs) (remove x ys) acc
        |x::xs                -> differ' xs ys (x::acc)
    in differ' xs ys []
;;









(* benchmarking *)

let rec mkrlist xs = function
  | 0 -> xs
  | n -> mkrlist (Random.bits () :: xs) (n-1);;

let _ =
  let l  = mkrlist [] 20000 in 
  let m  = mkrlist [] 20000 in 
  let t0 = Sys.time () in 
  let _ = sim_dif l m in 
  let t1 = Sys.time () in
  print_float (t1 -. t0); print_newline ();;


