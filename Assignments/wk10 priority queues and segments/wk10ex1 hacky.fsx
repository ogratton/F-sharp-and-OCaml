(* int list -> int *)
let sum (x::xs) t = List.fold (fun sum x -> sum + x) t;;
(*argh*)

let p xs t =
    match xs with
    | [] -> false
    | xs -> (sum xs t) = t;;

let take xs set =
    let x = List.nth set (Random.int (List.length set)) in
    (x::xs, List.filter (fun y -> y <> x) set);;

let swap (a,b) = (b,a);;
let pop xs set = swap (take set xs);;


let rec has_seg (xs:int list) (t:int) =
    Random.self_init();
    let rec aux xs t set =
        let f =
            match xs,set with
            | [], _  -> take
            | _ , [] -> pop
            | _      -> if Random.bool () then take else pop
        in
        let xs,set = f xs set in
        if (p xs t) then xs
        else aux xs t set
    in
    let res = aux [] xs in
    true;;