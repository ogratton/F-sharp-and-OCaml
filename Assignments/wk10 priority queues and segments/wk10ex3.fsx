let d = [1;1;2;3;4;3;2;1]

let sumTest xs t =
    let rec summer xs t acc =
        match xs with
        | []                     -> false
        | x::xs when (x+acc) = t -> true
        | x::xs when (x+acc) < t -> summer xs t (x+acc)
        | x::xs when (x+acc) > t -> false
        | _                      -> false
    in summer xs t 0
;;


let rec has_seg xs t =
    match xs with
    |[]    -> false
    |x::xs -> if (sumTest (x::xs) t) then true else has_seg xs t
;;


(*
weed out all the numbers bigger than the target first - or not cos that will change the order...
try the random solution from rosettacode
*)