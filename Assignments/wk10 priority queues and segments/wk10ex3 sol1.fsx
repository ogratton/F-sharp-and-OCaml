(*let sumTest ys t =
    let rec summer ys t sum =
        match ys with
        | [] -> (false,0)
        | x::xs when (x+sum) = t -> (true,(x+sum))
        | x::xs when (x+sum) < t -> summer xs t (x+sum)
        | x::xs when (x+sum) > t -> (false,(x+sum))
        | _                      -> failwith "sumTest"
    in summer ys t 0
;;

let rec has_seg xs t =
    match xs with
    |[]    -> false
    |x::xs -> if (sumTest (x::xs) t) = (true,t) then true else has_seg xs t
;;*)

let rec is_seg lst sum target lst2 =
    match lst2 with
    | []      -> false
    | (x::xs) -> if (x+sum) = target then true else if (x+sum) < target then is_seg lst (x+sum) target xs
                 else
                     match lst with
                     | [] -> false
                     | (newHead::ls) -> is_seg ls (sum-newHead) target (x::xs);;


let has_seg l t = is_seg l 0 t l;;

