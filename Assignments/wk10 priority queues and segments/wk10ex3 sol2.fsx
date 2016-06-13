let tail' = function
    | [] -> []
    | x::xs -> xs

let has_seg list target =
    let rec has_seg' list target sub seg =
        match list with
        |x::xs when (x+sub < target) -> has_seg' xs target (x+sub) (seg@[x])
        |x::xs when (x+sub > target) -> has_seg' (x::xs) target (sub-(List.head seg)) (tail' seg)
        |x::xs -> true,seg@[x]
        |[]    -> false,[]
    in has_seg' (tail' list) target (List.head list) [(List.head list)]