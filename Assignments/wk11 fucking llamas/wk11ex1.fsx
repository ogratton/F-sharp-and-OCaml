type llama = L | R | O

let ls = [L;L;L;O;R;R;R];;

let remove e l =
    let rec go l acc =
        match l with 
        | [] -> List.rev acc 
        | x::xs when e = x -> go xs acc 
        | x::xs -> go xs (x::acc) 
    in go l []
;;

let remove_duplicates l = 
    let rec go l acc =
        match l with 
        | []      -> List.rev acc 
        | x :: xs -> go (remove x xs) (x::acc) 
    in go l []
;;

let sublister xs =
    let rec packer current acc xs =
        match xs with
        |[]                 -> []
        |[x]                -> (x::current)::acc
        |x::(y::_ as xys) when y=x -> packer (x::current) acc xys
        |x::(y::_ as xys)          -> packer [] ((x::current)::acc) xys
    in List.rev (packer [] [] xs)
;;

let aux ls = sublister (remove O ls);;

let solved2 ls = if ls = [] then true else if List.length (aux ls) = 2 && (List.head (List.head (aux ls))) = L then true else false;;

let solved ls = if List.length (remove O (remove_duplicates ls)) < 2 then true else solved2 ls;;

