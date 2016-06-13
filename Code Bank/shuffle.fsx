let heads x yss = List.map (fun ys -> x :: ys) yss

let rec shuffle xs ys = 
    match xs, ys with 
    | [], ys -> [ys] 
    | xs, [] -> [xs] 
    | x :: xs', y :: ys' -> (heads x (shuffle xs' ys)) @ (heads y (shuffle xs ys'))

(* make a list of i 'x's*)
let size i x = 
    let rec size i x list =
        if i=0 then list else size (i-1) x (x::list)
    in size i x []

let f n w = if (w<n) && (1<w) then List.length (shuffle (size (w-2) 1) (size (n-(w+1)) 2)) else failwith "invalid parameters"