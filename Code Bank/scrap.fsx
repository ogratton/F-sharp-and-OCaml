let rec heads value list =
    match list with
    |[] -> []
    |x::xs -> (value::x)::(heads value xs)

let rec shuffle xs ys =
    match xs,ys with
    |[],ys -> [ys]
    |xs,[] -> [xs]
    |x::xs',y::ys' -> (heads x (shuffle xs' ys))@(heads y (shuffle xs ys'))

let cd list =
    let rec cd' list total =
       match list with
       | []        -> total
       | x::[]     -> total
       | x::y::xys -> if x=y then cd' (y::xys) (total+1) else cd' (y::xys) (total)
    in cd' list 0

let rec append xs ys =
    match xs with
    | [] -> ys
    | x::xs -> x :: (append xs ys)


(* sorts by frequency *)
let rec count x list tot = 
    match list with
    | [] -> tot
    | y::ys -> if (y=x) then count x ys (tot+1) else count x ys tot

let rec remove e xs = 
    match xs with
    | [] -> []
    | x::xs -> if (x=e) then remove e xs else x :: remove e xs

    (* dunt work *)
let rec sortTuple = function
    | []                -> []
    | (x,n)::(y,m)::xys -> if (n<m) then (x,n)::(sortTuple ((y,m)::xys)) 
                            else (y,m)::(sortTuple ((x,n)::xys))
    | (x,n)::xys        -> [(x,n)]

let rec fsts = function
    |[]         -> []
    |(x,n)::xs  -> x::(fsts xs)

let sf list =
    let rec quantify list acc = 
        match list with
        | []      -> acc
        | x :: xs -> quantify (remove x xs) ((x,count x xs 1)::acc)
    in fsts (sortTuple (quantify list []))
