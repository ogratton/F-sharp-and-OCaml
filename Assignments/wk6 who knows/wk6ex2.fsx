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


let rec dif (xs,ys) = 
    match xs,ys with
    |[],[]                    -> []
    |[],ys                    -> ys
    |x::xs, ys when isIn x ys -> dif ((remove x xs),(remove x ys))
    |x::xs, ys                -> [x]@(dif (xs,ys));;


let rec unzip = function
    |[]         -> ([],[])
    |(x,y)::xys ->
        let (xs,ys) = unzip xys in
        (x::xs,y::ys);;

let loop xys = if dif (unzip xys) = [] then true else false;;


(*
loop [(1,2); (2,3); (3,1)];;
*)