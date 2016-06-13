let rec zip xs ys = match (xs,ys) with
    |[]   , []    -> []
    |xs   , []    -> xs
    |[]   , ys    -> ys
    |x::xs, y::ys -> x::y::(zip xs ys);;


(* TEST FUNCTION PLS IGNORE

let rec zipEx xs ys = match (xs,ys) with
    |[]   , []    -> []
    |_    , []
    |[]   , _     -> failwith "zipEx"
    |x::xs, y::ys -> (x,y) :: (zipEx xs ys);;
    
*)