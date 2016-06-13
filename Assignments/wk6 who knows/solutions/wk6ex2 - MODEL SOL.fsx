let rec hasnext y edges =
    match edges with
    | []                       -> false
    | (y', z) :: _ when y = y' -> true
    | _ :: ys                  -> hasnext y ys
;;
 
let rec getnext y xs' edges =
    match edges with
    | []                        -> failwith "getnext"
    | (y', z) :: xs when y = y' -> (y', z), xs' @ xs
    | x :: ys                   -> getnext y (x :: xs') ys
;;
 
let rec loop edges =
    match edges with
    | []              -> true
    | [x, y]          -> x = y
    | (x, y) :: edges ->
        hasnext y edges &&
        let (y', z), edges' = 
            getnext y [] edges in
        loop ((x, z) :: edges')
;;

(* loop [(1,2);(2,3);(3,1)] *)