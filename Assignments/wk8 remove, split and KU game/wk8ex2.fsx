let split xs =
    let rec splitter current acc xs =
        match xs with
        |[]                 -> []
        |[x]                -> (x::current)::acc
        |x::(y::_ as xys) when y=x -> splitter (x::current) acc xys
        |x::(y::_ as xys)          -> splitter [] ((x::current)::acc) xys in
    List.rev (splitter [] [] xs)
;;

(* same but without the "as" *)
let split' xs =
    let rec splitter current acc xs =
        match xs with
        |[]                     -> []
        |[x]                    -> (x::current)::acc
        |x::(y::xys) when y=x   -> splitter (x::current) acc (y::xys)
        |x::(y::xys)            -> splitter [] ((x::current)::acc) (y::xys)
    in List.rev (splitter [] [] xs) 