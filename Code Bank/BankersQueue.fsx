type 'a queue = 'a list * 'a list

let enq a (xs,sx) = (a::xs),sx

let deq (xs,sx) =
    match sx with
    | x :: sx -> x, (xs, sx)
    | [] -> if xs = [] 
            then failwith("deq") 
            else let sx = List.rev xs in (List.head sx, ([],List.tail sx))
;;

let makeq n x =
    let rec makeq' n x (xs,sx) =
        match n with
        | 0 -> (xs,sx)
        | 1 -> makeq' (n-1) x (xs,[x]) (* bit of a shit way of getting around the annoying F# type errors *)
        | n -> makeq' (n-1) x (x::xs,[]) 
    in makeq' n x ([],[])
;;

let empty q = (q = ([], []))


(*deq (enq 10 ([],[1;2;3]));;*)