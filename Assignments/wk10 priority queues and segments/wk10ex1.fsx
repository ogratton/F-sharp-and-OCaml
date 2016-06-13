let q = [];;

let rec enq (i,p) q =
    match q with
    | []                       -> q@[(i,p)]
    | (i2,p2)::qs when p2 < p  -> (i,p)::(i2,p2)::qs
    | (i2,p2)::qs when p2 >= p -> [(i2,p2)]@(enq (i,p) qs)
    | _::_                     -> failwith "enq"
;;

let deq = function
    | (i,p)::qs -> (i,qs)
    | _         -> failwith "deq"
;;


let q = enq ('a',1) q;;
let q = enq ('b',3) q;;
let q = enq ('c',1) q;;

let x, q = deq q;;