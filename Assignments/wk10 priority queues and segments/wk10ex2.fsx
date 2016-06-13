(*let length list =
    let rec aux n list =
        match list with
        | [] -> n
        | _::t -> aux (n+1) t
    in aux 0 list
;;*)

let makeq xs =
    let rec splitter xs n acc =
        match xs with
        |[]    -> failwith "splitter"
        |x::xs -> if n=0 then List.rev acc,List.rev (x::xs) else splitter xs (n-1) (x::acc)
    in splitter xs ((List.length xs)/2) []
;;

let rec enq2 (i,p) (front,back) =
    match List.rev front with
    | []                          -> failwith "enq2"
    | (i2,p2)::backs when p < p2  -> (i,p)::(i2,p2)::backs
    | (i2,p2)::backs when p >= p2 -> [(i2,p2)]@(enq2 (i,p) (front,backs))
    | _::_                        -> failwith "enq2"

let rec enq (i,p) (front,back) =
    match back with
    | []                          -> List.rev (enq2 (i,p) (front,back))
    | (i2,p2)::backs when p < p2  -> (i,p)::(i2,p2)::backs
    | (i2,p2)::backs when p >= p2 -> [(i2,p2)]@(enq (i,p) (front,backs))
    | _::_                        -> failwith "enq"
;;

let deq2 (front,back) =
    match List.rev back with
    | (i,p)::qs -> (i,qs)
    | _         -> failwith "deq"
;;

let deq (front,back) = 
    match front with
    | []        -> deq2 (front,back)
    | (i,p)::qs -> (i,qs)
    | _         -> failwith "deq2"
;;


(*let q = makeq [('z',20);('k',17);('m',14);('y',10);('b',9);('a',3);('c',1)];;*)
let q = makeq [('x',9);('y',3);('z',1)];;

let q = enq ('a',1) q;;
let q = enq ('b',10) q;;
let q = enq ('c',1) q;;

let x, q = deq q;;