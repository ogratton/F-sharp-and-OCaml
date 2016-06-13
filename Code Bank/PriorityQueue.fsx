(* the enqueue function is the hard part *)

let examplei = [("a",10);("z",9);("m",6);("b",1);("p",2);("f",3)] (* example input *)
let exampleq = ([("a",10);("z",9);("m",6)],[("b",1);("p",2);("f",3)]) (* example formed queue *)

let rec sortTup xs =
    match xs with
    | [] -> []
    | (x,p) :: l -> insert (x,p) (sortTup l)
  and insert elem = function
    | [] -> [elem]
    | (x,p) :: l -> if (snd elem) < p then elem :: (x,p) :: l else (x,p) :: insert elem l
;;

let length list =
    let rec aux n list =
        match list with
        | [] -> n
        | _::t -> aux (n+1) t
    in aux 0 list
;;

let makeq list =
    let rec aux list p acc =
        match list,p with
        |[],n     -> failwith "split"
        |x::xs, 0 -> (List.rev list), (List.rev acc)
        |x::xs, n -> aux xs (p-1) (x::acc)
    in aux (sortTup list) ((length list)/2) []
;;

let rec enq_fst elem l =
    match l with
    | []          -> [elem] (* TODO check if it's higher priority than the last thing in snd *)
    | (x,p) :: l  -> if (snd elem) > p then elem :: (x,p) :: l else (x,p) :: enq_fst elem l
;;

(* enq needs to find the correct place for the new tuple *)
let enq e (xs,sx) = (enq_fst e xs), sx

let deq q =
    match q with
    | [],[] -> failwith "deq"
    | xs,x :: sx -> x, (xs, sx)
    | xs,[] -> let sx = List.rev xs in (List.head sx, ([],List.tail sx))

;;

(*

let q = exampleq;;
let q = enq ("i",11) q;;
let q = snd (deq q);;

*)