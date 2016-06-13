let rec enq'' (a,b) = function
|[] -> [(a,b)]
|(x, y)::xs -> if b = y then (a,b)::(x,y)::xs
                else
                begin
                    if b > y then (a,b)::(x,y)::xs
                    else (x,y)::(enq'' (a,b) xs)
                end;;

let enq' (a,b) (q1,q2) = 
    match (q1,q2) with
    |[],[] -> [(a,b)] , []
    |(c,d)::xs,[] -> ( ( (enq'' (a,b) q1) ) , [] )
    |(c,d)::xs , _ -> ((enq'' (a,b) q1),[]);;

let enq (a,b) (q1,q2) = enq' (a,b) (q1,(List.rev q2));;


let deq' q1 = 
    match q1 with
    |(a,b)::xs -> (a,([],xs));;

let deq (q1,q2) = 
    match (q1,q2) with
    |[],[] -> failwith "deq"
    |(a,b)::xs,[] -> (a,(xs,[]))
    |[],(a,b)::xs -> deq' ( List.rev ( (a,b)::xs ) )
    |(a,b)::xs,ys -> (a,(xs,ys));;