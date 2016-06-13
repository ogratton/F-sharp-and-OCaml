let state3 = ([1;2;3],([],[]));;

let valid e = function
    | [] -> true
    | x :: xs when x > e -> true 

let rec pop state x pole2 =
    match pole2 with
    |0 when x < List.head (fst state) -> (x:: fst state), ((fst (snd state)), snd (snd state))
    |1 when x < List.head (fst (snd state)) -> (fst state), ((x :: fst (snd state)), (snd (snd state)))
    |2 when x < List.head (snd (snd state)) -> (fst state), ((fst (snd state)), (x :: snd (snd state)))
    |_ -> failwith "oopsie in pop"

let rec move state pole1 pole2 =
    match pole1 with
    |0 -> pop (List.tail (fst state), snd state) (List.head(fst state)) pole2
    |1 -> pop (fst state, ( List.tail(fst (snd state)), snd(snd state))) (List.head(fst(snd state))) pole2
    |2 -> pop (fst state, ( fst(snd state), List.tail(snd(snd state)) )) (List.head(snd(snd state))) pole2
    |_ -> failwith "oopsie in move"