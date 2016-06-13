(* heap trees *)
(* we have to write the functions such that the first element can be treated by the user as 1 *)
(* this is a waste of time what are you doing *)


let l = [|96;90;70;80;75;42;60;17;44;10;72;14|]

let isRoot i = (i+1)=1;;
//let level i = log (i+1);;
let parent i = (i+1)/2;;
let left i = (i+1)*2;;
let right i = ((i+1)*2)+1

let size (array:int []) = array.Length;;
let heapEmpty heap = (size heap) = 0;;
let root heap = if heapEmpty heap then failwith "empty heap in 'root'" else heap.[0]
let lastLeaf heap = if heapEmpty heap then failwith "empty heap in 'lastLeaf'" else heap.[(size heap)-1]