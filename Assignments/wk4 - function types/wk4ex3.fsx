(*  tne : ((('a -> 'b) -> 'b) -> 'c) -> 'a -> 'c  *)

let dne a f = f a;;
let tne f a = f (dne a);;