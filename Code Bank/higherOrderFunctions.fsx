(*   ((('a -> 'b) -> 'b) -> 'c) -> 'a -> 'c = <fun>   *)

let twice (f:'a -> 'a) (x:'a) = f (f x);;
(*  val twice : f:('a -> 'a) -> x:'a -> 'a   *)

let twice2 (f : int -> int) = fun (x : int) -> f (f x);;
(*  val twice2 : f:(int -> int) -> x:int -> int  *)

let twice3 ((f : int -> int), (x : int)) : int = f (f x);;

let plus = function (x : int) -> function (y : int) -> x + y
(*  val plus : _arg1:int -> (int -> int)  *)

let MyFunc ((f), (x), (y)) = f x y;;
(*   val MyFunc : f:('a -> 'b -> 'c) * x:'a * y:'b -> 'c   DOESN'T WORK IN OCAML?*)

let myFunc f x y = f x y;;
(*   val myFunc : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun>   *)


(*let dec (a,b) = if true then a else b;;*)

let dec x = 
    let f y = 
        if y then fst x 
        else snd x
    in f;;
