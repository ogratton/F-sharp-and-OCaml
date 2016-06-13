(*
fun a -> (fun b -> sqrt(a*.a +. b*.b))1.0 2.0;;
*)

(*
let f g = g (g 1) in let h x = x + x in f h;;
*)

let (|>) x f = f x;;

let sqr x = x*x;;
let dbl x = x+x;;

let hyp x = x |> sqr |> dbl |> float |> sqrt;; (* calcs hypot of a right-angled isosceles of lengths x,x,hyp *)

(*
hyp 5;;
*)