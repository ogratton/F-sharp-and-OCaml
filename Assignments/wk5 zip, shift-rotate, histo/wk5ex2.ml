let rec shiftrotate xs n = match xs with
    |[]    -> []
    |x::ys -> if n <= 0 then [x]@ys else (shiftrotate (ys@[x]) (n-1));;
    

(*let rec shiftrotate xs n = match xs with
    |[]    -> []
    |x::ys -> if n = 1 then (ys@[x]) else (shiftrotate (ys@[x]) (n-1));;*)

    (* HOW DO I DEAL WITH 0? *)




(* EXAMPLE FUNCTIONS

let rec rev = function
    |[] -> []
    |x::xs -> (rev xs) @ [x];;

*)