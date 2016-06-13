(*
add [m; -1; -m; 2; -m; 8; 9; m];;

add [-48; -12; -27; 38; -2; -3; -10; -38; 43; -8; -42; -m; 43; -6; 35; -m; -m; -34; -44; -15; -41; 7; -44; 0; -28; 22; 25; -15; -28; 17];;

add [34; 16; -19; 35; -4611686018426896476; -30; 4611686018427349897; 35; -43; 43; 32; 38; -37; 31; 10; -42; 48; -38; -6; 23; 25; -8; 23; -23; 16; -17; -25; 13; 4; -25; 37; 17; -17; -46; -40; 25; -4611686018426786437; 39; 40; -11; 19; -48; -27; 36; -4; 6; 32; 31; -24; 3; -4; -32; 12; 8; -23; 13; 35; -1; -20; 0; 9; 33; 34; 34; -39; 32; -19; 31; 32; 45; 37; 42; -43; -29; 10; -4; -30; -4611686018427355752; -12; -27; 1; -30; 34; 32; -18; -32; 12; 31; 3; -40; -21; -2; 25; -7; 20; -5; 4611686018426659009; -1; -49; 4611686018427119322; -22; 12; 45; 19; 21; 9]
*)

let m = 100;
(*let m = Microsoft.FSharp.Core.int.MaxValue;;*)
(* for ocaml:
let m = max_int;;
*)

let rec adder xys (counter:int) (total:int) =
    match xys with
    |[]                  -> failwith "wtf"
    |x::[]               -> if counter = 0 then Some total else None
    |x::y::xys when y>0  -> if (x+y) > x then adder ([x+y]@xys) counter (x+y) else adder ([x+y]@xys) (counter+1) (x+y)
    |x::y::xys when y<=0 -> if (x+y) <= x then adder ([x+y]@xys) counter (x+y) else adder ([x+y]@xys) (counter-1) (x+y)
    |_::_::_             -> failwith "wtf"
;;

let add xs = adder xs 0 0;;