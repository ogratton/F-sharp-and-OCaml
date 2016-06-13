(* This doesn't use the rational numbers library yet cos I have no idea how *)


let abs (x:float) = if x < 0.0 then x-(2.0*x) else x;;

let f (x:float) = (x-4.0)*(x-6.0);;

let rec rooter f (a:float) (b:float) (error:float) =
    match a,b with
    |a,b when f ((a+b)/2.0) <= 0.0 -> if abs (f ((a+b)/2.0)) < error then (a+b)/2.0 else rooter f ((a+b)/2.0) b error
    |a,b when f ((a+b)/2.0) > 0.0  -> if abs (f ((a+b)/2.0)) < error then (a+b)/2.0 else rooter f a ((a+b)/2.0) error
    |_,_ -> failwith "rooter"

let root f (a:float) (b:float) (error:float) =
    if (f a) <= 0.0 && (f b) > 0.0
    then rooter f a b error
    else if (f b) <= 0.0 && (f a) > 0.0
    then rooter f b a error
    else failwith "root"