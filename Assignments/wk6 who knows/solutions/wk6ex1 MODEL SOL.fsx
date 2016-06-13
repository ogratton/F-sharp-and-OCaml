let rec isIn x ys = 
    match ys with
    |[]             -> false
    |y::ys when y=x -> true
    |y::ys          -> isIn x ys;;

let rec differ xs ys = match xs with
| []                         -> []
| x :: xs when isIn x ys     -> differ xs ys
| x :: xs                    -> x :: (differ xs ys);;

let dif xs ys = (differ xs ys) @ (differ ys xs);;
