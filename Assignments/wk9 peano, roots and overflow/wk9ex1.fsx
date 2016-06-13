type nat = Zero | Suc of nat;;

let zero = Zero;;
let one = Suc zero;;
let two = Suc one;;
let three = Suc two;;
let four = Suc three;;

let rec add m n =
    match m with
    | Zero -> n
    | Suc x -> add x (Suc n)
;;
let rec sub m n =
    match m,n with
    | _,Zero -> m
    | Zero,_ -> failwith "sub"
    | Suc x, Suc y -> sub x y 
;;
let mul m n =
    let rec muller m n p =
        match m with
        | Zero -> p
        | Suc m -> muller n m (add n p)
    in muller m n Zero
;;
let pow m n =
    let rec power m n p =
        match n with
        | Zero -> p
        | Suc n -> power m n (mul m p)
    in power m n one
;;

let rec nat_of_int = function
    | 0 -> Zero
    | n -> Suc (nat_of_int (n-1))
;;
let rec int_of_nat = function
    | Zero -> 0
    | Suc n -> 1 + (int_of_nat n)
;;