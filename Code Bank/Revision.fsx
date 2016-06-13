(* print "Hello World" n times *)
let rec hello_worlds n =
    printf("Hello World\n");
    if n > 1 then hello_worlds (n-1)
;;

(* repeat each element of a list n times *)
let repeat n list =
    let rec aux n_0 n list nlist =
        match list,n with
        |[],m    -> List.rev nlist
        |x::xs,0 -> aux n_0 n_0 xs nlist
        |x::xs,m -> aux n_0 (n-1) list (x::nlist)
    in aux n n list []
;;

(* filter an integer list given by returning only elements less than n *)
let filter n list =
    let rec aux n list = function
        |[]               -> List.rev list
        |x::xs when x < n -> aux n (x::list) xs
        |x::xs            -> aux n list xs
    in aux n [] list
;;

(* return only elements at an even index *)
let evens list =
    let rec aux list n nlist =
        match list with
        |[]                   -> List.rev nlist
        |x::xs when (n%2 = 0) -> aux xs (n+1) (x::nlist)
        |x::xs                -> aux xs (n+1) nlist
    in aux list 0 []
;;

(* non-tail-recursive version of the same function *)
let evens2 list =
    let rec aux list n =
        match list with
        |[]                   -> list
        |x::xs when (n%2 = 0) -> x :: (aux xs (n+1))
        |x::xs                -> aux xs (n+1)
    in aux list 0
;;

(* make an array of n items *)
let make_array n =
    let rec aux n list =
        if n = 0 then list else aux (n-1) (n::list)
    in aux n []
;;

(* Randomly populate a list of size size *)
let make_rand size =
    let rnd = System.Random()
    List.init size (fun _ -> rnd.Next (0,100))
;;

(* reverse a list *)
let reverse list =
    let rec aux list acc =
        match list with
        |[]    -> acc
        |x::xs -> aux xs (x::acc)
    in aux list []
;;

(* returns the sum of the odd elements of a list *)
let sum_odd list =
    let rec aux sum list =
        match list with
        |[]                   -> sum
        |x::xs when (x%2 = 1) -> aux (sum+x) xs
        |x::xs                -> aux sum xs
    in aux 0 list
;;

(* return the length of a list *)
let length list =
    let rec aux len list =
        match list with
        |[]    -> len
        |x::xs -> aux (len+1) xs
    in aux 0 list
;;

(* apply abs to every item in a list *)
let abby list =
    let rec aux list = function
        |[]    -> List.rev list
        |x::xs -> aux (Operators.abs(x)::list) xs
    in aux [] list
;;

let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n-1)
;;

(* expand e^x for 10 terms *)
(*let expand x:float =
    let rec aux (x:float) sum n =
        if (n<10) then aux x (sum + ( pown x n / (factorial n) )) (n+1) else sum
    in aux x 1 1
;;*)

(* Wk5 ex1 - zip *)
let rec zip xs ys =
    match xs,ys with
    |[]   ,[]    -> []
    |x::xs,[]    -> x::xs
    |[]   ,y::ys -> y::ys
    |x::xs,y::ys -> x :: y :: (zip xs ys)
;;

(* TODO tail-recursive version of the same *)
let zip_tr xs ys =
    let rec aux lst1 lst2 acc =
        match lst1,lst2 with
        |[]   ,[]    -> acc
        |x::xs,[]    -> lst1
        |[]   ,y::ys -> lst2
        |x::xs,y::ys -> aux xs ys (x::y::acc)
    in aux xs ys
;;

(* Wk5 ex2 - shift rotate *)
let rec shiftrotate list n =
    match list with
    |[]               -> []
    |x::xs when n = 0 -> list
    |x::xs            -> shiftrotate (xs@[x]) (n-1) 
;;

(* Wk5 ex3 - histogram *)
let rec increment x = function
    |[]                    -> [(x, 1)]
    |(i,j)::ijs when x = i -> (i, (j+1))::(ijs)
    |(i,j)::ijs            -> (i, j)::(increment x ijs)
;;
let histo list =
    let rec aux list pairs =
        match list with
        |[]    -> pairs
        |x::xs -> aux xs (increment x pairs)
    in aux list []
;;

(* Wk6 ex1 - symmetric difference *)
(* tail-recursive *)
let rec isIn x ys =
    match ys with
    |[]    -> false
    |y::ys -> if y=x then true else isIn x ys
;;
let rem x ys =
    let rec aux x ys acc =
        match ys with
        |[]    -> List.rev acc
        |y::ys -> if y=x then ys else aux x ys (y::acc)
    in aux x ys []
;;
let combine = function
    |(x,y) -> x@y
;;
let dif xs ys =
    let rec aux lst1 lst2 acc =
        match lst1 with
        |[]                     -> (List.rev acc)@lst1,lst2
        |x::xs when isIn x lst2 -> aux xs (rem x lst2) acc
        |x::xs                  -> aux xs lst2 (x::acc)
    in combine (aux xs ys [])
;;

 (* Randomly populate a list of size size *)
let mr size =
    let rnd = System.Random()
    List.init size (fun _ -> rnd.Next (0,10))
;;





