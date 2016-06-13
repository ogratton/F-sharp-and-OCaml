(* Weave and Unweave - intersperse elements of xs with ys and the undo function *)
let rec weave xs ys = 
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, y :: ys -> x :: y :: weave xs ys
;;
let rec unweave xs =
    match xs with
    |[]        -> ([],[])
    |x::y::xys ->
        let (xs,ys) = unweave xys in
        (x::xs,y::ys)
    |_         -> failwith "unweave"
;;


(* Zip and Unzip - pair corresponding elements of two equal length lists together into tuples and the undo function *)
let rec zip xs ys =
    match xs,ys with
    |[], [] -> []
    |[], _
    |_ , [] -> failwith "zip"
    |x::xs,y::ys -> (x,y)::(zip xs ys)
;;
let rec unzip xs =
    match xs with
    |[]         -> ([],[])
    |(x,y)::xys ->
        let (xs,ys) = unzip xys in
        (x::xs,y::ys)
;;


(* Sort a list with insert sort *)
let rec sort xs =
    match xs with
    | [] -> []
    | x :: l -> insert x (sort l)
  and insert elem = function
    | [] -> [elem]
    | x :: l -> if elem < x then elem :: x :: l
                else x :: insert elem l
;;


(* Remove a specific element from a list *)
let remove_elt e l =
    let rec go l acc =
        match l with 
        | [] -> List.rev acc 
        | x::xs when e = x -> go xs acc 
        | x::xs -> go xs (x::acc) 
    in go l []
;;
        (* Alternative remove function - probably not tail recursive, idk *)
let rec remove_alt x ys =
    match ys with
    |[]             -> []
    |y::ys when y=x -> remove_alt x ys
    |y::ys          -> [y]@(remove_alt x ys)
;;


(* Remove all duplicates from a list using the remove_elt function above*)
let remove_duplicates l = 
    let rec go l acc =
        match l with 
        | []      -> List.rev acc 
        | x :: xs -> go (remove_elt x xs) (x::acc) 
    in go l []
;;


(* nth element of a list *)
let rec nth xs n =
    match xs with
    |[]    -> failwith "nth"
    |x::xs -> if n=0 then x else nth xs (n-1)
;;
        (* Alternative nth function using 'when' syntax rather than 'if then else' *)
let rec nth_alt xs n =
    match xs with
    |[]             -> failwith "nth"
    |x::xs when n=0 -> x
    |x::xs          -> nth xs (n-1)
;;


(* Tail recursive length function *)
let length list =
    let rec aux n list =
        match list with
        | [] -> n
        | _::t -> aux (n+1) t
    in aux 0 list
;;


(* Tail recursive reverse function - this is the function Dan visualised with the blocks *)
        (* This function needs to be run with an empty list as its first argument, e.g. "rev [] [1;2;3;4;5;6;7;8;9;0]" *)
let rec rev sx = function
    |[]    -> sx
    |x::xs -> rev (x::sx) xs
;;
        (* This version does exactly the same thing as above but only needs the list to be reversed as its argument, e.g. "rev_alt [1;2;3;4;5;6;7;8;9;0]" *)
        (* The fact this one uses match with instead of function is not important - both work just as well but match is clearer to me *)       
let rev_alt xs =
    let rec revver sx xs =
        match xs with
        |[]    -> sx
        |x::xs -> revver (x::sx) xs
    in revver [] xs
;;


(* isIn - Returns true if x is a member of list xs. Equivalent to List.mem, I think *)
let rec isIn x ys = 
    match ys with
    |[]             -> false
    |y::ys when y=x -> true
    |y::ys          -> isIn x ys;;


