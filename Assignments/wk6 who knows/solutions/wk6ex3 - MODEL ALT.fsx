let hd = function
    |[]    -> []
    |x::xs -> x

let tl = function
    |[]    -> []
    |x::xs -> xs


let slump_length xs =
    let rec aux acc prev xs =
        match xs with
        |[]                  -> acc
        |x::xs when x < prev -> aux (acc+1) prev xs
        |x::xs               -> acc
            in aux 0 (hd xs) (tl xs);;

let slump xs =
    let rec aux long i = function
    |[]    -> long
    |x::xs -> 
        let l = slump_length (x::xs) in
        if l > fst long then aux (l,i) (i+1) xs 
        else aux long (i+1) xs
    in aux (0,0) 0;;
