let f b = if b=true then false else true

let enc f = (f true, f false);;

let dec (a,b) =
    let f x = 
        match x with
        |true -> a
        |false -> b
    in f;;