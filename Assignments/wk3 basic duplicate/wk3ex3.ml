let duplicate2(a,b) = if a = b then true else false;;

let sort2(a,b) = 
    if a < b
    then (a,b)
    else (b,a);;

let sort3(a,b,c) =
    let(a,b) = sort2(a,b) in
    let(b,c) = sort2(b,c) in
    let(a,b) = sort2(a,b) in
    (a,b,c);;

let sort4(a,b,c,d) =
    let(a,b) = sort2(a,b) in
    let(b,c) = sort2(b,c) in
    let(c,d) = sort2(c,d) in
    let(a,b,c) = sort3(a,b,c) in
    (a,b,c,d);;

let sort5(a,b,c,d,e) =
    let(a,b) = sort2(a,b) in
    let(b,c) = sort2(b,c) in
    let(c,d) = sort2(c,d) in
    let(d,e) = sort2(d,e) in
    let(a,b,c,d) = sort4(a,b,c,d) in
    (a,b,c,d,e);;

let duplicate5(a,b,c,d,e) =
    let(a,b,c,d,e) = sort5(a,b,c,d,e) in
    if duplicate2(a,b)
    then true
    else if duplicate2(b,c)
    then true
    else if duplicate2(c,d)
    then true
    else if duplicate2(d,e)
    then true
    else false;;
        
duplicate5(1,2,3,4,6);;
