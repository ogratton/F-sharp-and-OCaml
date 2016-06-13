let rec replace x y xs =
    match xs with
    |[]             -> xs
    |z::zs when z=x -> [y] @ replace x y (zs)
    |z::zs          -> [z] @ replace x y (zs)
;;