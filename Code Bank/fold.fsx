let snoc l x = x :: l;;

let reverse list = List.fold (fun acc x -> snoc acc x) [] list

