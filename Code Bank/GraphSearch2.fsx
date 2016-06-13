// can perform a BFS or DFS search (see strategy for how to swap between the two)
// run: 
// search roadmap expand ['A','-'] (goal 'V') strategy [];;

// TODO: figure out how to implement uniform cost search

let path visited =
    let rec aux visited follow path =
        match visited with
        | (n,p) :: vs when p = '-'    -> n :: path
        | (n,p) :: vs when n = follow -> aux vs p (n :: path)
        | (n,p) :: vs                 -> aux vs follow path
    in aux visited (snd (List.head visited)) [fst (List.head visited)]

let rec search graph expand fringe goal strategy visited =
    match fringe with
    | []                  -> None   // nowhere left to go
    | (n,p) :: ns when goal n -> Some (path ((n,p) :: visited))// reached goal //((n,p) :: visited)
    | (n,p) :: ns             -> 
                            let newnodes = expand n graph in                          // get where we can go now
                            let fringe' = strategy ns newnodes ((n,p) :: visited) in      // make a new fringe appropriately
                            search graph expand fringe' goal strategy ((n,p) :: visited)  // search again with the new fringe

let roadmap =
  [ ('A', 'Z', 75);  ('A', 'S', 140); ('A', 'T', 118);
    ('T', 'L', 111); ('L', 'M', 70);  ('M', 'D', 75);
    ('D', 'C', 120); ('C', 'R', 146); ('R', 'S', 80);
    ('R', 'P', 97);  ('S', 'O', 151); ('O', 'Z', 71);
    ('S', 'F', 99);  ('F', 'B', 211); ('B', 'P', 101);
    ('P', 'C', 138); ('B', 'G', 90);  ('B', 'U', 85);
    ('U', 'H', 98);  ('H', 'E', 86);  ('U', 'V', 142);
    ('V', 'I', 92);  ('I', 'N', 87)]

let rec expand node graph = 
    match graph with
    | []                                  -> [] // cannot expand an empty list
    | (n1, n2, _) :: edges when n1 = node -> (n2,n1) :: expand node edges // consider the node at which we are not already positioned
    | (n1, n2, _) :: edges when n2 = node -> (n1,n2) :: expand node edges
    | _ :: edges                          -> expand node edges

//let target = 'Z'
let goal n' n = (n = n') // if they are the same the we are at the goal

let rec sameKey x ys =
    match ys with
    | [] -> false
    | y :: ys when (fst y) = x -> true
    | y :: ys          -> sameKey x ys

let rec strategy fringe newnodes visited = // adds newnode to the fringe if it has not been visited
    let rec check fringe' visited =
        match fringe' with
        | []      -> []
        | (x,p) :: xs ->
                    if sameKey x visited // ignore it
                    then check xs visited
                    else (x,p) :: check xs visited 
    in check (fringe @ newnodes) visited // (fringe @ newnodes) is bfs, (newnodes @ fringe) is dfs

                        