(* Asg 7.0a *)
(* The following function computes the sum of the elements of a list of integers: *)

let rec sum = function
  | [] -> 0 (* The sum over the empty list is usually taken to be 0 *)
  | n :: ns -> n + sum ns (* add the value of the had to the sum of the tail elements *) 

(* Q: Although correct this function has an implementation problem. What is it? *)
(* A: For large lists the stack may overflow. *)

(* Q: Give an alternative implementation which solves this problem *)
let rec sum' s = function
  | [] -> s
  | n :: ns -> sum' (s+n) ns
let sum ns = sum' 0 ns

(* Asg 7.0b *)

(* A developer is required to implement a that function removes all the duplicate occurrences of each element in a list, returing a list in which all elements occur exactly once. The order is irrelevant. This is the (correct) function she produces: *)

let rec setify = function
  | []                         -> []
  | x :: xs when List.mem x xs -> setify xs
  | x :: xs                    -> x :: setify xs

(* Part 1. 
   Q: Is this a good implementation for working with large lists? If not, fix the implementation.
   A: No. Large lists will cause the stack to overflow. Below is an improved implementation. *)

(* We use the standard tail-recursive transformation where an extra argument holds the eventual result. *)
let rec setify' xs = function
  | []                         -> xs
  | y :: ys when List.mem y ys -> setify' xs ys
  | y :: ys                    -> y :: setify' xs ys

let setify = setify' []

let rec mkrlist xs = function
  | 0 -> xs
  | n -> mkrlist (Random.bits () :: xs) (n-1)

let _ =
  let l  = mkrlist [] 20000 in 
  let t0 = Sys.time () in 
  let _ = setify l in 
  let t1 = Sys.time () in
  print_float (t1 -. t0); print_newline ()

(* Part 2. 
   Q: Is this an efficient implementation? If not, improve the efficiency at least 100x using the benchmark below.
   A: No. The call to List.mem requires traversing the tail every single time. If we plotted the execution times on a graph we could see
      that they grow quadratically. Below is an efficient implementation. *)

(* Sorting the list is a fairly cheap operation. All the duplicate occurrences now become consecutive and can be removed easily.
   We will make it tail-recursive from the outset. *)

let rec setify_hd ys = function
  | []                        -> ys
  | [x]                       -> x :: ys
  | x :: x' :: xs when x = x' -> setify_hd ys (x :: xs)
  | x :: x' :: xs             -> setify_hd (x :: ys) (x' :: xs)

let setify xs = xs |> List.sort compare |> setify_hd []

(* benchmarking *)

let _ =
  let l  = mkrlist [] 20000 in 
  let t0 = Sys.time () in 
  let _ = setify l in 
  let t1 = Sys.time () in
  print_float (t1 -. t0); print_newline ()
