(* Question 1: let's compose something! *)

(* 1.1 Composition *)

let compose (fs : ('a -> 'a) list) : 'a -> 'a =
  fun x -> List.fold_right (fun f y -> (f y))  fs x

(* 1.2 Replication *)

let rec replicate (n : int) : 'a -> 'a list =
  if n  = 0 then
    fun x -> []
  else
    fun x -> (x::(replicate (n-1) x))

(* 1.3 Repeating *)

let repeat (n : int) (f : 'a -> 'a) : 'a -> 'a =
  fun x -> compose (replicate n f) x

(* Question 2: unfolding is like folding in reverse *)

(* 2.1 Compute the even natural numbers up to an exclusive limit. *)
let evens (max : int) : int list =
  unfold (fun b -> (b, b+2)) (fun b -> max <= b) 0

(* 2.2 Compute the fibonacci sequence up to an exclusive limit. *)
let fib (max : int) : int list =
  unfold (fun (a,b) -> 
      (a, (b,b+a))
    ) 
    (fun (a,b) -> max <= a) (1,1)

let removefirst l =
  List.tl l 
let rec removelast l =
  match l with
  | [] -> raise NotImplemented
  | [x] -> []
  | h::t -> h::(removelast t)
(* 2.3 Compute Pascal's triangle up to a maximum row length. *)
let pascal (max : int) : int list list =
  unfold (fun b -> 
      (b, (List.append (1::(List.map2 (+) (removefirst b) (removelast b))) [1]))) 
    (fun a -> max+1 <= List.length a) [1]

(* 2.4 Implement the zip, which joins two lists into a list of tuples.
 * e.g. zip [1;2] ['a', 'c'] = [(1, 'a'); (2, 'c')]
 * Note that if one list is shorter than the other, then the resulting
 * list should have the length of the smaller list. *)
let zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  unfold (fun (a,b) -> 
      ((List.hd a, List.hd b),(List.tl a, List.tl b)))
    (fun (a,b) -> match (a, b) with
       | [], _ -> true
       | _, [] -> true
       | x::xs, y::ys -> false) (l1, l2)

