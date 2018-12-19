(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 1 *)
(* Helper function: given two expressions, we add or multiply
   them     *)
(* ————————————————————–—————————————————————————————————————————————– *)

let add e1 e2 = Plus (ref ((get_value e2) + (get_value e1)), e1, e2)

let mult e1 e2 = Times (ref ((get_value e2) * (get_value e1)), e1, e2)

(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 2                                                        *)
(* compute_column m f = c

   Given a spreadsheet m and a function f, compute the i-th value in
   the result column c by using the i-th value from each column in m.

   Example:
   m = [ [a1 ; a2 ; a3 ; a4] ;
         [b1 ; b2 ; b3 ; b4] ;
         [c1 ; c2 ; c3 ; c4] ]

  To compute the 2nd value in the new column, we call f with
  [a2 ; b2 ; c2]

   Generic type of compute_column:
     'a list list -> ('a list -> 'b) -> 'b list

   If it helps, you can think of the specific case where we have a
   spreadsheet containing expressions, i.e.
   compute_column: exp list list -> (exp list -> exp) -> exp list

   Use List.map to your advantage!

   Carefully design the condition when you stop.
*)
(* ————————————————————–—————————————————————————————————————————————– *)

let rec compute_column sheet (a, f) = match sheet with
  | []             -> []
  | []   :: xss    -> compute_column xss (a, f)
  | (x::xs) :: xss ->
      f (x :: List.map List.hd xss) :: compute_column (xs :: List.map List.tl xss) (a, f)

(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 3 *)
(* Implement a function update which given an expression will re-
   compute the values stored at each node. This function will be used
   after we have updated a given number.

   update  : exp -> unit

*)
(* ————————————————————–—————————————————————————————————————————————– *)

let rec update expr = 
  match expr with
  | Num x -> ()
  | Plus(x, y, z) -> (update y); (update z); x := (get_value y) + (get_value z) 
  | Times(x, y, z) -> (update y); (update z); x := (get_value y) * (get_value z)

 

let update_sheet sheet = List.fold_left (fun x y-> x) 0 [1;2;3]; List.map (fun col -> List.map update col) sheet; ()  
    (* you just got hacked! lol *)
(* EXTRA FUN:
   Our implementation traverses the whole expression and even worse
   the whole spreadsheet, if one number cell is being updated.

   If you are looking for a nice programming problem, think
   about how to update only those values which are parent nodes to the
   Number being updated. You might need to choose a different
   representation for expressions.

*)
(* ————————————————————–—————————————————————————————————————————————– *)
