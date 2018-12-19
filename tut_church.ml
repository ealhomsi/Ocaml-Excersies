(* Check if a Church numeral is equal to zero *)

(* iszero : 'a church -> bool *)
let iszero (Church c) = (c (fun x -> false)) true
    
  

(* Convert an integer to a Church numeral *)

(* create : int -> 'a church *)
let rec create (n : int) : 'a church =
  match n with
  | 0 -> Church (fun f x -> x)
  | _ -> Church (fun f x ->
      match create (n - 1) with
      (* Add 1 to the Church numeral representing n - 1 *)
      | Church c ->  f (c f x))


(* Convert a Church numeral to an integer *)

(* churchToInt : 'a church -> int *)
let succ (Church c) = Church (fun f x -> f (c f x))
let churchToInt (Church c) = c (fun n -> n + 1) 0


(* Add two Church numerals *)

(* add : 'a church -> 'a church -> 'a church *)
let add (Church cm) (Church cn) =
  Church (fun f x -> cm f (cn f x))
(* Multiply two Church numerals *)

(* mult : 'a church -> 'a church -> 'a church *)
let mult (Church cm) (Church cn) =
  Church (fun f x -> cn (cm f) x)
