(* Q1:
   pow n k: Raises the number n to the power of k,
            provided k is a non-negative integer.
            Raises exception Domain otherwise.

   Examples:
   pow 0  5 ==> 0
   pow 5  0 ==> 1
   pow 2  3 ==> 8
   pow 2 -3 ==> Exception: Domain

*)

let pow  n k :int =
  let rec pow' n k  = match k with
    | 0 -> 1
    | _ -> n * pow' n (k - 1)
  in
  if(k >= 0) then
    pow' n k
  else
    raise Domain



(* Q1:

  fib n : Computes the n-th element of the Fibonacci sequence:

   1, 1, 2, 3, 5, ..., fib (n-1) + fib (n -2)

   Raises exception Domain if n is negative.

  Examples:

  fib 0 => 1
  fib 10 => 89
  fib -1 => Exception: Domain

*)

let rec fib (n : int) : int =
  let rec fib' n : int = match n with
    | 0 -> 1
    | 1 -> 1
    | n -> fib' (n - 1) + fib' (n - 2)
  in
  if n >= 0 then fib' n
  else raise Domain


(* Q2: Newton-Raphson method for computing the square root
*)
let square_root a =
  let rec findroot x acc =
    let equation a x =
      (((a /. x) +. x) /. 2.0)
    in
    let diff = abs_float  (x -. (equation a x))
    in
    if(diff < acc) then
      equation a x
    else
      findroot (equation a x) acc
  in
  if a > 0.0 then
    findroot 1.0 epsilon_float
  else
    raise Domain

(* Q3: Tail-recursive version of power function *)
let pow_tl n k =
  let rec aux n k acc =
    if( k = 0) then
      acc
    else
      aux n (k-1) (acc * n)
  in
  aux n k 1

(* Q4: Checking naivly whether a given integer is a prime number *)

let is_prime n =
  if n<= 1 then
    raise Domain
  else
    let rec is_prime' n x =
      if ( x * x > n) then
        true
      else
      if (n mod x == 0) then
        false
      else
        is_prime' n (x+1)
    in
    is_prime' n 2


(* Q5: Computing the greatest common divisor using Euclid's algorithm *)

let gcd a b =
  let rec gcd' a b =
    if( a < 0 || b < 0) then
      raise Domain
    else
    if( b == 0) then
      a
    else 
      gcd' b (a mod b)
  in
  gcd' a b

