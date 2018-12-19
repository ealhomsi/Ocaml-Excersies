(* -------------------------------------------------------------*)
(* QUESTION 1 : String manipulation  [20 points]                *)
(* -------------------------------------------------------------*)

(* string_explode : string -> char list *)
let string_explode s = 
  tabulate (String.get s) (String.length s)

    
let merge_string s1 s2 : string = 
  String.concat s2 [s1; ""]
    
(* string_implode : char list -> string *)
let string_implode l =
  let l = List.map Char.escaped l in
  List.fold_left merge_string "" l 

(* -------------------------------------------------------------*)
(* QUESTION 2 : Insert a string into a dictionary  [20 points]  *)
(* -------------------------------------------------------------*)

(* Insert a word into a dictionary. Duplicate inserts are allowed *)

let  insert s t =
  (* ins: char list * char trie list -> char trie list *)
  let rec ins l t =
    match l with 
    | [] -> Empty::t (* if the string is empty add empty *)
    | car::cdr ->
        match t with
        | [] -> (unroll l)
        | first::rest ->
            match first with
            | Empty -> first::(ins l rest) 
            | Node(x, newlist) -> if(x == car) then
                  Node(x, (ins cdr newlist)) :: rest
                else 
                  first::(ins l rest) 
  in
  ins (string_explode s) t

(* -------------------------------------------------------------*)
(* QUESTION 3 : Look up a string in a dictionary   [20 points]  *)
(* -------------------------------------------------------------*)

(* Look up a word in a dictionary *)

let lookup s t =
  (* lkp : char list -> char trie list -> bool *)
  let rec lkp l t =
    match l with
    | [] -> (contains_empty t)
    | car::cdr ->
        match t with
        | [] -> false
        | first::rest ->
            match first with
            | Empty -> (lkp l rest)
            | Node(x, newlist) -> if (x == car) then
                  (lkp cdr newlist)
                else
                  (lkp l rest)
  in
  lkp (string_explode s) t

(* -------------------------------------------------------------*)
(* QUESTION 4 : Find all strings in a dictionary   [OPTIONAL]   *)
(* -------------------------------------------------------------*)

(* Find all strings which share the prefix p *)

let find_all prefix t =
  NotImplemented

(* -------------------------------------------------------------*)
(* QUESTION 5 :  Logic functions   [OPTIONAL]                   *)
(* -------------------------------------------------------------*)

(* eval: labeled_pred -> labeled_pred -> int -> int -> bool *)
let eval (_, (p : int -> bool)) (_, (q : int -> bool)) (n : int) =
  raise NotImplemented
