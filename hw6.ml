(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account p =
  let pass = ref p in
  let balance = ref 0 in
  let failed = ref 0 in
  {
    update_passwd = (fun pass1 pass2 -> if (!pass = pass1) then (pass:=pass2; failed:= 0) else (failed:= !failed + 1;raise  wrong_pass)) ;
    retrieve      = (fun pass1 b -> if(!failed >= 3) then raise too_many_attempts else if(!pass = pass1) then (failed:= 0; if (b <= !balance) then balance := !balance - b else raise  no_money) else (failed:= !failed + 1;raise  wrong_pass));
    deposit       = (fun pass1 b -> if(!failed >= 3) then raise too_many_attempts else if(!pass = pass1) then (failed:= 0;balance := !balance + b) else (failed:= !failed + 1;raise  wrong_pass));
    print_balance = (fun pass1 -> if(!failed >= 3) then raise too_many_attempts else if(!pass = pass1) then (failed:= 0;!balance) else raise (failed:= !failed + 1;raise  wrong_pass))
  }
;;


(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec catalan_I n =
  let haha = ref { num_rec = 0; result = 0;} in
  let rec sum f i = match i with
    | 0 -> f 0
    | i -> f i + sum f (i - 1) in
  let rec catalan n = match n with
    | 0 -> ((haha := {num_rec = !haha.num_rec +1; result=0}); 1)
    | 1 -> ((haha := {num_rec = !haha.num_rec +1; result=0}); 1)
    | n -> ((haha := {num_rec = !haha.num_rec +1; result=0}); sum (fun i -> catalan i * catalan (n - 1 - i)) (n - 1)) in
  let res = catalan n in
  (haha := {num_rec = !haha.num_rec; result=res} ; !haha)
;;






(* Q 2.2 : Memoization with a global store *)

let rec catalan_memo n =
  let rec sum f i = match i with
    | 0 -> f 0
    | i -> f i + sum f (i - 1) in

  let rec catalan n = match (Hashtbl.find_opt store n) with
    | Some(x) -> x
    | None -> let res = 
                match n with
                | 0 -> 1
                | 1 -> 1
                | n -> sum (fun i -> catalan i * catalan (n - 1 - i)) (n - 1) in
        (Hashtbl.add store n res; res) 
  in
  catalan n
;;


(* Q 2.3 : General we need to return a function memoization function *)

let memo f stats =
  let memos = Hashtbl.create 1000 in
  let rec g input =
    match (Hashtbl.find_opt memos input) with
    | Some(x) -> (stats.lkp := !(stats.lkp) + 1; x)
    | None -> let res = 
                match input with
                | input -> f g input
        in
        (stats.entries := !(stats.entries) + 1 ;Hashtbl.add memos input res; res) 
  in
  g
;;


(* Q 2.4 : Using memo to efficiently compute the Hofstadter Sequence Q *)


let rec hofstadter_Q n = 
  let mystats = { entries = ref 0; lkp = ref 0} in 
  let biggerShoot wow input =
    match input with
    |  1 | 2 -> 1
    | input -> (wow(input - wow(input-1))) + (wow (input - wow(input-2)))
  in 
  let hof = ref (memo biggerShoot mystats) in
  (fun x -> ((!hof n), mystats)) memo 
;;
