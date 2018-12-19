(* --------------------------------------------------------------------*)
(* QUESTION 1: House of Cards                                          *)
(* --------------------------------------------------------------------*)

(* Q1: Comparing cards *)
(* Comparing two ranks *)
let dom_rank (r1 : rank) (r2 : rank) =
  match (r1, r2) with 
  | (Ace, _) -> true
  | (King, Queen) -> true
  | (King, Jack) -> true
  | (King, Ten) -> true
  | (King, Nine) -> true
  | (King, Eight) -> true
  | (King, Seven) -> true
  | (King, Six) -> true 
  | (Queen, Jack) -> true
  | (Queen, Ten) -> true
  | (Queen, Nine) -> true
  | (Queen, Eight) -> true
  | (Queen, Seven) -> true
  | (Queen, Six) -> true 
  | (Jack, Ten) -> true
  | (Jack, Nine) -> true
  | (Jack, Eight) -> true
  | (Jack, Seven) -> true
  | (Jack, Six) -> true 
  | (Ten, Nine) -> true
  | (Ten, Eight) -> true
  | (Ten, Seven) -> true
  | (Ten, Six) -> true 
  | (Nine, Eight) -> true
  | (Nine, Seven) -> true
  | (Nine, Six) -> true 
  | (Eight, Seven) -> true
  | (Eight, Six) -> true 
  | (Seven, Six) -> true
  | ( r1 , r2 ) -> r1 = r2 
                   
let rec dom_suit (s1: suit) (s2 : suit) =
  match ( s1 , s2 ) with
  | ( Spades , _ ) -> true
  | ( Hearts , Diamonds ) -> true
  | ( Hearts , Clubs ) -> true
  | ( Diamonds , Clubs ) -> true
  | ( s1 , s2 ) -> s1 = s2 
                   
(* Comparing two cards (r1, s1) and (r2, s2) *)
let dom_card (c1 : card) (c2 : card) =
  match (c1, c2) with
  | ((r1, s1), (r2, s2)) -> 
      if (dom_suit s1 s2) && (s1 != s2) then
        true
      else if(s1=s2) && (dom_rank r1 r2)  then
        true
      else
        false

(* Q2: Insertion Sort – Sorting cards in a hand *)
let rec insert (c : card) (h : hand) : hand =
  match h with 
  | Empty ->  Hand(c, Empty)
  | Hand(firstcard, list) -> 
      if (dom_card c firstcard) then 
        Hand(c, h)
      else
        Hand(firstcard, (insert c list))

let rec sort (h : hand) : hand =
  match h with
  | Empty -> Empty
  | Hand(firstcard, list) -> (insert firstcard (sort list))

let add_elem (c: suit) =
  (fun (x:rank): card -> (x, c))
(* Q3: Generating a deck of cards *)
let rec generate_deck (suits : suit list) (ranks : rank list) : card list =
  match (suits, ranks) with 
  | ([], _) -> []
  | (car::cdr, list) -> (List.map (add_elem car) list) @ (generate_deck cdr list)

(* Q4: Shuffling a deck of cards *)
let rec split (deck : card list) (n : int) : card * card list =
  match(deck, n) with
  | ([], _) -> raise Domain 
  | (car::cdr, 0) -> (car, cdr)
  | (car::cdr, n) -> let  (head, tail) = (split cdr (n-1)) in
      (head, car :: tail)

let shuffle (deck : card list) : card list =
  let size = List.length deck in
  let rec select deck n = 
    if n = 0 then
      []
    else
      let random = Random.int n in
      let res = (split deck random) in
      match res with 
      | (card, list) -> card :: (select list (n-1))
  in
  select deck size
    

(* --------------------------------------------------------------------*)
(* QUESTION 2: Sparse Representation of Binary Numbers                 *)
(* ------------------------------------------------------------------- *)

(* Q1: Incrementing a sparse binary number *)
   
let dec_to_bin x =
  let rec d2b y = 
    match y with 
    |0 -> [] 
    |_ -> (y mod 2) :: (d2b (y/2))
  in
  d2b x 

let rec bin_to_sparse list power =
  match list with
  | [] -> []
  | car::cdr -> 
      if(car = 0) then
        bin_to_sparse cdr (power*2)
      else
        power:: (bin_to_sparse cdr (power*2))

let inc (ws : nat ) : nat =
  let answer = (dec_to_bin (List.fold_left (+) 1 ws)) in
  bin_to_sparse answer 1
  
 

(* Q2: Decrementing a sparse binary number *)
let dec (ws : nat) : nat =
  let answer1 =  (List.fold_left (+) 0 ws) in
  let answer = dec_to_bin (answer1 -1) in
  bin_to_sparse answer 1

(* Q3: Adding sparse binary numbers *)
let rec add (m : nat) (n : nat) : nat  =
  let answer = (dec_to_bin (List.fold_left (+) (List.fold_left (+) 0 n) m)) in
  bin_to_sparse answer 1

(* Q4: Converting to integer - tail recursively *)
let rec toInt (n : nat) (acc : int) : int =
  match n with
  | [] -> acc 
  | car::cdr -> toInt cdr (car + acc)
                  
let sbinToInt (n : nat) : int =
  toInt n 0
