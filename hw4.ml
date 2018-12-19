(* -------------------------------------------------------------*)
(* QUESTION 1 : Let's have cake!                                *)
(* -------------------------------------------------------------*)

(* allergy_free : ingredient list -> cupcake list -> cupcake list *)
let rec check_contain element list=
  match list with
  | [] -> false
  | h::t -> 
      if (h = element) then
        false 
      else 
        (check_contain element t) 
          
let rec common_element list1 list2=
  match list1 with
  | [] -> false
  | h::t -> if(check_contain h list2) then true else common_element t list2

let equal x y =
  x == y 
  
let allergy_free allergens cupcakes =
  let filtersafe allergens cupcake= 
    let checksafe inglist allergen =
      let funnyequal = (equal allergen) in
      not (List.exists funnyequal inglist) in
    let Cupcake(_, _, _, inglist) = cupcake in
    List.for_all (checksafe inglist) allergens in
  
  
  List.filter (filtersafe allergens) cupcakes
      

(* -------------------------------------------------------------*)
(* QUESTION 2 : Generic Tree Traversals                         *)
(* -------------------------------------------------------------*)

(* map_tree : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec map_tree f t  =
  match t with
  | Empty -> Empty
  | Node(a, left, right) -> Node((f a), (map_tree f left ), (map_tree f right ))

(* delete_data : ('a * 'b) tree -> 'b tree *)
let delete_data t =
  (map_tree (fun (x, y) -> x) t)

(* fold_tree : ('a * 'b ' * 'b -> 'b) -> 'b -> 'a tree -> 'b *)
let rec fold_tree f e t =
  match t with 
  | Empty -> e
  | Node(a, left, right) -> (f (a, (fold_tree f e left), (fold_tree f e right)))

(* size : 'a tree -> int *)
let size tr =
  (fold_tree (fun (x, y, z) -> 1 + y + z) 0 tr)

(* reflect : 'a tree -> 'a tree *)
let reflect tr =
  (fold_tree (fun (x, y ,z) -> Node(x, z, y)) Empty tr)

(* inorder : 'a tree -> 'a list *)
let inorder tr =
  (fold_tree (fun (x, y, z) ->  y @ [x] @ z ) [] tr)
