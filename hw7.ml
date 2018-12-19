let rec parseExp toklist = match toklist with
  | [] -> raise (Error "Expected an expression: Nothing to parse")
  | _ ->
      try parseSExp toklist
      with
      | SumExpr (exp, [SEMICOLON]) -> exp
      | _ -> raise (Error "Expected a single semicolon")

and parseSExp toklist =
  try parsePExp toklist
  with
  | ProdExpr(exp, h::t) -> (
      if(h = PLUS) then 
        try parseSExp t
        with
        | SumExpr (exp1, t1) -> 
            raise (SumExpr ((Sum  (exp, exp1)), t1))
      else if(h = SUB) then 
        try parseSExp t
        with
        | SumExpr (exp1, t1) -> 
            raise (SumExpr ((Minus  (exp, exp1)), t1)) 
      else
        raise (SumExpr (exp, h::t)) )
  | ProdExpr(exp, []) -> raise (SumExpr (exp, []))

          
and parsePExp toklist =
  try parseAtom toklist
  with
  | AtomicExpr (exp, h::t) -> (
      if(h = DIV ) then 
        try parsePExp t
        with
        | ProdExpr (exp1, t1) -> 
            raise (ProdExpr ((Div (exp, exp1)), t1))
      else if(h = TIMES) then 
        try parsePExp t
        with
        | ProdExpr (exp1, t1) -> 
            raise (ProdExpr ((Prod  (exp, exp1)), t1)) 
      else 
        raise (ProdExpr (exp, h::t)))
  | AtomicExpr (exp, []) ->raise (ProdExpr (exp, []))
      

and parseAtom toklist =
  match toklist with 
  | [] -> raise (Error "atmomic lol")
  | h::t -> 
      match h with
      | (INT x) -> raise (AtomicExpr (Int x, t)) 
      | LPAREN ->(
          try parseSExp t
          with
          | SumExpr (exp, RPAREN::t1) -> 
              raise (AtomicExpr (exp, t1)) 
          | _ -> raise (Error "Expected a single RPAREN") )
      | _ -> raise (Error "Expected a single LPAREN OR INT")
              
;;


(* parse : string -> exp *)
let parse string =
  parseExp (lex string) ;;

(* eval : string -> int *)
let eval e = eval' (parse e) ;;
