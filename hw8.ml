let rec parseExp toklist sc =
  parseSExp
    toklist
    (fun toklist' exp -> match toklist' with
       | SEMICOLON :: toklist'' -> sc toklist'' exp
       | _ -> raise (Error "Expected a single semicolon"))

and parseSExp toklist sc =
  parsePExp
    toklist
    (fun toklist' exp -> match toklist' with
       | PLUS:: toklist'' ->  parseSExp 
                                toklist'' 
                                (fun toklist''' exp' -> sc toklist''' (Sum(exp, exp')))
       | SUB:: toklist'' ->  parseSExp 
                               toklist'' 
                               (fun toklist''' exp' -> sc toklist''' (Minus(exp, exp')))
       | toklist'' -> sc toklist'' exp)

and parsePExp toklist sc =
  parseAtom
    toklist
    (fun toklist' exp -> match toklist' with
       | TIMES :: toklist'' ->  parsePExp 
                                  toklist'' 
                                  (fun toklist''' exp' -> sc toklist''' (Prod(exp, exp')))
       | DIV :: toklist'' ->  parsePExp 
                                toklist'' 
                                (fun toklist''' exp' -> sc toklist''' (Div(exp, exp')))
       | toklist'' -> sc toklist'' exp)

and parseAtom toklist sc =
  match toklist with
  | INT n :: toklist' -> sc toklist' (Int n)
  | LPAREN :: toklist' -> 
      parseSExp
        toklist'
        (fun toklist'' exp -> match toklist'' with
           | RPAREN :: toklist''' -> sc toklist''' exp
           | _ -> raise (Error "Expected a RPAREN"))
  |_ -> raise (Error "Expected a ?")
          
          
(* parse : string -> exp *)
let parse string =
  parseExp
    (lex string)
    (fun s e -> match s with
       | [] -> e
       | _ -> raise (Error "Incomplete expression"))

(* eval : string -> int *)
let eval e = eval' (parse e)
