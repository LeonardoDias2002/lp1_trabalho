type tp =
  | TInt
  | TBool
  | TUnit (* unit *)
  | TRef of tp
 

type bop = 
  | Or
  | And
  | Eq
  | Lt
  | Gt
  | Le
  | Ge 
  | Sum
  | Sub
  | Mul
  | Div

type ast =
  | Num of int
  | Id of string
  | Bool of bool
  | Not of ast 
  | Binop of bop * ast * ast
  | Let of string * tp * ast * ast 
  | If of ast * ast * ast
  (* construçoes imperativas *)
  | Unit
  | Deref of ast
  | New of ast
  | Asg of ast * ast 
  | Seq of ast * ast 
  | Whi of ast * ast 
  | Loc of int
      
type token = 
  | NUM of int 
  | ID of string
  | TRUE | FALSE
  | NOT
  | OR | AND
  | EQ | LT | LE | GT | GE
  | SUM | SUB
  | MUL | DIV
  | LET | COLON | INT | BOOL | UNIT | REF | IN
  | IF | THEN | ELSE 
  | LRPAR | DEREF | NEW | ASG | SEMI 
  | WHILE | DO
  | LBRACE | RBRACE
  | LPAR | RPAR
  | EOF
  
  
let to_id_or_kw (s:string) : token =
  match s with
  | "true"  -> TRUE
  | "false" -> FALSE
  | "not"   -> NOT
  | "if"    -> IF
  | "then"  -> THEN
  | "else"  -> ELSE
  | "let"   -> LET
  | "in"    -> IN
  | "while" -> WHILE
  | "do"    -> DO
  | "new"   -> NEW
  | "int"   -> INT
  | "bool"  -> BOOL
  | "unit"  -> UNIT
  | "ref"   -> REF
  | _       -> ID s
                 
          
exception Erro_Lexico of int 

let is_letter c = c >= 'a' && c <= 'z'
let is_digit  c = c >= '0' && c <= '9' 

(* Consome o sufixo de um identificador. *)
let rec scan_id input pos =
  if pos < String.length input &&
     (is_letter input.[pos] || is_digit input.[pos])
  then scan_id input (pos + 1)
  else pos

(* Consome os sufixo de um numero. *)
let rec scan_nro input pos =
  if pos < String.length input && is_digit input.[pos]
  then scan_nro input (pos + 1)
  else pos 
    
  
   (* examina a partir de input.[pos] e retorna token e posição 
         a seguir do ultimo caracter do token *) 
let rec next_token (input:string) (pos:int) : token * int =
  let n = String.length input in
  if pos >= n  (* nao há carcteres a examinar *)
  then (EOF, pos)
  else
    match input.[pos] with
    
    | ' ' | '\t' | '\n' | '\r' -> failwith "descarte de espaço no implementado" 

    | 'a'..'z' -> failwith "scan de identificadors nao implementado" 
                    

    | '0'..'9' ->  failwith "scan de identificadors nao implementado" 
        

    | ')' -> failwith "nao implementado"
    | '{' -> failwith "nao implementado" 
    | '}' -> failwith "nao implementado"
    | ';' -> failwith "nao implementado" 
    | '=' -> failwith "nao implementado"
    | '+' -> failwith "nao implementado"
    | '-' -> failwith "nao implementado"
    | '*' -> failwith "nao implementado"
    | '/' -> failwith "nao implementado"
    | '!' -> failwith "nao implementado"
  
   (*  
    | '|' .....
                                                       
    | '&' .....
                                                       
    | ':'  .....
                           
    | '<'  .....
             
    | '>'  ....
             
    | '('   ....
*)
   
    | _ -> raise (Erro_Lexico pos)

(* Percorre input a partir de pos_ch produzindo tokens ate produzir token EOF *)
let rec scan_from (input:string) (pos_ch:int) : token list =
  let (tok, pos_next_ch) = next_token input pos_ch in
  match tok with
  | EOF -> [EOF]
  | _ -> tok :: (scan_from input pos_next_ch)
  
let scan (input:string) : token list = 
  scan_from input 0
    
  
    (*=======  PARSER ==========*)

let is_follow_El tok = (* é que segue E  *) 
  match tok with 
  | EOF | THEN | ELSE | RBRACE | IN | DO | RPAR -> true  (* o que segue E  *) 
  | _ -> false

let is_follow_Easgl tok = (* é o  que segue E_asg, ou seja é follow de El e first de El   *)
  is_follow_El tok || tok = SEMI

let is_follow_Eorl tok =  (* é o  que segue E_or, ou seja é follow de Easgl e first de Easgll   *)
  is_follow_Easgl tok || tok = ASG

let is_follow_Eandl tok = (* é o  que segue E_and, ou seja é follow de Eorl e first de Eorll   *)
  is_follow_Eorl tok || tok = OR

let is_follow_Ecmpl tok = (* é o  que segue E_cmp, ou seja é follow de Eandl e first de Eandl   *)
  is_follow_Eandl tok || tok = AND

let is_follow_Eaddl tok =  (* é o  que segue E_add, ou seja follow de Ecmpl e first de Ecmpl pois Ecmpl é nullable *)
  is_follow_Ecmpl tok ||  tok = EQ || tok =  LT || tok = LE || tok = GT || tok = GE 

let is_follow_Emull tok = (* é o  que segue E_mul, ou seja follow de Eaddl  e first de Eaddl pois Eaddl é nullable *)
  is_follow_Eaddl tok || tok = SUM || tok = SUB (* SUM e SUB sao first de Eaddl *)

  
                                                
let rec parse_T (tokens : token list) : tp * token list = 
  match tokens with
  | INT :: rest -> (TInt, rest)
  | BOOL :: rest -> (TBool, rest)
  | UNIT :: rest -> (TUnit, rest)
  | REF :: rest -> let (t, rest1) = parse_T rest in (TRef t, rest1)
  | _ -> failwith "erro na sintaxe do tipo"
  

(* E -> Ec El *)
let rec parse_E (tokens : token list) : ast * token list  = 
  let (e1, rest) = parse_Ec tokens in
  parse_El e1 rest 
                   
(* El -> ";" Ec El | ε *)

and parse_El (lhs : ast) (tokens: token list) :  ast * token list =
  match tokens with 
  | SEMI :: rest -> (* consome SEMI *)
      let (e2, rest') = parse_Ec rest in
      parse_El (Seq (lhs, e2)) rest'
  |  tok :: _  when is_follow_El tok ->
      (lhs, tokens)
  | _ -> failwith "erro na sequencia El de expressões"
  
   
   
   (* Ec -> if E then E else "{" E "}"
          | let id ":" T "=" E in "{" E "}"
          | while E do "{" E "}"
          | Easg 
*)
and parse_Ec (tokens: token list) :  ast * token list = 
  match tokens with 
  | IF :: rest ->  failwith "parser de if nao implementado"
  

  | LET :: rest -> 
      (match rest with
       | ID x :: COLON :: rest1 -> let (t, rest2) = parse_T rest1 in
           (match rest2 with
            | EQ :: rest3 -> let (e1, rest4) = parse_E rest3 in
                (match rest4 with
                 | IN :: LBRACE :: rest5 -> let (e2, rest6) = parse_E rest5 in
                     (match rest6 with
                      | RBRACE :: rest7 ->  (Let(x, t, e1, e2), rest7)
                      | _ -> failwith "sintaxe do let mal formulada - falta }")
                 | _ -> failwith "sintaxe do let mal formulada - falta in ou {")
            | _ -> failwith "sintaxe do let mal formulada - falta =")
       | _ -> failwith "sintaxe do let mal formulada - falta id ou :")
                                                                                  
       (* ...in  (Let (x, ty, e_val, e_body), rest?)   *)

  | WHILE :: rest -> failwith "parser de while nao implementado" 
                                                                                  
       (* ...in (Whi (cond, e_body), rest?)   *)      
    
  | _ -> parse_Easg tokens
           
 (* Easg -> Eor Easgl *)
and parse_Easg (tokens : token list) : ast * token list = failwith "parser de Easg nao implementado" 
  

(* Easgl -> ":=" Easg | ε  (Associativo à direita) *)
and parse_Easgl (lhs : ast) (tokens: token list) :  ast * token list = failwith "parser de Easgl nao implementado"
  
    
(* Eor -> Eand Eorl *)
and parse_Eor (tokens : token list) : ast * token list = failwith "parser de Eor nao implementado" 
  

(* Eorl -> "||" Eand Eorl | ε *)
and parse_Eorl (lhs : ast) (tokens: token list) : ast * token list = failwith "parser de Eorl nao implementado"
  
  
(* Eand -> Ecmp Eandl *)
and parse_Eand (tokens : token list) : ast * token list = failwith "parser de Eand nao implementado"
  

(* Eandl -> "&&" Ecmp Eandl | ε *)
and parse_Eandl (lhs : ast) (tokens: token list) : ast * token list = failwith "parser de Eandl nao implementado"
  
  

(* Ecmp -> Eadd Ecmpl *)
and parse_Ecmp (tokens : token list) : ast * token list = failwith "parser de Ecmp nao implementado"
  

(* Ecmpl -> "=" Eadd Ecmpl | "<" Eadd Ecmpl | "<=" Eadd Ecmpl 
          | ">" Eadd Ecmpl | ">=" Eadd Ecmpl | ε *)
and parse_Ecmpl (lhs : ast) (tokens: token list) : ast * token list = failwith "parser de Ecmpl nao implementado"
  
  

(* Eadd -> Emul Eaddl *)
and parse_Eadd (tokens : token list) : ast * token list = failwith "parser de Eadd nao implementado"
  

(* Eaddl -> "+" Emul Eaddl | "-" Emul Eaddl | ε *)
and parse_Eaddl (lhs : ast) (tokens:token list) : ast * token list = failwith "parser de Eaddl nao implementado"
  

(* Emul -> Eun Emull *)
and parse_Emul (tokens : token list) : ast * token list = failwith "parser de Emul nao implementado"
  
(* Emull -> "*" Eun Emull | "/" Eun Emull | ε *)
and parse_Emull (lhs : ast) (token :token list) : ast * token list = failwith "parser de Emull nao implementado"
  
(* Eun ->  not Eun  |  deref Eun   |  new Eun  |  Eatm  *)
and parse_Eun (tokens : token list) : ast * token list = 
  match tokens with
  | NEW :: rest -> let (e1, rest1) = parse_Eun rest in
     (New(e1), rest1)
  | _ -> parse_Eatm tokens
  
(*  Eatm -> true | false | n | id | "()" | "(" E ")"   *)     
and parse_Eatm (tokens : token list) : ast * token list =
  match tokens with
  | ID x :: rest -> (Id(x), rest)
  | NUM x :: rest -> (Num(x), rest)
  | TRUE :: rest -> (Bool(true), rest)
  | FALSE :: rest -> (Bool(false), rest)
  | LPAR :: RPAR :: rest -> (Unit, rest)
  | LPAR :: rest -> let (e1, rest1) = parse_E rest in
      (match rest1 with
       | RPAR :: rest2 -> (e1, rest2)
       | _ -> failwith "erro de sintaxe - falta )")
  | _ -> failwith "erro de sintaxe - valor atomico não reconhecido"
    
        
    (*====================*)      


let parse (tokens: token list) : ast =
  let (ast_E, resto_tokens) = parse_E tokens in
  match resto_tokens with
  | [EOF] -> ast_E
  | _ -> failwith "há tokens sobrando" 


let parse_string (input:string) : ast =
  let tokens : token list = scan input in
  parse tokens
      
let teste = [TRUE; FALSE; NUM 5; ID "ab"; LPAR; RPAR; LPAR; NUM 10; RPAR; EOF]