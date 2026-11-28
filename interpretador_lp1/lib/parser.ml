open Datatypes
open Scanner

(* ===== Funções auxiliares para reconhecimento do conjunto Follow =====  *) 
let is_follow_El tok = (* é que segue E  *) 
  match tok with 
  | EOF | THEN | ELSE | RBRAC | IN | DO | RPAR -> true  (* o que segue E  *) 
  | _ -> false

let is_follow_Easgl tok = (* é o  que segue E_asg, ou seja é follow de El e first de El   *)
  is_follow_El tok || tok = SEQ

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

  
(* ===== Analisa a sintaxe dos tipos =====  *)                         
let rec parse_T (tokens : token list) : tp * token list = 
  match tokens with
  | TINT :: rest -> (TInt, rest)
  | TBOOL :: rest -> (TBool, rest)
  | TUNIT :: rest -> (TUnit, rest)
  | TREF :: rest -> let (t, rest1) = parse_T rest in (TRef t, rest1)
  | _ -> failwith "erro na sintaxe do tipo"
  

(* E -> Ec El *)
let rec parse_E (tokens : token list) : ast * token list  = 
  let (e1, rest) = parse_Ec tokens in
  parse_El e1 rest 
                   
(* El -> ";" Ec El | ε *)
and parse_El (lhs : ast) (tokens: token list) :  ast * token list =
  match tokens with 
  | SEQ :: rest -> (* consome SEQ *)
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
  | IF :: rest ->
      let (cond, rest1) = parse_E rest in
      (match rest1 with
       | THEN :: rest2 ->
           let (e_then, rest3) = parse_E rest2 in
           (match rest3 with
            | ELSE :: LBRAC :: rest4 ->
                let (e_else, rest5) = parse_E rest4 in
                (match rest5 with
                 | RBRAC :: rest6 -> (If (cond, e_then, e_else), rest6)
                 | _ -> failwith "sintaxe do if mal formulada - falta }")
            | _ -> failwith "sintaxe do if mal formulada - falta else ou {")
       | _ -> failwith "sintaxe do if mal formulada - falta then")
  
  | LET :: rest -> 
      (match rest with
       | ID x :: COLON :: rest1 -> let (t, rest2) = parse_T rest1 in
           (match rest2 with
            | EQ :: rest3 -> let (e1, rest4) = parse_E rest3 in
                (match rest4 with
                 | IN :: LBRAC :: rest5 -> let (e2, rest6) = parse_E rest5 in
                     (match rest6 with
                      | RBRAC :: rest7 ->  (Let(x, t, e1, e2), rest7)
                      | _ -> failwith "sintaxe do let mal formulada - falta }")
                 | _ -> failwith "sintaxe do let mal formulada - falta in ou {")
            | _ -> failwith "sintaxe do let mal formulada - falta =")
       | _ -> failwith "sintaxe do let mal formulada - falta id ou :")
                                                                                  
  | WHILE :: rest ->
      let (cond, rest1) = parse_E rest in
      (match rest1 with
       | DO :: rest2 ->
           (match rest2 with
            | LBRAC :: rest3 ->
                let (corpo, rest4) = parse_E rest3 in
                (match rest4 with
                 | RBRAC :: rest5 -> (Whi (cond, corpo), rest5)
                 | _ -> failwith "sintaxe do while mal formulada - falta }")
            | _ -> failwith "sintaxe do while mal formulada - falta {")
       | _ -> failwith "sintaxe do while mal formulada - falta do")
                                                                
  | _ -> parse_Easg tokens
           
 (* Easg -> Eor Easgl *)
and parse_Easg (tokens : token list) : ast * token list =
  let (e1, rest) = parse_Eor tokens in
  parse_Easgl e1 rest


(* Easgl -> ":=" Easg | ε  (Associativo à direita) *)
and parse_Easgl (lhs : ast) (tokens: token list) :  ast * token list =
  match tokens with
  | ASG :: rest ->
      (match lhs with
       | Id x ->
           let (e2, rest1) = parse_Easg rest in   (* lado direito lido pelo mesmo nível (Easg): associativo à direita *)
           (Asg (x, e2), rest1)
       | _ -> failwith "lado esquerdo da atribuição deve ser uma variável")
  | tok :: _ when is_follow_Easgl tok ->
      (lhs, tokens)
  | _ -> failwith "erro na atribuição Easgl"


(* Eor -> Eand Eorl *)
and parse_Eor (tokens : token list) : ast * token list =
(* código temporário apenas para testes, remover quando implementar *)
parse_Eun tokens
  

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
  | UNIT :: rest -> (Unit, rest)
  | LPAR :: rest -> let (e1, rest1) = parse_E rest in
      (match rest1 with
       | RPAR :: rest2 -> (e1, rest2)
       | _ -> failwith "erro de sintaxe - falta )")
  | _ -> failwith "erro de sintaxe - valor atomico não reconhecido"
    
        
    

(* ===== Função principal de parsing =====  *) 
let parse (tokens: token list) : ast =
  let (ast_E, resto_tokens) = parse_E tokens in
  match resto_tokens with
  | [EOF] -> ast_E
  | _ -> failwith "há tokens sobrando" 

(* ===== Função para analisar uma string com Scanner e Parser =====  *) 
let parse_string (input:string) : ast =
  let tokens : token list = scan input in
  parse tokens