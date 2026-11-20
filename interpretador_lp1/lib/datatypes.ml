(* ===== Datatypes para ASTs, tipos, memória e ambiente de execução =====  *) 

type tp =
  | TInt
  | TBool
  | TUnit
  | TRef of tp
  
type bop = 
  | Or
  | And
  | Eq
  | Lt
  | Gt
 (* | Le
  | Ge *)
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
  | Asg of string * ast (* x := e *)
  | Seq of ast * ast 
  | Whi of ast * ast 
  | Loc of int

type value = 
  | VNum of int
  | VBool of bool 
  | VUnit
  | VLoc of int
and runEnv = 
  (string * value) list 
      
type memory = (int * value) list

type tyenv = (string * tp)  list

(* funções auxiliares *)

let insert k v lst = (k, v) :: lst

let rec lookup k lst = 
  match lst with 
  | [] -> None  
  | (k', v) :: t -> if k = k' then Some v else lookup k t 



(* temporário *)
type token =
  | ID of string
  | NUM of int
  | TRUE | FALSE
  | LET  | IN
  | LBRAC | RBRAC
  | LPAR | RPAR
  | NEW
  | COLON
  | TINT | TBOOL | TUNIT | TREF
  | UNIT
  | EQ
  | IF | THEN | ELSE
  | WHILE | DO
  | ASG
  | SUM | SUB | MUL | DIV
  | LT | GT | LE | GE
  | AND | OR | NOT
  | DEREF
  | SEQ
  | EOF