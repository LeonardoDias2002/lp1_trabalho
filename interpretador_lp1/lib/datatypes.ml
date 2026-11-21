(* ===== Datatypes para Tipos, Operações Binárias e ASTs =====  *) 

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


(* ===== Funções auxiliares de inserção e busca em listas =====  *) 
let insert k v lst = (k, v) :: lst

let rec lookup k lst = 
  match lst with 
  | [] -> None  
  | (k', v) :: t -> if k = k' then Some v else lookup k t 


(* ===== Datatype para Tokens =====  *) 
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