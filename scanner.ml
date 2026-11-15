type token =
  | IDENT of string
  | NUM of int
  | TRUE | FALSE
  | LET  | IN
  | LKEY | RKEY
  | LPAREN | RPAREN
  | NEW
  | COLON
  | TINT | TBOOL | TUNIT | TREF
  | UNIT
  | EQ
  | IF | THEN | ELSE
  | WHILE | DO
  | ASSIGN
  | SUM | SUB | MUL | DIV
  | LT | GT | LE | GE | NEQ
  | AND | OR | NOT
  | DEREF
  | SEQ
  | EOF

let to_id_or_kw s =
  match s with
  | "true"  -> TRUE
  | "false" -> FALSE
  | "let"   -> LET
  | "in"    -> IN
  | "new"   -> NEW
  | "int"   -> TINT
  | "bool"  -> TBOOL
  | "unit"  -> TUNIT
  | "ref"   -> TREF
  | "if"    -> IF
  | "then"  -> THEN
  | "else"  -> ELSE
  | "while" -> WHILE
  | "do"    -> DO
  | "and"   -> AND
  | "or"    -> OR
  | "not"   -> NOT
  | _       -> IDENT s


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


(* Reconhece o proximo token, ignorando espacos e comentarios. *)
let rec next_token input pos =
  let n = String.length input in
  if pos >= n then (EOF, pos)
  else
    match input.[pos] with
    | ' ' | '\t' | '\n' | '\r' ->
        next_token input (pos + 1) (* despreza caracteres "espaço em branco" *)

    | 'a'..'z' ->
        let stop = scan_id input (pos + 1) in
        let lexeme = String.sub input pos (stop - pos) in
        (to_id_or_kw lexeme, stop)

    | '0'..'9' ->
        let stop = scan_nro input (pos + 1) in

        if stop < n && is_letter input.[stop] then (* rejeita "123abc" por exemplo *)
          raise (Erro_Lexico stop)
        else

          let lexeme = String.sub input pos (stop - pos) in

          let value =
            try int_of_string lexeme (*  ativa exceção se o nro representado pelo string está fora do intervalo possível *)
            with Failure _ -> raise (Erro_Lexico pos)
          in

          (NUM value, stop)

    | '(' when pos + 1 < n && input.[pos + 1] = ')' ->
        (UNIT, pos + 2)
    | '(' -> (LPAREN, pos + 1)


    | ':' when pos + 1 < n && input.[pos + 1] = '=' -> (ASSIGN, pos + 2)
    | ':' -> (COLON, pos + 1)
    | '{' -> (LKEY, pos + 1)
    | '}' -> (RKEY, pos + 1)
    | '=' -> (EQ, pos + 1)
    | ')' -> (RPAREN, pos + 1)

    | '+' -> (SUM, pos + 1)
    | '-' -> (SUB, pos + 1)
    | '*' -> (MUL, pos + 1)
    | '/' -> (DIV, pos + 1)

    | '<' when pos + 1 < n && input.[pos + 1] = '=' -> (LE, pos + 2)
    | '<' when pos + 1 < n && input.[pos + 1] = '>' -> (NEQ, pos + 2)
    | '<' -> (LT, pos + 1)
    | '>' when pos + 1 < n && input.[pos + 1] = '=' -> (GE, pos + 2)
    | '>' -> (GT, pos + 1)

    | '&' when pos + 1 < n && input.[pos + 1] = '&' -> (AND, pos + 2)
    | '|' when pos + 1 < n && input.[pos + 1] = '|' -> (OR, pos + 2)

    | '!' -> (DEREF, pos + 1)
    | ';' -> (SEQ, pos + 1)

    | _ -> raise (Erro_Lexico pos)

(* Percorre a entrada produzindo tokens. *)
let rec scan_from input pos =
  let (tok, next) = next_token input pos in
  match tok with
  | EOF -> [EOF]
  | _ -> tok :: (scan_from input next)

let scan input = scan_from input 0


(*   exemplos para teste
      =====================


Identificadores*)
let id1 = scan "x";;
let id2 = scan "abc";;
let id3 = scan "x123";;
let id4 = scan "let";;
let id5 = scan "true false";;
let id6 = scan "letx";;
let id7 = scan "let x = 10 in x";;


(*Números e operadores*)
let num1 = scan "123";;
let num2 = scan "00123";;
let num3 = scan "10+20";;
let num4 = scan "10-5";;
let num5 = scan "2*3+4";;
let num6 = scan "x<=10";;
let num7 = scan "x>=20";;
let num8 = scan "x<>y";;
let num9 = scan "x=(10+2)";;
let num10 = scan "a<b";;
let num11 = scan "10/2";;
let num12 = scan "a>b && c<d || not e";;


(*Com espaços em branco*)
let esp1 = scan "";;
let esp2 = scan "   ";;
let esp3 = scan "  let   x = 10   in x  ";;
let esp4 = scan "\tlet\tx\t=\t10";;
let esp5 = scan "let\nx = 10\nin x";;
let esp6 = scan "let\r\nx = 10";;


(*Com erros léxicos*)
(*
let err1 = scan "123abc";;
let err2 = scan "12x";;
let err3 = scan "abc_def";;
let err4 = scan "ABC";;
let err5 = scan "@";;
let err6 = scan "4611686018427387904";;
*)


(*If*)
let if1 = scan "if then else";;
let if2 = scan "if x then {1} else {2}";;
let if3 = scan "if true then {x} else {y}";;
let if4 = scan "ifx thenx elsex";;
let if5 = scan "if x then {if y then {1} else {2}} else {3}";;


(*While*)
let wh1 = scan "while x do {y := 1}";;
let wh2 = scan "while true do {()}";;
let wh3 = scan "whilex dox";;
let wh4 = scan "do while";;
let wh5 = scan "let x : int = 0 in {while true do {x := 1}}";;


(*Atribuição*)
let atr1 = scan "x := 1";;
let atr2 = scan "x:=1";;
let atr3 = scan "x := new 5";;

(*Dois-pontos*)
let col1 = scan "x : = 1";;
let col2 = scan "let x : int = 5 in {x}";;
let col3 = scan ":=:";;
let col4 = scan "x :";;

(*Desreferência e Sequência*)
let der1 = scan "!x";;
let der2 = scan "!x + 1";;
let seq1 = scan "x := 1; !x";;
let seq2 = scan "let x : ref int = new 0 in {x := !x + 1; !x}";;