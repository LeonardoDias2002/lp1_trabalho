open Datatypes

(* ===== Função para reconhcer palavras chave e identificadores =====  *) 

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
  | "not"   -> NOT
  | _       -> ID s


exception Erro_Lexico of int

(* ===== Reconhece intervalo de letras e dígitos =====  *) 

let is_letter c = c >= 'a' && c <= 'z'
let is_digit  c = c >= '0' && c <= '9'

(* Consome o sufixo de um identificador *)
let rec scan_id input pos =
  if pos < String.length input &&
     (is_letter input.[pos] || is_digit input.[pos])
  then scan_id input (pos + 1)
  else pos

(* Consome os sufixo de um numero *)
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
    | '(' -> (LPAR, pos + 1)


    | ':' when pos + 1 < n && input.[pos + 1] = '=' -> (ASG, pos + 2)
    | ':' -> (COLON, pos + 1)
    | '{' -> (LBRAC, pos + 1)
    | '}' -> (RBRAC, pos + 1)
    | '=' -> (EQ, pos + 1)
    | ')' -> (RPAR, pos + 1)

    | '+' -> (SUM, pos + 1)
    | '-' -> (SUB, pos + 1)
    | '*' -> (MUL, pos + 1)
    | '/' -> (DIV, pos + 1)

    | '<' when pos + 1 < n && input.[pos + 1] = '=' -> (LE, pos + 2)
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

(* ===== Gera uma lista de tokens começando na posição 0 da entrada =====  *) 
let scan input = scan_from input 0