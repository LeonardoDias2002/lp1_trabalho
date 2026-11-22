open Interpretador_lp1
open Datatypes
open Scanner
open Parser
open TypeInfer
open Eval

(* ==== Funções auxiliares para imprimir resultados =====  *)

let string_of_value (v: value) : string =
  match v with 
  | VNum n  -> string_of_int n 
  | VBool b -> string_of_bool b 
  | VUnit -> "unit"
  | VLoc l -> "address " ^ string_of_int l
      
  
let rec string_of_type (t : tp) : string =
  match t with
  | TInt  -> "int"
  | TBool -> "bool"
  | TUnit -> "Tunit"
  | TRef x -> "ref " ^ string_of_type x
    
(* ==== Interpretador =====  *)

let interpreter (codigo : string) : string = 
  try
    let ast = parse_string codigo in
    let t = type_of [] ast in
    let v = eval    [] ast []  in
    (match v with
     | (v1, mem) -> (string_of_value v1) ^ " : "  ^ (string_of_type t))
  with 
  | Erro_Lexico pos -> "Erro léxico na posição " ^ string_of_int pos
  | Failure msg -> "Erro sintático - " ^ msg
  | TypeError msg    -> "Erro de tipo - " ^ msg 
  | RunTimeError msg -> "Erro de execuçao - " ^ msg 


let ler_arquivo (caminho : string) : string =
  In_channel.with_open_text caminho In_channel.input_all

let () =
  print_endline (interpreter "let x : int = 42 in { x }");
  print_endline (interpreter "let r : ref int = new 10 in { r }");
  print_endline (interpreter "let x : int = true in { x }"); (* Deve acusar Erro de tipo *)
  
  if Array.length Sys.argv < 2 then
    prerr_endline "Uso: dune exec interpretador_lp1 -- <caminho_do_arquivo>"
  else 
    let caminho = Sys.argv.(1) in
    try 
      let codigo = ler_arquivo caminho in
      print_endline (interpreter codigo)
    with
    | Sys_error msg -> prerr_endline ("Erro ao ler o arquivo: " ^ msg)