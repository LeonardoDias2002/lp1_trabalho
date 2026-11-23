open Interpretador_lp1.Datatypes
open Interpretador_lp1.Scanner

let () =
  (* 1. Teste de caso válido *)
  let tokens = scan "let x : int = 10" in
  assert (tokens = [ LET; ID "x"; COLON; TINT; EQ; NUM 10; EOF ]);

  (* 2. Teste de erro esperado *)
  let falhou_como_esperado =
    try
      let _ = scan "10 @ 20" in
      false
    with Erro_Lexico _ -> true
  in
  assert falhou_como_esperado;

  print_endline "Testes do Lexer passaram!"