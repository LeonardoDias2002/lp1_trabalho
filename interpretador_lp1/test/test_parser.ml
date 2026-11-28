open Interpretador_lp1.Datatypes
open Interpretador_lp1.Parser

let () =
  (* 1. Teste de caso válido *)
  let ast = parse_string "let x : int = 10 in { x }" in
  assert (ast = Let ("x", TInt, Num 10, Id "x"));

  (* 2. Teste de erro esperado *)
  let falhou_como_esperado =
    try
      let _ = parse_string "let x : int = true in x }" in
      false
    with Failure _ -> true
  in
  assert falhou_como_esperado;

  print_endline "Testes do Parser passaram!"