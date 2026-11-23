open Interpretador_lp1.Datatypes
open Interpretador_lp1.Scanner

let () =
  (* 1. Teste de caso válido com let*)
  let let_completo = scan "let x : int = 10" in
  assert (let_completo = [ LET; ID "x"; COLON; TINT; EQ; NUM 10; EOF ]);

  (* 2. Teste de caso válido com identificadores e números *)
  let id_e_num = scan "x986      +     5 * 2 - \n(var / 8) \t> 10" in
  assert (id_e_num = [ ID "x986"; SUM; NUM 5; MUL; NUM 2; SUB; LPAR; ID "var"; DIV; NUM 8; RPAR; GT; NUM 10; EOF ]);

  (* 3. Teste de caso válido com if, while e atribuição *)
  let if_while_atribuicao = scan "if (x > 0) then { y := 1 } else { y := 2 } while (y < 10) do { y := y + 1 }" in
  assert (if_while_atribuicao = [ IF; LPAR; ID "x"; GT; NUM 0; RPAR; THEN; LBRAC; ID "y"; ASG; NUM 1; RBRAC; ELSE; LBRAC; ID "y"; ASG; NUM 2; RBRAC; WHILE; LPAR; ID "y"; LT; NUM 10; RPAR; DO; LBRAC; ID "y"; ASG; ID "y"; SUM; NUM 1; RBRAC; EOF ]);

  (* 4. Teste de erro com símbolo inválido *)
  let simbolo_invalido =
    try
      let _ = scan "10 @ 20" in
      false
    with Erro_Lexico _ -> true
  in
  assert simbolo_invalido;

  (* 5. Teste de erro com número inválido *)
let numero_invalido =
    try
      let _ = scan "12345678901234567890" in
      false
    with Erro_Lexico _ -> true
  in
  assert numero_invalido;

  (* 6. Teste de erro com identificador inválido *)
  let id_invalido =
    try
      let _ = scan "var$" in
      false
    with Erro_Lexico _ -> true
  in
  assert id_invalido;

  print_endline "Testes do Lexer passaram!"