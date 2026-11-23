open Interpretador_lp1.Datatypes
open Interpretador_lp1.Eval

let () =
  (* Testa: 2 + 3 * 4 -> deve resultar em 14 *)
  let ast = Binop (Sum, Num 2, Binop (Mul, Num 3, Num 4)) in
  let valor, _mem = eval [] ast [] in
  assert (valor = VNum 14);

  (* Testa divisão por zero *)
  let div_zero =
    try
      let _ = eval [] (Binop (Div, Num 10, Num 0)) [] in
      false
    with RunTimeError _ -> true
  in
  assert div_zero;

  print_endline "Testes do Eval passaram!"