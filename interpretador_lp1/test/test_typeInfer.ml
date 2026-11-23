open Interpretador_lp1.Datatypes
open Interpretador_lp1.TypeInfer

let () =
  (* Testa inferência de tipo para uma expressão simples *)
  let ast = Binop (Sum, Num 2, Num 3) in
  let t = type_of [] ast in
  assert (t = TInt);

  (* Testa inferência de tipo para uma expressão com erro de tipo *)
  let ast_errado = Binop (Sum, Num 2, Bool true) in
  let erro_de_tipo =
    try
      let _ = type_of [] ast_errado in
      false
    with TypeError _ -> true
  in
  assert erro_de_tipo;

  print_endline "Testes do TypeInfer passaram!"