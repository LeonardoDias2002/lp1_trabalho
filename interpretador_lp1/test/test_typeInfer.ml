open Interpretador_lp1.Datatypes
open Interpretador_lp1.TypeInfer

let () =
  (* Testa inferência de tipo para uma soma *)
  let soma = Binop (Sum, Num 2, Num 3) in
  let t = type_of [] soma in
  assert (t = TInt);

  (* Testa inferência de tipo para uma expressão de soma com erro *)
  let soma_errada = Binop (Sum, Num 2, Bool true) in
  let erro_de_tipo_soma =
    try
      let _ = type_of [] soma_errada in
      false
    with TypeError _ -> true
  in
  assert erro_de_tipo_soma;

  (* Testa inferência de tipo para uma expressão if *)
  let if_expr = If (Bool true, Num 1, Num 0) in
  let t_if = type_of [] if_expr in
  assert (t_if = TInt);

  (* Testa inferência de tipo para uma expressão if com erro *)
  let if_errada = If (Bool false, Num 1, Bool true) in
  let erro_de_tipo_if =
    try
      let _ = type_of [] if_errada in
      false
    with TypeError _ -> true
  in
  assert erro_de_tipo_if;

  (* Testa inferência de tipo para uma expressão let *)
  let let_expr = Let ("x", TInt, Num 10, Binop (Sum, Id "x", Num 5)) in
  let t_let = type_of [] let_expr in
  assert (t_let = TInt);

  (* Testa inferência de tipo para uma expressão let com erro *)
  let let_errada = Let ("x", TInt, Bool true, Binop (Sum, Id "x", Num 5)) in
  let erro_de_tipo_let =
    try
      let _ = type_of [] let_errada in
      false
    with TypeError _ -> true
  in
  assert erro_de_tipo_let;

(* Testa inferência de tipo para uma expressão com variável não declarada*)
  let var_nao_declarada = Id "y" in
  let erro_var_nao_declarada =
    try
      let _ = type_of [] var_nao_declarada in
      false
    with TypeError _ -> true
  in
  assert erro_var_nao_declarada;

  (* Testa inferência de tipo para uma expressão de atribuição *)
  let atrib = Let ("x", TRef TInt, New (Num 10), Asg ("x", Num 5)) in
  let t_atrib = type_of [] atrib in
  assert (t_atrib = TUnit);

  (* Testa inferência de tipo para uma expressão de atribuição com erro *)
  let atrib_errada = Let ("x", TRef TInt, New (Num 10), Asg ("x", Bool true)) in
  let erro_de_tipo_atrib =
    try
      let _ = type_of [] atrib_errada in
      false
    with TypeError _ -> true
  in
  assert erro_de_tipo_atrib;

  (* Testa inferência de tipo para uma expressão while *)
  let while_expr = Whi (Bool true, Seq (Asg ("x", Num 1), Asg("x", Num 3))) in
  let t_while = type_of [("x", TRef TInt)] while_expr in
  assert (t_while = TUnit);

  (* Testa inferência de tipo para uma expressão while com erro *)
  let while_errada = Whi (Bool false, Seq (Asg ("x", Num 1), Deref (Id "x"))) in
  let erro_de_tipo_while =
    try
      let _ = type_of [("x", TRef TInt)] while_errada in
      false
    with TypeError _ -> true
  in
  assert erro_de_tipo_while;

  print_endline "Testes do TypeInfer passaram!"