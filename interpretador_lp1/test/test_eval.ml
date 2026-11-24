open Interpretador_lp1.Datatypes
open Interpretador_lp1.Eval

let () =
  (* Testa operações aritméticas *)
  let ast_aritm = Binop (Sum, Num 2, Binop (Mul, Num 3, Num 4)) in
  let valor, _mem = eval [] ast_aritm [] in
  assert (valor = VNum 14);

  (* Testa operações na memória *)
  let ast_op_mem = Let ("x", TRef TInt, New (Num 10), Asg("x", Binop(Sum, Deref (Id "x"), Num 5))) in
  let valor_mem, _mem = eval [] ast_op_mem [] in
  assert (valor_mem = VUnit);
  assert (_mem = [(0, VNum 15)]);

  (* Testa while *)
  let ast_while = Let ("x", TRef TInt, New(Num 0), Whi(Binop (Lt, Deref (Id "x"), Num 5), Asg ("x", Binop (Sum, Deref (Id "x"), Num 1)))) in
  let valor_while, _mem_while = eval [] ast_while [] in
  assert (valor_while = VUnit);
  assert (_mem_while = [(0, VNum 5)]);

  (* Testa sequência *)
  let ast_seq = Let ("x", TRef TInt, New(Num 0), Seq (Asg ("x", Num 10), Asg ("x", Num 20))) in
  let valor_seq, _mem_seq = eval [] ast_seq [] in
  assert (valor_seq = VUnit);
  assert (_mem_seq = [(0, VNum 20)]);

  (* Testa atribuição com erro *)
  let ast_asg_erro = Let ("x", TInt, Num 10, Asg("x", Num 5)) in
  let erro_atribuicao =
    try
      let _ = eval [] ast_asg_erro [] in
      false
    with RunTimeError _ -> true
  in
  assert erro_atribuicao;

  (* Testa dereferência de variável não inicializada *)
  let ast_deref_erro = Let ("x", TRef TInt, New (Num 10), Deref (Id "y")) in
  let erro_deref =
    try
      let _ = eval [] ast_deref_erro [] in
      false
    with RunTimeError _ -> true
  in
  assert erro_deref;

  (* Testa divisão por zero *)
  let div_zero =
    try
      let _ = eval [] (Binop (Div, Num 10, Num 0)) [] in
      false
    with RunTimeError _ -> true
  in
  assert div_zero;

  print_endline "Testes do Eval passaram!"