(*++++++++++++++++++++++++++++++++++++++*)
(*  Trabalho de Linguagens              *)
(*   de Programação I                   *)
(*   Autores: Leonardo Leal,            *)
(*  Pedro Schuck e Rafael Stephanou     *)
(*++++++++++++++++++++++++++++++++++++++*)


(* ===== Datatypes para ASTs e funções auxiliares sobre ASTs =====  *)

type tp =
  | TInt
  | TBool
  | TUnit
  | TRef of tp


type bop =
  | Or
  | And
  | Eq
  | Lt
  | Gt
  | Sum
  | Sub
  | Mul
  | Div

type ast =
  | Num of int
  | Id of string
  | Bool of bool
  | Not of ast
  | Binop of bop * ast * ast
  | Let of string * tp * ast * ast
  | If of ast * ast * ast
    (* construçoes imperativas *)
  | Unit
  | Deref of ast
  | New of ast
  | Asg of string * ast (* x := e *)
  | Seq of ast * ast
  | Whi of ast * ast
  | Loc of int

type value =
  | VNum of int
  | VBool of bool
  | VUnit
  | VLoc of int
and runEnv =
  (string * value) list

type memory = (int * value) list



(* ==== Análise Semântica ===== *)


type tyenv = (string * tp)  list


let insert k v lst = (k, v) :: lst

let rec lookup k lst =
  match lst with
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let insertMem v lst =
  let next_loc = List.length lst in
  (next_loc, v) :: lst

let rec updateMem k v lst =
  match lst with
  | [] -> []
  | (k', v') :: t -> if k = k' then (k, v) :: t else (k', v') :: (updateMem k v t)

exception TypeError of string  (* erro a ser reportado para o usuário do interpretador *)


(* type_of pode ativar exceçao TypeError *)
let rec type_of (g:tyenv) (e:ast) : tp =
  match e with
  | Num _  -> TInt

  | Bool _ -> TBool

  | Loc _ -> raise (TypeError "Locacoes de memoria so existem em tempo de execucao")

  | Binop(bop, e1,e2) ->
      let t1 = type_of g e1 in
      let t2 = type_of g e2 in
      (match bop with
       | Sum | Sub | Mul | Div ->
           if t1 = TInt && t2 = TInt then TInt
           else raise (TypeError "Operadores aritméticos requerem operandos inteiros")
       | Eq | Lt | Gt ->
           if t1 = TInt && t2 = TInt then TBool
           else raise (TypeError "Operadores relacionais requerem operandos inteiros")
       | And | Or  ->
           if t1 = TBool && t2 = TBool then TBool
           else raise (TypeError "Operadores lógicos requerem operandos booleanos"))


  | Not(e1) -> if type_of g e1 <> TBool then raise (TypeError "Not deve receber um booleano")
      else
        TBool

  | If(e1,e2,e3) ->
      if type_of g e1 <> TBool then raise (TypeError "Condiçao de if-then-else deve ser do tipo bool")
      else
        let t2 = type_of g e2 in
        let t3 = type_of g e3 in
        if t2 <> t3 then raise (TypeError "Expressoes do then e do else devem ser do mesmo tipo")
        else t2

  | Id x ->
      (match lookup x g with
       | Some t -> t
       | None -> raise (TypeError ("Variável não declarada: " ^ x)))

  | Let(x,t,e1,e2) ->
      let t1 = type_of g e1 in
      if t1 <> t then raise (TypeError "Tipo da expressão difere do tipo declarado no Let")
      else type_of (insert x t g) e2

  | Deref(e1) ->
      (match type_of g e1 with
       | TRef(t) -> t
       |_ -> raise (TypeError "Tentativa de acessar referência de valor não ponteiro"))


  | New(e1) ->
      let t1 = type_of g e1 in
      TRef t1

  | Unit -> TUnit


  | Asg(x, e1) -> let t1 = type_of g (Id x) in
      let t2 = type_of g e1 in
      (match t1 with
       | TRef(t') -> if t' <> t2 then raise (TypeError "Tipo do endereço e elemento atribuído devem ser do mesmo tipo")
           else
             TUnit
       | _ -> raise (TypeError "Alvo da atribuição não é endereço de memória"))

  | Seq(e1, e2) ->
      if type_of g e1 <> TUnit then raise (TypeError "Expressão à esquerda da sequência não é do tipo unit")
      else type_of g e2

  | Whi(e1, e2) -> if type_of g e1 <> TBool then raise (TypeError "Condiçao de while deve ser do tipo bool")
      else
      if type_of g e2 <> TUnit then raise (TypeError "Expressao do while deve ser do tipo unit")
      else
        TUnit


(* === Avaliador ==== *)


exception RunTimeError of string (* erro a ser reportado para o usuário do interpretador *)


(* implementa semantica big step *)
let rec eval (r: runEnv) (e:ast) (m:memory): value * memory  =
  match e with
  | Num n   -> (VNum n, m)
  | Bool b  -> (VBool b, m)
  | Loc l ->(VLoc l, m)

  | Binop(bop, e1,e2)  ->
      let (v1, m1) = eval r e1 m in
      let (v2, m2) = eval r e2 m1 in
      (match (bop, v1, v2) with
       | (Sum, VNum n1, VNum n2)  -> (VNum(n1 + n2), m2)
       | (Sub, VNum n1, VNum n2)  -> (VNum(n1 - n2), m2)
       | (Mul, VNum n1, VNum n2)  -> (VNum(n1 * n2), m2)
       | (Div, VNum n1, VNum 0)  -> raise (RunTimeError "Divisao por zero")
       | (Div, VNum n1, VNum n2)  -> (VNum(n1 / n2), m2)
       | (And, VBool b1, VBool b2)  -> (VBool(b1 && b2), m2)
       | (Or, VBool b1, VBool b2)  -> (VBool(b1 || b2), m2)
       | (Eq, VNum n1, VNum n2)  -> (VBool(n1 = n2), m2)
       | (Gt, VNum n1, VNum n2)  -> (VBool(n1 > n2), m2)
       | (Lt, VNum n1, VNum n2)  -> (VBool(n1 < n2), m2)
       | _ -> raise (RunTimeError "Tentativa de executar operaçao binária com operandos invalidos" ))

  | If(e1,e2,e3) ->
      let (v1, m1) = eval r e1 m in
      (match v1 with
       | VBool true -> eval r e2 m1
       | VBool false -> eval r e3 m1
       | _ -> raise (RunTimeError "Condicao do if nao e booleana"))

  | Not(e1)  -> let (v1, m1) = eval r e1 m in
      (match (v1) with
       | (VBool b1) -> (VBool(not b1), m1)
       | _ -> raise (RunTimeError "Tentativa de executar not com operando invalido"))

  | Id x ->
      (match lookup x r with
       | Some v -> (v, m)
       | None -> raise (RunTimeError ("Variável não declarada: " ^ x)))

  | Let(x,_,e1,e2) ->
      let (v1, m1) = eval r e1 m in
      eval ((x,v1) :: r) e2 m1

  | Deref(e1) -> let (v1, m1) = eval r e1 m in
      (match v1 with
       | VLoc l -> (match lookup l m1 with
           | None -> raise (RunTimeError "Endereço acessado não existe na memória")
           | Some v -> (v, m1))
       | _ -> raise (RunTimeError "Expressão da desreferência deve ser endereço"))

  | New(e1) ->
      let (v1, m1) = eval r e1 m in
      let l = List.length m1 in
      (VLoc l, insertMem v1 m1)

  | Unit ->(VUnit, m)

  | Asg(x, e1) ->
      let (v1, m1) = eval r e1 m in
      (match lookup x r with
       | Some (VLoc l) ->
           (match lookup l m1 with
            | None -> raise (RunTimeError "Endereço acessado não existe na memória")
            | Some _ -> (VUnit, updateMem l v1 m1))
       | Some _ -> raise (RunTimeError ("Tentativa de atribuicao em variavel que nao e referencia: " ^ x))
       | None -> raise (RunTimeError ("Variavel nao declarada: " ^ x)))

  | Seq(e1, e2) -> let (v1, m1) = eval r e1 m in
      if v1 <> VUnit then raise (RunTimeError "Primeira expressão da sequência não produz Unit")
      else
        eval r e2 m1

  | Whi(e1, e2) ->
      let (v1, m1) = eval r e1 m in
      (match v1 with
       | VBool false -> (VUnit, m1)
       | VBool true ->
           let (v2, m2) = eval r e2 m1 in
           if v2 <> VUnit then raise (RunTimeError "Expressão do while não produz Unit")
           else eval r (Whi(e1, e2)) m2
       | _ -> raise (RunTimeError "Condicao do while nao e booleana"))


(* ====== TESTES ===== *)

let tst1 = Binop(Sum, Num 10, Num 5)
let tst2 = If(Binop(Eq, tst1, Num 15), Num 5,  Num 50)
let tst3 = If(Binop(Sum, tst1, Num 15), Num 5,  Num 50)
let tst4 = If(Binop(Eq, tst1, Bool true), Num 5,  Num 50)
let tst5 = If(Bool true, Num 5, Bool false)
let tst6 = Binop(Div, Num 6, Num 0)
let tst7 = Binop(Mul, Num 10, Bool true)
let tst15 = Binop(Sum, Id "z", Num 5)
let tst16 = Let("r", TInt, Bool true, Binop(Div, Id "r", Num 2))

(* Testes do TypeInfer (ambiente vazio) *)

(* --- Casos sem erros --- *)
let infer_ok_1 = type_of [] (Let("x", TInt, Num 10, Binop(Sum, Id "x", Num 5)))
let infer_ok_2 = type_of [] (If(Binop(Gt, Num 10, Num 5), Bool true, Bool false))
let infer_ok_3 = type_of [] (New(Num 5))
let infer_ok_4 = type_of [] (Let("ptr", TRef TInt, New(Num 0), Deref(Id "ptr")))
let infer_ok_5 = type_of [] (Let("ptr", TRef TInt, New(Num 0), Seq(Asg("ptr", Num 10), Deref(Id "ptr"))))
let infer_ok_6 = type_of [] (Whi(Binop(Lt, Num 0, Num 10), Unit))

(* Testes do Avaliador Big Step *)

(* Testa If verdadeiro *)
let eval_ok_1 = eval [] (If(Bool true, Num 10, Num 20)) []

(* Testa If falso *)
let eval_ok_2 = eval [] (If(Bool false, Num 10, Num 20)) []

(* Testa execucao completa de alocacao, atribuicao e desreferencia *)
let eval_ok_3 = eval [] (Let("ptr", TRef TInt, New(Num 0), Seq(Asg("ptr", Num 42), Deref(Id "ptr")))) []

(* Testa laco While com alocacao, desreferencia, operacao binaria e atribuicao *)
(* Equivalente a: let c = new 3 in while (!c > 0) do c := !c - 1; !c *)
let eval_ok_4 = eval []
  (Let("c", TRef TInt, New(Num 3),
    Seq(
      Whi(Binop(Gt, Deref(Id "c"), Num 0),
          Asg("c", Binop(Sub, Deref(Id "c"), Num 1))),
      Deref(Id "c")))) []