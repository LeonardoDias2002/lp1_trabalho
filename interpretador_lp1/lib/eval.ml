open Datatypes

(* =====  Valores, Ambiente de Execução e Memória =====  *) 
type value = 
  | VNum of int
  | VBool of bool 
  | VUnit
  | VLoc of int
and runEnv = 
  (string * value) list 
      
type memory = (int * value) list

exception RunTimeError of string (* erro de execução a ser reportado para o usuário do interpretador *)  
    
(* ===== Funções auxiliares de manipulação de memória =====  *) 
let insertMem v lst =
  let next_loc = List.length lst in
  (next_loc, v) :: lst
  
let rec updateMem l v (m: memory) = 
  match m with 
  | [] -> raise (RunTimeError "Endereço não encontrado para atualização")
  | (l', v') :: t -> if l = l' then (l, v) :: t else (l', v') :: updateMem l v t      


(* === Avaliador com semântica big step ==== *)                       
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
       | Some (VLoc l) -> let m2 = updateMem l v1 m1 in
           (VUnit, m2) 
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
           else
             eval r (Whi(e1, e2)) m2
       | _ -> raise (RunTimeError "Condicao do while nao e booleana"))