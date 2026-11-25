open Datatypes  

(* ===== Ambiente de Tipos =====  *) 
type tyenv = (string * tp)  list

exception TypeError of string  (* erro de tipo a ser reportado para o usuário do interpretador *)

(* type_of pode ativar exceçao TypeError *) 
(* ===== Função para inferir o tipo de uma expressão em um ambiente de tipos =====  *) 
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
       | Eq | Lt | Gt | Le | Ge ->
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