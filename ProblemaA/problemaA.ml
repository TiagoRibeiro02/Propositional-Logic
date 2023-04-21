(*
  Joao Tiago a47817
  Tiago Ribeiro a46346
*)


open F_parser

(*Definicao o tipo nor_formula que representa uma formula Nor e a variavel*)
type nor_formula =
  | V of string
  | Nor of nor_formula * nor_formula

(*Esta funcao recebe um auxiliar(que inicialmente e sempre o maior valor "Z") e compara ate achar a menor variavel na expressao, a qual vai ser usada para representar False.*)
let rec smallest_variable expression aux =
  match expression with
  | Var v -> if v<aux then v else aux
  | Not f -> smallest_variable f aux
  | And(f, g) -> let acc = smallest_variable f aux in smallest_variable g acc
  | Or(f, g) -> let acc = smallest_variable f aux in smallest_variable g acc
  | Implies(f, g) -> let acc = smallest_variable f aux in smallest_variable g acc
  | Equiv(f, g) -> let acc = smallest_variable f aux in smallest_variable g acc
  | False -> aux
  | True -> aux

  (*Esta funcao realiza a transformacao da formula para a forma normal de Nor utilizando as regras de equivalencia logica. E passada a string smallest que foi calculada anteriormente para representar False.*)
let rec to_nor (f : formula_t) (smallest: string) : nor_formula = 
  match f with
  | Var f -> V f
  | Not f ->
    (match f with
     | Or(a, b) -> Nor(to_nor a smallest, to_nor b smallest)
     | _ -> Nor(to_nor f smallest, to_nor f smallest))
  | And (a, b) -> Nor(Nor(to_nor a smallest, to_nor a smallest), Nor(to_nor b smallest, to_nor b smallest))
  | Or (a, b) -> Nor(Nor(to_nor a smallest, to_nor b smallest), Nor(to_nor a smallest, to_nor b smallest))
  | Implies (a, b) -> to_nor (Or(Not(a), b)) smallest
  | Equiv (a, b) -> to_nor (And(Implies(a, b), Implies(b, a))) smallest
  | False -> Nor(V smallest, (Nor(V smallest, V smallest)))
  | True -> to_nor (Not(False)) smallest

(*Transforma a nor_formula em string*)
let rec formula_to_string (f : nor_formula) : string =
  match f with
  | Nor (f1, f2) -> Printf.sprintf "(%s %% %s)" (formula_to_string f1) (formula_to_string f2)
  | V f -> Printf.sprintf "%s" (f)

let f_list = parse "stdin"

let form = 
  match f_list with
  | Some l -> List.hd l
  | _ -> assert false

let smallest = smallest_variable form "Z";;

let () =
  let result = to_nor form smallest in
  formula_to_string result |> print_endline

(*
Algoritmo de "smallest_variable":

EXEMPLO 1:
let smallest = smallest_variable AND(B, C) 'Z' =
let acc = smallest_variable B aux='Z' in smallest_variable C acc
smallest_variable B aux='Z' = "B"
smallest_variable C "B"
= {B}
logo, AND(B,C) = {B}
-----------------------------------------------------------------------------

EXEMPLO 2:
let smallest = smallest_variable And[C, Or(FALSE, D)] 'Z' =
let acc = smallest_variable C aux='Z' in smallest_variable (Or(FALSE, D)) acc
smallest_variable C aux='Z' = "C"
smallest_variable {Or(FALSE, D)} "C"
smallest_variable {D} "C"
= C

Calculos auxiliares:
let smallest = smallest_variable Or(FALSE, D) 'Z' =
let acc = smallest_variable FALSE aux='Z' in smallest_variable D acc
smallest_variable FALSE aux='Z' = "Z"
smallest_variable D "Z"
= {D}
logo, Or(FALSE, D) = {D}


*)