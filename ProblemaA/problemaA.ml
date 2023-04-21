(*
  Joao Tiago a47817
  Tiago Ribeiro a46346
*)

open F_parser

(* Definicao do tipo nor_formula que representa uma formula Nor e a variavel *)
type nor_formula =
  | V of string
  | Nor of nor_formula * nor_formula

(* Esta funcao recursiva terminal recebe um auxiliar 'aux' (que inicialmente começa a 'Z') e compara recursivamente ate achar a menor variavel na expressao, a qual vai ser usada para representar False.*)
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

(* Esta funcao recursiva normal realiza a transformacao da formula_t 'f' para a nor_formula, utilizando as regras de equivalencia logica. Passa-se ainda a string 'smallest' que foi calculada anteriormente para representar False.*)
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

(* Transforma a nor_formula 'f' em string *)
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
Para calcular o "result", vamos inserir a expressão 'form' (que foi dada em input), e o 'smallest', que é a variável com a letra menor (exemplo: em And(C,F)smallest=C).

Para calcular o 'smallest', vamos utilizar a funcao recursiva terminal "smallest variable".
A razão porque o enunciado pedia para guardar o 'smallest' vem da necessidade de determinar a variável com letra menor, paraque haja homogeneidade de resultados. Portanto, a "determinização" invoca que se determine sempre o mesmo resultado.
A "smallest variable" recebe uma expressao 'expression' e um auxiliar 'aux', este que vai comecar sempre em 'Z' por ser a maior letra, a funcao vai "desmontando" a expressao ate chegar a variavel, por fim compara se a variavel e menor que o aux, se for entao devolvera essa variavel, caso contrario devolvera o aux.

Em seguida entrara na funcao "to_nor", a qual vai receber a expressao e a 'smallest', esta ultima e passada uma vez que ira ser preciso para fazer a transformacao do False, na funcao vai recursivamente analizar cada expressao dada e no final acabara com uma expressao do tipo 'nor_formula' a qual apenas tera nor e variaveis.
Na expressao Not, ha o caso de a expressao ser Not(A or B), uma vez que Nor e a negacao de or, simplemente passaria para um Nor(A,B) diferente dos outros casos que usam a expressao mais regular. No caso do False e do True sempre devolvera aux, porque o mesmo se nao possuir nenhuma outra variavel ficara com o maior valor que e "Z".

Por fim entrara na funcao "formula_to_string" a qual recebera a formula 'nor_formula' que apenas contem nor e variaveis, e transformara em string, dando print da mesma.

*)

(*----------------------------------------------------------------------------
EXEMPLO do Algoritmo de "smallest_variable":

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
-----------------------------------------------------------------------------*)
