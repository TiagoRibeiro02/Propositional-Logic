open F_parser
(*
comando para executar:
ocamlopt -I f_parser/ f_parser.cmxa problemaA.ml -o problemaA.exe
*)
type variable = string

type nor_formula =
  | Var of string
  | Nor of formula * formula

let rec smallest_variable form aux =
  match form with
  | Var v -> if v < aux then aux = v else aux = aux
  | Implies(f, g) -> smallest_variable f aux; smallest_variable g aux
  (* if ('Z')<(aux='Z') *)

let smallest = smallest_variable form 'Z';;

let rec to_nor (f : formula) : nor_formula = 
  match f with
  | Var f -> Var f
  | Not f -> Nor(to_nor f, to_nor f)
  | And (a, b) -> Nor(Nor(to_nor a, to_nor a), Nor(to_nor b, to_nor b))
  | Or (a, b) -> Nor(Nor(to_nor a, to_nor b), Nor(to_nor a, to_nor b))
  | Implies (a, b) -> to_nor (Not (Nor((Not a), b)))
  | Equiv (a, b) -> to_nor (And(Implies(a, b), Implies(b, a)))
  | False -> Nor(Var smallest, (Nor(Var smallest, Var smallest))) (*to_nor And(Not smallest, smallest)*)
  | True -> to_nor (Not(False f))

let rec formula_to_string (f : formula) : string =
  match f with
  | Nor (f1, f2) -> Printf.sprintf "(%s %% %s)" (formula_to_string f1) (formula_to_string f2)
  | Var f -> Printf.sprintf "(%s)" (string f)

let f_list = parse "stdin"

let form = 
  match f_list with
  | Some l -> List.hd l
  | _ -> assert false

let () =
  let result = to_nor form in
  formula_to_string result |> print_endline
