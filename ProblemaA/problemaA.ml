open F_parser
(*
comando para executar:
ocamlopt -I f_parser/ f_parser.cmxa problemaA.ml -o problemaA.exe
*)
type variable = string

type formula =
  | Var of variable
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Implies of formula * formula
  | Equiv of formula * formula
  | True
  | False

type nor_formula =
  | Var of variable
  | Nor of formula * formula

let rec to_nor = function
  | Nor (a, b) -> Nor (to_nor a, to_nor b)
  | Var f -> Var f
  | Not f -> Nor(to_nor f, to_nor f)
  | And (a, b) -> Nor(Nor(to_nor a, to_nor a), Nor(to_nor b, to_nor b))
  | Or (a, b) -> Nor(Nor(to_nor a, to_nor b), Nor(to_nor a, to_nor b))
  | Implies (a, b) -> to_nor (Not (Nor((Not a), b)))
  | Equiv (a, b) -> to_nor (And(Implies(a, b), Implies(b, a)))
  | False f -> Nor(to_nor f, (Nor(to_nor f, to_nor f)))
  | True f -> to_nor (Not(False f))

let rec formula_to_string (f : formula) : string =
  match f with
  | Nor (f1, f2) -> Printf.sprintf "(%s %% %s)" (formula_to_string f1) (formula_to_string f2)
  | And (f1, f2) -> Printf.sprintf "(%s & %s)" (formula_to_string f1) (formula_to_string f2)
  | Or (f1, f2) -> Printf.sprintf "(%s | %s)" (formula_to_string f1) (formula_to_string f2)
  | Not f1 -> Printf.sprintf "!%s" (formula_to_string f1)
  | Implies (f1, f2) -> Printf.sprintf "(%s -> %s)" (formula_to_string f1) (formula_to_string f2)
  | Equiv (f1, f2) -> Printf.sprintf "(%s <-> %s)" (formula_to_string f1) (formula_to_string f2)
  | True f1 -> "TRUE"
  | False f1 -> "FALSE"

let f_list = parse "stdin"

let form = 
  match f_list with
  | Some l -> List.hd l
  | _ -> assert false

let () =
  let result = to_nor form in
  formula_to_string result |> print_endline
