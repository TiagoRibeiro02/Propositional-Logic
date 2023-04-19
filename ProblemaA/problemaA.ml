open F_parser
(*
comando para executar:
ocamlopt -I f_parser/ f_parser.cmxa problemaA.ml -o problemaA.exe
*)
type variable = string

type nor_formula =
  | V of string
  | Nor of nor_formula * nor_formula

let rec smallest_variable form aux =
  match form with
  | Var v -> if v<aux then smallest_variable form v else smallest_variable form aux
  | Not f -> smallest_variable f aux
  | And(f, g) -> let acc = smallest_variable f aux in smallest_variable g acc
  | Or(f, g) -> let acc = smallest_variable f aux in smallest_variable g acc
  | Implies(f, g) -> let acc = smallest_variable f aux in smallest_variable g acc
  | Equiv(f, g) -> let acc = smallest_variable f aux in smallest_variable g acc

let rec to_nor (f : formula_t) (smallest: string) : nor_formula = 
  match f with
  | Var f -> V f
  | Not f -> Nor(to_nor f smallest, to_nor f smallest)
  | And (a, b) -> Nor(Nor(to_nor a smallest, to_nor a smallest), Nor(to_nor b smallest, to_nor b smallest))
  | Or (a, b) -> Nor(Nor(to_nor a smallest, to_nor b smallest), Nor(to_nor a smallest, to_nor b smallest))
  | Implies (a, b) -> to_nor (Or(Not(a), b)) smallest
  | Equiv (a, b) -> to_nor (And(Implies(a, b), Implies(b, a))) smallest
  | False -> Nor(V smallest, (Nor(V smallest, V smallest)))
  | True -> to_nor (Not(False)) smallest

let rec formula_to_string (f : nor_formula) : string =
  match f with
  | Nor (f1, f2) -> Printf.sprintf "(%s %% %s)" (formula_to_string f1) (formula_to_string f2)
  | V f -> Printf.sprintf "(%s)" (string f)

let f_list = parse "stdin"

let form = 
  match f_list with
  | Some l -> List.hd l
  | _ -> assert false

let smallest = smallest_variable form "Z";;

let () =
  let result = to_nor form smallest in
  formula_to_string result |> print_endline
