(* I need code in ocaml that receives an expression as input, and using "F_parser",
   it returns an expression of formula_t. Then, it should pass the expression to CNF
   (conjuctive normal form), and then apply the algorythm
   Davis-Putnam–Logemann–Loveland (DPLL). The output should be SAT or UNSAT
   (UNSAT is for when the formula is contradictory) *)

open F_parser

type cnf_literal = string * bool
type cnf_clause = cnf_literal list
type cnf_formula = cnf_clause list

let rec to_formula_t (f : formula_t) : formula_t =
  match f with
  | Var v -> Var v
  | Not f -> Not (to_formula_t f)
  | And (f1, f2) -> And (to_formula_t f1, to_formula_t f2)
  | Or (f1, f2) -> Or (to_formula_t f1, to_formula_t f2)
  | Implies (f1, f2) -> Implies (to_formula_t f1, to_formula_t f2)
  | Equiv (f1, f2) -> Equiv (to_formula_t f1, to_formula_t f2)
  | False -> False
  | True -> True

let rec cnf_transform (f : formula_t) : cnf_formula =
  let fresh_var_counter = ref 0 in
  let fresh_var () =
    let var = "x" ^ string_of_int !fresh_var_counter in
    fresh_var_counter := !fresh_var_counter + 1;
    var
  in
  let rec aux f =
    match f with
    | Var v -> [[(v, true)]]
    | Not (Var v) -> [[(v, false)]]
    | Not f' ->
        let var = fresh_var () in
        [[(var, true)]] @ aux f' @ [[(var, false)]]
    | And (f1, f2) -> (aux f1) @ (aux f2)
    | Or (f1, f2) -> [List.flatten (List.map (fun clause -> (aux f1) @ clause) (aux f2))]
    | Implies (f1, f2) -> aux (Or (Not f1, f2))
    | Equiv (f1, f2) ->
        let f1_impl_f2 = aux (Implies (f1, f2)) in
        let f2_impl_f1 = aux (Implies (f2, f1)) in
        f1_impl_f2 @ f2_impl_f1
    | False -> [[]]
    | True -> []
  in
  aux f

let rec eval_literal (literal : cnf_literal) (assignment : (string * bool) list) : bool =
  match assignment with
  | [] -> failwith "Incomplete assignment"
  | (var, value) :: rest ->
      if var = fst literal then
        if snd literal = value then
          true
        else
          false
      else
        eval_literal literal rest

let rec eval_clause (clause : cnf_clause) (assignment : (string * bool) list) : bool =
  List.exists (fun literal -> eval_literal literal assignment) clause

let rec dpll (cnf : cnf_formula) (assignment : (string * bool) list) : bool =
  if List.for_all (fun clause -> eval_clause clause assignment) cnf then
    true
  else if List.exists (fun clause -> eval_clause clause assignment) cnf then
    let unassigned_var =
      List.find (fun (var, _) -> not (List.exists (fun (v, _) -> v = var) assignment)) (List.hd cnf)
      |> fst
    in
    dpll cnf ((unassigned_var, true) :: assignment) || dpll cnf ((unassigned_var, false) :: assignment)
  else
    false

let solve_formula (input : string) : string =
  match F_parser.parse input with
  | Some [expr] ->
      let formula_t_expr = to_formula_t expr in
      let cnf = cnf_transform formula_t_expr in
      let satisfiable = dpll cnf [] in
      if satisfiable then
        "SAT"
      else
        "UNSAT"
  | _ -> "Invalid input"

let () =
  let input = read_line () in
  let result = solve_formula input in
  print_endline result
