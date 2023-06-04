(*
a "literal" is a single proposition or its negation (P or !P)
a "clause" is a disjunction of literals (P v Q v !R)
*)
type literal =
  | Pos of string
  | Neg of string

type clause = literal list

(*
- "CNF" refers to the "Conjuctive Normal Form"
- F = A1 ^ A2 ^ ... ^ An where Ai is a clause
- F is only satisfiable if it exists at least one
valid interpretation.
*)
type cnf_formula = clause list

let rec to_cnf (f : formula_t) : cnf_formula =
  match f with
  | Var v -> [[Pos v]]
  | Not (Var v) -> [[Neg v]]
  | And (f1, f2) -> (to_cnf f1) @ (to_cnf f2)
  | Or (f1, f2) -> [List.flatten (to_cnf f1 @ to_cnf f2)]
  | Implies (f1, f2) -> to_cnf (Or (Not f1, f2))
  | Equiv (f1, f2) -> to_cnf (And (Implies (f1, f2), Implies (f2, f1)))
  | False -> [[]]
  | True -> []

(*
We should apply "unit_propagate" as long as possible:
- as long a clause appears with only one literal lit (a unit clause)
1) remove !lit from all clauses containing !lit.
2) remove all clauses containing lit.
*)
let rec unit_propagate (f : cnf_formula) : cnf_formula =
  let rec remove_literal (lit : literal) (clauses : cnf_formula) : cnf_formula =
    List.map (fun clause -> List.filter (fun l -> l <> lit) clause) clauses
    |> List.filter (fun clause -> clause <> [])
  in
  match f with
  | [] -> []
  | clause :: rest ->
      match clause with
      | [] -> [[]]
      | [lit] ->
          let propagated_formula = remove_literal lit rest in
          unit_propagate propagated_formula
      | _ :: _ -> clause :: unit_propagate rest

(*
This function returns "UNSAT" if there is [] inside any of the clauses
*)
let rec dpll (f : cnf_formula) : string =
  match f with
  | [] -> "SAT"
  | clauses ->
      if List.exists (fun clause -> clause = []) clauses then
        "UNSAT"
      else
        let unit_propagated = unit_propagate f in
        match unit_propagated with
        | [] -> "SAT"
        | _ -> dpll unit_propagated

let f_list = parse "stdin"

let form = 
  match f_list with
  | Some l -> List.hd l
  | _ -> assert false

let () =
  let cnf = to_cnf form in
  let simplificada = unit_propagate cnf in
  let result = dpll simplificada in
  print_endline result

(*
---------------------------------
X1 & (X1 | !X2 | X3) & (X2 | X3)
 |     |
 v     v
 T     T

fica:
(X2 | X3)

X2=T
SAT
----------------------------------

[ [X2,X3], [X4, !X3], [!X3] ]
       |          |      |
       v          v      v
       F          T      T

fica:
[ [X2,F] [X4,T], [T] ]
[ [X2,F], [T], [T] ]
[ [X2] ]
UNSAT
------------------------------------

Exemplo: Caso nao haja unitarios(literals)

1. !a + b + c
2. a + c + d
3. a + c + !d
4. a + !c + d
5. a + !c + !d
6. !b + !c + d
7. !a + b + !c
8. !a + !b + c

             a
       a=F /   \a=T
         b       b
   b=F /  \     /  \ b=T
     c     c   x    implicacao:
    / \   / \          c=T
   x   x x   x         d=T


decisoes:
A=T
B=T

implicacoes:
C=T

logo:
D=T

*)