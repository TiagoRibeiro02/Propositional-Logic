(* ocamlc -I f_parser/ f_parser.cma dpll.ml -o pbB *)

open F_parser

type FNC_formula =
  |
  |

(* Esta funcao recursiva realiza a transformacao da formula_t 'f' para a FNC_formula, utilizando as regras ?? de equivalencia logica ?? *)
let rec to_FNC (f : formula_t) : FNC_formula =
  match f with
  |

let f_list = parse "stdin"

let form = 
  match f_list with
  | Some l -> List.hd l
  | _ -> assert false

  let () =
    check_if_its_SAT_or_UNSAT form |> print_endline
    (* the previous line of code can also be written as "print_endline (check_if_its_SAT_or_UNSAT form)" *)
