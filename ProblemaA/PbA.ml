open F_parser

parse "stdin"

type formula =
| Var of variable
| Not of formula
| And of formula * formula
| Or of formula * formula
| Implies of formula * formula
| Equiv of formula * formula
| True
| False

val parse: string -> formula list option



(*ocamlc -I f_parser/ f_parser.cma PbA.ml -o PbA*)