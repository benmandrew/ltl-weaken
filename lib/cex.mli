type 'a lasso = { prefix : 'a list; loop : 'a list }
(** A lasso trace: a prefix of states followed by a repeating loop. *)

val parse_lasso : string -> Why3.Term.term lasso
(** [parse_lasso xml] parses a nuXmv counterexample XML into a lasso structure.*)
