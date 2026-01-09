open Why3

type 'a lasso = { prefix : 'a list; loop : 'a list }
(** A lasso trace: a prefix of states followed by a repeating loop. *)

val parse_lasso : string -> Term.term lasso
(** [parse_lasso xml] parses a nuXmv counterexample XML into a lasso trace.*)

val print_lasso : Term.term lasso -> unit
(** [print_lasso lasso] prints a lasso trace in tabular format using the lasso's
    prefix and loop states (● for true, space for false). *)
