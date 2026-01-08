type 'a lasso = { prefix : 'a list; loop : 'a list }
(** A lasso trace: a prefix of states followed by a repeating loop. *)

val parse_lasso : string -> Why3.Term.term lasso
(** [parse_lasso xml] parses a nuXmv counterexample XML into a lasso structure.*)

val print_lasso : (string * bool) list list -> int -> unit
(** [print_lasso states loop_start] prints a counterexample in tabular format.
    [states] is a list of state assignments (variable name and value).
    [loop_start] is the 1-indexed step where the loop begins. Format shows
    variables as rows, time steps as columns, with ● for true and space for
    false. *)
