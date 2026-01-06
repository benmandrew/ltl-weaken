type state = (string * bool) list

val parse_states : string -> state list
(** [parse_states xml] parses a nuXmv counterexample XML into a list of states.
    Each state is a list of (variable_name, value) pairs. Supports boolean
    variables (TRUE/FALSE). *)

val terms_of_states :
  ?cache:(string, Why3.Term.lsymbol) Core.Hashtbl.t ->
  state list ->
  Why3.Term.term list
(** [terms_of_states ?cache states] converts parsed states into Why3 terms by
    mapping each variable name to a boolean variable symbol (reusing entries in
    [cache] if provided). For each state, returns a conjunction of literals: a
    variable for TRUE and its negation for FALSE. *)

val parse_xml_to_terms :
  ?cache:(string, Why3.Term.lsymbol) Core.Hashtbl.t ->
  string ->
  Why3.Term.term list
(** Convenience: [parse_xml_to_terms ?cache xml] =
    [xml |> parse_states |> terms_of_states ?cache]. *)
