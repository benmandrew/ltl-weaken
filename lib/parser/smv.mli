(** Parse SMV propositional formulae into Why3 terms *)

val parse_formula : string -> Ltl.t
(** [parse_formula input] parses a single SMV formula and returns a Why3 term.
    @raise Invalid_argument if parsing fails *)

val parse_formulae : string -> Ltl.t list
(** [parse_formulae input] parses multiple comma/semicolon-separated formulae.
    @return a list of Why3 terms *)

val parse_ident : string -> string
(** [parse_ident input] parses a variable identifier *)
