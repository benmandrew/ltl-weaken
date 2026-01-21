open Core

type prop_kind
type temporal_kind

type _ formula =
  (* Propositional operators - work on any formula *)
  | PTrue : 'k formula
  | PFalse : 'k formula
  | PAtom : string -> 'k formula
  | PNot : 'k formula -> 'k formula
  | PAnd : 'k formula list -> 'k formula
  | POr : 'k formula list -> 'k formula
  | PImply : 'k formula * 'k formula -> 'k formula
  | PIff : 'k formula * 'k formula -> 'k formula
  (* Temporal operators - only on temporal formulas *)
  | Next : _ formula -> temporal_kind formula
  | Until : _ formula * _ formula -> temporal_kind formula
  | Release : _ formula * _ formula -> temporal_kind formula
  | Globally : _ formula -> temporal_kind formula
  | Finally : _ formula -> temporal_kind formula

type any_formula = Any : 'k formula -> any_formula
type prop = prop_kind formula
type t = temporal_kind formula

val to_string : 'k formula -> string
val index : 'k formula -> int list -> any_formula

module Any_formula : sig
  val sexp_of_t : any_formula -> Sexplib.Sexp.t

  include Hashable.S_plain with type t = any_formula
end
