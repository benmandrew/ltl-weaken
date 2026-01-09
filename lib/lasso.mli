type t

val of_states : (string * bool) list list -> int -> t
(** [of_states states loop_point] constructs a lasso trace from a list of states
    and the index of the loop point. Each state is represented as a list of
    (variable name, value) pairs. The [loop_point] indicates the start index of
    the loop within the states list. *)

val get_assignments : t -> (string * bool) list list
(** [get_assignments lasso] returns the list of state assignments in the lasso
    trace, combining both the prefix and loop states. *)

val print : t -> unit
(** [print lasso] prints the lasso trace in a human-readable format. *)
