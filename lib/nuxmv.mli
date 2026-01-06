type check_result =
  | Valid
  | Invalid of string (* XML counterexample *)
  | Error of string (* stdout/stderr from NuSMV *)

val check : Gr1.t -> check_result
(** [check spec] check validity for the GR(1) spec and returns either [Valid] or
    [Invalid xml], where [xml] is the dumped counterexample in XML form (if
    produced), or [Error message] if nuXmv failed to run. *)
