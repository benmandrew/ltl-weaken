open Core
open Why3

type t = {
  asm_init : Term.term;
  asm_safety : Term.term list;
  asm_liveness : Term.term list;
  gnt_init : Term.term;
  gnt_safety : Term.term list;
  gnt_liveness : Term.term list;
}

let make ~asm_init ~asm_safety ~asm_liveness ~gnt_init ~gnt_safety ~gnt_liveness
    =
  { asm_init; asm_safety; asm_liveness; gnt_init; gnt_safety; gnt_liveness }

let asm_init t = t.asm_init
let asm_safety t = t.asm_safety
let asm_liveness t = t.asm_liveness
let gnt_init t = t.gnt_init
let gnt_safety t = t.gnt_safety
let gnt_liveness t = t.gnt_liveness

(* Convert GR(1) specification to SMV LTL formula *)
let to_smv t =
  let format_list name terms =
    match terms with
    | [] -> None
    | _ ->
        let formulas = List.map terms ~f:Print.term_to_smv in
        let combined = String.concat ~sep:" & " formulas in
        Some (sprintf "%s: %s" name combined)
  in
  let parts =
    [
      Some (sprintf "-- GR(1) Specification in SMV LTL\n");
      Some (sprintf "-- Assumptions");
      Some (sprintf "asm_init: %s" (Print.term_to_smv t.asm_init));
      format_list "asm_safety" t.asm_safety;
      format_list "asm_liveness" t.asm_liveness;
      Some (sprintf "\n-- Guarantees");
      Some (sprintf "gnt_init: %s" (Print.term_to_smv t.gnt_init));
      format_list "gnt_safety" t.gnt_safety;
      format_list "gnt_liveness" t.gnt_liveness;
    ]
  in
  List.filter_map parts ~f:Fn.id |> String.concat ~sep:"\n"

(* Convert GR(1) to a single SMV LTL formula *)
let to_smv_ltl t =
  let combine_with_and terms =
    match terms with
    | [] -> None
    | [ x ] -> Some (Print.term_to_smv x)
    | _ ->
        let formulas = List.map terms ~f:Print.term_to_smv in
        Some (sprintf "(%s)" (String.concat ~sep:" & " formulas))
  in
  let always_terms terms =
    match combine_with_and terms with
    | None -> None
    | Some formula -> Some (sprintf "G (%s)" formula)
  in
  let always_eventually_terms terms =
    match terms with
    | [] -> None
    | _ ->
        let formulas =
          List.map terms ~f:(fun t -> sprintf "(G F %s)" (Print.term_to_smv t))
        in
        Some (sprintf "(%s)" (String.concat ~sep:" & " formulas))
  in
  let assumptions =
    [
      Some (Print.term_to_smv t.asm_init);
      always_terms t.asm_safety;
      always_eventually_terms t.asm_liveness;
    ]
    |> List.filter_map ~f:Fn.id
  in
  let guarantees =
    [
      Some (Print.term_to_smv t.gnt_init);
      always_terms t.gnt_safety;
      always_eventually_terms t.gnt_liveness;
    ]
    |> List.filter_map ~f:Fn.id
  in
  let asm_formula = String.concat ~sep:" & " assumptions in
  let gnt_formula = String.concat ~sep:" & " guarantees in
  sprintf "(%s -> %s)" asm_formula gnt_formula

let eval t lasso =
  let length = Lasso.length lasso in
  for i = 0 to length - 1 do
    Eval.eval_state lasso i t.asm_init |> ignore;
    Eval.eval_state lasso i t.gnt_init |> ignore;
    List.map t.asm_safety ~f:(Eval.eval_safety lasso i) |> ignore;
    List.map t.gnt_safety ~f:(Eval.eval_safety lasso i) |> ignore;
    List.map t.asm_liveness ~f:(Eval.eval_liveness lasso i) |> ignore;
    List.map t.gnt_liveness ~f:(Eval.eval_liveness lasso i) |> ignore
  done
