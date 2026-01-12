open Core
open Why3

(** X (p) is implemented as X & p, so it must be disambiguated from real
    conjunction *)
let rec eval_and lasso i p q =
  match p.Term.t_node with
  | Tapp (fs, args)
    when String.equal fs.ls_name.Ident.id_string "X" && List.is_empty args ->
      eval_state lasso (i + 1) q
  | _ -> eval_state lasso i p && eval_state lasso i q

and eval_state_aux lasso i f =
  match f.Term.t_node with
  | Tbinop (Tand, p, q) -> eval_and lasso i p q
  | Tbinop (Tor, p, q) -> eval_state lasso i p || eval_state lasso i q
  | Tbinop (Timplies, p, q) ->
      (not @@ eval_state lasso i p) || eval_state lasso i q
  | Tbinop (Tiff, p, q) ->
      Bool.equal (eval_state lasso i p) (eval_state lasso i q)
  | Tnot p -> not @@ eval_state lasso i p
  | Ttrue -> true
  | Tfalse -> false
  | Tapp (p, []) ->
      Format.printf "Proposition '%s' not found\n" p.ls_name.Ident.id_string;
      raise (Not_found_s (Core.Sexp.Atom "Proposition not found"))
  | _ ->
      Format.printf "'%a' not supported\n" Pretty.print_term f;
      raise (Not_found_s (Core.Sexp.Atom "Unsupported term in eval"))

and eval_state lasso i f =
  let state = Lasso.get_state lasso i in
  match Hashtbl.find state (Lasso.Prop f) with
  | Some v -> v
  | None ->
      let res = eval_state_aux lasso i f in
      Hashtbl.set state ~key:(Lasso.Prop f) ~data:res;
      res

let eval_safety lasso i f =
  let initial_state = Lasso.get_state lasso i in
  match Hashtbl.find initial_state (Lasso.Safety f) with
  | Some v -> v
  | None ->
      let states = Lasso.get_future_states lasso i in
      let indices = List.init (List.length states) ~f:(fun idx -> i + idx) in
      let res = List.for_all indices ~f:(fun i -> eval_state lasso i f) in
      Hashtbl.set initial_state ~key:(Lasso.Safety f) ~data:res;
      res

let eval_liveness lasso i f =
  let initial_state = Lasso.get_state lasso i in
  match Hashtbl.find initial_state (Lasso.Liveness f) with
  | Some v -> v
  | None ->
      let prefix_length = Lasso.prefix_length lasso in
      let indices =
        let states = Lasso.get_future_states lasso i in
        let indices = List.init (List.length states) ~f:(fun idx -> i + idx) in
        if i < prefix_length then List.drop indices (prefix_length - i)
        else indices
      in
      let res = List.exists indices ~f:(fun i -> eval_state lasso i f) in
      Hashtbl.set initial_state ~key:(Lasso.Liveness f) ~data:res;
      res
