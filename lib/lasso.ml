open Core

type state = (Ltl.any_formula, bool) Hashtbl.t
type t = { prefix : state list; loop : state list }

let prefix_length t = List.length t.prefix
let loop_length t = List.length t.loop
let length t = prefix_length t + loop_length t
let lit_of (name, value) = (Ltl.Any (Ltl.PAtom name), value)

let of_states states loop_idx =
  assert (loop_idx >= 0 && loop_idx < List.length states);
  (* Split states into prefix and loop *)
  let state_prefix = List.take states loop_idx in
  let state_loop = List.drop states loop_idx in
  let state_to_term assigns =
    assigns |> List.map ~f:lit_of |> Hashtbl.of_alist (module Ltl.Any_formula)
    |> function
    | `Duplicate_key _ -> raise (Invalid_argument "Duplicate variable in state")
    | `Ok pair -> pair
  in
  {
    prefix = List.map state_prefix ~f:state_to_term;
    loop = List.map state_loop ~f:state_to_term;
  }

let get_state t i =
  if i < List.length t.prefix then List.nth_exn t.prefix i
  else List.nth_exn t.loop ((i - List.length t.prefix) % List.length t.loop)

let get_future_states t i =
  if i < List.length t.prefix then
    let future_prefix = List.drop t.prefix i in
    future_prefix @ t.loop
  else
    let loop_start_idx = (i - List.length t.prefix) % List.length t.loop in
    let future_loop = List.drop t.loop loop_start_idx in
    let wrap_around = List.take t.loop loop_start_idx in
    future_loop @ wrap_around

let get_assignments t =
  t.prefix @ t.loop
  |> List.map ~f:(fun t ->
      Hashtbl.to_alist t
      |> List.map ~f:(fun (Any k, v) ->
          let var_name = Ltl.to_string k in
          (var_name, v)))

let collect_variable_names states =
  states
  |> List.concat_map ~f:(List.map ~f:fst)
  |> List.dedup_and_sort ~compare:String.compare

let header width num_states =
  if num_states >= 10 then begin
    printf "%*s" width "";
    for i = 0 to num_states - 1 do
      if i < 10 then printf "  " else printf " %d" (i / 10)
    done;
    printf "\n"
  end;
  printf "%*s" width "";
  for i = 0 to num_states - 1 do
    printf " %d" (i mod 10)
  done;
  printf "\n"

let print_rows width vars num_states get_value =
  List.iter vars ~f:(fun var_name ->
      printf "%*s" (-width) var_name;
      for i = 0 to num_states - 1 do
        let char =
          match get_value i var_name with
          | Some true -> "●"
          | Some false -> " "
          | None -> "?"
        in
        printf "│%s" char
      done;
      printf "│\n")

let print_indicator width prefix_len num_states =
  printf "%*s" (-(width + 1)) "=Lasso=";
  for i = 0 to num_states - 1 do
    if i = prefix_len && i = num_states - 1 then printf "⊔"
    else if i = prefix_len then printf "└─"
    else if i = num_states - 1 then printf "┘ "
    else if i > prefix_len then printf "──"
    else printf "  "
  done;
  printf "\n"

let lasso_str_len = String.length "=Lasso="

let print t =
  let state_assigns = get_assignments t in
  let num_states = List.length state_assigns in
  let prefix_len = List.length t.prefix in
  let vars = collect_variable_names state_assigns in
  let width =
    let var_widths = List.map vars ~f:String.length in
    let all_widths = lasso_str_len :: var_widths in
    List.max_elt all_widths ~compare:Int.compare
    |> Option.value ~default:lasso_str_len
    |> Int.succ
  in
  let state_map = Array.of_list state_assigns in
  let get_value state_idx var_name =
    if state_idx < Array.length state_map then
      List.Assoc.find state_map.(state_idx) ~equal:String.equal var_name
    else None
  in
  header width num_states;
  print_rows width vars num_states get_value;
  print_indicator width prefix_len num_states

let print_state state =
  Hashtbl.to_alist state
  |> List.sort ~compare:(fun (k1, _) (k2, _) -> Ltl.Any_formula.compare k1 k2)
  |> List.iter ~f:(fun (Ltl.Any k, v) ->
      let var_name = Ltl.to_string k in
      printf "%s: %b\n" var_name v)
