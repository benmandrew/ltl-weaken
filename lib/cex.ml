(* Why3 contains an internal Xml module that conflicts with Xml_light *)
open Core
module Xml_light = Xml
open Why3
module Xml = Xml_light

type 'a lasso = { prefix : 'a list; loop : 'a list }

let parse_bool s =
  match String.uppercase (String.strip s) with
  | "TRUE" -> Some true
  | "FALSE" -> Some false
  | _ -> None

let tag_is name x = String.equal (Xml.tag x) name
let child_elems x = Xml.children x

let text_of_element x =
  match Xml.children x with
  | [ y ] -> Xml.to_string y
  | ys -> String.concat ~sep:"" (List.map ys ~f:Xml.to_string)

let parse_states_from_xml (nodes : Xml.xml list) : (string * bool) list list =
  List.filter_map nodes ~f:(fun node ->
      match List.find (child_elems node) ~f:(tag_is "state") with
      | None -> None
      | Some st ->
          let values = List.filter (child_elems st) ~f:(tag_is "value") in
          let pairs =
            List.filter_map values ~f:(fun v ->
                let name = Xml.attrib v "variable" in
                let data = text_of_element v in
                Option.map (parse_bool data) ~f:(fun b -> (name, b)))
          in
          Some pairs)

let extract_loop_point (root : Xml.xml) : int =
  match
    List.find (child_elems root) ~f:(fun x ->
        match Xml.children x with
        | [ Xml.PCData _ ] -> tag_is "loops" x
        | _ -> false)
  with
  | Some loops_elem -> (
      try text_of_element loops_elem |> String.strip |> Int.of_string
      with _ -> invalid_arg "Invalid <loops> value")
  | None -> invalid_arg "<loops> tag not found"

let lasso_of_states (states : (string * bool) list list) (loop_point : int) :
    Term.term lasso =
  (* Split states into prefix and loop *)
  let state_prefix = List.take states (loop_point - 1) in
  let state_loop = List.drop states (loop_point - 1) in
  (* Convert states to terms using shared cache *)
  let cache = Hashtbl.create (module String) in
  let prop_of name =
    match Hashtbl.find cache name with
    | Some ps -> ps
    | None ->
        let ps = Term.create_psymbol (Ident.id_fresh name) [] in
        Hashtbl.set cache ~key:name ~data:ps;
        ps
  in
  let lit_of (name, value) =
    let atom = Term.ps_app (prop_of name) [] in
    if value then atom else Term.t_not atom
  in
  let state_to_term assigns = assigns |> List.map ~f:lit_of |> Term.t_and_l in
  {
    prefix = List.map state_prefix ~f:state_to_term;
    loop = List.map state_loop ~f:state_to_term;
  }

let parse_lasso (xml_str : string) : Term.term lasso =
  let root = Xml.parse_string xml_str in
  if not (tag_is "counter-example" root) then
    invalid_arg "Expected <counter-example> root";
  let nodes = List.filter (child_elems root) ~f:(tag_is "node") in
  let states = parse_states_from_xml nodes in
  let loop_point = extract_loop_point root in
  lasso_of_states states loop_point

let collect_variable_names (states : (string * bool) list list) : string list =
  states
  |> List.concat_map ~f:(List.map ~f:fst)
  |> List.dedup_and_sort ~compare:String.compare

let rec lits_of_term term : (string * bool) list option =
  match term.Term.t_node with
  | Tapp (fs, []) -> Some [ (fs.ls_name.Ident.id_string, true) ]
  | Tnot t -> (
      match t.Term.t_node with
      | Tapp (fs, []) -> Some [ (fs.ls_name.Ident.id_string, false) ]
      | _ -> None)
  | Tbinop (Tand, a, b) ->
      Option.bind (lits_of_term a) ~f:(fun la ->
          Option.map (lits_of_term b) ~f:(fun lb -> la @ lb))
  | Ttrue -> Some []
  | _ -> None

let assignments_of_lasso (lasso : Term.term lasso) : (string * bool) list list =
  lasso.prefix @ lasso.loop
  |> List.map ~f:(fun t -> Option.value (lits_of_term t) ~default:[])

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

let print_lasso (lasso : Term.term lasso) : unit =
  let state_assigns = assignments_of_lasso lasso in
  let num_states = List.length state_assigns in
  let prefix_len = List.length lasso.prefix in
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
