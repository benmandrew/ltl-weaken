open Core

type state = (string * bool) list

let parse_bool s =
  match String.uppercase (String.strip s) with
  | "TRUE" -> true
  | "FALSE" -> false
  | other -> invalid_arg (sprintf "Unexpected boolean value: %s" other)

let tag_is name x = String.equal (Xml.tag x) name
let child_elems x = Xml.children x

let text_of_element x =
  match Xml.children x with
  | [ y ] -> Xml.to_string y
  | ys ->
      (* Fallback: concat textual children *)
      String.concat ~sep:"" (List.map ys ~f:Xml.to_string)

let parse_states (xml_str : string) : state list =
  let root = Xml.parse_string xml_str in
  if not (tag_is "counter-example" root) then
    invalid_arg "Expected <counter-example> root";
  let nodes = List.filter (child_elems root) ~f:(tag_is "node") in
  List.filter_map nodes ~f:(fun node ->
      match List.find (child_elems node) ~f:(tag_is "state") with
      | None -> None
      | Some st ->
          let values = List.filter (child_elems st) ~f:(tag_is "value") in
          let pairs =
            List.map values ~f:(fun v ->
                let name = Xml.attrib v "variable" in
                let data = text_of_element v in
                (name, parse_bool data))
          in
          Some pairs)

let terms_of_states ?cache (sts : state list) : Why3.Term.term list =
  let cache =
    match cache with Some t -> t | None -> Hashtbl.create (module String)
  in
  let prop_of name =
    match Hashtbl.find cache name with
    | Some ps -> ps
    | None ->
        let ps = Why3.Term.create_psymbol (Why3.Ident.id_fresh name) [] in
        Hashtbl.set cache ~key:name ~data:ps;
        ps
  in
  let lit_of (name, value) =
    let atom = Why3.Term.ps_app (prop_of name) [] in
    if value then atom else Why3.Term.t_not atom
  in
  let conj = function
    | [] -> Why3.Term.t_true
    | x :: xs -> List.fold_left xs ~init:x ~f:Why3.Term.t_and
  in
  List.map sts ~f:(fun assigns -> assigns |> List.map ~f:lit_of |> conj)

let parse_xml_to_terms ?cache xml = parse_states xml |> terms_of_states ?cache
