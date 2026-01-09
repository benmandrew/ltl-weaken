open Core

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

let parse (xml_str : string) : Lasso.t =
  let root = Xml.parse_string xml_str in
  if not (tag_is "counter-example" root) then
    invalid_arg "Expected <counter-example> root";
  let nodes = List.filter (child_elems root) ~f:(tag_is "node") in
  let states = parse_states_from_xml nodes in
  let loop_point = extract_loop_point root in
  Lasso.of_states states loop_point
