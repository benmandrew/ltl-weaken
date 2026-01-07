open Core
open Why3

(* Parse a standard TLSF GR(1) specification into a Gr1.t record. *)

let is_comment line =
  String.is_prefix line ~prefix:"#" || String.is_prefix line ~prefix:"//"

let parse_signal_decl line =
  (* Format: "name : boolean;" or just "name ;" *)
  line |> String.strip
  |> (fun s -> String.chop_suffix s ~suffix:";" |> Option.value ~default:s)
  |> String.strip
  |> fun s ->
  match String.lsplit2 s ~on:':' with
  | Some (name, _) -> Some (String.strip name)
  | None -> Some (String.strip s)

let parse_signals (section : string list) : string list =
  section
  |> List.filter_map ~f:(fun line ->
      let line = String.strip line in
      if String.is_empty line || is_comment line then None
      else parse_signal_decl line)

let parse_formulas (section : string list) : string list =
  let text = String.concat ~sep:" " section in
  text
  |> String.split_on_chars ~on:[ ';' ]
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not (String.is_empty s))

let conj = function
  | [] -> Term.t_true
  | x :: xs -> List.fold_left xs ~init:x ~f:Term.t_and

(** Parse a standard TLSF GR(1) specification. *)
let parse_gr1 (input : string) : Gr1.t =
  (* Extract MAIN { ... } block *)
  let main_match =
    try
      let start_idx = String.substr_index_exn input ~pattern:"MAIN" in
      let text_from_main = String.drop_prefix input start_idx in
      let brace_start = String.index_exn text_from_main '{' in
      let brace_end = String.rindex_exn input '}' in
      let adjusted_start = start_idx + brace_start + 1 in
      String.sub input ~pos:adjusted_start ~len:(brace_end - adjusted_start)
    with _ -> input
  in

  (* Parse sections *)
  let section_tbl = Hashtbl.create (module String) in
  let lines = String.split_lines main_match in
  let current_section = ref None in

  List.iter lines ~f:(fun raw ->
      let line = String.strip raw in
      if String.is_empty line || is_comment line then ()
      else if String.is_suffix line ~suffix:"{" then
        let section_name =
          line
          |> (fun s ->
          String.chop_suffix s ~suffix:"{" |> Option.value ~default:s)
          |> String.strip |> String.uppercase
        in
        current_section := Some section_name
      else if String.equal line "}" then current_section := None
      else
        match !current_section with
        | None -> ()
        | Some section -> Hashtbl.add_multi section_tbl ~key:section ~data:line);

  let get_section name =
    Hashtbl.find section_tbl name |> Option.value ~default:[]
  in

  (* Extract and parse sections *)
  let inputs = parse_signals (get_section "INPUTS") in
  let outputs = parse_signals (get_section "OUTPUTS") in
  let _ = (inputs, outputs) in
  (* Avoid unused variable warning *)

  let initially = parse_formulas (get_section "INITIALLY") in
  let preset = parse_formulas (get_section "PRESET") in
  let require = parse_formulas (get_section "REQUIRE") in
  let assert_ = parse_formulas (get_section "ASSERT") in
  let assume = parse_formulas (get_section "ASSUME") in
  let guarantee = parse_formulas (get_section "GUARANTEE") in

  (* Parse formulas *)
  let parse_formula_list = List.map ~f:Smv.parse_formula in

  (* Map TLSF sections to GR(1) *)
  let asm_init = initially @ preset |> parse_formula_list |> conj in
  let asm_safety = require |> parse_formula_list in
  let asm_liveness = assume |> parse_formula_list in
  let gnt_init = Term.t_true in
  (* Default system init to true *)
  let gnt_safety = assert_ |> parse_formula_list in
  let gnt_liveness = guarantee |> parse_formula_list in

  Gr1.make ~asm_init ~asm_safety ~asm_liveness ~gnt_init ~gnt_safety
    ~gnt_liveness
