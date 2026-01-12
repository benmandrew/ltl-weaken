open Core
open Why3

let rec term_to_smv ?(indent = 0) (term : Term.term) : string =
  let ind = String.make (indent * 2) ' ' in
  match term.t_node with
  | Tvar vs ->
      (* Variables *)
      vs.vs_name.Ident.id_string
  | Tconst _ ->
      (* Constants - use generic string representation *)
      Format.asprintf "%a" Pretty.print_term term
  | Tapp (fs, args) -> (
      let fname = fs.ls_name.Ident.id_string in
      let args_str = List.map args ~f:(term_to_smv ~indent) in
      match (fname, args) with
      | "True", [] -> "TRUE"
      | "False", [] -> "FALSE"
      (* Logical operators *)
      | "and", [ a; b ] ->
          sprintf "(%s & %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "or", [ a; b ] ->
          sprintf "(%s | %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "not", [ a ] -> sprintf "!%s" (term_to_smv ~indent a)
      | "->", [ a; b ] | "implies", [ a; b ] ->
          sprintf "(!%s | %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "<->", [ a; b ] | "iff", [ a; b ] ->
          sprintf "(%s <-> %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      (* Arithmetic operators *)
      | "+", [ a; b ] ->
          sprintf "(%s + %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "-", [ a; b ] ->
          sprintf "(%s - %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "*", [ a; b ] ->
          sprintf "(%s * %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "/", [ a; b ] ->
          sprintf "(%s / %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "mod", [ a; b ] ->
          sprintf "(%s mod %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      (* Comparison operators *)
      | "=", [ a; b ] | "==", [ a; b ] ->
          sprintf "(%s == %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "!=", [ a; b ] | "<>", [ a; b ] ->
          sprintf "(%s != %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "<", [ a; b ] ->
          sprintf "(%s < %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "<=", [ a; b ] ->
          sprintf "(%s <= %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | ">", [ a; b ] ->
          sprintf "(%s > %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | ">=", [ a; b ] ->
          sprintf "(%s >= %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      (* Temporal operators (LTL) *)
      | "G", [ a ] | "[]", [ a ] -> sprintf "(G %s)" (term_to_smv ~indent a)
      | "F", [ a ] | "<>", [ a ] -> sprintf "(F %s)" (term_to_smv ~indent a)
      | "X", [ a ] | "next", [ a ] -> sprintf "(X %s)" (term_to_smv ~indent a)
      | "U", [ a; b ] ->
          sprintf "(%s U %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "R", [ a; b ] ->
          sprintf "(%s R %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      (* Default: function application *)
      | _ ->
          if List.is_empty args then fname
          else sprintf "%s(%s)" fname (String.concat ~sep:", " args_str))
  | Tif (cond, then_t, else_t) ->
      sprintf "(%s ? %s : %s)" (term_to_smv ~indent cond)
        (term_to_smv ~indent then_t)
        (term_to_smv ~indent else_t)
  | Tlet (t, tb) ->
      let vs, body = Term.t_open_bound tb in
      sprintf "/* let %s = %s in */\n%s%s" vs.vs_name.Ident.id_string
        (term_to_smv ~indent t) ind (term_to_smv ~indent body)
  | Tcase (scrutinee, branches) ->
      let scrutinee_str = term_to_smv ~indent scrutinee in
      let branches_str =
        List.mapi branches ~f:(fun i br ->
            let _pat, body = Term.t_open_branch br in
            sprintf "%s/* case %d: */ %s" ind i
              (term_to_smv ~indent:(indent + 1) body))
        |> String.concat ~sep:"\n"
      in
      sprintf "/* match %s */\n%s" scrutinee_str branches_str
  | Tquant (quant, tq) ->
      let vsl, _, body = Term.t_open_quant tq in
      let quant_str =
        match quant with Tforall -> "forall" | Texists -> "exists"
      in
      let vars =
        List.map vsl ~f:(fun vs -> vs.vs_name.Ident.id_string)
        |> String.concat ~sep:", "
      in
      sprintf "/* %s %s */ %s" quant_str vars (term_to_smv ~indent body)
  | Tbinop (op, t1, t2) -> (
      match op with
      | Tand -> (
          (* Special-case X marker: X & φ -> X (φ) *)
          match t1.t_node with
          | Tapp (fs, args)
            when String.equal fs.ls_name.Ident.id_string "X"
                 && List.is_empty args ->
              sprintf "(X %s)" (term_to_smv ~indent t2)
          | _ ->
              sprintf "(%s & %s)" (term_to_smv ~indent t1)
                (term_to_smv ~indent t2))
      | Tor ->
          sprintf "(%s | %s)" (term_to_smv ~indent t1) (term_to_smv ~indent t2)
      | Timplies ->
          sprintf "(!%s | %s)" (term_to_smv ~indent t1) (term_to_smv ~indent t2)
      | Tiff ->
          sprintf "(%s <-> %s)" (term_to_smv ~indent t1)
            (term_to_smv ~indent t2))
  | Tnot t -> sprintf "!%s" (term_to_smv ~indent t)
  | Ttrue -> "TRUE"
  | Tfalse -> "FALSE"
  | Teps _ -> "/* epsilon term */"
