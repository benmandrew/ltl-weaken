open Core
open Gr1_weaken
open Why3

let lasso_of_states (states : (string * bool) list list) (prefix_len : int) :
    Term.term Cex.lasso =
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
  let prefix = List.take states prefix_len |> List.map ~f:state_to_term in
  let loop = List.drop states prefix_len |> List.map ~f:state_to_term in
  { prefix; loop }

let%expect_test "parse simple lasso from XML" =
  let xml =
    {|<counter-example>
  <node>
    <state>
      <value variable="p">TRUE</value>
      <value variable="q">FALSE</value>
    </state>
  </node>
  <node>
    <state>
      <value variable="p">FALSE</value>
      <value variable="q">TRUE</value>
    </state>
  </node>
  <loops>2</loops>
</counter-example>|}
  in
  let lasso = Cex.parse_lasso xml in
  printf "Prefix length: %d\n" (List.length lasso.prefix);
  printf "Loop length: %d\n" (List.length lasso.loop);
  [%expect {|
    Prefix length: 1
    Loop length: 1
    |}]

let%expect_test "print lasso in tabular format" =
  let states =
    [
      [ ("p", true); ("q", false) ];
      [ ("p", false); ("q", true) ];
      [ ("p", true); ("q", true) ];
    ]
  in
  Cex.print_lasso (lasso_of_states states 1);
  [%expect
    {|
             0 1 2
    p       │●│ │●│
    q       │ │●│●│
    =Lasso=    └─┘
    |}]

let%expect_test "print lasso with longer prefix" =
  let states =
    [ [ ("a", true) ]; [ ("a", true) ]; [ ("a", false) ]; [ ("a", true) ] ]
  in
  Cex.print_lasso (lasso_of_states states 0);
  [%expect
    {|
             0 1 2 3
    a       │●│●│ │●│
    =Lasso=  └─────┘
    |}]

let%expect_test "print lasso with multiple variables" =
  let states =
    [
      [ ("busy", false); ("grant", false) ];
      [ ("busy", true); ("grant", false) ];
      [ ("busy", false); ("grant", true) ];
    ]
  in
  Cex.print_lasso (lasso_of_states states 2);
  [%expect
    {|
             0 1 2
    busy    │ │●│ │
    grant   │ │ │●│
    =Lasso=      ⊔
    |}]
