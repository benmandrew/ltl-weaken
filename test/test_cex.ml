open Core
open Gr1_weaken

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
  Cex.print_lasso states 2;
  [%expect
    {|
                                            │ │ │
                                            │0│1│2
    p                                       │●│ │●
    q                                       │ │●│●
    =Lasso=                                    └─┘
    |}]

let%expect_test "print lasso with longer prefix" =
  let states =
    [ [ ("a", true) ]; [ ("a", true) ]; [ ("a", false) ]; [ ("a", true) ] ]
  in
  Cex.print_lasso states 3;
  [%expect
    {|
                                            │ │ │ │
                                            │0│1│2│3
    a                                       │●│●│ │●
    =Lasso=                                      └─┘
    |}]

let%expect_test "print lasso with multiple variables" =
  let states =
    [
      [ ("busy", false); ("grant", false) ];
      [ ("busy", true); ("grant", false) ];
      [ ("busy", false); ("grant", true) ];
    ]
  in
  Cex.print_lasso states 2;
  [%expect
    {|
                                            │ │ │
                                            │0│1│2
    busy                                    │ │●│
    grant                                   │ │ │●
    =Lasso=                                    └─┘
    |}]
