open Core
open Gr1_weaken

let%expect_test "print lasso in tabular format" =
  let states =
    [
      [ ("p", true); ("q", false) ];
      [ ("p", false); ("q", true) ];
      [ ("p", true); ("q", true) ];
    ]
  in
  Lasso.print (Lasso.of_states states 1);
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
  Lasso.print (Lasso.of_states states 0);
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
  Lasso.print (Lasso.of_states states 2);
  [%expect
    {|
             0 1 2
    busy    │ │●│ │
    grant   │ │ │●│
    =Lasso=      ⊔
    |}]
