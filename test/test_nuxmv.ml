open Core
open Gr1_weaken

let%expect_test "Parse nuXmv lasso" =
  let xml =
    {|
        <?xml version="1.0" encoding="UTF-8"?>
        <counter-example type="0" id="1" desc="LTL Counterexample" >
        <node>
        <state id="1">
        <value variable="p">FALSE</value>
        </state>
        </node>
        <node>
        <state id="2">
        <value variable="p">TRUE</value>
        </state>
        </node>
        <node>
        <state id="3">
        <value variable="p">TRUE</value>
        </state>
        </node>
        <loops> 2 </loops>
        </counter-example>
        |}
  in
  let lasso = Cex.parse_lasso xml in
  printf "Prefix:\n";
  List.iter lasso.prefix ~f:(fun t -> printf "  %s\n" (Gr1.term_to_smv t));
  printf "Loop:\n";
  List.iter lasso.loop ~f:(fun t -> printf "  %s\n" (Gr1.term_to_smv t));
  [%expect {|
    Prefix:
      !(p)
    Loop:
      p
      p
    |}]
