open Core
open Why3
open Gr1_weaken

let make_var name =
  let id = Ident.id_fresh name in
  let ty = Ty.ty_bool in
  Term.create_vsymbol id ty

let%expect_test "Parse nuXmv xml" =
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
  let terms = Cex.parse_xml_to_terms xml in
  List.iter terms ~f:(fun t -> print_endline (Gr1.term_to_smv t));
  [%expect {|
    !(p)
    p
    p
    |}]
