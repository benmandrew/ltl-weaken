open Core
open Ltl_weaken

(* Test smart constructors for propositional formulas *)

let%expect_test "p_not on atom" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let neg_p = Ltl.p_not p in
  printf "%s\n" (Ltl.to_string_any neg_p);
  [%expect {| (!p) |}]

let%expect_test "p_not on negation (double negation)" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let neg_p = Ltl.p_not p in
  let neg_neg_p = Ltl.p_not neg_p in
  printf "%s\n" (Ltl.to_string_any neg_neg_p);
  [%expect {| (!(!p)) |}]

let%expect_test "p_and with empty list" =
  let conj = Ltl.p_and [] in
  printf "%s\n" (Ltl.to_string_any conj);
  [%expect {| TRUE |}]

let%expect_test "p_and with single atom" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let conj = Ltl.p_and [ p ] in
  printf "%s\n" (Ltl.to_string_any conj);
  [%expect {| (p) |}]

let%expect_test "p_and with two atoms" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let conj = Ltl.p_and [ p; q ] in
  printf "%s\n" (Ltl.to_string_any conj);
  [%expect {| (p & q) |}]

let%expect_test "p_and with three atoms" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let r = Ltl.Any (Ltl.PAtom "r") in
  let conj = Ltl.p_and [ p; q; r ] in
  printf "%s\n" (Ltl.to_string_any conj);
  [%expect {| (p & q & r) |}]

let%expect_test "p_or with empty list" =
  let disj = Ltl.p_or [] in
  printf "%s\n" (Ltl.to_string_any disj);
  [%expect {| FALSE |}]

let%expect_test "p_or with two atoms" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let disj = Ltl.p_or [ p; q ] in
  printf "%s\n" (Ltl.to_string_any disj);
  [%expect {| (p | q) |}]

let%expect_test "p_or with three atoms" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let r = Ltl.Any (Ltl.PAtom "r") in
  let disj = Ltl.p_or [ p; q; r ] in
  printf "%s\n" (Ltl.to_string_any disj);
  [%expect {| (p | q | r) |}]

let%expect_test "p_imply simple" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let impl = Ltl.p_imply p q in
  printf "%s\n" (Ltl.to_string_any impl);
  [%expect {| (p -> q) |}]

let%expect_test "p_iff simple" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let iff = Ltl.p_iff p q in
  printf "%s\n" (Ltl.to_string_any iff);
  [%expect {| (p <-> q) |}]

(* Test smart constructors for temporal formulas *)

let%expect_test "p_next on atom" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let next_p = Ltl.p_next p in
  printf "%s\n" (Ltl.to_string_any next_p);
  [%expect {| (X p) |}]

let%expect_test "p_globally on atom" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let globally_p = Ltl.p_globally p in
  printf "%s\n" (Ltl.to_string_any globally_p);
  [%expect {| (G p) |}]

let%expect_test "p_finally on atom" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let finally_p = Ltl.p_finally p in
  printf "%s\n" (Ltl.to_string_any finally_p);
  [%expect {| (F p) |}]

let%expect_test "p_until simple" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let until = Ltl.p_until p q in
  printf "%s\n" (Ltl.to_string_any until);
  [%expect {| (p U q) |}]

let%expect_test "p_release simple" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let release = Ltl.p_release p q in
  printf "%s\n" (Ltl.to_string_any release);
  [%expect {| (p R q) |}]

(* Test nested formulas *)

let%expect_test "nested conjunction and negation" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let neg_q = Ltl.p_not q in
  let conj = Ltl.p_and [ p; neg_q ] in
  printf "%s\n" (Ltl.to_string_any conj);
  [%expect {| (p & (!q)) |}]

let%expect_test "implication with conjunction" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let r = Ltl.Any (Ltl.PAtom "r") in
  let conj = Ltl.p_and [ p; q ] in
  let impl = Ltl.p_imply conj r in
  printf "%s\n" (Ltl.to_string_any impl);
  [%expect {| ((p & q) -> r) |}]

let%expect_test "globally with conjunction" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let conj = Ltl.p_and [ p; q ] in
  let globally_conj = Ltl.p_globally conj in
  printf "%s\n" (Ltl.to_string_any globally_conj);
  [%expect {| (G (p & q)) |}]

let%expect_test "next on implication" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let impl = Ltl.p_imply p q in
  let next_impl = Ltl.p_next impl in
  printf "%s\n" (Ltl.to_string_any next_impl);
  [%expect {| (X (p -> q)) |}]

(* Test mixed prop/temporal formulas *)

let%expect_test "conjunction with temporal and prop" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let next_q = Ltl.p_next q in
  let conj = Ltl.p_and [ p; next_q ] in
  printf "%s\n" (Ltl.to_string_any conj);
  [%expect {| (p & (X q)) |}]

let%expect_test "implication from prop to temporal" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let finally_q = Ltl.p_finally q in
  let impl = Ltl.p_imply p finally_q in
  printf "%s\n" (Ltl.to_string_any impl);
  [%expect {| (p -> (F q)) |}]

let%expect_test "implication from temporal to prop" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let globally_p = Ltl.p_globally p in
  let impl = Ltl.p_imply globally_p q in
  printf "%s\n" (Ltl.to_string_any impl);
  [%expect {| ((G p) -> q) |}]

(* Test evaluation with smart constructors *)

let%expect_test "eval smart constructor conjunction" =
  let states = [ [ ("p", true); ("q", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let conj = Ltl.p_and [ p; q ] in
  let result = Eval.eval lasso 0 conj in
  printf "eval(p & q) = %b\n" result;
  [%expect {| eval(p & q) = true |}]

let%expect_test "eval smart constructor disjunction" =
  let states = [ [ ("p", true); ("q", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let disj = Ltl.p_or [ p; q ] in
  let result = Eval.eval lasso 0 disj in
  printf "eval(p | q) = %b\n" result;
  [%expect {| eval(p | q) = true |}]

let%expect_test "eval smart constructor negation" =
  let states = [ [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = Ltl.Any (Ltl.PAtom "p") in
  let neg_p = Ltl.p_not p in
  let result = Eval.eval lasso 0 neg_p in
  printf "eval(!p) = %b\n" result;
  [%expect {| eval(!p) = false |}]

let%expect_test "eval smart constructor implication" =
  let states = [ [ ("p", true); ("q", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let impl = Ltl.p_imply p q in
  let result = Eval.eval lasso 0 impl in
  printf "eval(p -> q) = %b\n" result;
  [%expect {| eval(p -> q) = true |}]

let%expect_test "eval smart constructor iff" =
  let states = [ [ ("p", true); ("q", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let iff = Ltl.p_iff p q in
  let result = Eval.eval lasso 0 iff in
  printf "eval(p <-> q) = %b\n" result;
  [%expect {| eval(p <-> q) = true |}]

let%expect_test "eval smart constructor next" =
  let states = [ [ ("p", true) ]; [ ("p", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = Ltl.Any (Ltl.PAtom "p") in
  let next_p = Ltl.p_next p in
  let result = Eval.eval lasso 0 next_p in
  printf "eval(X p) = %b\n" result;
  [%expect {| eval(X p) = false |}]

let%expect_test "eval smart constructor globally" =
  let states = [ [ ("p", true) ]; [ ("p", true) ]; [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 1 in
  let p = Ltl.Any (Ltl.PAtom "p") in
  let globally_p = Ltl.p_globally p in
  let result = Eval.eval lasso 0 globally_p in
  printf "eval(G p) = %b\n" result;
  [%expect {| eval(G p) = true |}]

let%expect_test "eval smart constructor finally" =
  let states = [ [ ("p", false) ]; [ ("p", false) ]; [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 1 in
  let p = Ltl.Any (Ltl.PAtom "p") in
  let finally_p = Ltl.p_finally p in
  let result = Eval.eval lasso 0 finally_p in
  printf "eval(F p) = %b\n" result;
  [%expect {| eval(F p) = true |}]

let%expect_test "eval smart constructor until" =
  let states =
    [ [ ("p", true) ]; [ ("p", true) ]; [ ("p", false); ("q", true) ] ]
  in
  let lasso = Lasso.of_states states 1 in
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let until = Ltl.p_until p q in
  let result = Eval.eval lasso 0 until in
  printf "eval(p U q) = %b\n" result;
  [%expect {| eval(p U q) = true |}]

let%expect_test "eval complex formula with smart constructors" =
  let states = [ [ ("p", true); ("q", false); ("r", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let r = Ltl.Any (Ltl.PAtom "r") in
  (* (p & !q) -> r *)
  let neg_q = Ltl.p_not q in
  let conj = Ltl.p_and [ p; neg_q ] in
  let impl = Ltl.p_imply conj r in
  let result = Eval.eval lasso 0 impl in
  printf "eval((p & !q) -> r) = %b\n" result;
  [%expect {| eval((p & !q) -> r) = true |}]

(* Test that constants work *)

let%expect_test "PTrue constant" =
  let t = Ltl.Any Ltl.PTrue in
  printf "%s\n" (Ltl.to_string_any t);
  [%expect {| TRUE |}]

let%expect_test "PFalse constant" =
  let f = Ltl.Any Ltl.PFalse in
  printf "%s\n" (Ltl.to_string_any f);
  [%expect {| FALSE |}]

let%expect_test "eval PTrue" =
  let states = [ [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let t = Ltl.Any Ltl.PTrue in
  let result = Eval.eval lasso 0 t in
  printf "eval(TRUE) = %b\n" result;
  [%expect {| eval(TRUE) = true |}]

let%expect_test "eval PFalse" =
  let states = [ [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let f = Ltl.Any Ltl.PFalse in
  let result = Eval.eval lasso 0 f in
  printf "eval(FALSE) = %b\n" result;
  [%expect {| eval(FALSE) = false |}]

(* Test type verification - propositional formulas *)

let%expect_test "PTrue is propositional" =
  let t = Ltl.Any Ltl.PTrue in
  printf "is_prop(TRUE) = %b\n" (Ltl.is_prop t);
  printf "is_temporal(TRUE) = %b\n" (Ltl.is_temporal t);
  [%expect {|
    is_prop(TRUE) = true
    is_temporal(TRUE) = false
    |}]

let%expect_test "PFalse is propositional" =
  let f = Ltl.Any Ltl.PFalse in
  printf "is_prop(FALSE) = %b\n" (Ltl.is_prop f);
  printf "is_temporal(FALSE) = %b\n" (Ltl.is_temporal f);
  [%expect {|
    is_prop(FALSE) = true
    is_temporal(FALSE) = false
    |}]

let%expect_test "PAtom is propositional" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  printf "is_prop(p) = %b\n" (Ltl.is_prop p);
  printf "is_temporal(p) = %b\n" (Ltl.is_temporal p);
  [%expect {|
    is_prop(p) = true
    is_temporal(p) = false
    |}]

(* Test type verification - temporal formulas *)

let%expect_test "Next is temporal" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let next_p = Ltl.p_next p in
  printf "is_prop(X p) = %b\n" (Ltl.is_prop next_p);
  printf "is_temporal(X p) = %b\n" (Ltl.is_temporal next_p);
  [%expect {|
    is_prop(X p) = false
    is_temporal(X p) = true
    |}]

let%expect_test "Globally is temporal" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let globally_p = Ltl.p_globally p in
  printf "is_prop(G p) = %b\n" (Ltl.is_prop globally_p);
  printf "is_temporal(G p) = %b\n" (Ltl.is_temporal globally_p);
  [%expect {|
    is_prop(G p) = false
    is_temporal(G p) = true
    |}]

let%expect_test "Finally is temporal" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let finally_p = Ltl.p_finally p in
  printf "is_prop(F p) = %b\n" (Ltl.is_prop finally_p);
  printf "is_temporal(F p) = %b\n" (Ltl.is_temporal finally_p);
  [%expect {|
    is_prop(F p) = false
    is_temporal(F p) = true
    |}]

let%expect_test "Until is temporal" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let until = Ltl.p_until p q in
  printf "is_prop(p U q) = %b\n" (Ltl.is_prop until);
  printf "is_temporal(p U q) = %b\n" (Ltl.is_temporal until);
  [%expect {|
    is_prop(p U q) = false
    is_temporal(p U q) = true
    |}]

let%expect_test "Release is temporal" =
  let p = Ltl.Any (Ltl.PAtom "p") in
  let q = Ltl.Any (Ltl.PAtom "q") in
  let release = Ltl.p_release p q in
  printf "is_prop(p R q) = %b\n" (Ltl.is_prop release);
  printf "is_temporal(p R q) = %b\n" (Ltl.is_temporal release);
  [%expect {|
    is_prop(p R q) = false
    is_temporal(p R q) = true
    |}]

(* Test static typing - demonstrate compile-time type safety *)

let%expect_test "static prop_kind formula" =
  (* This formula has statically known type prop_kind *)
  let (p : Ltl.prop) = Ltl.PAtom "p" in
  let (q : Ltl.prop) = Ltl.PAtom "q" in
  (* ANil is prop_kind, so this and_list is prop_kind *)
  let (conj_list : Ltl.prop_kind Ltl.and_list) =
    Ltl.AProp (p, Ltl.AProp (q, Ltl.ANil))
  in
  let (conj : Ltl.prop) = Ltl.PAnd conj_list in
  printf "%s\n" (Ltl.to_string conj);
  [%expect {| (p & q) |}]

let%expect_test "static temporal_kind formula" =
  (* This formula has statically known type temporal_kind *)
  let (p : Ltl.prop) = Ltl.PAtom "p" in
  let (next_p : Ltl.t) = Ltl.Next p in
  printf "%s\n" (Ltl.to_string next_p);
  [%expect {| (X p) |}]

let%expect_test "static temporal with ATemp" =
  (* ATemp creates a temporal_kind and_list *)
  let (p : Ltl.prop) = Ltl.PAtom "p" in
  let (next_p : Ltl.t) = Ltl.Next p in
  let (q : Ltl.prop) = Ltl.PAtom "q" in
  let (next_q : Ltl.t) = Ltl.Next q in
  (* ATemp with temporal formulas creates temporal_kind and_list *)
  let (temp_list : Ltl.temporal_kind Ltl.and_list) =
    Ltl.ATemp (next_p, Ltl.ATemp (next_q, Ltl.ANil))
  in
  let (conj : Ltl.t) = Ltl.PAnd temp_list in
  printf "%s\n" (Ltl.to_string conj);
  [%expect {| ((X p) & (X q)) |}]

let%expect_test "static bin_args Bin_PP" =
  (* Bin_PP: both args prop -> result is prop *)
  let (p : Ltl.prop) = Ltl.PAtom "p" in
  let (q : Ltl.prop) = Ltl.PAtom "q" in
  let (args : Ltl.prop_kind Ltl.bin_args) = Ltl.Bin_PP (p, q) in
  let (impl : Ltl.prop) = Ltl.PImply args in
  printf "%s\n" (Ltl.to_string impl);
  [%expect {| (p -> q) |}]

let%expect_test "static bin_args Bin_PT" =
  (* Bin_PT: prop -> temporal = temporal *)
  let (p : Ltl.prop) = Ltl.PAtom "p" in
  let (q : Ltl.prop) = Ltl.PAtom "q" in
  let (next_q : Ltl.t) = Ltl.Next q in
  let (args : Ltl.temporal_kind Ltl.bin_args) = Ltl.Bin_PT (p, next_q) in
  let (impl : Ltl.t) = Ltl.PImply args in
  printf "%s\n" (Ltl.to_string impl);
  [%expect {| (p -> (X q)) |}]

let%expect_test "static bin_args Bin_TP" =
  (* Bin_TP: temporal -> prop = temporal *)
  let (p : Ltl.prop) = Ltl.PAtom "p" in
  let (globally_p : Ltl.t) = Ltl.Globally p in
  let (q : Ltl.prop) = Ltl.PAtom "q" in
  let (args : Ltl.temporal_kind Ltl.bin_args) = Ltl.Bin_TP (globally_p, q) in
  let (impl : Ltl.t) = Ltl.PImply args in
  printf "%s\n" (Ltl.to_string impl);
  [%expect {| ((G p) -> q) |}]

let%expect_test "static bin_args Bin_TT" =
  (* Bin_TT: temporal -> temporal = temporal *)
  let (p : Ltl.prop) = Ltl.PAtom "p" in
  let (q : Ltl.prop) = Ltl.PAtom "q" in
  let (globally_p : Ltl.t) = Ltl.Globally p in
  let (finally_q : Ltl.t) = Ltl.Finally q in
  let (args : Ltl.temporal_kind Ltl.bin_args) =
    Ltl.Bin_TT (globally_p, finally_q)
  in
  let (impl : Ltl.t) = Ltl.PImply args in
  printf "%s\n" (Ltl.to_string impl);
  [%expect {| ((G p) -> (F q)) |}]
