open Core
open Gr1_weaken

(* Command-line interface using cmdliner *)
let tlsf_file =
  let doc = "Path to TLSF GR(1) specification file" in
  Cmdliner.Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc)

let output_cex lasso =
  printf "Counterexample Lasso:\n";
  printf "Prefix:\n";
  List.iter lasso.Cex.prefix ~f:(fun t -> printf "  %s\n" (Gr1.term_to_smv t));
  printf "Loop:\n";
  List.iter lasso.loop ~f:(fun t -> printf "  %s\n" (Gr1.term_to_smv t))

let check_tlsf file =
  try
    (* Read the TLSF file *)
    let content = In_channel.read_all file in
    let spec = Parser.Tlsf.parse_gr1 content in
    match Nuxmv.check spec with
    | Nuxmv.Valid -> printf "✓ Specification is VALID\n"
    | Nuxmv.Invalid xml ->
        printf "✗ Specification is INVALID\n";
        output_cex @@ Cex.parse_lasso xml
    | Nuxmv.Error msg -> printf "✗ Error during verification:\n%s\n" msg
  with e -> printf "✗ Failed to process specification: %s\n" (Exn.to_string e)

let check_cmd =
  let doc = "Check TLSF specification validity using nuXmv" in
  let man =
    [
      `S Cmdliner.Manpage.s_description;
      `P "Parse a TLSF GR(1) specification and verify its validity using nuXmv.";
    ]
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "check" ~doc ~man)
    Cmdliner.Term.(const check_tlsf $ tlsf_file)

let () =
  let cmd = check_cmd in
  Cmdliner.Cmd.eval cmd |> exit
