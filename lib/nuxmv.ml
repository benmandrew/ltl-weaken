open Core

type check_result = Valid | Invalid of string | Error of string

let model_filename = "model.smv"
let trace_filename = "trace.xml"
let commands_filename = "commands.txt"

let commands =
  [
    "set on_failure_script_quits 1";
    "set counter_examples 1";
    "go";
    "check_ltlspec";
    "show_traces -p 4 -o \"" ^ trace_filename ^ "\"";
    "quit";
  ]

let get_tmp_dir () =
  let base = Stdlib.Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base
      (sprintf "gr1_%d_%d"
         (Pid.to_int (Core_unix.getpid ()))
         (Random.int 1000000))
  in
  Core_unix.mkdir dir ~perm:0o700;
  dir

let write_commands ~tmp_dir =
  let buf = Buffer.create 512 in
  List.iter commands ~f:(fun cmd ->
      Buffer.(
        add_string buf cmd;
        add_string buf "\n"));
  Out_channel.write_all
    (Filename.concat tmp_dir commands_filename)
    ~data:(Buffer.contents buf)

let write_model ~tmp_dir model_path spec =
  let tmp_model_path = Filename.concat tmp_dir model_filename in
  let without_ltlspecs =
    In_channel.read_lines model_path
    |> List.filter ~f:(fun line ->
        let trimmed = String.strip line in
        not (String.is_prefix trimmed ~prefix:"LTLSPEC"))
    |> String.concat ~sep:"\n"
  in
  Out_channel.with_file tmp_model_path ~f:(fun oc ->
      Out_channel.output_string oc without_ltlspecs;
      List.iter
        ~f:(fun (Ltl.Any p) ->
          Out_channel.output_string oc ("LTLSPEC " ^ Ltl.to_string p ^ "\n"))
        spec)

let read_file_if_exists path =
  try Some (In_channel.read_all path) with Sys_error _ -> None

let environment = Core_unix.environment ()

let run_nuxmv ~tmp_dir =
  let cmd =
    sprintf "cd %s && nuXmv -source %s %s" tmp_dir commands_filename
      model_filename
  in
  try
    let process = Core_unix.open_process_full cmd ~env:environment in
    let stdout = In_channel.input_all process.stdout in
    let stderr = In_channel.input_all process.stderr in
    let status = Core_unix.close_process_full process in
    match status with
    | Ok () -> Ok (stdout, stderr)
    | Error (`Exit_non_zero n) -> Error (n, stdout, stderr)
    | Error (`Signal _) -> raise (Failure "nuXmv terminated by signal")
  with e ->
    Error (127, "", sprintf "Failed to run nuXmv: %s" (Exn.to_string e))

let check model_path spec =
  let tmp_dir = get_tmp_dir () in
  write_commands ~tmp_dir;
  write_model ~tmp_dir model_path spec;
  match run_nuxmv ~tmp_dir with
  | Ok (out, _err) -> (
      if
        String.is_substring out ~substring:"-- specification"
        && String.is_substring out ~substring:"is true"
      then Valid
      else
        match read_file_if_exists (Filename.concat tmp_dir trace_filename) with
        | Some xml -> Invalid xml
        | None -> Invalid out)
  | Error (status, out, err) ->
      Error
        (sprintf "nuXmv failed (exit %d). stdout:\n%s\nstderr:\n%s" status out
           err)
