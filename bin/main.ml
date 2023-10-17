open Printf

let json_file = ref ""
let output_file = ref None

let specs = [
  ("--json", Arg.String (fun s -> json_file := s), "Path to the JSON file");
  ("--output", Arg.String (fun s -> output_file := Some s), "Optional output file")
]

let process_json json =
  (* Your JSON processing code here *)
  (* "Successfully parsed JSON file" (1* Replace with your actual output *1) *)
  Yojson.Safe.pretty_to_string json

let () =
  Arg.parse specs (fun _ -> ()) "Usage: prog_name --json [json_file_path] [--output output_file_path]";

  if !json_file = "" then (
    print_endline "Please provide a JSON file path using the --json option.";
    exit 1
  );

  try
    let json = Yojson.Safe.from_file !json_file in
    let output = process_json json in
    match !output_file with
    | None -> print_endline output
    | Some file ->
      let oc = open_out file in
      fprintf oc "%s\n" output;
      close_out oc
  with
  | Yojson.Json_error msg ->
    printf "Invalid JSON: %s\n" msg;
    exit 1
  | Sys_error msg ->
    printf "Could not read file: %s\n" msg;
    exit 1
