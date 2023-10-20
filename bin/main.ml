open Printf

let json_file = ref ""
let output_file = ref None

let specs = [
  ("--json", Arg.String (fun s -> json_file := s), "Path to the JSON file");
  ("--output", Arg.String (fun s -> output_file := Some s), "Optional output file")
]

let process_json json =
  (* Ocaml_json_schema.Base_types.pp_to_string (Ocaml_json_schema.Base_types.pp_json_schema ~indent:0) (Ocaml_json_schema.Json_parser.parse_json_schema json) *)
  Ocaml_json_schema.Generate_types.json_schema_to_ocaml_type "Hello" (Ocaml_json_schema.Json_parser.parse_json_schema json)

let _ =
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

