open Base_types

let parse_nonNegativeInteger i =
  if i >= 0 then i
  else failwith "Invalid nonNegativeInteger"

let parse_nonNegativeIntegerDefault0 i =
  if i >= 0 then i
  else 0

let get_opt_nonNegativeInteger field json =
  match Yojson.Safe.Util.member field json with
  | `Int i -> Some (parse_nonNegativeInteger i)
  | `Null -> None
  | _ -> failwith (Printf.sprintf "Expected nonNegativeInteger or null for field %s" field)

let get_opt_nonNegativeIntegerDefault0 field json =
  match Yojson.Safe.Util.member field json with
  | `Int i -> parse_nonNegativeIntegerDefault0 i
  | `Null -> 0
  | _ -> failwith (Printf.sprintf "Expected nonNegativeInteger or null for field %s" field)

let get_opt_string field json =
  match Yojson.Safe.Util.member field json with
  | `String s -> Some s
  | `Null -> None
  | _ -> failwith (Printf.sprintf "Expected string or null for field %s" field)

let get_opt_bool default field json =
  match Yojson.Safe.Util.member field json with
  | `Bool b -> b
  | _ -> default

let get_opt_float field json =
  match Yojson.Safe.Util.member field json with
  | `Float f -> Some f
  | `Null -> None
  | _ -> failwith (Printf.sprintf "Expected float or null for field %s" field)

let rec parse_json_schema (json: Yojson.Safe.t): json_schema =
  let open Yojson.Safe.Util in
  let id = get_opt_string "id" json in
  let schema = get_opt_string "$schema" json in
  let ref = get_opt_string "$ref" json in
  let comment = get_opt_string "comment" json in
  let title = get_opt_string "title" json in
  let description = get_opt_string "description" json in

  let default = 
    match member "default" json with 
    | `Null -> None
    | j -> Some j
  in

  let readOnly = get_opt_bool false "readOnly" json in
  let writeOnly = get_opt_bool false "writeOnly" json in

  let examples = member "examples" json |> to_list in

  let multipleOf = get_opt_float "multipleOf" json in
  let maximum = get_opt_float "maximum" json in
  let exclusiveMaximum = get_opt_float "exclusiveMaximum" json in
  let minimum = get_opt_float "minimum" json in
  let exclusiveMinimum = get_opt_float "exclusiveMinimum" json in

  let maxLength = 
    match member "maxLength" json with
    | `Int i when i >= 0 -> Some i
    | `Null -> None
    | _ -> failwith "Invalid maxLength"
  in

  let minLength = 
    match member "maxLength" json with
    | `Int i when i >= 0 -> Some i
    | `Null -> None
    | _ -> failwith "Invalid maxLength"
  in

  (* ... and so on for the other fields *)

  let additionalItems =
    match member "additionalItems" json with
    | `Assoc _ as ai -> Some (parse_json_schema ai)
    | `Null -> None
    | _ -> failwith "Invalid additionalItems"
  in
  (* ... and so on for the other fields *)

  let properties =
    let props_tbl = Hashtbl.create 16 in
    member "properties" json |> to_assoc |> List.iter (fun (key, value) ->
      Hashtbl.add props_tbl key (parse_json_schema value)
    );
    props_tbl
  in
  (* ... and so on for the other fields *)

  {
    id;
    schema;
    ref;
    comment;
    title;
    description;
    (* ... and so on for the other fields *)
    additionalItems;
    properties;
    (* ... and so on for the other fields *)
  }


(* default *)
(* readOnly *)
(* writeOnly *)
(* examples *)
(* multipleOf *)
(* maximum *)
(* exclusiveMaximum *)
(* minimum *)
(* exclusiveMinimum *)
(* maxLength *)
(* minLength *)
(* pattern *)
(* items *)
(* maxItems *)
(* minItems *)
(* uniqueItems *)
(* contains *)
(* maxProperties *)
(* minProperties *)
(* required *)
(* additionalProperties *)
(* definitions *)
(* patternProperties *)
(* dependencies *)
(* propertyNames *)
(* const *)
(* enum *)
(* type_field *)
(* format *)
(* contentMediaType *)
(* contentEncoding *)
(* if_field *)
(* then_field *)
(* else_field *)
(* allOf *)
(* anyOf *)
(* oneOf *)
(* not_field *)
