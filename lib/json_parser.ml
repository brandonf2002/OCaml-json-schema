open Base_types

let parse_nonNegativeInteger i : Base_types.nonNegativeInteger =
  if i >= 0 then i 
  else failwith "Invalid nonNegativeInteger"

let parse_nonNegativeIntegerDefault0 i : Base_types.nonNegativeIntegerDefault0 =
  if i >= 0 then i
  else 0

let string_to_simpleType str =
  match str with
  | "array" -> `Array
  | "boolean" -> `Boolean
  | "integer" -> `Integer
  | "null" -> `Null
  | "number" -> `Number
  | "object" -> `Object
  | "string" -> `String
  | _ -> failwith ("Unknown simpleType: " ^ str)

let get_opt_type_field json : [`SimpleTypes of simpleTypes | `ArraySimpleTypes of simpleTypes list] option =
  match json with
  | `String s -> Some (`SimpleTypes (string_to_simpleType s))
  | `List types ->
      let simple_types_list = List.map (fun j ->
        match j with
        | `String s -> string_to_simpleType s
        | _ -> failwith "Unexpected JSON type in 'type_field' array"
      ) types in
      Some (`ArraySimpleTypes simple_types_list)
  | `Null -> None
  | _ -> failwith "Unexpected JSON type for 'type_field'"

let get_opt_string_array field json : Base_types.stringArray option =
  match Yojson.Safe.Util.member field json with
  | `List strings -> Some (List.map Yojson.Safe.Util.to_string strings)
  | `Null -> None
  | _ -> failwith (Printf.sprintf "Expected string array or null for field %s" field)

let get_opt_nonNegativeInteger field json =
  match Yojson.Safe.Util.member field json with
  | `Int i -> Some (parse_nonNegativeInteger i)
  | `Null -> None
  | _ -> failwith (Printf.sprintf "Expected nonNegativeInteger or null for field %s" field)

let get_opt_nonNegativeIntegerDefault0 field json =
  match Yojson.Safe.Util.member field json with
  | `Int i -> Some (parse_nonNegativeIntegerDefault0 i)
  | `Null -> Some 0
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
  | `Int i -> Some (float_of_int i)
  | `Null -> None
  | _ -> failwith (Printf.sprintf "Expected float or null for field %s" field)


let rec parse_json_schema (json: Yojson.Safe.t): json_schema =
  let get_opt_json_schema field json =
    match Yojson.Safe.Util.member field json with
  | `Assoc _ as j -> Some (parse_json_schema j)
  | `Bool _ as j -> Some (parse_json_schema j)
  | `Null -> None
  | _ -> failwith (Printf.sprintf "Expected object or null for field %s" field)
  in

  let get_opt_schema_array json : Base_types.schemaArray option = 
    match json with
    | `List schemas -> 
        let schema_list = List.map parse_json_schema schemas in
        Some (schema_list)
    | `Null -> None
    | _ -> failwith "Expected schema array or null"
  in

  let get_opt_schema_array2 json = 
    match json with
    | `List schemas -> 
        let schema_list = List.map parse_json_schema schemas in
        Some (`SchemaArray schema_list)
    | `Null -> None
    | _ -> failwith "Expected schema array or null"
  in

  let get_opt_items json : [`Schema of json_schema | `SchemaArray of schemaArray] option = 
    match json with
    | `Assoc _ | `Bool _ -> Some (`Schema (parse_json_schema json))
    | `List schemas -> get_opt_schema_array2 (`List schemas)
    | `Null -> None
    | _ -> failwith "Expected object, array or null"
  in

  let parse_dependencies json =
    let open Yojson.Safe.Util in
    let table = Hashtbl.create 16 in
    match json with
    | `Null -> table
    | _ ->
      let json_obj = to_assoc json in
      List.iter (fun (key, value) ->
        match value with
        | `Assoc _ -> (* Treat as another json_schema *)
            let schema = parse_json_schema value in
            Hashtbl.add table key (`Schema schema)
        | `List arr -> (* Treat as an array of strings *)
            let str_arr = List.map to_string arr in
            Hashtbl.add table key (`StringArray str_arr)
        | _ -> failwith "Unexpected JSON type in dependencies"
      ) json_obj;
      table
  in

  match json with
  | `Bool b -> Bool b
  | _ -> 
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

    let examples = match member "examples" json with 
    | `Null -> None 
    | j -> Some (to_list j)
    in

    let multipleOf = get_opt_float "multipleOf" json in
    let maximum = get_opt_float "maximum" json in
    let exclusiveMaximum = get_opt_float "exclusiveMaximum" json in
    let minimum = get_opt_float "minimum" json in
    let exclusiveMinimum = get_opt_float "exclusiveMinimum" json in

    let maxLength = get_opt_nonNegativeInteger "maxLength" json in
    let minLength = get_opt_nonNegativeIntegerDefault0 "minLength" json in

    let pattern = get_opt_string "pattern" json in
    (* ... and so on for the other fields *)

    let additionalItems = get_opt_json_schema "additionalItems" json in

    (* let additionalItems = *)
    (*   match member "additionalItems" json with *)
    (*   | `Assoc _ as ai -> Some (parse_json_schema ai) *)
    (*   | `Null -> None *)
    (*   | _ -> failwith "Invalid additionalItems" *)
    (* in *)

    let items = get_opt_items (member "items" json) in
    let maxItems = get_opt_nonNegativeInteger "maxItems" json in
    let minItems = get_opt_nonNegativeIntegerDefault0 "minItems" json in
    let uniqueItems = get_opt_bool false "uniqueItems" json in
    let contains = get_opt_json_schema "contains" json in
    let maxProperties = get_opt_nonNegativeInteger "maxProperties" json in
    let minProperties = get_opt_nonNegativeIntegerDefault0 "minProperties" json in
    let required = get_opt_string_array "required" json in
    let additionalProperties = get_opt_json_schema "additionalProperties" json in

    (* ... and so on for the other fields *)

    let properties =
      let props_tbl = Hashtbl.create 16 in
        match member "properties" json with
        | `Null -> props_tbl
        | j -> 
            to_assoc j |> List.iter (fun (key, value) ->
              Hashtbl.add props_tbl key (parse_json_schema value)
          );
          props_tbl
    in

    let definitions =
      let defs_tbl = Hashtbl.create 16 in
      match member "definitions" json with 
      | `Null -> defs_tbl
      | j -> to_assoc j |> List.iter (fun (key, value) ->
        Hashtbl.add defs_tbl key (parse_json_schema value)
      );
      defs_tbl
    in

    let patternProperties =
      let props_tbl = Hashtbl.create 16 in
      match member "patternProperties" json with 
      | `Null -> props_tbl
      | j -> to_assoc j |> List.iter (fun (key, value) ->
        Hashtbl.add props_tbl key (parse_json_schema value)
      );
      props_tbl
    in

    let dependencies = parse_dependencies (member "dependencies" json) in
    (* let dependencies = *)
    (*   let deps_tbl = Hashtbl.create 16 in *)
    (*   member "dependencies" json |> to_assoc |> List.iter (fun (key, value) -> *)
    (*     match value with *)
    (*     | `Assoc _ as j -> Hashtbl.add deps_tbl key (parse_json_schema j) *)
    (*     | `List _ as j -> Hashtbl.add deps_tbl key (parse_json_schema j) *)
    (*     | _ -> failwith "Invalid dependencies" *)
    (*   ); *)
    (*   deps_tbl *)
    (* in *)

    let propertyNames = get_opt_json_schema "propertyNames" json in
    
    let const = 
      match member "const" json with 
      | `Null -> None
      | j -> Some j
    in

    let enum = match member "enum" json with
    | `Null -> None
    | f -> Some (to_list f)
    in

    let type_field = get_opt_type_field (member "type" json) in
    let format = get_opt_string "format" json in
    let contentMediaType = get_opt_string "contentMediaType" json in
    let contentEncoding = get_opt_string "contentEncoding" json in
    let if_field = get_opt_json_schema "if" json in
    let then_field = get_opt_json_schema "then" json in
    let else_field = get_opt_json_schema "else" json in
    let allOf = get_opt_schema_array (member "allOf" json) in
    let anyOf = get_opt_schema_array (member "anyOf" json) in
    let oneOf = get_opt_schema_array (member "oneOf" json) in
    let not_field = get_opt_json_schema "not" json in
    (* ... and so on for the other fields *)
    Schema {
      id;
      schema;
      ref;
      comment;
      title;
      description;
      default;
      readOnly;
      writeOnly;
      examples;
      multipleOf;
      maximum;
      exclusiveMaximum;
      minimum;
      exclusiveMinimum;
      maxLength;
      minLength;
      pattern;
      additionalItems;
      items;
      maxItems;
      minItems;
      uniqueItems;
      contains;
      maxProperties;
      minProperties;
      required;
      additionalProperties;
      definitions;
      properties;
      patternProperties;
      dependencies;
      propertyNames;
      const;
      enum;
      type_field;
      format;
      contentMediaType;
      contentEncoding;
      if_field;
      then_field;
      else_field;
      allOf;
      anyOf;
      oneOf;
      not_field;
    }

  (* ... and so on for the other fields *)

  (* { *)
  (*   id; *)
  (*   schema; *)
  (*   ref; *)
  (*   comment; *)
  (*   title; *)
  (*   description; *)
  (*   (1* ... and so on for the other fields *1) *)
  (*   additionalItems; *)
  (*   properties; *)
  (*   (1* ... and so on for the other fields *1) *)
  (* } *)


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
