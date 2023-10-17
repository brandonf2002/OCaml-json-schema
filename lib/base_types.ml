(* Definitions section *)
type simpleTypes = [`Array | `Boolean | `Integer | `Null | `Number | `Object | `String]
type nonNegativeInteger = int  (* Constrained to be >= 0 in logic *)
type nonNegativeIntegerDefault0 = int  (* Constrained to be >= 0 in logic, with default 0 *)
type stringArray = string list  (* Constrained to have unique elements *)

type json_schema = 
  | Schema of json_schema_
  | Bool of bool

and schemaArray = json_schema list  (* This will refer to the main json_schema type, defined later *)

and json_schema_ = {
  id : string option;  (* URI Reference *)
  schema : string option;  (* URI *)
  ref : string option;  (* URI Reference *)
  comment : string option;
  title : string option;
  description : string option;
  default : Yojson.Safe.t option;
  readOnly : bool;
  writeOnly : bool;
  examples : (Yojson.Safe.t list) option;  (* Any JSON type could be an example *)
  multipleOf : float option;
  maximum : float option;
  exclusiveMaximum : float option;
  minimum : float option;
  exclusiveMinimum : float option;
  maxLength : nonNegativeInteger option;
  minLength : nonNegativeIntegerDefault0 option;
  pattern : string option;  (* Regex format *)
  additionalItems : json_schema option;
  items : [`Schema of json_schema | `SchemaArray of schemaArray] option;
  maxItems : nonNegativeInteger option;
  minItems : nonNegativeIntegerDefault0 option;
  uniqueItems : bool;
  contains : json_schema option;
  maxProperties : nonNegativeInteger option;
  minProperties : nonNegativeIntegerDefault0 option;
  required : stringArray option;
  additionalProperties : json_schema option;
  definitions : (string, json_schema) Hashtbl.t;  (* Default to empty hash table *)
  properties : (string, json_schema) Hashtbl.t;  (* Default to empty hash table *)
  patternProperties : (string, json_schema) Hashtbl.t;  (* Regex as key, default to empty hash table *)
  dependencies : (string, [`Schema of json_schema | `StringArray of stringArray]) Hashtbl.t;
  propertyNames : json_schema option;
  const : Yojson.Safe.t option;
  enum : (Yojson.Safe.t list) option;
  type_field : [`SimpleTypes of simpleTypes | `ArraySimpleTypes of simpleTypes list] option;
  format : string option;
  contentMediaType : string option;
  contentEncoding : string option;
  if_field : json_schema option;
  then_field : json_schema option;
  else_field : json_schema option;
  allOf : schemaArray option;
  anyOf : schemaArray option;
  oneOf : schemaArray option;
  not_field : json_schema option;
} 


open Format

let pp_to_string pp_val v =
  let buf = Buffer.create 16 in
  let fmt = formatter_of_buffer buf in
  pp_val fmt v;
  pp_print_flush fmt ();
  Buffer.contents buf


let rec pp_json_schema fmt ?(indent=0) (json_schema: json_schema) =
  let pp_json_schema_ fmt ~indent (json_schema: json_schema_) =
    let ind = String.make (2 * indent) ' ' in
    (* let pp_option pp_val fmt = function *)
    (*   (1* | None -> fprintf fmt "None" *1) *)
    (*   | None -> () *)
    (*   | Some v -> fprintf fmt "Some (%a)" pp_val v *)
    (* in *)

    let pp_string fmt s = fprintf fmt "%S" s in
    (* let pp_int fmt i = fprintf fmt "%d" i in *)
    let pp_bool fmt b = fprintf fmt "%b" b in
    let pp_list pp_val fmt lst =
      fprintf fmt "[";
      List.iteri (fun i v ->
          if i > 0 then fprintf fmt "";
          pp_val fmt v) lst;
      fprintf fmt "%s]" ind
    in
    let pp_hashtbl pp_val fmt tbl =
      fprintf fmt "{\n";
      Hashtbl.iter (fun k v ->
        fprintf fmt "%s%S: %a\n" ind k pp_val v) tbl;
      fprintf fmt "%s}" ind
    in
    let pp_dependency fmt = function
      | `Schema s -> fprintf fmt "`Schema (%a)" (pp_json_schema ~indent:(indent+1))  s
      | `StringArray arr -> fprintf fmt "`StringArray (%a)" (pp_list pp_string) arr
    in

    let pp_simple_types fmt = function
      | `Array -> fprintf fmt "`Array"
      | `Boolean -> fprintf fmt "`Boolean"
      | `Integer -> fprintf fmt "`Integer"
      | `Null -> fprintf fmt "`Null"
      | `Number -> fprintf fmt "`Number"
      | `Object -> fprintf fmt "`Object"
      | `String -> fprintf fmt "`String"
    in

    let pp_type_field fmt = function
      | `SimpleTypes st -> fprintf fmt "`SimpleTypes (%a)" pp_simple_types st
      | `ArraySimpleTypes arr -> fprintf fmt "`ArraySimpleTypes (%a)" (pp_list pp_simple_types) arr
    in

    let pp_line_opt field pp_val value =
      match value with
      | None -> ()
      | Some v -> fprintf fmt "%s%s: %a\n" ind field pp_val v
    in

    let pp_line field pp_val value =
      fprintf fmt "%s%s: %a\n" ind field pp_val value
    in

    fprintf fmt "{\n";
    pp_line_opt "id" (pp_string) json_schema.id;
    pp_line_opt "schema" (pp_string) json_schema.schema;
    pp_line_opt "ref" (pp_string) json_schema.ref;
    pp_line_opt "comment" (pp_string) json_schema.comment;
    pp_line_opt "title" (pp_string) json_schema.title;
    pp_line_opt "description" (pp_string) json_schema.description;
    pp_line "readOnly" pp_bool json_schema.readOnly;
    pp_line "writeOnly" pp_bool json_schema.writeOnly;
    pp_line_opt "required" ((pp_list pp_string)) json_schema.required;
    pp_line "dependencies" (pp_hashtbl pp_dependency) json_schema.dependencies;
    pp_line_opt "format" (pp_string) json_schema.format;
    pp_line_opt "contentMediaType" (pp_string) json_schema.contentMediaType;
    pp_line_opt "contentEncoding" (pp_string) json_schema.contentEncoding;
    pp_line_opt "if_field" ((pp_json_schema ~indent:(indent+1))) json_schema.if_field;
    pp_line_opt "then_field" ((pp_json_schema ~indent:(indent+1))) json_schema.then_field;
    pp_line_opt "else_field" ((pp_json_schema ~indent:(indent+1))) json_schema.else_field;
    pp_line_opt "allOf" ((pp_list (pp_json_schema ~indent:(indent+1)))) json_schema.allOf;
    pp_line_opt "anyOf" ((pp_list (pp_json_schema ~indent:(indent+1)))) json_schema.anyOf;
    pp_line_opt "oneOf" ((pp_list (pp_json_schema ~indent:(indent+1)))) json_schema.oneOf;
    pp_line_opt "not_field" ((pp_json_schema ~indent:(indent+1))) json_schema.not_field;
    pp_line_opt "type_field" (pp_type_field) json_schema.type_field;
    pp_line "properties" (pp_hashtbl (pp_json_schema ~indent:(indent+1))) json_schema.properties;
    pp_line "definitions" (pp_hashtbl (pp_json_schema ~indent:(indent+1))) json_schema.definitions;
    fprintf fmt "}\n"
  in
  match json_schema with
  | Bool b -> fprintf fmt "%b" b
  | Schema s -> pp_json_schema_ fmt ~indent s
