(* Definitions section *)
type simpleTypes = [`Array | `Boolean | `Integer | `Null | `Number | `Object | `String]
type nonNegativeInteger = int  (* Constrained to be >= 0 in logic *)
type stringArray = string list  (* Constrained to have unique elements *)
type schemaArray = json_schema list  (* This will refer to the main json_schema type, defined later *)
and nonNegativeIntegerDefault0 = int  (* Constrained to be >= 0 in logic, with default 0 *)

and json_schema = {
  id : string option;  (* URI Reference *)
  schema : string option;  (* URI *)
  ref : string option;  (* URI Reference *)
  comment : string option;
  title : string option;
  description : string option;
  default : Yojson.Safe.t option;
  readOnly : bool;
  writeOnly : bool;
  examples : Yojson.Safe.t list;  (* Any JSON type could be an example *)
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
