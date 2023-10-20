open Base_types

open Printf
open Hashtbl
(* open Yojson.Safe *)

let counter = ref 0

let generate_new_type_name () =
  incr counter;
  sprintf "anonymous_type_%d" !counter

let rec json_schema_to_ocaml_type (name : string) (js : json_schema) : string =
  match js with
  | Bool _ -> (* For boolean schemas, use a simple bool type *)
      sprintf "type %s = bool" name
  | Schema js' ->
      match js'.properties with
      | properties when length properties > 0 -> handle_properties name properties
      | _ -> "(* Complex schema, not handled yet. *)"

and handle_properties name properties =
  let buffer = Buffer.create 16 in
  iter
    (fun k v ->
      let field_type = json_schema_to_simple_type v in
      Buffer.add_string buffer (sprintf "  %s: %s;\n" k field_type))
    properties;
  sprintf "type %s = {\n%s}" name (Buffer.contents buffer)

and json_schema_to_simple_type (js : json_schema) : string =
  match js with
  | Bool _ -> "bool" (* Simplification: represent schema booleans as OCaml bools *)
  | Schema s' -> (
      match s'.type_field with
      | Some (`SimpleTypes `Integer) -> "int"
      | Some (`SimpleTypes `Number) -> "float"
      | Some (`SimpleTypes `String) -> "string"
      | Some (`SimpleTypes `Boolean) -> "bool"
      | Some (`SimpleTypes `Null) -> "unit"
      | Some (`SimpleTypes `Array) -> (
          match s'.items with
          | Some (`Schema sub_schema) ->
              sprintf "%s list" (json_schema_to_ocaml_type "test" sub_schema)
          | _ -> "(* Array of unknown types *)"
        )
      | Some (`SimpleTypes `Object) -> 
          let type_name = generate_new_type_name () in
          let properties_code = handle_properties type_name s'.properties in
          sprintf "%s\n%s" properties_code type_name
      | _ -> "(* Unknown type. *)")



(* let () = *)
(*   (1* Define a small example json_schema for testing. *1) *)
(*   let example_properties = Hashtbl.create 16 in *)
(*   Hashtbl.add example_properties "id" (Schema { (1* ... fill out this *)
