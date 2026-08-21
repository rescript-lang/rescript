(* The runtime representation of variants: plain data describing how each
   constructor is laid out in JavaScript and how a whole variant is
   dispatched on. This module sits below [Types] so the canonical layout can
   be stored on type declarations; [Ast_untagged_variants] re-exports these
   definitions and derives them from declarations. *)

module Instance = struct
  type t =
    | Array
    | ArrayBuffer
    | BigInt64Array
    | BigUint64Array
    | Blob
    | DataView
    | Date
    | File
    | Float32Array
    | Float64Array
    | Int16Array
    | Int32Array
    | Int8Array
    | Promise
    | RegExp
    | Uint16Array
    | Uint32Array
    | Uint8Array
    | Uint8ClampedArray
    | Set
    | Map
    | WeakSet
    | WeakMap
  let to_string = function
    | Array -> "Array"
    | ArrayBuffer -> "ArrayBuffer"
    | BigInt64Array -> "BigInt64Array"
    | BigUint64Array -> "BigUint64Array"
    | Blob -> "Blob"
    | DataView -> "DataView"
    | Date -> "Date"
    | File -> "File"
    | Float32Array -> "Float32Array"
    | Float64Array -> "Float64Array"
    | Int16Array -> "Int16Array"
    | Int32Array -> "Int32Array"
    | Int8Array -> "Int8Array"
    | Promise -> "Promise"
    | RegExp -> "RegExp"
    | Uint16Array -> "Uint16Array"
    | Uint32Array -> "Uint32Array"
    | Uint8Array -> "Uint8Array"
    | Uint8ClampedArray -> "Uint8ClampedArray"
    | Set -> "Set"
    | Map -> "Map"
    | WeakSet -> "WeakSet"
    | WeakMap -> "WeakMap"
end

(* Type of the runtime representation of an untagged block (case with payload) *)
type block_type =
  | IntType
  | StringType
  | FloatType
  | BigintType
  | BooleanType
  | InstanceType of Instance.t
  | FunctionType
  | ObjectType
  | UnknownType

(*
  Type of the runtime representation of a tag.
  Can be a literal (case with no payload), or a block (case with payload).
  In the case of block it can be tagged or untagged.
*)
type tag_type =
  | String of string
  | Int of int
  | Float of string
  | BigInt of string
  | Bool of bool
  | Null
  | Undefined (* literal or tagged block *)
  | Untagged of block_type (* untagged block *)
type tag = {name: string; tag_type: tag_type option}

type block_runtime = {tag: tag; tag_name: string option; untagged: bool}
(** Runtime information shared by construction and pattern matching for a
    constructor carrying a payload. [block_type] is deliberately not part of
    this value: it describes how a matcher recognizes an unboxed payload, not
    how the value itself is constructed. *)

type block = {runtime: block_runtime; block_type: block_type option}

type constructor_case = Constant of tag | Block of block

type variant_dispatch = {
  tag_name: string option;
  block_types: block_type list;
  literal_tags: tag_type list;
  has_null: bool;
  has_undefined: bool;
  has_other_literal: bool;
}
(** The whole-variant information needed to choose a JavaScript dispatch
    strategy. Constructor identity is carried by each switch arm instead. *)

type variant_layout = constructor_case array
(** Canonical runtime layout in source-constructor order. *)

let case_name = function
  | Constant {name} -> name
  | Block {runtime = {tag = {name}}} -> name

let constructor_position (layout : variant_layout) name =
  let rec find i =
    if i >= Array.length layout then
      invalid_arg ("Variant_runtime.constructor_position: " ^ name)
    else if case_name layout.(i) = name then i
    else find (i + 1)
  in
  find 0

let constructor_by_name (layout : variant_layout) name =
  layout.(constructor_position layout name)

let num_constants (layout : variant_layout) =
  Array.fold_left
    (fun n case ->
      match case with
      | Constant _ -> n + 1
      | Block _ -> n)
    0 layout

let num_blocks (layout : variant_layout) =
  Array.fold_left
    (fun n case ->
      match case with
      | Block _ -> n + 1
      | Constant _ -> n)
    0 layout

let dispatch (constructors : variant_layout) : variant_dispatch =
  let tag_name = ref None in
  let block_types = ref [] in
  let literal_tags = ref [] in
  let has_null = ref false in
  let has_undefined = ref false in
  let has_other_literal = ref false in
  Array.iter
    (function
      | Constant {name; tag_type} -> (
        let tag =
          match tag_type with
          | Some tag -> tag
          | None -> String name
        in
        literal_tags := tag :: !literal_tags;
        match tag with
        | Null -> has_null := true
        | Undefined -> has_undefined := true
        | String _ | Int _ | Float _ | BigInt _ | Bool _ | Untagged _ ->
          has_other_literal := true)
      | Block {runtime = {tag_name = constructor_tag_name}; block_type} -> (
        if !tag_name = None then tag_name := constructor_tag_name;
        match block_type with
        | Some block_type -> block_types := block_type :: !block_types
        | None -> ()))
    constructors;
  {
    tag_name = !tag_name;
    block_types = !block_types;
    literal_tags = !literal_tags;
    has_null = !has_null;
    has_undefined = !has_undefined;
    has_other_literal = !has_other_literal;
  }

(* Placeholder used while a recursive declaration group is being typed;
   [Typedecl] replaces it with the computed layout once the group is in the
   environment *)
let dummy_layout : variant_layout = [||]

(* Layout of a variant that carries no representation attributes; used for
   predefined types, whose declarations are built by hand *)
let plain_layout (cases : (string * bool (* has payload *)) list) :
    variant_layout =
  let case (name, has_payload) =
    if has_payload then
      Block
        {
          runtime =
            {tag = {name; tag_type = None}; tag_name = None; untagged = false};
          block_type = None;
        }
    else Constant {name; tag_type = None}
  in
  Array.of_list (List.map case cases)
