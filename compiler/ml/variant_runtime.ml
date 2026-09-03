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

(* The literal value a constructor is represented by: what [@as] states, or
   the constructor's own name when it states nothing. Unlike [tag_type], this
   can never describe an inferred untagged payload shape. *)
type literal_tag =
  | String of string
  | Int of int
  | Float of string
  | BigInt of string
  | Bool of bool
  | Null
  | Undefined

(*
  Type of the runtime representation of a tag.
  Can be a literal (case with no payload), or a block (case with payload).
  In the case of block it can be tagged or untagged.
*)
type tag_type =
  | Literal of literal_tag (* literal or tagged block *)
  | Untagged of block_type (* untagged block *)

type tag = {name: string; tag_type: tag_type option}

type block_runtime = {tag: tag; tag_name: string option; untagged: bool}
(** Runtime information shared by construction and pattern matching for a
    constructor carrying a payload. [block_type] is deliberately not part of
    this value: it describes how a matcher recognizes an unboxed payload, not
    how the value itself is constructed. *)

type block = {runtime: block_runtime; block_type: block_type option}

type constructor_case = Constant of tag | Block of block

type configuration = {unboxed: bool; tag_name: string option}

type matching_facts = {
  tag_name: string option;
      (** Custom object field containing constructor tags. [None] means the
          standard [TAG] field. *)
  block_types: block_type list;
      (** Runtime shapes of constructors represented directly by their
          payload. Tagged object constructors do not appear here. *)
  literal_tags: tag_type list;
      (** Runtime values of all nullary constructors. *)
  has_null: bool;
  has_undefined: bool;
  has_other_literal: bool;
}
(** Declaration-level facts needed when lowering a constructor match. This is
    not an occurrence-specific matching plan: it contains no arms, actions,
    default, guard, or exhaustiveness information. *)

type layout = {
  unboxed: bool;
  constructors: constructor_case array;
  matching_facts: matching_facts;
}
(** Completed, immutable runtime representation of a variant declaration.
    Constructor cases are kept in source order and [matching_facts] is computed
    once from those cases. *)

type layout_state = Pending | Complete of layout

type layout_ref = layout_state ref
(** Stable forward reference used while translating a recursive declaration.
    It is completed exactly once, after the whole recursive group is available
    in the environment. *)

type constructor_reference = {variant: layout_ref; position: int}
(** Stable, constant-time reference from a constructor description to its
    entry in the declaring variant. *)

let get_layout layout_ref =
  match !layout_ref with
  | Complete layout -> layout
  | Pending ->
    failwith
      "Variant_runtime.get_layout: layout accessed before type declaration was \
       complete"

let constructor_at (layout : layout) position = layout.constructors.(position)

let constructor_tag layout position =
  match constructor_at layout position with
  | Constant tag -> tag.tag_type
  | Block {runtime = {tag}} -> tag.tag_type

let constructor_is_untagged layout position =
  match constructor_at layout position with
  | Constant _ -> false
  | Block {runtime = {untagged}} -> untagged

let representation ({variant; position} : constructor_reference) =
  constructor_at (get_layout variant) position

let length (layout : layout) = Array.length layout.constructors

let num_constants (layout : layout) =
  Array.fold_left
    (fun n case ->
      match case with
      | Constant _ -> n + 1
      | Block _ -> n)
    0 layout.constructors

let num_blocks (layout : layout) =
  Array.fold_left
    (fun n case ->
      match case with
      | Block _ -> n + 1
      | Constant _ -> n)
    0 layout.constructors

let compute_matching_facts ~tag_name (constructors : constructor_case array) :
    matching_facts =
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
          | None -> Literal (String name)
        in
        literal_tags := tag :: !literal_tags;
        match tag with
        | Literal Null -> has_null := true
        | Literal Undefined -> has_undefined := true
        | Literal _ | Untagged _ -> has_other_literal := true)
      | Block {block_type} -> (
        match block_type with
        | Some block_type -> block_types := block_type :: !block_types
        | None -> ()))
    constructors;
  {
    tag_name;
    block_types = !block_types;
    literal_tags = !literal_tags;
    has_null = !has_null;
    has_undefined = !has_undefined;
    has_other_literal = !has_other_literal;
  }

let make_layout ~(configuration : configuration) constructors =
  {
    unboxed = configuration.unboxed;
    constructors;
    matching_facts =
      compute_matching_facts ~tag_name:configuration.tag_name constructors;
  }

let matching_facts layout = layout.matching_facts

let configuration layout =
  {unboxed = layout.unboxed; tag_name = layout.matching_facts.tag_name}

let pending_layout () = ref Pending

let complete_layout layout_ref layout =
  match !layout_ref with
  | Pending -> layout_ref := Complete layout
  | Complete _ ->
    failwith
      "Variant_runtime.complete_layout: type declaration layout completed twice"

(* Layout of a variant that carries no representation attributes; used for
   predefined types, whose declarations are built by hand *)
let plain_layout (cases : (string * bool (* has payload *)) list) : layout_ref =
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
  ref
    (Complete
       (make_layout
          ~configuration:{unboxed = false; tag_name = None}
          (Array.of_list (List.map case cases))))
