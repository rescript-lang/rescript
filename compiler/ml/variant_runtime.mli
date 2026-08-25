module Instance : sig
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

  val to_string : t -> string
end

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

type tag_type =
  | String of string
  | Int of int
  | Float of string
  | BigInt of string
  | Bool of bool
  | Null
  | Undefined
  | Untagged of block_type

type tag = {name: string; tag_type: tag_type option}
type block_runtime = {tag: tag; tag_name: string option; untagged: bool}
type block = {runtime: block_runtime; block_type: block_type option}
type constructor_case = Constant of tag | Block of block

type matching_facts = {
  tag_name: string option;
  block_types: block_type list;
  literal_tags: tag_type list;
  has_null: bool;
  has_undefined: bool;
  has_other_literal: bool;
}

type layout
type layout_ref

type constructor_reference = {variant: layout_ref; position: int}

val make_layout : constructor_case array -> layout
val pending_layout : unit -> layout_ref
val complete_layout : layout_ref -> layout -> unit
val get_layout : layout_ref -> layout
val matching_facts : layout -> matching_facts
val constructor_at : layout -> int -> constructor_case
val representation : constructor_reference -> constructor_case
val length : layout -> int
val num_constants : layout -> int
val num_blocks : layout -> int
val plain_layout : (string * bool) list -> layout_ref
