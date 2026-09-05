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

(** The literal value a constructor is represented by: what [@as] states, or
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

type tag_type = Literal of literal_tag | Untagged of block_type

type tag = {name: string; literal: literal_tag option}
(** A constructor's name and optional explicitly declared runtime literal. *)

type matchable_tag = {name: string; tag_type: tag_type option}
(** A constructor tag widened for matching, where an untagged payload shape
    can participate alongside declared literals. *)

type block_runtime = {tag: tag; tag_name: string option; untagged: bool}

type block = {runtime: block_runtime; block_type: block_type option}
type constructor_case = Constant of tag | Block of block

val to_matchable_tag : tag -> matchable_tag
(** Widen a tag as stated by a declaration into one a match can compare
    against, which also covers an untagged payload's shape. *)

type configuration = {
  unboxed: bool;
      (** Whether the declaration carries [@unboxed]. This is retained even
          when the declaration has no payload constructor, where it cannot be
          recovered by inspecting constructor layouts. *)
  tag_name: string option;
      (** Custom object field containing constructor tags. This is retained
          even when the variant currently has no object constructor. *)
}

type matching_facts = {
  tag_name: string option;
  block_types: block_type list;
  literal_tags: literal_tag list;
  has_null: bool;
  has_undefined: bool;
  has_other_literal: bool;
}

type layout
type layout_ref

type constructor_reference = {variant: layout_ref; position: int}

val make_layout :
  configuration:configuration -> constructor_case array -> layout
val pending_layout : unit -> layout_ref
val complete_layout : layout_ref -> layout -> unit
val get_layout : layout_ref -> layout
val matching_facts : layout -> matching_facts
val configuration : layout -> configuration
val constructor_at : layout -> int -> constructor_case
val constructor_tag : layout -> int -> literal_tag option
val constructor_is_untagged : layout -> int -> bool
val representation : constructor_reference -> constructor_case
val length : layout -> int
val num_constants : layout -> int
val num_blocks : layout -> int
val plain_layout : (string * bool) list -> layout_ref
