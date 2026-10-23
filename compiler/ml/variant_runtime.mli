module Instance : sig
  (** A JavaScript class whose instances a value is recognized by:
      [name] is the global constructor's path, such as ["Date"] or
      ["Intl.Locale"], and [ancestors] are the paths of its superclasses that
      are declared too, nearest first. Arrays are recognized by
      [Array.isArray] instead. *)
  type t = Array | Class of {name: string; ancestors: string list}

  val to_string : t -> string

  val is_subclass : t -> of_:t -> bool
  (** Whether every instance of the first is an instance of [of_] too, without
      the two being the same class. *)
end

type block_type =
  | IntType
  | StringType
  | FloatType
  | BigintType
  | BooleanType
  | InstanceType of Instance.t
  | SymbolType
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

val literal_value_key : literal_tag -> string
(** A key that two literals share exactly when they are the same JavaScript
    value, as [===] compares them: ints are emitted as 32-bit numbers, so
    [1], [1.0] and [4294967297] share one, while [1n] is a bigint. *)

val same_literal_value : literal_tag -> literal_tag -> bool

(** Information used to recognize a constructor during matching. A literal
    identifies a constant or an object's tag; a payload shape identifies a
    value represented directly by its payload. *)
type tag_type = Literal of literal_tag | Payload_shape of block_type

type tag = {name: string; literal: literal_tag option}
(** A constructor's name and optional explicitly declared runtime literal. *)

type matchable_tag = {name: string; tag_type: tag_type option}
(** A constructor tag widened for matching, where an untagged payload shape
    can participate alongside declared literals. *)

type tagged_block = {tag: tag; tag_name: string option}

(** An untagged payload always carries the runtime shape used for dispatch.
    Only tagged blocks have an emitted tag field. *)
type block =
  | Tagged of tagged_block
  | Untagged of {tag: tag; block_type: block_type}

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
val representation : constructor_reference -> constructor_case
val length : layout -> int
val num_constants : layout -> int
val num_blocks : layout -> int
val plain_layout : (string * bool) list -> layout_ref
