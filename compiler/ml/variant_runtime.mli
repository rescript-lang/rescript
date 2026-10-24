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
  | NumberType  (** [int] and [float] alike *)
  | StringType
  | BigintType
  | BooleanType
  | InstanceType of Instance.t
  | SymbolType
  | FunctionType
  | ObjectType
  | UnknownType

val typeof_name : block_type -> string option
(** What [typeof] returns for values of a kind it recognizes; [None] for
    instances, which are recognized by their class, and unknown payloads. *)

val block_type_of_typeof : string -> block_type option

(** What a type declaration states about its values at run time, through
    [@notUndefined], [@instanceOf] or [@typeOf]. Each level implies the one
    before: a value recognized as a class instance or [typeof] kind is never
    [undefined]. *)
type value_shape =
  | Unstated
      (** Nothing: an abbreviation has the shape of the type it abbreviates *)
  | Never_undefined  (** [@notUndefined] *)
  | Recognized_as of block_type
      (** [@instanceOf] or [@typeOf]: a payload of this kind, with the class's
          declared superclasses *)

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

val literal_passes : literal_tag -> block_type -> bool
(** Whether a literal passes the check of a payload case of this kind, as
    [typeof] sees it: ["a"] a string case, [null] an object case, anything an
    unknown case, which is not checked *)

(** Information used to recognize a constructor during matching. A literal
    identifies a constant or an object's tag; a payload shape identifies a
    value represented directly by its payload. *)
type tag_type = Literal of literal_tag | Payload_shape of block_type

type tag = {name: string; literal: literal_tag}
(** A constructor's name and its runtime literal: what [@as] states, or the
    name itself *)

type tagged_block = {tag: tag; tag_name: string option}

(** An untagged payload always carries the runtime shape used for dispatch.
    Only tagged blocks have an emitted tag field. *)
type block =
  | Tagged of tagged_block
  | Untagged of {tag: tag; block_type: block_type}

type constructor_case = Constant of tag | Block of block

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

val constructor_tag : layout -> int -> literal_tag
(** The literal of a constant constructor, or of the tag of a tagged one *)

val representation : constructor_reference -> constructor_case
val length : layout -> int
val num_constants : layout -> int
val num_blocks : layout -> int
val plain_layout : (string * bool) list -> layout_ref
