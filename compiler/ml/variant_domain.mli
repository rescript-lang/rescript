(** Runtime value sets shared by declaration checking and code generation.
    [IntType] covers all JavaScript numbers, including floats. [ObjectType]
    describes ordinary object cases. Its runtime predicate excludes arrays;
    declaration-level exclusions separate competing known instances. *)

val normalize_literal :
  Variant_runtime.literal_tag -> Variant_runtime.literal_tag
(** Canonicalize JavaScript values; integers narrow to int32. Invalid numeric
    spellings and nonfinite floats raise [Invalid_argument]. *)

val literal_equal :
  Variant_runtime.literal_tag -> Variant_runtime.literal_tag -> bool
(** Compare runtime values, including equal signed infinities. NaN and invalid
    spellings compare unequal. Declaration domains still reject nonfinite atoms. *)

val literal_mem :
  Variant_runtime.literal_tag -> Variant_runtime.literal_tag list -> bool

val literal_matches_shape :
  Variant_runtime.literal_tag -> Variant_runtime.block_type -> bool
(** Classify runtime scalar values, including nonfinite numbers. Domain
    construction separately validates declaration literals. *)

val shapes_overlap :
  Variant_runtime.block_type -> Variant_runtime.block_type -> bool
(** Compare shapes after declaration-wide instance exclusions. [ObjectType]
    and instance shapes are separate only when the object case excludes the
    competing instances; their raw runtime predicates can overlap. *)

val normalize : Variant_runtime.value_domain -> Variant_runtime.value_domain
(** Sort and deduplicate atoms, unify numeric shapes, and remove literals
    covered by shapes. Unknown and discriminated object shapes are invalid. *)

val singleton_literal :
  Variant_runtime.literal_tag -> Variant_runtime.value_domain
val singleton_shape : Variant_runtime.block_type -> Variant_runtime.value_domain
val union :
  Variant_runtime.value_domain ->
  Variant_runtime.value_domain ->
  Variant_runtime.value_domain

type overlap =
  | Shared_literal of Variant_runtime.literal_tag
  | Shared_shape of Variant_runtime.block_type * Variant_runtime.block_type
  | Literal_shape of Variant_runtime.literal_tag * Variant_runtime.block_type

val overlaps :
  Variant_runtime.value_domain -> Variant_runtime.value_domain -> overlap list
(** [Literal_shape] permits literal-first dispatch only after the caller proves
    that reinterpreting the payload is safe. Other overlaps are ambiguous. *)

val contains_literal :
  Variant_runtime.value_domain -> Variant_runtime.literal_tag -> bool
val is_empty : Variant_runtime.value_domain -> bool

val literal_to_string : Variant_runtime.literal_tag -> string
val shape_to_string : Variant_runtime.block_type -> string
