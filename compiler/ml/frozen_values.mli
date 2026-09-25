(** The directly indexed slice of an immutable compiled interface. The image
    is safe to share between compiler domains; every [view] and materialized
    declaration belongs to one compilation request. *)

type t
type view
type scope

val freeze : Cmi_format.cmi_infos -> (t, string) result
(** Snapshot an imported signature, its nested scopes, type graph, module
    declarations, and name indexes into project-shareable data. *)

val create_view : t -> view

val copy_signature : view -> Types.signature
(** Materialize and prefix an entire request-owned signature when a caller
    needs one. This is intentionally a full-copy compatibility path. *)

val source_signature : view -> Types.signature
(** Decode a private source signature for legacy component expansion. *)

val root_scope : view -> scope

val value_names : scope -> string list
val type_names : scope -> string list
val label_names : scope -> string list
val constructor_names : scope -> string list
val module_names : scope -> string list
val modtype_names : scope -> string list
val has_value : scope -> string -> bool
val has_type : scope -> string -> bool
val has_label : scope -> string -> bool
val has_constructor : scope -> string -> bool
val has_module : scope -> string -> bool
val has_modtype : scope -> string -> bool

val find_module :
  scope -> string -> (scope * int * Location.t * string option) option
(** Return a nested signature and its path position, location, and deprecation
    message. Literal signatures and instances of locally declared module
    types are indexed. Aliases, functors, and shadowed module names return
    [None] here and have separate declaration or alias accessors. *)

val find_module_info :
  scope -> string -> (int * Location.t * string option) option

val find_module_alias : view -> scope -> string -> Path.t option
(** Return the target of a module alias after prefixing local binders. *)

val find_module_declaration :
  view -> scope -> string -> (Types.module_declaration * int) option
(** Decode and substitute one module declaration in the request view. *)

val find_modtype_declaration :
  view -> scope -> string -> Types.modtype_declaration option
(** Decode and substitute one module-type declaration in the request view. *)

val find_in_scope :
  view -> scope -> string -> (Types.value_description * int) option

val find_type_in_scope :
  view ->
  scope ->
  string ->
  (Types.type_declaration
  * (Types.constructor_description list * Types.label_description list))
  option

val find_labels_in_scope :
  view -> scope -> string -> Types.label_description list option

val find_constructors_in_scope :
  view -> scope -> string -> Types.constructor_description list option

val find_extension_in_scope :
  view -> scope -> string -> Types.constructor_description option

val is_type_name_in_scope : scope -> string -> bool

val find : view -> string -> (Types.value_description * int) option
(** Return a request-local description and its signature position. No type
    graph or declaration record from the image escapes this operation. *)

val find_type :
  view ->
  string ->
  (Types.type_declaration
  * (Types.constructor_description list * Types.label_description list))
  option
(** Materialize a root type and its descriptions in the request-local view. *)

val find_labels : view -> string -> Types.label_description list option
(** Return request-local root record labels. [None] means a duplicate or
    unsupported declaration needs the legacy component path. *)

val find_constructors :
  view -> string -> Types.constructor_description list option
(** Return request-local top-level variant and extension constructors. [None]
    means an unsupported declaration needs the existing component path. *)

val find_extension : view -> string -> Types.constructor_description option
(** Return a unique extension constructor for a constructor type path. *)

val value_count : t -> int

val is_type_name : view -> string -> bool
(** Test whether a top-level name denotes a type, without materializing it. *)

val type_count : t -> int
val type_node_count : t -> int
