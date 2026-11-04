(** An index-addressed store for type nodes.

    This module is the core of the planned switch of [Types.type_expr] from a
    heap record [{mutable desc; mutable level; id}] to an integer index into
    a store owned by one compiler request. It is not used by the type checker
    yet. Until the switch-over it lives next to [Types]:

    - {!node} is the future [Types.type_expr]: an immediate integer, so
      physical equality of types ([==]) becomes integer equality, setting a
      level or linking a node is an integer store into an array (no write
      barrier, nothing for the minor collector to promote), and the store is
      freed in bulk when the request ends.
    - {!desc} and the types below it are [Types.type_desc] and its companions
      with every [type_expr] replaced by {!node}. They have the same
      constructors and field names, so that at the switch-over they become
      the definitions in [Types].
    - {!Sig} mirrors the signature types of [Types] in the same way.
    - {!Import} and {!Export} convert between the boxed graphs of today's
      [Types] (the representation [Marshal] reads from and writes to [.cmi]
      files) and the store. Both keep exactly one node per boxed record and
      one block per boxed block, so exporting an imported signature
      reproduces its sharing and therefore its marshalled bytes.

    The design, its boundaries and the switch-over plan are described in
    rescript-perf-investigation [results/arena/DESIGN.md]. *)

(** {1 Nodes and descriptors} *)

type node = private int
(** A type node: [slot lsl index_bits lor index]. The slot names the store
    that owns the node, so reading a node needs no domain-local lookup. *)

type arg = {lbl: Asttypes.arg_label; typ: node}

and desc =
  | Tvar of string option
  | Tarrow of arg list * node
  | Ttuple of node list
  | Tconstr of Path.t * node list * abbrev_memo ref
  | Tobject of node
  | Tfield of {
      name: string;
      mutability: Types.field_mutability ref;
      typ: node;
      rest: node;
    }
  | Tnil
  | Tlink of node
  | Tsubst of node
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of node * node list
  | Tpackage of Path.t * Longident.t list * node list

and row_desc = {
  row_fields: (Asttypes.label * row_field) list;
  row_more: node;
  row_closed: bool;
  row_fixed: bool;
  row_name: (Path.t * node list) option;
}

and row_field =
  | Rpresent of node option
  | Reither of bool * node list * bool * row_field option ref
  | Rabsent

and abbrev_memo =
  | Mnil
  | Mcons of Asttypes.private_flag * Path.t * node * node * abbrev_memo
  | Mlink of abbrev_memo ref

(** {1 Stores}

    A store holds the nodes of one compiler request. Each domain has a
    default store, used outside {!with_store}; {!with_store} runs an action
    with a fresh store as the domain's current one and frees it afterwards.
    New nodes are allocated in the current store. Reading or writing a node
    goes to the store that owns it, whichever store is current.

    A node must not be used after its store is freed: such a node reads
    whatever a later request put at its index (memory safe, but wrong). *)

type store

val index_bits : int
(** Number of low bits holding the index inside a store. *)

val max_slots : int
(** Maximum number of stores alive at the same time. *)

val current : unit -> store
(** The current store of this domain. *)

val with_store : ?capacity:int -> (unit -> 'a) -> 'a
(** [with_store action] runs [action] with a new current store, restores the
    previous one afterwards (also when [action] raises) and frees the new
    store. Calls nest. *)

val length : store -> int
(** Number of nodes allocated in the store. *)

val store_of : node -> store
(** The store which owns the node. *)

val nth : store -> int -> node
(** [nth s i] is the [i]-th node allocated in [s]. *)

val index : node -> int
(** Position of the node in its store. *)

(** {1 Nodes} *)

val newty : level:int -> id:int -> desc -> node
(** Allocate a node in the current store. *)

val newty_in : store -> level:int -> id:int -> desc -> node

val get_desc : node -> desc
(** The node's descriptor, without following [Tlink]. Reading a [Tlink] or
    [Tsubst] descriptor allocates it: those are kept as integers, see
    {!link_of}. *)

val set_desc : node -> desc -> unit
(** Unlogged update. [Tlink] and [Tsubst] are stored as integers. *)

val get_level : node -> int
val set_level : node -> int -> unit
val get_id : node -> int

val link_of : node -> int
(** The target of a [Tlink] descriptor, or [-1]: [repr] without allocating. *)

val subst_of : node -> int
(** The target of a [Tsubst] descriptor, or [-1]. *)

val link_target : node -> node
(** The target of a [Tlink] descriptor, or the node itself. *)

val subst_target : node -> node
(** The target of a [Tsubst] descriptor, or the node itself. *)

val equal : node -> node -> bool

val compare : node -> node -> int
(** By identifier, as [Types.Type_ops.compare]. *)

(** {1 Signatures}

    [Types]' signature types with [type_expr] replaced by {!node}. Fields
    which hold no type are the very values of the boxed signature. *)

module Sig : sig
  type value_description = {
    val_type: node;
    val_kind: Types.value_kind;
    val_loc: Location.t;
    val_attributes: Parsetree.attributes;
  }

  type type_declaration = {
    type_params: node list;
    type_arity: int;
    type_kind: type_kind;
    type_private: Asttypes.private_flag;
    type_manifest: node option;
    type_variance: Types.Variance.t list;
    type_newtype_level: (int * int) option;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_immediate: bool;
    type_representation: Types.type_representation;
    type_shape: Variant_runtime.value_shape;
    type_inlined_types: type_inlined_type list;
  }

  and type_inlined_type =
    | Record of {type_name: string; labels: label_declaration list}

  and type_kind =
    | Type_abstract
    | Type_record of label_declaration list * Types.record_representation
    | Type_variant of constructor_declaration list * Variant_runtime.layout_ref
    | Type_open

  and label_declaration = {
    ld_id: Ident.t;
    ld_runtime_name: string option;
    ld_mutable: Asttypes.mutable_flag;
    ld_optional: bool;
    ld_type: node;
    ld_loc: Location.t;
    ld_attributes: Parsetree.attributes;
  }

  and constructor_declaration = {
    cd_id: Ident.t;
    cd_runtime_tag: Variant_runtime.literal_tag option;
    cd_args: constructor_arguments;
    cd_res: node option;
    cd_loc: Location.t;
    cd_attributes: Parsetree.attributes;
  }

  and constructor_arguments =
    | Cstr_tuple of node list
    | Cstr_record of label_declaration list

  type extension_constructor = {
    ext_type_path: Path.t;
    ext_type_params: node list;
    ext_args: constructor_arguments;
    ext_ret_type: node option;
    ext_private: Asttypes.private_flag;
    ext_loc: Location.t;
    ext_attributes: Parsetree.attributes;
    ext_is_exception: bool;
  }

  type module_type =
    | Mty_ident of Path.t
    | Mty_signature of signature
    | Mty_functor of Ident.t * module_type option * module_type
    | Mty_alias of Types.alias_presence * Path.t

  and signature = signature_item list

  and signature_item =
    | Sig_value of Ident.t * value_description
    | Sig_type of Ident.t * type_declaration * Types.rec_status
    | Sig_typext of Ident.t * extension_constructor * Types.ext_status
    | Sig_module of Ident.t * module_declaration * Types.rec_status
    | Sig_modtype of Ident.t * modtype_declaration

  and module_declaration = {
    md_type: module_type;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
  }

  and modtype_declaration = {
    mtd_type: module_type option;
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
  }
end

(** {1 Boundaries}

    Conversions between boxed graphs and the store, in sessions. A session
    memoizes every converted record and block by physical identity, so it
    maps a graph with sharing and cycles to a graph with the same sharing
    and cycles; use one session for everything that must keep its sharing
    (for example a whole signature, or all the types of one [.cmt]).
    Identifiers and levels are copied, so ids and bytes do not change.

    While a session runs it temporarily marks the abbreviation cells
    ([Tconstr]'s [abbrev_memo ref]) of the source graph, as a copy marks
    nodes with [Tsubst]: nothing else may read the source graph until the
    session ends. The source graph must not change during the session. *)

module Import : sig
  type t

  val run : ?store:store -> (t -> 'a) -> 'a
  (** [run f] runs a session importing into [store], by default the current
      store at the time of the call. *)

  val type_expr : t -> Types.type_expr -> node
  val signature : t -> Types.signature -> Sig.signature

  val nodes : t -> int
  (** Number of nodes created so far. *)
end

module Export : sig
  type t

  val run : (t -> 'a) -> 'a
  val type_expr : t -> node -> Types.type_expr
  val signature : t -> Sig.signature -> Types.signature
end
