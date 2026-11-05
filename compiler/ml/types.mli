(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {0 Representation of types and declarations} *)

(** [Types] defines the representation of types and declarations (that is, the
    content of module signatures).

    CMI files are made of marshalled types.
*)

open Asttypes
(** Asttypes exposes basic definitions shared both by Parsetree and Types. *)

type type_expr [@@immediate]
(** Type expressions for the core language: a node of a {!Store}, an
    immediate integer naming the store and the node's position in it. Physical
    equality [==] of types is equality of nodes. The node's descriptor, level
    and identifier are read with {!get_desc}, {!get_level} and {!get_id}.

    The [type_desc] variant defines all the possible type expressions one can
    find in OCaml.

    The level tracks the level of polymorphism associated to a type,
    guiding the generalization algorithm.
    Put shortly, when referring to a type in a given environment, both the type
    and the environment have a level. If the type has an higher level, then it
    can be considered fully polymorphic (type variables will be printed as
    ['a]), otherwise it'll be weakly polymorphic, or non generalized (type
    variables printed as ['_a]).
    See [http://okmij.org/ftp/ML/generalization.html] for more information.

    Polymorphic comparison and hashing of a structure holding types compare
    and hash the nodes, not the types they denote: use {!Type_ops} or
    [Btype]'s tables. [.cmi] and [.cmt] files hold the boxed representation of
    {!Types_boxed}; [Type_arena] converts between the two. *)

and arg = {lbl: arg_label; typ: type_expr}

and type_desc =
  | Tvar of string option
      (** [Tvar (Some "a")] ==> ['a] or ['_a]
      [Tvar None]       ==> [_] *)
  | Tarrow of arg list * type_expr
      (** [Tarrow ([e1; ~l:e2; ?l:e3], ret)] ==> [(e1, ~l:e2, ~l:e3=?) => ret]
      The n-ary uncurried function type: the arity is the length of the
      parameter list, and a function returning a function is a nested
      [Tarrow] in the return type. Invariant: the list is non-empty. *)
  | Ttuple of type_expr list  (** [Ttuple [t1;...;tn]] ==> [(t1 * ... * tn)] *)
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
      (** [Tconstr (`A.B.t', [t1;...;tn], _)] ==> [(t1,...,tn) A.B.t]
      The last parameter keep tracks of known expansions, see [abbrev_memo]. *)
  | Tobject of type_expr
      (** [Tobject `f1:t1;...;fn: tn'] ==> [{"f1": t1, ..., "fn": tn}]
      f1, fn are represented as a linked list of types using Tfield and Tnil
      constructors, terminated by a row variable when the row is open. *)
  | Tfield of {
      name: string;
      mutability: field_mutability ref;
      typ: type_expr;
      rest: type_expr;
    }
      (** [Tfield {name = "foo"; mutability; typ; rest}]
      ==> [{.. "foo": typ, rest}]. [mutability] is a shared cell; inspect its
      semantic value with [Btype.mutability_repr]. *)
  | Tnil  (** [Tnil] ==> [<...; >] *)
  | Tlink of type_expr  (** Indirection used by unification engine. *)
  | Tsubst of type_expr (* for copying *)
      (** [Tsubst] is used temporarily to store information in low-level
      functions manipulating representation of types, such as
      instantiation or copy.
      This constructor should not appear outside of these cases. *)
  | Tvariant of row_desc
      (** Representation of polymorphic variants, see [row_desc]. *)
  | Tunivar of string option
      (** Occurrence of a type variable introduced by a
      forall quantifier / [Tpoly]. *)
  | Tpoly of type_expr * type_expr list
      (** [Tpoly (ty,tyl)] ==> ['a1... 'an. ty],
      where 'a1 ... 'an are names given to types in tyl
      and occurrences of those types in ty. *)
  | Tpackage of Path.t * Longident.t list * type_expr list
      (** Type of a first-class module (a.k.a package). *)

and row_desc = {
  row_fields: (label * row_field) list;
  row_more: type_expr;
  row_closed: bool;
  row_fixed: bool;
  row_name: (Path.t * type_expr list) option;
}
(** [  `X | `Y ]       (row_closed = true)
    [< `X | `Y ]       (row_closed = true)
    [> `X | `Y ]       (row_closed = false)
    [< `X | `Y > `X ]  (row_closed = true)

    type t = [> `X ] as 'a      (row_more = Tvar a)
    type t = private [> `X ]    (row_more = Tconstr (t#row, [], ref Mnil)

    And for:

        let f = function `X -> `X -> | `Y -> `X

    the type of "f" will be a [Tarrow] whose lhs will (basically) be:

        Tvariant { row_fields = [("X", _)];
                   row_more   =
                     Tvariant { row_fields = [("Y", _)];
                                row_more   =
                                  Tvariant { row_fields = [];
                                             row_more   = _;
                                             _ };
                                _ };
                   _
                 }

*)

and row_field =
  | Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
  (* 1st true denotes a constant constructor *)
  (* 2nd true denotes a tag in a pattern matching, and
     is erased later *)
  | Rabsent

(** [abbrev_memo] allows one to keep track of different expansions of a type
    alias. This is done for performance purposes.

    For instance, when defining [type 'a pair = 'a * 'a], when one refers to an
    ['a pair], it is just a shortcut for the ['a * 'a] type.
    This expansion will be stored in the [abbrev_memo] of the corresponding
    [Tconstr] node.

    In practice, [abbrev_memo] behaves like list of expansions with a mutable
    tail.

    Note on marshalling: [abbrev_memo] must not appear in saved types.
    [Btype], with [cleanup_abbrev] and [memo], takes care of tracking and
    removing abbreviations.
*)
and abbrev_memo =
  | Mnil  (** No known abbreviation *)
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
      (** Found one abbreviation.
      A valid abbreviation should be at least as visible and reachable by the
      same path.
      The first expression is the abbreviation and the second the expansion. *)
  | Mlink of abbrev_memo ref
      (** Abbreviations can be found after this indirection *)

(** Assignment capability of an object field.

    The semantic value is [Immutable] or [Mutable]. [Mutability_link] is an
    internal edge between cells which have the same capability, not a third
    value. Code making a semantic decision must use [Btype.mutability_repr]
    rather than inspect a cell directly. *)
and field_mutability =
  | Mutability_value of Asttypes.mutable_flag
  | Mutability_link of field_mutability ref

(** {1 Access to type nodes}

    Code reads a node through these accessors and changes it through the
    logged setters in [Btype] ([link_type], [set_level], [log_type]).

    [get_desc] does not call [Btype.repr]: it returns the node's own
    descriptor, which can be [Tlink]. Reading a [Tlink] or [Tsubst] descriptor
    allocates it, as the store keeps those as integers ({!link_target}). *)

val get_desc : type_expr -> type_desc
(** The node's descriptor, without following [Tlink]. *)

val stored_desc : type_expr -> type_desc
(** The descriptor under the node's [Tlink] or [Tsubst], if any; else the
    same as {!get_desc}. *)

val get_level : type_expr -> int

val get_id : type_expr -> int
(** The node's unique identifier, allocated by [Btype.newty2]. It is not the
    node's position in its store: ids are kept when a type graph is read from
    or written to a file. *)

val link_target : type_expr -> type_expr
(** The target of the node's [Tlink] descriptor, or the node itself; does not
    allocate. *)

val subst_target : type_expr -> type_expr
(** The target of the node's [Tsubst] descriptor, or the node itself. *)

(** Stores of type nodes.

    A store holds the nodes of one compiler request. Each domain has a
    default store, used outside {!with_store}; {!with_store} runs an action
    with a fresh store as the domain's current one and frees it afterwards.
    New nodes are allocated in the current store. Reading or writing a node
    goes to the store that owns it, whichever store is current.

    A node must not be used after its store is freed: such a node reads
    whatever a later user put at its index (memory safe, but wrong). *)
module Store : sig
  type t

  val current : unit -> t

  val with_store : ?capacity:int -> (unit -> 'a) -> 'a
  (** [with_store action] runs [action] with a new current store, restores
      the previous one afterwards (also when [action] raises) and frees the new
      store. Calls nest. *)

  val create : ?capacity:int -> unit -> t
  (** A store that is not current; nodes are put into it with {!newty_in}.
      Free it with {!release} once no node of it is used any more. *)

  val release : t -> unit
  val length : t -> int

  val of_node : type_expr -> t
  (** The store which owns the node. *)

  val nth : t -> int -> type_expr
  (** [nth s i] is the [i]-th node allocated in [s]. *)

  val index : type_expr -> int
  (** Position of the node in its store. *)

  val index_bits : int
  val max_slots : int

  val newty_in : t -> type_desc -> level:int -> id:int -> type_expr
end

(** Creation and unlogged updates of type nodes. The setters change a node
    without recording its old value on the backtracking trail. Their users are
    [Btype] (node creation, the trail itself, copy sessions and marking), and
    the parts of [Ctype] which either restore their own temporary changes
    (copy marks, level marks, temporary links during unification) or call
    [Btype.log_type] first, which records the old descriptor before a
    persistent change. Other code uses the logged setters of [Btype]. *)
module Transient_expr : sig
  val create : type_desc -> level:int -> id:int -> type_expr
  (** A new node in the current store. *)

  val set_desc : type_expr -> type_desc -> unit
  val set_level : type_expr -> int -> unit
end

(** {1 Kind-specific accessors}

    The store encodes most descriptors as integers, and {!get_desc} builds a
    boxed view of them: it allocates for [Tarrow], [Ttuple], [Tconstr],
    [Tobject] and [Tpoly]. Hot paths read the encoded descriptor with these
    accessors instead, which allocate nothing. They read the descriptor
    under a link or copy mark, so the caller must have checked {!kind}
    first. *)

type kind =
  | Kvar
  | Karrow
  | Ktuple
  | Kconstr
  | Kobject
  | Knil
  | Kunivar
  | Kpoly
  | Kboxed  (** [Tfield], [Tvariant] or [Tpackage]: see {!boxed_desc}. *)
  | Klink
  | Ksubst

val kind : type_expr -> kind
(** [Klink]/[Ksubst] when the node is a link or copy mark, else the kind of
    its descriptor. *)

val stored_kind : type_expr -> kind
(** The kind of the descriptor under a link or copy mark. *)

val var_name : type_expr -> string option
val arrow_arity : type_expr -> int
val arrow_arg : type_expr -> int -> type_expr
val arrow_label : type_expr -> int -> arg_label
val arrow_label_is_nolabel : type_expr -> int -> bool
val arrow_ret : type_expr -> type_expr
val tuple_length : type_expr -> int
val tuple_elem : type_expr -> int -> type_expr
val constr_path : type_expr -> Path.t
val constr_length : type_expr -> int
val constr_arg : type_expr -> int -> type_expr

val same_constr_path : type_expr -> type_expr -> bool
(** [Path.same] of the paths of two [Tconstr] nodes. *)

val object_fields : type_expr -> type_expr
val poly_body : type_expr -> type_expr
val poly_length : type_expr -> int
val poly_var : type_expr -> int -> type_expr

val boxed_desc : type_expr -> type_desc
(** The descriptor of a [Kboxed] node (no allocation). *)

val abbrev_cell : type_expr -> abbrev_memo ref
(** The abbreviation cell of a [Tconstr] node, the one its view holds;
    created holding [Mnil] on first use. *)

val abbrev_memo : type_expr -> abbrev_memo
(** The contents of the abbreviation cell ([Mnil] if it was never asked
    for), without creating it. *)

val clear_abbrev_memo : type_expr -> unit
(** [abbrev_cell t := Mnil], without creating the cell. *)

val set_abbrev_cell : type_expr -> abbrev_memo ref -> unit

val link_code : type_expr -> int
(** -1: no link; [t >= 0]: [Tlink t]; [t <= -2]: [Tsubst (-2 - t)]. *)

val set_link_code : type_expr -> int -> unit
(** Unlogged: set or (with -1) remove a link or copy mark. Removing it
    restores the encoded descriptor underneath. *)

val set_link : type_expr -> type_expr -> unit
(** Unlogged [Transient_expr.set_desc t (Tlink t')], without the block. *)

val set_subst : type_expr -> type_expr -> unit
(** Unlogged [Transient_expr.set_desc t (Tsubst t')], without the block. *)

val iter_children : (type_expr -> unit) -> type_expr -> unit
(** The children of a node not [Kboxed], in [Btype.iter_type_expr]'s order;
    a link's or copy mark's target. *)

val copy_into :
  keep_names:bool ->
  norm:(type_expr -> type_expr) ->
  dst:type_expr ->
  type_expr ->
  (type_expr -> type_expr) ->
  unit
(** [copy_into ~keep_names ~norm ~dst src f] gives [dst] the descriptor
    [Btype.copy_type_desc ~keep_names f] gives for [src]'s, evaluating [f] in
    the same order ([norm (f v)] for a [Tpoly]'s variables). [src] must not
    be [Kboxed]; its link or copy mark is ignored. Unlogged. *)

val new_constr : level:int -> id:int -> Path.t -> type_expr array -> type_expr
(** A new node [Tconstr (p, args, ref Mnil)] in the current store. *)

val set_constr_args : type_expr -> Path.t -> type_expr array -> unit
(** As {!set_constr}, with the arguments in an array. *)

val has_abbrev_cell : type_expr -> bool
(** Whether {!abbrev_cell} was asked for (or the node was given one). *)

val set_constr : type_expr -> Path.t -> type_expr list -> unit
(** Unlogged: [Transient_expr.set_desc t (Tconstr (p, args, ref Mnil))]
    without allocating a cell. *)

module Type_ops : sig
  type t = type_expr
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module Vars : Map.S with type key = string

(* Value descriptions *)

type value_description = {
  val_type: type_expr; (* Type of the value *)
  val_kind: value_kind;
  val_loc: Location.t;
  val_attributes: Parsetree.attributes;
}

and value_kind =
  | Val_reg (* Regular value *)
  | Val_prim of Primitive.description (* Primitive *)

(* Variance *)

module Variance : sig
  type t
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  val null : t (* no occurrence *)
  val full : t (* strictly invariant *)
  val covariant : t (* strictly covariant *)
  val may_inv : t (* maybe invariant *)
  val union : t -> t -> t
  val inter : t -> t -> t
  val subset : t -> t -> bool
  val set : f -> bool -> t -> t
  val mem : f -> t -> bool
  val conjugate : t -> t (* exchange positive and negative *)
  val get_upper : t -> bool * bool (* may_pos, may_neg   *)
  val get_lower : t -> bool * bool * bool * bool (* pos, neg, inv, inj *)
end

(* Type definitions *)

type type_declaration = {
  type_params: type_expr list;
  type_arity: int;
  type_kind: type_kind;
  type_private: private_flag;
  type_manifest: type_expr option;
  type_variance: Variance.t list;
  (* covariant, contravariant, weakly contravariant, injective *)
  type_newtype_level: (int * int) option;
  (* definition level * expansion level *)
  type_loc: Location.t;
  type_attributes: Parsetree.attributes;
  type_immediate: bool; (* true iff type should not be a pointer *)
  type_representation: type_representation;
  type_shape: Variant_runtime.value_shape;
      (** What the declaration itself states about its values at run time,
          resolved in its own environment when it is declared. An
          abbreviation without an annotation states [Unstated]; readers take
          the shape of the first type on an expansion chain that states one
          ([Variant_layout.value_shape]). *)
  type_inlined_types: type_inlined_type list;
      (** Representation of inlined types, needed for printing *)
}

and type_inlined_type =
  | Record of {type_name: string; labels: label_declaration list}

and type_kind =
  | Type_abstract
  | Type_record of label_declaration list * record_representation
  | Type_variant of constructor_declaration list * Variant_runtime.layout_ref
  | Type_open

and record_representation =
  | Record_regular (* All fields are boxed / tagged *)
  | Record_unboxed of bool (* Unboxed single-field record, inlined or not *)
  | Record_inlined of
      (* Inlined record *)
       {
      name: string;
      representation: Variant_runtime.constructor_reference;
    }
  | Record_extension (* Inlined record under extension *)

and label_declaration = {
  ld_id: Ident.t;
  ld_runtime_name: string option;
      (* The name the field has at run time, when it differs from [ld_id].
         Comes from [@as] on the declaration, as its decoded value: two
         spellings of one name are one name, so this compares by [=] and is
         stable in the cmi. Note the asymmetry with [ld_attributes] below,
         which still carries whole parsetree attributes, source spelling
         included. That is safe only because every reader of an attribute
         payload goes through a semantic accessor; anything comparing
         attributes structurally would be comparing keystrokes. *)
  ld_mutable: mutable_flag;
  ld_optional: bool;
  ld_type: type_expr;
  ld_loc: Location.t;
  ld_attributes: Parsetree.attributes;
}

and constructor_declaration = {
  cd_id: Ident.t;
  cd_runtime_tag: Variant_runtime.literal_tag option;
      (* The decoded [@as] value of the constructor. *)
  cd_args: constructor_arguments;
  cd_res: type_expr option;
  cd_loc: Location.t;
  cd_attributes: Parsetree.attributes;
}

and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

and type_representation = Boxed | Unboxed
(* Single-payload @unboxed type: the payload is the whole runtime
         value. Multi-constructor unboxed variants are tracked by their layout,
         not here. *)

type extension_constructor = {
  ext_type_path: Path.t;
  ext_type_params: type_expr list;
  ext_args: constructor_arguments;
  ext_ret_type: type_expr option;
  ext_private: private_flag;
  ext_loc: Location.t;
  ext_attributes: Parsetree.attributes;
  ext_is_exception: bool;
}

module Concr : Set.S with type elt = string

(* Type expressions for the module language *)

type module_type =
  | Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of Ident.t * module_type option * module_type
  | Mty_alias of alias_presence * Path.t

and alias_presence = Mta_present | Mta_absent

and signature = signature_item list

and signature_item =
  | Sig_value of Ident.t * value_description
  | Sig_type of Ident.t * type_declaration * rec_status
  | Sig_typext of Ident.t * extension_constructor * ext_status
  | Sig_module of Ident.t * module_declaration * rec_status
  | Sig_modtype of Ident.t * modtype_declaration

and module_declaration = {
  md_type: module_type;
  md_attributes: Parsetree.attributes;
  md_loc: Location.t;
}

and modtype_declaration = {
  mtd_type: module_type option; (* None: abstract *)
  mtd_attributes: Parsetree.attributes;
  mtd_loc: Location.t;
}

and rec_status =
  | Trec_not (* first in a nonrecursive group *)
  | Trec_first (* first in a recursive group *)
  | Trec_next (* not first in a recursive/nonrecursive group *)

and ext_status =
  | Text_first (* first constructor in an extension *)
  | Text_next (* not first constructor in an extension *)
  | Text_exception

(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description = {
  cstr_name: string; (* Constructor name *)
  cstr_res: type_expr; (* Type of the result *)
  cstr_existentials: type_expr list; (* list of existentials *)
  cstr_args: type_expr list; (* Type of the arguments *)
  cstr_arity: int; (* Number of arguments *)
  cstr_kind: constructor_kind;
  cstr_generalized: bool; (* Constrained return type? *)
  cstr_private: private_flag; (* Read-only constructor? *)
  cstr_loc: Location.t;
  cstr_attributes: Parsetree.attributes;
  cstr_inlined: type_declaration option;
}

and constructor_kind =
  | Ordinary_constructor of Variant_runtime.constructor_reference
    (* Constructor introduced by a variant type declaration. This stable
       reference addresses its runtime representation directly; the shared
       variant value also supplies declaration-level matching facts. *)
  | Extension_constructor of Path.t
(* Extension constructor, identified by its own path since extension
         constructors can be rebound *)

(* Constructor descriptions of a common scrutinee type denote the same
   constructor: ordinary constructors compare by name (re-exported variants
   carry a different type path), extension constructors by path. Not a
   general-purpose identity test across unrelated types. *)
val same_constructor :
  constructor_description -> constructor_description -> bool

(* Constructors may be the same, given potential rebinding *)
val may_equal_constr :
  constructor_description -> constructor_description -> bool

type label_description = {
  lbl_name: string; (* Short name *)
  lbl_runtime_name: string;
      (* The name the field has at run time: its [@as] payload when it has
         one, otherwise [lbl_name]. Decided once when the declaration is
         typed, so no consumer re-reads the attribute. *)
  lbl_res: type_expr; (* Type of the result *)
  lbl_arg: type_expr; (* Type of the argument *)
  lbl_mut: mutable_flag; (* Is this a mutable field? *)
  lbl_optional: bool; (* Is this an optional field? *)
  lbl_pos: int; (* Position in block *)
  mutable lbl_all: label_description array;
  (* All the labels in this type. Mutable only to tie the knot when
     [Dict_pattern] synthesizes labels; never mutated afterwards. *)
  lbl_repres: record_representation; (* Representation for this record *)
  lbl_private: private_flag; (* Read-only field? *)
  lbl_loc: Location.t;
  lbl_attributes: Parsetree.attributes;
}

val same_record_representation :
  record_representation -> record_representation -> bool
