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

type type_expr = {mutable desc: type_desc; mutable level: int; id: int}
(** Type expressions for the core language.

    The [type_desc] variant defines all the possible type expressions one can
    find in OCaml. [type_expr] wraps this with some annotations.

    The [level] field tracks the level of polymorphism associated to a type,
    guiding the generalization algorithm.
    Put shortly, when referring to a type in a given environment, both the type
    and the environment have a level. If the type has an higher level, then it
    can be considered fully polymorphic (type variables will be printed as
    ['a]), otherwise it'll be weakly polymorphic, or non generalized (type
    variables printed as ['_a]).
    See [http://okmij.org/ftp/ML/generalization.html] for more information.

    Note about [type_declaration]: one should not make the confusion between
    [type_expr] and [type_declaration].

    [type_declaration] refers specifically to the [type] construct in OCaml
    language, where you create and name a new type or type alias.

    [type_expr] is used when you refer to existing types, e.g. when annotating
    the expected type of a value.

    Also, as the type system of OCaml is generative, a [type_declaration] can
    have the side-effect of introducing a new type constructor, different from
    all other known types.
    Whereas [type_expr] is a pure construct which allows referring to existing
    types.

    Note on mutability: TBD.
 *)

and type_desc =
  | Tvar of string option
      (** [Tvar (Some "a")] ==> ['a] or ['_a]
      [Tvar None]       ==> [_] *)
  | Tarrow of Noloc.arg_label * type_expr * type_expr * commutable * arity
      (** [Tarrow (Nolabel,      e1, e2, c)] ==> [e1    -> e2]
      [Tarrow (Labelled "l", e1, e2, c)] ==> [l:e1  -> e2]
      [Tarrow (Optional "l", e1, e2, c)] ==> [?l:e1 -> e2]

      See [commutable] for the last argument. *)
  | Ttuple of type_expr list  (** [Ttuple [t1;...;tn]] ==> [(t1 * ... * tn)] *)
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
      (** [Tconstr (`A.B.t', [t1;...;tn], _)] ==> [(t1,...,tn) A.B.t]
      The last parameter keep tracks of known expansions, see [abbrev_memo]. *)
  | Tobject of type_expr * (Path.t * type_expr list) option ref
      (** [Tobject (`f1:t1;...;fn: tn', `None')] ==> [< f1: t1; ...; fn: tn >]
      f1, fn are represented as a linked list of types using Tfield and Tnil
      constructors.

      [Tobject (_, `Some (`A.ct', [t1;...;tn]')] ==> [(t1, ..., tn) A.ct].
      where A.ct is the type of some class.

      There are also special cases for so-called "class-types", cf. [Typeclass]
      and [Ctype.set_object_name]:

        [Tobject (Tfield(_,_,...(Tfield(_,_,rv)...),
                         Some(`A.#ct`, [rv;t1;...;tn])]
             ==> [(t1, ..., tn) #A.ct]
        [Tobject (_, Some(`A.#ct`, [Tnil;t1;...;tn])] ==> [(t1, ..., tn) A.ct]

      where [rv] is the hidden row variable.
  *)
  | Tfield of string * field_kind * type_expr * type_expr
      (** [Tfield ("foo", Fpresent, t, ts)] ==> [<...; foo : t; ts>] *)
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

and field_kind = Fvar of field_kind option ref | Fpresent | Fabsent

(** [commutable] is a flag appended to every arrow type.

    When typing an application, if the type of the functional is
    known, its type is instantiated with [Cok] arrows, otherwise as
    [Clink (ref Cunknown)].

    When the type is not known, the application will be used to infer
    the actual type.  This is fragile in presence of labels where
    there is no principal type.

    Two incompatible applications relying on [Cunknown] arrows will
    trigger an error.

    let f g =
      g ~a:() ~b:();
      g ~b:() ~a:();

    Error: This function is applied to arguments
    in an order different from other calls.
    This is only allowed when the real type is known.
*)
and commutable = Cok | Cunknown | Clink of commutable ref

module TypeOps : sig
  type t = type_expr
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

(* Maps of methods and instance variables *)

module Meths : Map.S with type key = string
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
  type_unboxed: unboxed_status;
  type_inlined_types: type_inlined_type list;
      (** Representation of inlined types, needed for printing *)
}

and type_inlined_type =
  | Record of {type_name: string; labels: label_declaration list}

and type_kind =
  | Type_abstract
  | Type_record of label_declaration list * record_representation
  | Type_variant of constructor_declaration list
  | Type_open

and record_representation =
  | Record_regular (* All fields are boxed / tagged *)
  | Record_float_unused (* Was: all fields are floats. Now: unused *)
  | Record_unboxed of bool (* Unboxed single-field record, inlined or not *)
  | Record_inlined of
      (* Inlined record *)
       {
      tag: int;
      name: string;
      num_nonconsts: int;
      attrs: Parsetree.attributes;
    }
  | Record_extension (* Inlined record under extension *)

and label_declaration = {
  ld_id: Ident.t;
  ld_mutable: mutable_flag;
  ld_optional: bool;
  ld_type: type_expr;
  ld_loc: Location.t;
  ld_attributes: Parsetree.attributes;
}

and constructor_declaration = {
  cd_id: Ident.t;
  cd_args: constructor_arguments;
  cd_res: type_expr option;
  cd_loc: Location.t;
  cd_attributes: Parsetree.attributes;
}

and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

and unboxed_status =
  private
  (* This type must be private in order to ensure perfect sharing of the
     four possible values. Otherwise, ocamlc.byte and ocamlc.opt produce
     different executables. *) {
  unboxed: bool;
  default: bool; (* True for unannotated unboxable types. *)
}

val unboxed_false_default_false : unboxed_status
val unboxed_false_default_true : unboxed_status
val unboxed_true_default_false : unboxed_status
val unboxed_true_default_true : unboxed_status

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
  | Sig_class of unit
  | Sig_class_type of unit (* Dummy AST node *)

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
  cstr_tag: constructor_tag; (* Tag for heap blocks *)
  cstr_consts: int; (* Number of constant constructors *)
  cstr_nonconsts: int; (* Number of non-const constructors *)
  cstr_generalized: bool; (* Constrained return type? *)
  cstr_private: private_flag; (* Read-only constructor? *)
  cstr_loc: Location.t;
  cstr_attributes: Parsetree.attributes;
  cstr_inlined: type_declaration option;
}

and constructor_tag =
  | Cstr_constant of int (* Constant constructor (an int) *)
  | Cstr_block of int (* Regular constructor (a block) *)
  | Cstr_unboxed (* Constructor of an unboxed type *)
  | Cstr_extension of Path.t (* Extension constructor *)

(* Constructors are the same *)
val equal_tag : constructor_tag -> constructor_tag -> bool

(* Constructors may be the same, given potential rebinding *)
val may_equal_constr :
  constructor_description -> constructor_description -> bool

type label_description = {
  lbl_name: string; (* Short name *)
  lbl_res: type_expr; (* Type of the result *)
  lbl_arg: type_expr; (* Type of the argument *)
  lbl_mut: mutable_flag; (* Is this a mutable field? *)
  lbl_optional: bool; (* Is this an optional field? *)
  lbl_pos: int; (* Position in block *)
  mutable lbl_all: label_description array;
  (* All the labels in this type. This is mutable only because of a specific feature related to dicts, and should not be mutated elsewhere. *)
  lbl_repres: record_representation; (* Representation for this record *)
  lbl_private: private_flag; (* Read-only field? *)
  lbl_loc: Location.t;
  lbl_attributes: Parsetree.attributes;
}

val same_record_representation :
  record_representation -> record_representation -> bool
