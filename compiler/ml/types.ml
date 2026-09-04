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

(* Representation of types and declarations *)

open Asttypes

(* Type expressions for the core language *)

type type_expr = {mutable desc: type_desc; mutable level: int; id: int}

and arg = {lbl: arg_label; typ: type_expr}

and type_desc =
  | Tvar of string option
  | Tarrow of arg list * type_expr
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr
  | Tfield of {
      name: string;
      mutability: field_mutability ref;
      typ: type_expr;
      rest: type_expr;
    }
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr (* for copying *)
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * Longident.t list * type_expr list

and row_desc = {
  row_fields: (label * row_field) list;
  row_more: type_expr;
  row_closed: bool;
  row_fixed: bool;
  row_name: (Path.t * type_expr list) option;
}

and row_field =
  | Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
  (* 1st true denotes a constant constructor *)
  (* 2nd true denotes a tag in a pattern matching, and
     is erased later *)
  | Rabsent

and abbrev_memo =
  | Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and field_mutability =
  | Mutability_value of Asttypes.mutable_flag
  | Mutability_link of field_mutability ref

module Type_ops = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

(* Maps of methods and instance variables *)

module Ordered_string = struct
  type t = string
  let compare (x : t) y = compare x y
end
module Vars = Map.Make (Ordered_string)

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

module Variance = struct
  type t = int
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  let single = function
    | May_pos -> 1
    | May_neg -> 2
    | May_weak -> 4
    | Inj -> 8
    | Pos -> 16
    | Neg -> 32
    | Inv -> 64
  let union v1 v2 = v1 lor v2
  let inter v1 v2 = v1 land v2
  let subset v1 v2 = v1 land v2 = v1
  let set x b v = if b then v lor single x else v land lnot (single x)
  let mem x = subset (single x)
  let null = 0
  let may_inv = 7
  let full = 127
  let covariant = single May_pos lor single Pos lor single Inj
  let swap f1 f2 v =
    let v' = set f1 (mem f2 v) v in
    set f2 (mem f1 v) v'
  let conjugate v = swap May_pos May_neg (swap Pos Neg v)
  let get_upper v = (mem May_pos v, mem May_neg v)
  let get_lower v = (mem Pos v, mem Neg v, mem Inv v, mem Inj v)
end

(* Type definitions *)

type type_declaration = {
  type_params: type_expr list;
  type_arity: int;
  type_kind: type_kind;
  type_private: private_flag;
  type_manifest: type_expr option;
  type_variance: Variance.t list;
  type_newtype_level: (int * int) option;
  type_loc: Location.t;
  type_attributes: Parsetree.attributes;
  type_immediate: bool;
  type_representation: type_representation;
  type_inlined_types: type_inlined_type list;
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
  | Record_float_unused (* Was: all fields are floats. Now: unused *)
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

(* Type expressions for the class language *)

module Concr = Set.Make (Ordered_string)

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
  mtd_type: module_type option; (* Note: abstract *)
  mtd_attributes: Parsetree.attributes;
  mtd_loc: Location.t;
}

and rec_status =
  | Trec_not (* first in a nonrecursive group *)
  | Trec_first (* first in a recursive group *)
  | Trec_next (* not first in a recursive/nonrecursive group *)

and ext_status =
  | Text_first (* first constructor of an extension *)
  | Text_next (* not first constructor of an extension *)
  | Text_exception (* an exception *)

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

(* Whether two constructor descriptions denote the same constructor of a
   common scrutinee type. Because a re-exported variant mints descriptions
   with a different type path, ordinary constructors are compared by name
   only; the shared scrutinee type makes the name unambiguous. Not a
   general-purpose identity test across unrelated types. *)
let same_constructor c1 c2 =
  match (c1.cstr_kind, c2.cstr_kind) with
  | Ordinary_constructor _, Ordinary_constructor _ ->
    c1.cstr_name = c2.cstr_name
  | Extension_constructor p1, Extension_constructor p2 -> Path.same p1 p2
  | (Ordinary_constructor _ | Extension_constructor _), _ -> false

let may_equal_constr c1 c2 =
  match (c1.cstr_kind, c2.cstr_kind) with
  | Extension_constructor _, Extension_constructor _ ->
    (* extension constructors may be rebound, so paths cannot disprove
       equality; arity can *)
    c1.cstr_arity = c2.cstr_arity
  | _ -> same_constructor c1 c2

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
      (* All the labels in this type. This is mutable only because of a specific feature related to dicts, and should not be mutated elsewhere. *)
  lbl_repres: record_representation; (* Representation for this record *)
  lbl_private: private_flag; (* Read-only field? *)
  lbl_loc: Location.t;
  lbl_attributes: Parsetree.attributes;
}
let same_record_representation x y =
  match x with
  | Record_regular -> y = Record_regular
  | Record_float_unused -> y = Record_float_unused
  | Record_inlined {name} -> (
    match y with
    | Record_inlined y -> name = y.name
    | _ -> false)
  | Record_extension -> y = Record_extension
  | Record_unboxed x -> (
    match y with
    | Record_unboxed y -> x = y
    | _ -> false)
