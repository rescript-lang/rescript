(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Outcometree]: printable representation of types and
   signature items *)

(* [Printtyp] builds these trees. They are printed through the hooks
   [Oprint.out_type], [Oprint.out_sig_item],
   [Oprint.out_signature] and related refs, which bsc sets to the ReScript
   printer through [Res_outcome_printer.setup]. *)

type out_ident =
  | Oide_apply of out_ident * out_ident
  | Oide_dot of out_ident * string
  | Oide_ident of string

type out_attribute = {oattr_name: string}

type out_type =
  | Otyp_abstract
  | Otyp_open
  | Otyp_alias of out_type * string
  | Otyp_arrow of (Asttypes.Noloc.arg_label * out_type) list * out_type
  | Otyp_constr of out_ident * out_type list
  | Otyp_manifest of out_type * out_type
  | Otyp_object of (string * bool * out_type) list * bool option
    (* fields are (name, mutable, type) *)
  | Otyp_record of (string * bool * bool * out_type) list
  | Otyp_stuff of string
  | Otyp_sum of (string * out_type list * out_type option * string option) list
  | Otyp_tuple of out_type list
  | Otyp_var of bool * string
  | Otyp_variant of bool * out_variant * bool * string list option
  | Otyp_poly of string list * out_type
  | Otyp_module of string * string list * out_type list
  | Otyp_attribute of out_type * out_attribute

and out_variant =
  | Ovar_fields of (string * bool * out_type list) list
  | Ovar_typ of out_type

type out_module_type =
  | Omty_abstract
  | Omty_functor of string * out_module_type option * out_module_type
  | Omty_ident of out_ident
  | Omty_signature of out_sig_item list
  | Omty_alias of out_ident
and out_sig_item =
  | Osig_typext of out_extension_constructor * out_ext_status
  | Osig_modtype of string * out_module_type
  | Osig_module of string * out_module_type * out_rec_status
  | Osig_type of out_type_decl * out_rec_status
  | Osig_value of out_val_decl
  | Osig_ellipsis
and out_type_decl = {
  otype_name: string;
  otype_params: (string * (bool * bool)) list;
  otype_type: out_type;
  otype_private: Asttypes.private_flag;
  otype_immediate: bool;
  otype_unboxed: bool;
  otype_cstrs: (out_type * out_type) list;
}
and out_extension_constructor = {
  oext_name: string;
  oext_type_name: string;
  oext_type_params: string list;
  oext_args: out_type list;
  oext_ret_type: out_type option;
  oext_repr: string option;
  oext_private: Asttypes.private_flag;
}
and out_type_extension = {
  otyext_name: string;
  otyext_params: string list;
  otyext_constructors:
    (string * out_type list * out_type option * string option) list;
  otyext_private: Asttypes.private_flag;
}
and out_val_decl = {
  oval_name: string;
  oval_type: out_type;
  oval_prim: Parsetree.primitive_repr option;
  oval_attributes: out_attribute list;
}
and out_rec_status = Orec_not | Orec_first | Orec_next
and out_ext_status = Oext_first | Oext_next | Oext_exception
