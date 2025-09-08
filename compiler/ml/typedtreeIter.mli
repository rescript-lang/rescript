(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree

module type IteratorArgument = sig
  val enter_structure : structure -> unit
  val enter_value_description : value_description -> unit
  val enter_type_extension : type_extension -> unit
  val enter_extension_constructor : extension_constructor -> unit
  val enter_pattern : pattern -> unit
  val enter_expression : expression -> unit
  val enter_package_type : package_type -> unit
  val enter_signature : signature -> unit
  val enter_signature_item : signature_item -> unit
  val enter_module_type_declaration : module_type_declaration -> unit
  val enter_module_type : module_type -> unit
  val enter_module_expr : module_expr -> unit
  val enter_with_constraint : with_constraint -> unit
  val enter_core_type : core_type -> unit
  val enter_structure_item : structure_item -> unit

  val leave_structure : structure -> unit
  val leave_value_description : value_description -> unit
  val leave_type_extension : type_extension -> unit
  val leave_extension_constructor : extension_constructor -> unit
  val leave_pattern : pattern -> unit
  val leave_expression : expression -> unit
  val leave_package_type : package_type -> unit
  val leave_signature : signature -> unit
  val leave_signature_item : signature_item -> unit
  val leave_module_type_declaration : module_type_declaration -> unit
  val leave_module_type : module_type -> unit
  val leave_module_expr : module_expr -> unit
  val leave_with_constraint : with_constraint -> unit
  val leave_core_type : core_type -> unit
  val leave_structure_item : structure_item -> unit

  val enter_bindings : rec_flag -> unit
  val enter_binding : value_binding -> unit
  val leave_binding : value_binding -> unit
  val leave_bindings : rec_flag -> unit

  val enter_type_declarations : rec_flag -> unit
  val enter_type_declaration : type_declaration -> unit
  val leave_type_declaration : type_declaration -> unit
  val leave_type_declarations : rec_flag -> unit
end

module MakeIterator : functor (Iter : IteratorArgument) -> sig
  val iter_structure : structure -> unit
  val iter_signature : signature -> unit
  val iter_structure_item : structure_item -> unit
  val iter_signature_item : signature_item -> unit
  val iter_expression : expression -> unit
  val iter_module_type : module_type -> unit
  val iter_pattern : pattern -> unit
end
[@@warning "-67"]

module DefaultIteratorArgument : IteratorArgument
