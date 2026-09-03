(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type module_bind_name =
  | Phint_name of string
  (* explicit hint name *)
  | Phint_nothing

type import_attributes = (string * string) list (* ordered as written *)

type external_module_name = {
  bundle: string;
  module_bind_name: module_bind_name;
  import_attributes: import_attributes option;
}

type arg_type = External_arg_spec.attr

type arg_label = External_arg_spec.label

(* The declaration, as the attribute language states it. The backend
   compiles it directly; digestion validates it with [check_decl]. *)
type module_source =
  | Module_named of external_module_name (* @module("...") payload forms *)
  | Module_itself
(* bare @module: the external is the module itself, under its
         primitive name *)

type decl_kind =
  | Decl_val of {name: string} (* @val, or no attribute at all *)
  | Decl_send of {name: string} (* @send *)
  | Decl_new of {name: string} (* @new *)
  | Decl_get of {name: string} (* @get *)
  | Decl_set of {name: string} (* @set *)
  | Decl_get_index (* @get_index *)
  | Decl_set_index (* @set_index *)

type external_decl = {
  kind: decl_kind;
  module_: module_source option;
  scopes: string list; (* @scope *)
  variadic: bool; (* @variadic *)
  effective_arity: int;
      (* counted (non-@ignore) declared parameters; a function of the
         declared type, stored because the padded trailing unit makes it
         unrecoverable from the parameter specs alone *)
}

type return_wrapper =
  | Return_unset
  | Return_identity
  | Return_null_to_opt
  | Return_null_undefined_to_opt

(* An external declared as an inline constant is only ever a literal; the
   frontend parses delimiters and bigint signs before constructing this. *)
type inline_const =
  | Const_string of string
      (** A decoded runtime string value. For example, an inline external
          declared with ["a\\n"] stores a string containing an actual newline;
          source spelling is not needed after FFI processing. *)
  | Const_bool of bool
  | Const_int of int32
  | Const_bigint of {negative: bool; digits: string}
  | Const_float of string

type t = private
  | Ffi_bs of External_arg_spec.params * return_wrapper * external_decl
  | Ffi_obj_create of External_arg_spec.obj_params

val check_decl : ?loc:Location.t -> external_decl -> prim_name:string -> bool
(** Validation of a declaration; returns whether the binding refers to a
    package-relative path (which disables cross-module inlining). *)

val inclusion_compatible : t -> t -> bool
(** [inclusion_compatible impl intf]: can an implementation's spec satisfy an
    interface's spec during signature inclusion? Equal specs always can;
    object-creation specs additionally accept a widening of an optional
    field's [for_sure_no_nested_option] from false (implementation) to true
    (interface). *)

val ffi_bs : External_arg_spec.params -> return_wrapper -> external_decl -> t

val ffi_obj_create : External_arg_spec.obj_params -> t
