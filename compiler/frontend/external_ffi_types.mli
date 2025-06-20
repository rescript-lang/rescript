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

type import_attributes = (string, string) Hashtbl.t

type external_module_name = {
  bundle: string;
  module_bind_name: module_bind_name;
  import_attributes: import_attributes option;
}

type arg_type = External_arg_spec.attr

type arg_label = External_arg_spec.label

type external_spec =
  | Js_var of {
      name: string;
      external_module_name: external_module_name option;
      scopes: string list;
    }
  | Js_module_as_var of external_module_name
  | Js_module_as_fn of {
      external_module_name: external_module_name;
      splice: bool;
    }
  | Js_module_as_class of external_module_name
  | Js_call of {
      name: string;
      external_module_name: external_module_name option;
      splice: bool;
      scopes: string list;
      tagged_template: bool;
    }
  | Js_send of {name: string; splice: bool; js_send_scopes: string list}
    (* we know it is a js send, but what will happen if you pass an ocaml objct *)
  | Js_new of {
      name: string;
      external_module_name: external_module_name option;
      splice: bool;
      scopes: string list;
    }
  | Js_set of {js_set_name: string; js_set_scopes: string list}
  | Js_get of {js_get_name: string; js_get_scopes: string list}
  | Js_get_index of {js_get_index_scopes: string list}
  | Js_set_index of {js_set_index_scopes: string list}

type return_wrapper =
  | Return_unset
  | Return_identity
  | Return_null_to_opt
  | Return_null_undefined_to_opt
  | Return_replaced_with_unit

type params = Params of External_arg_spec.params | Param_number of int

type t = private
  | Ffi_bs of params * return_wrapper * external_spec
  | Ffi_obj_create of External_arg_spec.obj_params
  | Ffi_inline_const of Lam_constant.t
  | Ffi_normal
(* When it's normal, it is handled as normal c functional ffi call *)

(* val name_of_ffi : external_spec -> string *)

val check_ffi : ?loc:Location.t -> external_spec -> bool

val to_string : t -> string

val from_string : string -> t
(** Note *)

val inline_string_primitive : string -> string option -> string list

val inline_bool_primitive : bool -> string list

val inline_int_primitive : int32 -> string list

val inline_float_primitive : string -> string list

val inline_bigint_primitive : string -> string list

val ffi_bs : External_arg_spec.params -> return_wrapper -> external_spec -> t

val ffi_bs_as_prims :
  External_arg_spec.params -> return_wrapper -> external_spec -> string list

val ffi_obj_create : External_arg_spec.obj_params -> t

val ffi_obj_as_prims : External_arg_spec.obj_params -> string list
