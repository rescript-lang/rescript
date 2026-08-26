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

(* An external declared as an inline constant is only ever a literal; the
   frontend parses delimiters and bigint signs before constructing this. *)
type inline_const =
  | Const_str of {s: string; delim: External_arg_spec.delim option}
  | Const_bool of bool
  | Const_int of int32
  | Const_bigint of {negative: bool; digits: string}
  | Const_float of string

type t = private
  | Ffi_bs of External_arg_spec.params * return_wrapper * external_spec
  | Ffi_obj_create of External_arg_spec.obj_params

(* val name_of_ffi : external_spec -> string *)

val check_ffi : ?loc:Location.t -> external_spec -> bool

val inclusion_compatible : t -> t -> bool
(** [inclusion_compatible impl intf]: can an implementation's spec satisfy an
    interface's spec during signature inclusion? Equal specs always can;
    object-creation specs additionally accept a widening of an optional
    field's [for_sure_no_nested_option] from false (implementation) to true
    (interface). *)

val ffi_bs : External_arg_spec.params -> return_wrapper -> external_spec -> t

val ffi_obj_create : External_arg_spec.obj_params -> t
