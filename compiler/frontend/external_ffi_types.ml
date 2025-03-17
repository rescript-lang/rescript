(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
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
[@@@warning "+9"]
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
(** TODO: information between [arg_type] and [arg_label] are duplicated,
    design a more compact representation so that it is also easy to seralize by hand
*)

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

(* let not_inlineable (x : external_spec) =     *)

(* let name_of_ffi ffi =
   match ffi with
   | Js_get_index _scope -> "@get_index .."
   | Js_set_index _scope -> "@set_index .."
   | Js_get { js_get_name = s} -> Printf.sprintf "[@@get %S]" s
   | Js_set { js_set_name = s} -> Printf.sprintf "[@@set %S]" s
   | Js_call v  -> Printf.sprintf "[@@val %S]" v.name
   | Js_send v  -> Printf.sprintf "[@@send %S]" v.name
   | Js_module_as_fn v  -> Printf.sprintf "[@@val %S]" v.external_module_name.bundle
   | Js_new v  -> Printf.sprintf "[@@new %S]" v.name
   | Js_module_as_class v
    -> Printf.sprintf "[@@module] %S " v.bundle
   | Js_module_as_var v
    ->
    Printf.sprintf "[@@module] %S " v.bundle
   | Js_var v (* FIXME: could be [@@module "xx"] as well *)
    ->
    Printf.sprintf "[@@val] %S " v.name *)

type return_wrapper =
  | Return_unset
  | Return_identity
  | Return_undefined_to_opt
  | Return_null_to_opt
  | Return_null_undefined_to_opt
  | Return_replaced_with_unit

type params = Params of External_arg_spec.params | Param_number of int

type t =
  | Ffi_bs of params * return_wrapper * external_spec
      (**  [Ffi_bs(args,return,attr) ]
       [return] means return value is unit or not,
        [true] means is [unit]
  *)
  | Ffi_obj_create of External_arg_spec.obj_params
  | Ffi_inline_const of Lam_constant.t
  | Ffi_normal
(* When it's normal, it is handled as normal c functional ffi call *)

let valid_js_char =
  let a =
    Array.init 256 (fun i ->
        let c = Char.chr i in
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '_' || c = '$')
  in
  fun c -> Array.unsafe_get a (Char.code c)

let valid_first_js_char =
  let a =
    Array.init 256 (fun i ->
        let c = Char.chr i in
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$')
  in
  fun c -> Array.unsafe_get a (Char.code c)

(** Approximation could be improved *)
let valid_ident (s : string) =
  let len = String.length s in
  len > 0
  && valid_js_char s.[0]
  && valid_first_js_char s.[0]
  &&
  let exception E in
  try
    for i = 1 to len - 1 do
      if not (valid_js_char (String.unsafe_get s i)) then raise_notrace E
    done;
    true
  with E -> false

let is_package_relative_path (x : string) =
  Ext_string.starts_with x "./" || Ext_string.starts_with x "../"

let valid_global_name ?loc txt =
  if not (valid_ident txt) then
    let v = Ext_string.split_by ~keep_empty:true (fun x -> x = '.') txt in
    Ext_list.iter v (fun s ->
        if not (valid_ident s) then
          Location.raise_errorf ?loc "Not a valid global name %s" txt)

(*
  We loose such check (see #2583),
  it also helps with the implementation deriving abstract [@as]
*)

let valid_method_name ?loc:_ _txt = ()
(* if not (valid_ident txt) then
   Location.raise_errorf ?loc "Not a valid method name %s"  txt *)

let check_external_module_name ?loc x =
  match x with
  | {bundle = ""; _} | {module_bind_name = Phint_name ""; bundle = _; _} ->
    Location.raise_errorf ?loc "empty name encountered"
  | _ -> ()

let check_ffi ?loc ffi : bool =
  let xrelative = ref false in
  let upgrade bool = if not !xrelative then xrelative := bool in
  (match ffi with
  | Js_var {name; external_module_name; scopes = _} ->
    upgrade (is_package_relative_path name);
    Ext_option.iter external_module_name (fun name ->
        upgrade (is_package_relative_path name.bundle));
    valid_global_name ?loc name
  | Js_send {name; splice = _; js_send_scopes = _}
  | Js_set {js_set_name = name; js_set_scopes = _}
  | Js_get {js_get_name = name; js_get_scopes = _} ->
    valid_method_name ?loc name
  | Js_get_index _ (* TODO: check scopes *) | Js_set_index _ -> ()
  | Js_module_as_var external_module_name
  | Js_module_as_fn {external_module_name; splice = _}
  | Js_module_as_class external_module_name ->
    upgrade (is_package_relative_path external_module_name.bundle);
    check_external_module_name external_module_name
  | Js_new {external_module_name; name; splice = _; scopes = _}
  | Js_call
      {external_module_name; name; splice = _; scopes = _; tagged_template = _}
    ->
    Ext_option.iter external_module_name (fun external_module_name ->
        upgrade (is_package_relative_path external_module_name.bundle));
    Ext_option.iter external_module_name (fun name ->
        check_external_module_name ?loc name);

    valid_global_name ?loc name);
  !xrelative

(* let bs_prefix = "BS:"
   let bs_prefix_length = String.length bs_prefix
*)

(** TODO: Make sure each version is not prefix of each other
    Solution:
    1. fixed length
    2. non-prefix approach
*)
(* let bs_external = bs_prefix  *)

(* let bs_external_length = String.length bs_external *)

let to_string (t : t) = Marshal.to_string t []

(* \132\149\166\190
   0x84 95 A6 BE Intext_magic_small intext.h
   https://github.com/ocaml/merlin/commit/b094c937c3a360eb61054f7652081b88e4f3612f
*)
let is_bs_primitive s =
  String.length s >= 20
  (* Marshal.header_size*) && String.unsafe_get s 0 = '\132'
  && String.unsafe_get s 1 = '\149'

let () =
  Oprint.map_primitive_name :=
    fun s -> if is_bs_primitive s then "BS:external" else s

(* TODO:  better error message when version mismatch *)
let from_string s : t =
  if is_bs_primitive s then Ext_marshal.from_string s else Ffi_normal

let () =
  Primitive.coerce :=
    fun ({
           prim_name;
           prim_arity;
           prim_native_name;
           prim_alloc = _;
           prim_from_constructor = _;
         } :
          Primitive.description)
      (p2 : Primitive.description)
    ->
      let p2_native = p2.prim_native_name in
      prim_name = p2.prim_name && prim_arity = p2.prim_arity
      && prim_native_name = p2_native
      ||
      match (from_string prim_native_name, from_string p2_native) with
      | Ffi_obj_create obj_parms, Ffi_obj_create obj_parms2 ->
        Ext_list.for_all2_no_exn obj_parms obj_parms2
          (fun {obj_arg_type; obj_arg_label} b ->
            let b_obj_arg_label = b.obj_arg_label in
            obj_arg_type = b.obj_arg_type
            && (obj_arg_label = b_obj_arg_label
               ||
               match (obj_arg_label, b_obj_arg_label) with
               | Obj_optional {name; for_sure_no_nested_option}, Obj_optional p
                 ->
                 name = p.name
                 && (Obj.magic for_sure_no_nested_option : int)
                    <= Obj.magic p.for_sure_no_nested_option
               | _ -> false))
      | Ffi_bs _, Ffi_bs _ -> false
      | _ -> false
let inline_string_primitive (s : string) (op : string option) : string list =
  let lam : Lam_constant.t =
    let unicode =
      match op with
      | Some op -> Ast_utf8_string_interp.is_unicode_string op
      | None -> false
    in
    Const_string {s; unicode}
  in
  [""; to_string (Ffi_inline_const lam)]

(* Let's only do it for string ATM
    for boolean, and ints, a good optimizer should
    do it by default?
    But it may not work after layers of indirection
    e.g, submodule
*)
let inline_bool_primitive b : string list =
  let lam : Lam_constant.t =
    if b then Lam_constant.Const_js_true else Lam_constant.Const_js_false
  in
  [""; to_string (Ffi_inline_const lam)]

(* FIXME: check overflow ?*)
let inline_int_primitive (i : int32) : string list =
  [""; to_string (Ffi_inline_const (Const_int {i; comment = None}))]

let inline_bigint_primitive (i : string) : string list =
  let sign, i = Bigint_utils.parse_bigint i in
  [""; to_string (Ffi_inline_const (Const_bigint (sign, i)))]

let inline_float_primitive (i : string) : string list =
  [""; to_string (Ffi_inline_const (Const_float i))]
let rec ffi_bs_aux acc (params : External_arg_spec.params) =
  match params with
  | {arg_type = Nothing; arg_label = Arg_empty}
      (* same as External_arg_spec.dummy*)
    :: rest ->
    ffi_bs_aux (acc + 1) rest
  | _ :: _ -> -1
  | [] -> acc

let ffi_bs (params : External_arg_spec.params) return attr =
  let n = ffi_bs_aux 0 params in
  if n < 0 then Ffi_bs (Params params, return, attr)
  else Ffi_bs (Param_number n, return, attr)

let ffi_bs_as_prims params return attr =
  [""; to_string (ffi_bs params return attr)]

let ffi_obj_create obj_params = Ffi_obj_create obj_params

let ffi_obj_as_prims obj_params = [""; to_string (Ffi_obj_create obj_params)]
