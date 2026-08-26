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
  | Return_null_to_opt
  | Return_null_undefined_to_opt
  | Return_replaced_with_unit

type params = Params of External_arg_spec.params | Param_number of int

(* An external declared as an inline constant is only ever a literal; the
   frontend parses delimiters and bigint signs before constructing this. *)
type inline_const =
  | Const_str of {s: string; delim: External_arg_spec.delim option}
  | Const_bool of bool
  | Const_int of int32
  | Const_bigint of {negative: bool; digits: string}
  | Const_float of string

type t =
  | Ffi_bs of params * return_wrapper * external_spec
      (**  [Ffi_bs(args,return,attr) ]
       [return] means return value is unit or not,
        [true] means is [unit]
  *)
  | Ffi_obj_create of External_arg_spec.obj_params
  | Ffi_inline_const of inline_const

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
  | Js_call {external_module_name; name; splice = _; scopes = _} ->
    Ext_option.iter external_module_name (fun external_module_name ->
        upgrade (is_package_relative_path external_module_name.bundle));
    Ext_option.iter external_module_name (fun name ->
        check_external_module_name ?loc name);

    valid_global_name ?loc name);
  !xrelative

(* Can an implementation's spec satisfy an interface's spec during signature
   inclusion? Equal specs always can. Object-creation specs additionally
   accept a widening of an optional field's [for_sure_no_nested_option]: the
   implementation may be unsure (false) where the interface is sure (true),
   but never the reverse. *)
let inclusion_compatible (impl : t) (intf : t) : bool =
  match (impl, intf) with
  | Ffi_obj_create obj_parms, Ffi_obj_create obj_parms2 ->
    Ext_list.for_all2_no_exn obj_parms obj_parms2
      (fun {obj_arg_type; obj_arg_label} b ->
        let b_obj_arg_label = b.obj_arg_label in
        obj_arg_type = b.obj_arg_type
        && (obj_arg_label = b_obj_arg_label
           ||
           match (obj_arg_label, b_obj_arg_label) with
           | Obj_optional {name; for_sure_no_nested_option}, Obj_optional p ->
             name = p.name
             && ((not for_sure_no_nested_option) || p.for_sure_no_nested_option)
           | _ -> false))
  | _ -> impl = intf

let ffi_inline_const (c : inline_const) : t = Ffi_inline_const c

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

let ffi_obj_create obj_params = Ffi_obj_create obj_params
