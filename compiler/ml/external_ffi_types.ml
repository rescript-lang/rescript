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
  | Const_str of {s: string; delim: External_arg_spec.delim option}
  | Const_bool of bool
  | Const_int of int32
  | Const_bigint of {negative: bool; digits: string}
  | Const_float of string

type t =
  | Ffi_bs of External_arg_spec.params * return_wrapper * external_decl
  | Ffi_obj_create of External_arg_spec.obj_params

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

(* Validation of a declaration; returns whether the binding refers to a
   package-relative path (which disables cross-module inlining). The checks
   mirror the historical per-runtime-shape checks exactly, including their
   asymmetries (a zero-arity value also checks its own name for
   relativeness; an applied one checks the module name for emptiness). *)
let check_decl ?loc (decl : external_decl) ~prim_name : bool =
  let xrelative = ref false in
  let upgrade bool = if not !xrelative then xrelative := bool in
  (match decl.module_ with
  | Some Module_itself ->
    upgrade (is_package_relative_path prim_name);
    if prim_name = "" then Location.raise_errorf ?loc "empty name encountered"
  | (None | Some (Module_named _)) as module_ -> (
    let emn =
      match module_ with
      | Some (Module_named emn) -> Some emn
      | _ -> None
    in
    match decl.kind with
    | Decl_val {name} ->
      if decl.effective_arity = 0 then (
        upgrade (is_package_relative_path name);
        Ext_option.iter emn (fun emn ->
            upgrade (is_package_relative_path emn.bundle));
        valid_global_name ?loc name)
      else (
        Ext_option.iter emn (fun emn ->
            upgrade (is_package_relative_path emn.bundle));
        Ext_option.iter emn (fun emn -> check_external_module_name ?loc emn);
        valid_global_name ?loc name)
    | Decl_new {name} ->
      Ext_option.iter emn (fun emn ->
          upgrade (is_package_relative_path emn.bundle));
      Ext_option.iter emn (fun emn -> check_external_module_name ?loc emn);
      valid_global_name ?loc name
    | Decl_send {name} | Decl_set {name} | Decl_get {name} ->
      valid_method_name ?loc name
    | Decl_get_index | Decl_set_index -> ()));
  !xrelative

(* Can an implementation's spec satisfy an interface's spec during signature
   inclusion? Equal specs always can. Object-creation specs compare their
   optional fields by name only: [for_sure_no_nested_option] is per-module
   codegen conservatism derived from each side's own view of the field's
   type (false is always sound), not part of the declaration, so it plays
   no role in compatibility. *)
let inclusion_compatible (impl : t) (intf : t) : bool =
  match (impl, intf) with
  | Ffi_obj_create obj_parms, Ffi_obj_create obj_parms2 ->
    Ext_list.for_all2_no_exn obj_parms obj_parms2
      (fun {obj_arg_type; obj_arg_label} b ->
        obj_arg_type = b.obj_arg_type
        &&
        match (obj_arg_label, b.obj_arg_label) with
        | ( Obj_optional {name = n1; for_sure_no_nested_option = _},
            Obj_optional {name = n2; for_sure_no_nested_option = _} ) ->
          n1 = n2
        | l1, l2 -> l1 = l2)
  | _ -> impl = intf

let ffi_bs (params : External_arg_spec.params) return attr =
  Ffi_bs (params, return, attr)

let ffi_obj_create obj_params = Ffi_obj_create obj_params
