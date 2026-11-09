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

val ocaml_to_js :
  arg_label:External_arg_spec.label_noname ->
  arg_type:External_arg_spec.attr ->
  J.expression ->
  Js_of_lam_variant.arg_expression
(** The JS arguments that an argument of an external, prepared by
    [hold_arg], is passed as *)

val hold_arg :
  arg_type:External_arg_spec.attr ->
  J.block * J.expression ->
  J.block * J.expression
(** Prepares a compiled argument and its statements for [ocaml_to_js], so
    that it is evaluated once and in its place among the arguments. An
    event-style polymorphic variant argument, which [ocaml_to_js] reads
    twice, is bound to a temporary after the statements unless reading it
    twice is the same as reading it once. The effects of a [unit] or
    [@ignore] argument, which [ocaml_to_js] leaves out, become a statement
    of the argument. *)

val hold_args :
  External_ffi_types.external_decl ->
  External_arg_spec.params ->
  (J.block * J.expression) list ->
  (J.block * J.expression) list
(** [hold_arg] for the compiled arguments of a call to an external, but for
    the object of a [@send] call, which [translate_ffi] uses as it is *)

val translate_ffi :
  Lam_compile_context.t ->
  External_arg_spec.params ->
  prim_name:string ->
  External_ffi_types.external_decl ->
  J.expression list ->
  J.expression

(** TODO: document supported attributes
    Attributes starting with `js` are reserved
    examples: "variadic"
*)

val jsx_callee : External_ffi_types.external_decl -> J.expression
(** The function a JSX external refers to, for [J.Jsx] *)
