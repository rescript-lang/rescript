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

(** Creator utilities for the [J] module *)

type t = J.expression
(** check if a javascript ast is constant 

    The better signature might be 
    {[
      J.expresssion -> Js_output.t
    ]}
    for exmaple
    {[
      e ?print_int(3) :  0
                         --->
                         if(e){print_int(3)}
    ]}
*)

val remove_pure_sub_exp : t -> t option

val var : ?comment:string -> J.ident -> t

val js_global : ?comment:string -> string -> t

val runtime_var_dot : ?comment:string -> string -> string -> t

(* val runtime_var_vid : string -> string -> J.vident *)

val ml_var_dot :
  ?comment:string -> ?dynamic_import:bool -> Ident.t -> string -> t
(** [ml_var_dot ocaml_module name]
*)

val external_var_field :
  ?import_attributes:External_ffi_types.import_attributes ->
  ?comment:string ->
  external_name:string ->
  Ident.t ->
  field:string ->
  default:bool ->
  t
(** [external_var_field ~external_name ~dot id]
    Used in FFI
*)

val external_var :
  ?import_attributes:External_ffi_types.import_attributes ->
  ?comment:string ->
  external_name:string ->
  Ident.t ->
  t

val ml_module_as_var : ?comment:string -> ?dynamic_import:bool -> Ident.t -> t

val runtime_call :
  string ->
  (* module_name *)
  string ->
  (* fn_name *)
  t list ->
  (* args *)
  t

val pure_runtime_call :
  string ->
  (* module_name *)
  string ->
  (* fn_name *)
  t list ->
  (* args *)
  t

val runtime_ref : string -> string -> t

val str : ?delim:J.delim -> ?comment:string -> string -> t

val ocaml_fun :
  ?comment:string ->
  ?immutable_mask:bool array ->
  ?directive:string ->
  return_unit:bool ->
  async:bool ->
  one_unit_arg:bool ->
  J.ident list ->
  J.block ->
  t

val method_ :
  ?comment:string ->
  ?immutable_mask:bool array ->
  return_unit:bool ->
  J.ident list ->
  J.block ->
  t

val econd : ?comment:string -> t -> t -> t -> t

val int : ?comment:string -> ?c:int -> int32 -> t

val small_int : int -> t

val bigint : ?comment:string -> bool -> string -> t

val float : ?comment:string -> string -> t

(* val empty_string_literal : t  *)
(* TODO: we can do hash consing for small integers *)
val zero_int_literal : t

(* val one_int_literal : t *)
val zero_float_lit : t

val zero_bigint_literal : t

val is_out : ?comment:string -> t -> t -> t
(** [is_out e range] is equivalent to [e > range or e <0]

*)

val dot : ?comment:string -> t -> string -> t

val module_access : t -> string -> int32 -> t

val array_length : ?comment:string -> t -> t

val string_length : ?comment:string -> t -> t

val function_length : ?comment:string -> t -> t

val string_append : ?comment:string -> t -> t -> t
(**
   When in ES6 mode, we can use Symbol to guarantee its uniquess,
   we can not tag [js] object, since it can be frozen 
*)

(* val var_dot : ?comment:string -> Ident.t -> string -> t *)

(* val bind_var_call : ?comment:string -> Ident.t -> string -> t list -> t  *)

(* val bind_call : ?comment:string -> J.expression -> string -> J.expression list -> t *)
(* val js_global_dot : ?comment:string -> string -> string -> t *)

val string_index : ?comment:string -> t -> t -> t

val array_index : ?comment:string -> t -> t -> t

val array_index_by_int : ?comment:string -> t -> Int32.t -> t

val record_access : t -> string -> Int32.t -> t

val inline_record_access : t -> string -> Int32.t -> t

val variant_access : t -> int32 -> t

val cons_access : t -> int32 -> t

val extension_access : t -> string option -> Int32.t -> t

val record_assign : t -> int32 -> string -> t -> t

val poly_var_tag_access : t -> t

val poly_var_value_access : t -> t

val extension_assign : t -> int32 -> string -> t -> t

val assign_by_int : ?comment:string -> t -> int32 -> t -> t
(** 
    [assign_by_int  e i v]
    if the expression [e] is a temporay block 
    which has no side effect,
    write to it does not really make sense, 
    optimize it away *)

val assign_by_exp : t -> t -> t -> t

val assign : ?comment:string -> t -> t -> t

val tag_type : Ast_untagged_variants.tag_type -> t

val emit_check : t Ast_untagged_variants.DynamicChecks.t -> t

val triple_equal : ?comment:string -> t -> t -> t
(* TODO: reduce [triple_equal] use *)

val float_equal : ?comment:string -> t -> t -> t

val int_equal : ?comment:string -> t -> t -> t

val string_equal : ?comment:string -> t -> t -> t

val eq_null_undefined_boolean : ?comment:string -> t -> t -> t

val neq_null_undefined_boolean : ?comment:string -> t -> t -> t

val is_type_number : ?comment:string -> t -> t

val is_int_tag : ?has_null_undefined_other:bool * bool * bool -> t -> t

val is_a_literal_case :
  literal_cases:Ast_untagged_variants.tag_type list ->
  block_cases:Ast_untagged_variants.block_type list ->
  t ->
  t

val is_type_string : ?comment:string -> t -> t

val is_type_object : t -> t

val typeof : ?comment:string -> t -> t
val instanceof : ?comment:string -> t -> t -> t
val is_array : t -> t

val to_int32 : ?comment:string -> t -> t

val unchecked_int32_add : ?comment:string -> t -> t -> t

val int32_add : ?comment:string -> t -> t -> t

val offset : t -> int -> t

val unchecked_int32_minus : ?comment:string -> t -> t -> t

val int32_minus : ?comment:string -> t -> t -> t

val int32_mul : ?comment:string -> t -> t -> t

val unchecked_int32_mul : ?comment:string -> t -> t -> t

val int32_div : checked:bool -> ?comment:string -> t -> t -> t

val int32_mod : checked:bool -> ?comment:string -> t -> t -> t

val int32_lsl : ?comment:string -> t -> t -> t

val int32_lsr : ?comment:string -> t -> t -> t

val int32_asr : ?comment:string -> t -> t -> t

val int32_bxor : ?comment:string -> t -> t -> t

val int32_band : ?comment:string -> t -> t -> t

val int32_bor : ?comment:string -> t -> t -> t

val float_add : ?comment:string -> t -> t -> t

val float_minus : ?comment:string -> t -> t -> t

val float_mul : ?comment:string -> t -> t -> t

val float_div : ?comment:string -> t -> t -> t

val float_notequal : ?comment:string -> t -> t -> t

val float_mod : ?comment:string -> t -> t -> t

val int_comp : Lam_compat.comparison -> ?comment:string -> t -> t -> t

val bool_comp : Lam_compat.comparison -> ?comment:string -> t -> t -> t

val string_comp : Lam_compat.comparison -> ?comment:string -> t -> t -> t

val float_comp : Lam_compat.comparison -> ?comment:string -> t -> t -> t

val bigint_op : ?comment:string -> Js_op.binop -> t -> t -> t

val bigint_comp : Lam_compat.comparison -> ?comment:string -> t -> t -> t

val bigint_div : checked:bool -> ?comment:string -> t -> t -> t

val bigint_mod : checked:bool -> ?comment:string -> t -> t -> t

val js_comp : Lam_compat.comparison -> ?comment:string -> t -> t -> t

val not : t -> t

val call : ?comment:string -> info:Js_call_info.t -> t -> t list -> t

val flat_call : ?comment:string -> t -> t -> t

val tagged_template : ?comment:string -> t -> t list -> t list -> t

val new_ : ?comment:string -> J.expression -> J.expression list -> t

val array : ?comment:string -> J.mutable_flag -> J.expression list -> t

val optional_block : J.expression -> J.expression

val optional_not_nest_block : J.expression -> J.expression

val make_block :
  ?comment:string ->
  J.expression ->
  (* tag *)
  J.tag_info ->
  (* tag_info *)
  J.expression list ->
  J.mutable_flag ->
  t

val seq : ?comment:string -> t -> t -> t

val fuse_to_seq : t -> t list -> t

val obj : ?comment:string -> ?dup:J.expression -> J.property_map -> t

val true_ : t

val false_ : t

val bool : bool -> t

val unit : t
(** [unit] in ocaml will be compiled into [0]  in js *)

val undefined : t

val tag : ?comment:string -> ?name:string -> J.expression -> t

(** Note that this is coupled with how we encode block, if we use the 
    `Object.defineProperty(..)` since the array already hold the length,
    this should be a nop 
*)

val obj_length : ?comment:string -> J.expression -> t

val and_ : ?comment:string -> t -> t -> t

val or_ : ?comment:string -> t -> t -> t

(** we don't expose a general interface, since a general interface is generally not safe *)

val dummy_obj : ?comment:string -> Lam_tag_info.t -> t
(** used combined with [caml_update_dummy]*)

val of_block : ?comment:string -> ?e:J.expression -> J.statement list -> t
(** convert a block to expresion by using IIFE *)

val raw_js_code : ?comment:string -> Js_raw_info.code_info -> string -> t

val nil : t

val is_null : ?comment:string -> t -> t

val is_undef : ?comment:string -> t -> t

val is_null_undefined_constant : J.expression -> bool

val is_null_undefined : ?comment:string -> t -> t

val make_exception : string -> t

val variadic_args : t list -> t list

val string_of_expression : (t -> string) ref
