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

type apply_status = App_na | App_infer_full | App_uncurry

type ap_info = {
  ap_loc: Location.t;
  ap_inlined: Lambda.inline_attribute;
  ap_status: apply_status;
}

type ident = Ident.t

type lambda_switch = {
  sw_consts_full: bool;
  sw_consts: (int * t) list;
  sw_blocks_full: bool;
  sw_blocks: (int * t) list;
  sw_failaction: t option;
  sw_names: Ast_untagged_variants.switch_names option;
}

and apply = private {
  ap_func: t;
  ap_args: t list;
  ap_info: ap_info;
  ap_transformed_jsx: bool;
}

and lfunction = {
  arity: int;
  params: ident list;
  body: t;
  attr: Lambda.function_attribute;
}

and prim_info = private {
  primitive: Lam_primitive.t;
  args: t list;
  loc: Location.t;
}

and t = private
  | Lvar of ident
  | Lglobal_module of ident * bool
  | Lconst of Lam_constant.t
  | Lapply of apply
  | Lfunction of lfunction
  | Llet of Lam_compat.let_kind * ident * t * t
  | Lletrec of (ident * t) list * t
  | Lprim of prim_info
  | Lswitch of t * lambda_switch
  | Lstringswitch of t * (string * t) list * t option
  | Lstaticraise of int * t list
  | Lstaticcatch of t * (int * ident list) * t
  | Ltrywith of t * ident * t
  | Lifthenelse of t * t * t
  | Lsequence of t * t
  | Lwhile of t * t
  | Lfor of ident * t * t * Asttypes.direction_flag * t
  | Lassign of ident * t

(* | Lsend of Lambda.meth_kind * t * t * t list * Location.t *)
(* | Levent of t * Lambda.lambda_event
   [Levent] in the branch hurt pattern match,
   we should use record for trivial debugger info
*)

val inner_map : t -> (t -> t) -> t

val handle_bs_non_obj_ffi :
  ?transformed_jsx:bool ->
  External_arg_spec.params ->
  External_ffi_types.return_wrapper ->
  External_ffi_types.external_spec ->
  t list ->
  Location.t ->
  string ->
  dynamic_import:bool ->
  t

(**************************************************************)

val var : ident -> t
(** Smart constructors *)

val global_module : ?dynamic_import:bool -> ident -> t

val const : Lam_constant.t -> t

val apply : ?ap_transformed_jsx:bool -> t -> t list -> ap_info -> t

val function_ :
  attr:Lambda.function_attribute ->
  arity:int ->
  params:ident list ->
  body:t ->
  t

val let_ : Lam_compat.let_kind -> ident -> t -> t -> t

val letrec : (ident * t) list -> t -> t

val if_ : t -> t -> t -> t
(**  constant folding *)

val switch : t -> lambda_switch -> t
(** constant folding*)

val stringswitch : t -> (string * t) list -> t option -> t
(** constant folding*)

(* val true_ : t  *)
val false_ : t

val unit : t

val sequor : t -> t -> t
(** convert [l || r] to [if l then true else r]*)

val sequand : t -> t -> t
(** convert [l && r] to [if l then r else false *)

val not_ : Location.t -> t -> t
(** constant folding *)

val seq : t -> t -> t
(** drop unused block *)

val while_ : t -> t -> t

(* val event : t -> Lambda.lambda_event -> t   *)
val try_ : t -> ident -> t -> t

val assign : ident -> t -> t

val prim : primitive:Lam_primitive.t -> args:t list -> Location.t -> t
(** constant folding *)

val staticcatch : t -> int * ident list -> t -> t

val staticraise : int -> t list -> t

val for_ : ident -> t -> t -> Asttypes.direction_flag -> t -> t

(**************************************************************)

val eq_approx : t -> t -> bool
