(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type jbl_label = int

module HandlerMap = Map_int

type value = {exit_id: Ident.t; bindings: Ident.t list; order_id: int}

(* delegate to the callee to generate expression
      Invariant: [output] should return a trailing expression
*)
type return_label = {
  id: Ident.t;
  params: Ident.t list;
  immutable_mask: bool array;
  mutable new_params: Ident.t Map_ident.t;
  mutable triggered: bool;
}

type tail = {label: return_label option; in_staticcatch: bool}

type maybe_tail = Tail_in_try | Tail_with_name of tail

type tail_type = Not_tail | Maybe_tail_is_return of maybe_tail

(* Note [return] does indicate it is a tail position in most cases
   however, in an exception handler, return may not be in tail position
   to fix #1701 we play a trick that (Maybe_tail_is_return None)
   would never trigger tailcall, however, it preserves [return]
   semantics
*)
(* have a mutable field to notifiy it's actually triggered *)
(* anonoymous function does not have identifier *)

type let_kind = Lam_compat.let_kind

type continuation =
  | EffectCall of tail_type
  | NeedValue of tail_type
  | Declare of let_kind * J.ident (* bound value *)
  | Assign of J.ident
(* when use [Assign], var is not needed, since it's already declared  *)

type jmp_table = value HandlerMap.t

let continuation_is_return (x : continuation) =
  match x with
  | EffectCall (Maybe_tail_is_return _) | NeedValue (Maybe_tail_is_return _) ->
    true
  | EffectCall Not_tail | NeedValue Not_tail | Declare _ | Assign _ -> false

type t = {continuation: continuation; jmp_table: jmp_table; meta: Lam_stats.t}

let empty_handler_map = HandlerMap.empty

type handler = {label: jbl_label; handler: Lam.t; bindings: Ident.t list}

let no_static_raise_in_handler (x : handler) : bool =
  not (Lam_exit_code.has_exit_code x.handler (fun _code -> true))

(* always keep key id positive, specifically no [0] generated
   return a tuple
   [tbl, handlers]
   [tbl] is used for compiling [staticraise]
   [handlers] is used for compiling [staticcatch]
*)
let add_jmps (m : jmp_table) (exit_id : Ident.t) (code_table : handler list) :
    jmp_table * (int * Lam.t) list =
  let map, handlers =
    Ext_list.fold_left_with_offset code_table (m, [])
      (HandlerMap.cardinal m + 1)
      (fun {label; handler; bindings} (acc, handlers) order_id ->
        ( HandlerMap.add acc label {exit_id; bindings; order_id},
          (order_id, handler) :: handlers ))
  in
  (map, List.rev handlers)

let add_pseudo_jmp (m : jmp_table)
    (exit_id : Ident.t) (* TODO not needed, remove it later *)
    (code_table : handler) : jmp_table * Lam.t =
  ( HandlerMap.add m code_table.label
      {exit_id; bindings = code_table.bindings; order_id = -1},
    code_table.handler )

let find_exn cxt i = Map_int.find_exn cxt.jmp_table i
