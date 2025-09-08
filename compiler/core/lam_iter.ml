(* Copyright (C) 2018 - Hongbo Zhang, Authors of ReScript
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

type t = Lam.t

type ident = Ident.t

let inner_iter (l : t) (f : t -> unit) : unit =
  match l with
  | Lvar (_ : ident) | Lconst (_ : Lam_constant.t) -> ()
  | Lapply {ap_func; ap_args; ap_info = _} ->
    f ap_func;
    List.iter f ap_args
  | Lfunction {body; arity = _; params = _} -> f body
  | Llet (_str, _id, arg, body) ->
    f arg;
    f body
  | Lletrec (decl, body) ->
    f body;
    Ext_list.iter_snd decl f
  | Lswitch
      ( arg,
        {
          sw_consts;
          sw_consts_full = _;
          sw_blocks;
          sw_blocks_full = _;
          sw_failaction;
        } ) ->
    f arg;
    Ext_list.iter_snd sw_consts f;
    Ext_list.iter_snd sw_blocks f;
    Ext_option.iter sw_failaction f
  | Lstringswitch (arg, cases, default) ->
    f arg;
    Ext_list.iter_snd cases f;
    Ext_option.iter default f
  | Lglobal_module _ -> ()
  | Lprim {args; primitive = _; loc = _} -> List.iter f args
  | Lstaticraise (_id, args) -> List.iter f args
  | Lstaticcatch (e1, _vars, e2) ->
    f e1;
    f e2
  | Ltrywith (e1, _exn, e2) ->
    f e1;
    f e2
  | Lifthenelse (e1, e2, e3) ->
    f e1;
    f e2;
    f e3
  | Lsequence (e1, e2) ->
    f e1;
    f e2
  | Lwhile (e1, e2) ->
    f e1;
    f e2
  | Lfor (_v, e1, e2, _dir, e3) ->
    f e1;
    f e2;
    f e3
  | Lassign (_id, e) -> f e

let inner_exists (l : t) (f : t -> bool) : bool =
  match l with
  | Lvar (_ : ident) | Lglobal_module _ | Lconst (_ : Lam_constant.t) -> false
  | Lapply {ap_func; ap_args; ap_info = _} ->
    f ap_func || Ext_list.exists ap_args f
  | Lfunction {body; arity = _; params = _} -> f body
  | Llet (_str, _id, arg, body) -> f arg || f body
  | Lletrec (decl, body) -> f body || Ext_list.exists_snd decl f
  | Lswitch
      ( arg,
        {
          sw_consts;
          sw_consts_full = _;
          sw_blocks;
          sw_blocks_full = _;
          sw_failaction;
        } ) ->
    f arg
    || Ext_list.exists_snd sw_consts f
    || Ext_list.exists_snd sw_blocks f
    || Ext_option.exists sw_failaction f
  | Lstringswitch (arg, cases, default) ->
    f arg || Ext_list.exists_snd cases f || Ext_option.exists default f
  | Lprim {args; primitive = _; loc = _} -> Ext_list.exists args f
  | Lstaticraise (_id, args) -> Ext_list.exists args f
  | Lstaticcatch (e1, _vars, e2) -> f e1 || f e2
  | Ltrywith (e1, _exn, e2) -> f e1 || f e2
  | Lifthenelse (e1, e2, e3) -> f e1 || f e2 || f e3
  | Lsequence (e1, e2) -> f e1 || f e2
  | Lwhile (e1, e2) -> f e1 || f e2
  | Lfor (_v, e1, e2, _dir, e3) -> f e1 || f e2 || f e3
  | Lassign (_id, e) -> f e
