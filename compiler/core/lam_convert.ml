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

(** Global modules a unit depends on. Convert used to drop [Lglobal_module]
    references and have them added back by module analysis (see #3852); they
    are collected here instead, from the Lambda term directly. *)
let required_modules (lam : Lambda.lambda) : Lam_module_ident.Hash_set.t =
  let required = Lam_module_ident.Hash_set.create 0 in
  let rec collect (lam : Lambda.lambda) =
    (match lam with
    | Lglobal_module id ->
      Lam_module_ident.Hash_set.add required (Lam_module_ident.of_ml id)
    | _ -> ());
    Lambda.iter collect lam
  in
  collect lam;
  required

let convert (lam : Lambda.lambda) : Lam.t =
  let rec convert_aux (lam : Lambda.lambda) : Lam.t =
    match lam with
    | Lvar x -> Lam.var x
    | Lconst x -> Lam.const x
    | Lapply {ap_func = fn; ap_args = args; ap_info; ap_transformed_jsx} ->
      (* we need do this eargly in case [aux fn] add some wrapper *)
      Lam.apply (convert_aux fn)
        (Ext_list.map args convert_aux)
        ap_info ~ap_transformed_jsx
    | Lfunction {params; body; attr; loc} ->
      Lam.function_ ~loc ~attr ~params ~body:(convert_aux body)
    | Llet (kind, id, e, body) ->
      Lam.let_ kind id (convert_aux e) (convert_aux body)
    | Lletrec (bindings, body) ->
      Lam.letrec (Ext_list.map_snd bindings convert_aux) (convert_aux body)
    | Lglobal_module id -> Lam.global_module id
    | Lprim {primitive; args; loc} ->
      (* [Lam_primitive.t] is [Lambda.primitive]: nothing to translate. *)
      Lam.prim ~primitive ~args:(Ext_list.map args convert_aux) loc
    | Lswitch (e, s) -> convert_switch e s
    | Lstringswitch (e, cases, default) ->
      Lam.stringswitch (convert_aux e)
        (Ext_list.map_snd cases convert_aux)
        (Ext_option.map default convert_aux)
    | Lstaticraise (id, args) ->
      Lam.staticraise id (Ext_list.map args convert_aux)
    | Lstaticcatch (b, (i, ids), handler) ->
      Lam.staticcatch (convert_aux b) (i, ids) (convert_aux handler)
    | Ltrywith (b, id, handler) ->
      Lam.try_ (convert_aux b) id (convert_aux handler)
    | Lifthenelse (b, then_, else_) ->
      Lam.if_ (convert_aux b) (convert_aux then_) (convert_aux else_)
    | Lsequence (a, b) -> Lam.seq (convert_aux a) (convert_aux b)
    | Lbreak -> Lam.break
    | Lcontinue -> Lam.continue
    | Lwhile (b, body) -> Lam.while_ (convert_aux b) (convert_aux body)
    | Lfor (id, from_, to_, dir, loop) ->
      Lam.for_ id (convert_aux from_) (convert_aux to_) dir (convert_aux loop)
    | Lfor_of (id, iterable, body) ->
      Lam.for_of id (convert_aux iterable) (convert_aux body)
    | Lfor_await_of (id, iterable, body) ->
      Lam.for_await_of id (convert_aux iterable) (convert_aux body)
    | Lassign (id, body) -> Lam.assign id (convert_aux body)
  and convert_switch (e : Lambda.lambda) (s : Lambda.lambda_switch) =
    Lam.switch (convert_aux e)
      {
        sw_consts_full = s.sw_consts_full;
        sw_consts = Ext_list.map_snd s.sw_consts convert_aux;
        sw_blocks_full = s.sw_blocks_full;
        sw_blocks = Ext_list.map_snd s.sw_blocks convert_aux;
        sw_failaction = Ext_option.map s.sw_failaction convert_aux;
        sw_dispatch = s.sw_dispatch;
      }
  in
  convert_aux lam
