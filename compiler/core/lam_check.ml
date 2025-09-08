(* Copyright (C) 2018 Hongbo Zhang, Authors of ReScript
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

(**
   checks
   1. variables are not bound twice
   2. all variables are of right scope
*)
let check file lam =
  let defined_variables = Hash_set_ident.create 1000 in
  let success = ref true in
  let use (id : Ident.t) =
    if not @@ Hash_set_ident.mem defined_variables id then (
      Format.fprintf Format.err_formatter
        "\n[SANITY]:%s/%d used before defined in %s@." id.name id.stamp file;
      success := false)
  in
  let def (id : Ident.t) =
    if Hash_set_ident.mem defined_variables id then (
      Format.fprintf Format.err_formatter "\n[SANITY]:%s/%d bound twice in %s@."
        id.name id.stamp file;
      success := false)
    else Hash_set_ident.add defined_variables id
  in
  (* TODO: replaced by a slow version of {!Lam_iter.inner_iter} *)
  let rec check_list xs (cxt : Set_int.t) =
    Ext_list.iter xs (fun x -> check_staticfails x cxt)
  and check_list_snd : 'a. ('a * Lam.t) list -> _ -> unit =
   fun xs cxt -> Ext_list.iter_snd xs (fun x -> check_staticfails x cxt)
  and check_staticfails (l : Lam.t) (cxt : Set_int.t) =
    match l with
    | Lvar _ | Lconst _ | Lglobal_module _ -> ()
    | Lprim {args; _} -> check_list args cxt
    | Lapply {ap_func; ap_args; _} -> check_list (ap_func :: ap_args) cxt
    (* check invariant that staticfaill does not cross function/while/for loop*)
    | Lfunction {body; params = _} -> check_staticfails body Set_int.empty
    | Lwhile (e1, e2) ->
      check_staticfails e1 cxt;
      check_staticfails e2 Set_int.empty
    | Lfor (_v, e1, e2, _dir, e3) ->
      check_staticfails e1 cxt;
      check_staticfails e2 cxt;
      check_staticfails e3 Set_int.empty
    | Llet (_str, _id, arg, body) -> check_list [arg; body] cxt
    | Lletrec (decl, body) ->
      check_list_snd decl cxt;
      check_staticfails body cxt
    | Lswitch (arg, sw) ->
      check_staticfails arg cxt;
      check_list_snd sw.sw_consts cxt;
      check_list_snd sw.sw_blocks cxt;
      Ext_option.iter sw.sw_failaction (fun x -> check_staticfails x cxt)
    | Lstringswitch (arg, cases, default) ->
      check_staticfails arg cxt;
      check_list_snd cases cxt;
      Ext_option.iter default (fun x -> check_staticfails x cxt)
    | Lstaticraise (i, args) ->
      if Set_int.mem cxt i then check_list args cxt
      else failwith ("exit " ^ string_of_int i ^ " unbound")
    | Lstaticcatch (e1, (j, _vars), e2) ->
      check_staticfails e1 (Set_int.add cxt j);
      check_staticfails e2 cxt
    | Ltrywith (e1, _exn, e2) ->
      check_staticfails e1 cxt;
      check_staticfails e2 cxt
    | Lifthenelse (e1, e2, e3) -> check_list [e1; e2; e3] cxt
    | Lsequence (e1, e2) -> check_list [e1; e2] cxt
    | Lassign (_id, e) -> check_staticfails e cxt
  in
  let rec iter_list xs = Ext_list.iter xs iter
  and iter_list_snd : 'a. ('a * Lam.t) list -> unit =
   fun xs -> Ext_list.iter_snd xs iter
  and iter (l : Lam.t) =
    match l with
    | Lvar id -> use id
    | Lglobal_module _ -> ()
    | Lprim {args; _} -> iter_list args
    | Lconst _ -> ()
    | Lapply {ap_func; ap_args; _} ->
      iter ap_func;
      iter_list ap_args
    | Lfunction {body; params} ->
      List.iter def params;
      iter body
    | Llet (_str, id, arg, body) ->
      iter arg;
      def id;
      iter body
    | Lletrec (decl, body) ->
      Ext_list.iter_fst decl def;
      iter_list_snd decl;
      iter body
    | Lswitch (arg, sw) ->
      iter arg;
      iter_list_snd sw.sw_consts;
      iter_list_snd sw.sw_blocks;
      Ext_option.iter sw.sw_failaction iter;
      assert (
        not (sw.sw_failaction <> None && sw.sw_consts_full && sw.sw_blocks_full))
    | Lstringswitch (arg, cases, default) ->
      iter arg;
      iter_list_snd cases;
      Ext_option.iter default iter
    | Lstaticraise (_i, args) -> iter_list args
    | Lstaticcatch (e1, (_, vars), e2) ->
      iter e1;
      List.iter def vars;
      iter e2
    | Ltrywith (e1, exn, e2) ->
      iter e1;
      def exn;
      iter e2
    | Lifthenelse (e1, e2, e3) ->
      iter e1;
      iter e2;
      iter e3
    | Lsequence (e1, e2) ->
      iter e1;
      iter e2
    | Lwhile (e1, e2) ->
      iter e1;
      iter e2
    | Lfor (v, e1, e2, _dir, e3) ->
      iter e1;
      iter e2;
      def v;
      iter e3
    | Lassign (id, e) ->
      use id;
      iter e
  in
  check_staticfails lam Set_int.empty;
  iter lam;
  assert !success;
  lam
