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

let pass_free_variables (l : Lam.t) : Set_ident.t =
  let fv = ref Set_ident.empty in
  let rec free_list xs = List.iter free xs
  and free_list_snd : 'a. ('a * Lam.t) list -> unit =
   fun xs -> Ext_list.iter_snd xs free
  and free (l : Lam.t) =
    match l with
    | Lvar id -> fv := Set_ident.add !fv id
    | Lassign (id, e) ->
      free e;
      fv := Set_ident.add !fv id
    | Lstaticcatch (e1, (_, vars), e2) ->
      free e1;
      free e2;
      Ext_list.iter vars (fun id -> fv := Set_ident.remove !fv id)
    | Ltrywith (e1, exn, e2) ->
      free e1;
      free e2;
      fv := Set_ident.remove !fv exn
    | Lfunction {body; params} ->
      free body;
      Ext_list.iter params (fun param -> fv := Set_ident.remove !fv param)
    | Llet (_str, id, arg, body) ->
      free arg;
      free body;
      fv := Set_ident.remove !fv id
    | Lletrec (decl, body) ->
      free body;
      free_list_snd decl;
      Ext_list.iter decl (fun (id, _exp) -> fv := Set_ident.remove !fv id)
    | Lfor (v, e1, e2, _dir, e3) ->
      free e1;
      free e2;
      free e3;
      fv := Set_ident.remove !fv v
    | Lconst _ -> ()
    | Lapply {ap_func; ap_args; _} ->
      free ap_func;
      free_list ap_args
    | Lglobal_module _ -> ()
    (* according to the existing semantics:
       [primitive] is not counted
    *)
    | Lprim {args; _} -> free_list args
    | Lswitch (arg, sw) ->
      free arg;
      free_list_snd sw.sw_consts;
      free_list_snd sw.sw_blocks;
      Ext_option.iter sw.sw_failaction free
    | Lstringswitch (arg, cases, default) ->
      free arg;
      free_list_snd cases;
      Ext_option.iter default free
    | Lstaticraise (_, args) -> free_list args
    | Lifthenelse (e1, e2, e3) ->
      free e1;
      free e2;
      free e3
    | Lsequence (e1, e2) ->
      free e1;
      free e2
    | Lwhile (e1, e2) ->
      free e1;
      free e2
  in
  free l;
  !fv

(**
        [hit_any_variables fv l]
        check the lambda expression [l] if has some free
        variables captured by [fv].
        Note it does not do any checking like below
        [Llet(str,id,arg,body)]
        it only check [arg] or [body] is hit or not, there
        is a case that [id] is hit in [arg] but also exists
        in [fv], this is ignored.
*)
