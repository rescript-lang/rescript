(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

(* Apply a substitution to a lambda-term.
   Assumes that the bound variables of the lambda-term do not
   belong to the domain of the substitution.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst (s : Lam.t Map_ident.t) lam =
  let rec subst_aux (x : Lam.t) : Lam.t =
    match x with
    | Lvar id -> Map_ident.find_default s id x
    | Lconst _ -> x
    | Lapply {ap_func; ap_args; ap_info} ->
      Lam.apply (subst_aux ap_func) (Ext_list.map ap_args subst_aux) ap_info
    | Lfunction {arity; params; body; attr} ->
      Lam.function_ ~arity ~params ~body:(subst_aux body) ~attr
    | Llet (str, id, arg, body) ->
      Lam.let_ str id (subst_aux arg) (subst_aux body)
    | Lletrec (decl, body) ->
      Lam.letrec (Ext_list.map decl subst_decl) (subst_aux body)
    | Lprim {primitive; args; loc} ->
      Lam.prim ~primitive ~args:(Ext_list.map args subst_aux) loc
    | Lglobal_module _ -> x
    | Lswitch (arg, sw) ->
      Lam.switch (subst_aux arg)
        {
          sw with
          sw_consts = Ext_list.map sw.sw_consts subst_case;
          sw_blocks = Ext_list.map sw.sw_blocks subst_case;
          sw_failaction = subst_opt sw.sw_failaction;
        }
    | Lstringswitch (arg, cases, default) ->
      Lam.stringswitch (subst_aux arg)
        (Ext_list.map cases subst_strcase)
        (subst_opt default)
    | Lstaticraise (i, args) -> Lam.staticraise i (Ext_list.map args subst_aux)
    | Lstaticcatch (e1, io, e2) ->
      Lam.staticcatch (subst_aux e1) io (subst_aux e2)
    | Ltrywith (e1, exn, e2) -> Lam.try_ (subst_aux e1) exn (subst_aux e2)
    | Lifthenelse (e1, e2, e3) ->
      Lam.if_ (subst_aux e1) (subst_aux e2) (subst_aux e3)
    | Lsequence (e1, e2) -> Lam.seq (subst_aux e1) (subst_aux e2)
    | Lwhile (e1, e2) -> Lam.while_ (subst_aux e1) (subst_aux e2)
    | Lfor (v, e1, e2, dir, e3) ->
      Lam.for_ v (subst_aux e1) (subst_aux e2) dir (subst_aux e3)
    | Lassign (id, e) -> Lam.assign id (subst_aux e)
  and subst_decl (id, exp) = (id, subst_aux exp)
  and subst_case (key, case) = (key, subst_aux case)
  and subst_strcase (key, case) = (key, subst_aux case)
  and subst_opt = function
    | None -> None
    | Some e -> Some (subst_aux e)
  in
  subst_aux lam
