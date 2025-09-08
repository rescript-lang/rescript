(* Copyright (C) 2015 - Hongbo Zhang, Authors of ReScript
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

let hit_variables (fv : Set_ident.t) (l : t) : bool =
  let rec hit_opt (x : t option) =
    match x with
    | None -> false
    | Some a -> hit a
  and hit_var (id : Ident.t) = Set_ident.mem fv id
  and hit_list_snd : 'a. ('a * t) list -> bool =
   fun x -> Ext_list.exists_snd x hit
  and hit_list xs = Ext_list.exists xs hit
  and hit (l : t) =
    match (l : t) with
    | Lvar id -> hit_var id
    | Lassign (id, e) -> hit_var id || hit e
    | Lstaticcatch (e1, (_, _vars), e2) -> hit e1 || hit e2
    | Ltrywith (e1, _exn, e2) -> hit e1 || hit e2
    | Lfunction {body; params = _} -> hit body
    | Llet (_str, _id, arg, body) -> hit arg || hit body
    | Lletrec (decl, body) -> hit body || hit_list_snd decl
    | Lfor (_v, e1, e2, _dir, e3) -> hit e1 || hit e2 || hit e3
    | Lconst _ -> false
    | Lapply {ap_func; ap_args; _} -> hit ap_func || hit_list ap_args
    | Lglobal_module _ (* global persistent module, play safe *) -> false
    | Lprim {args; _} -> hit_list args
    | Lswitch (arg, sw) ->
      hit arg || hit_list_snd sw.sw_consts || hit_list_snd sw.sw_blocks
      || hit_opt sw.sw_failaction
    | Lstringswitch (arg, cases, default) ->
      hit arg || hit_list_snd cases || hit_opt default
    | Lstaticraise (_, args) -> hit_list args
    | Lifthenelse (e1, e2, e3) -> hit e1 || hit e2 || hit e3
    | Lsequence (e1, e2) -> hit e1 || hit e2
    | Lwhile (e1, e2) -> hit e1 || hit e2
  in
  hit l

let hit_variable (fv : Ident.t) (l : t) : bool =
  let rec hit_opt (x : t option) =
    match x with
    | None -> false
    | Some a -> hit a
  and hit_var (id : Ident.t) = Ident.same id fv
  and hit_list_snd : 'a. ('a * t) list -> bool =
   fun x -> Ext_list.exists_snd x hit
  and hit_list xs = Ext_list.exists xs hit
  and hit (l : t) =
    match (l : t) with
    | Lvar id -> hit_var id
    | Lassign (id, e) -> hit_var id || hit e
    | Lstaticcatch (e1, (_, _vars), e2) -> hit e1 || hit e2
    | Ltrywith (e1, _exn, e2) -> hit e1 || hit e2
    | Lfunction {body; params = _} -> hit body
    | Llet (_str, _id, arg, body) -> hit arg || hit body
    | Lletrec (decl, body) -> hit body || hit_list_snd decl
    | Lfor (_v, e1, e2, _dir, e3) -> hit e1 || hit e2 || hit e3
    | Lconst _ -> false
    | Lapply {ap_func; ap_args; _} -> hit ap_func || hit_list ap_args
    | Lglobal_module _ (* global persistent module, play safe *) -> false
    | Lprim {args; _} -> hit_list args
    | Lswitch (arg, sw) ->
      hit arg || hit_list_snd sw.sw_consts || hit_list_snd sw.sw_blocks
      || hit_opt sw.sw_failaction
    | Lstringswitch (arg, cases, default) ->
      hit arg || hit_list_snd cases || hit_opt default
    | Lstaticraise (_, args) -> hit_list args
    | Lifthenelse (e1, e2, e3) -> hit e1 || hit e2 || hit e3
    | Lsequence (e1, e2) -> hit e1 || hit e2
    | Lwhile (e1, e2) -> hit e1 || hit e2
  in
  hit l
