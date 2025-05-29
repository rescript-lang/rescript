(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Pident of Ident.t | Pdot of t * string * int | Papply of t * t

let nopos = -1

let rec same p1 p2 =
  match (p1, p2) with
  | Pident id1, Pident id2 -> Ident.same id1 id2
  | Pdot (p1, s1, _pos1), Pdot (p2, s2, _pos2) -> s1 = s2 && same p1 p2
  | Papply (fun1, arg1), Papply (fun2, arg2) -> same fun1 fun2 && same arg1 arg2
  | _, _ -> false

let rec compare p1 p2 =
  match (p1, p2) with
  | Pident id1, Pident id2 -> Ident.compare id1 id2
  | Pdot (p1, s1, _pos1), Pdot (p2, s2, _pos2) ->
    let h = compare p1 p2 in
    if h <> 0 then h else String.compare s1 s2
  | Papply (fun1, arg1), Papply (fun2, arg2) ->
    let h = compare fun1 fun2 in
    if h <> 0 then h else compare arg1 arg2
  | (Pident _ | Pdot _), (Pdot _ | Papply _) -> -1
  | (Pdot _ | Papply _), (Pident _ | Pdot _) -> 1

let rec isfree id = function
  | Pident id' -> Ident.same id id'
  | Pdot (p, _s, _pos) -> isfree id p
  | Papply (p1, p2) -> isfree id p1 || isfree id p2

let rec binding_time = function
  | Pident id -> Ident.binding_time id
  | Pdot (p, _s, _pos) -> binding_time p
  | Papply (p1, p2) ->
    Ext_pervasives.max_int (binding_time p1) (binding_time p2)

let kfalse _ = false

let rec name ?(paren = kfalse) = function
  | Pident id -> Ident.name id
  | Pdot (p, s, _pos) ->
    name ~paren p ^ if paren s then ".( " ^ s ^ " )" else "." ^ s
  | Papply (p1, p2) -> name ~paren p1 ^ "(" ^ name ~paren p2 ^ ")"

let rec head = function
  | Pident id -> id
  | Pdot (p, _s, _pos) -> head p
  | Papply _ -> assert false

let flatten =
  let rec flatten acc = function
    | Pident id -> `Ok (id, acc)
    | Pdot (p, s, _) -> flatten (s :: acc) p
    | Papply _ -> `Contains_apply
  in
  fun t -> flatten [] t

let heads p =
  let rec heads p acc =
    match p with
    | Pident id -> id :: acc
    | Pdot (p, _s, _pos) -> heads p acc
    | Papply (p1, p2) -> heads p1 (heads p2 acc)
  in
  heads p []

let rec last = function
  | Pident id -> Ident.name id
  | Pdot (_, s, _) -> s
  | Papply (_, p) -> last p

let is_uident s =
  assert (s <> "");
  match s.[0] with
  | 'A' .. 'Z' -> true
  | _ -> false

type typath =
  | Regular of t
  | Ext of t * string
  | LocalExt of Ident.t
  | Cstr of t * string

let constructor_typath = function
  | Pident id when is_uident (Ident.name id) -> LocalExt id
  | Pdot (ty_path, s, _) when is_uident s ->
    if is_uident (last ty_path) then Ext (ty_path, s) else Cstr (ty_path, s)
  | p -> Regular p

let is_constructor_typath p =
  match constructor_typath p with
  | Regular _ -> false
  | _ -> true
