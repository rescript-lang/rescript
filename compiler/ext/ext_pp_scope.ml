(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type t = int Map_int.t Map_string.t

(*
   -- "name" --> int map -- stamp --> index suffix
*)
let empty : t = Map_string.empty

let rec print fmt v =
  Format.fprintf fmt "@[<v>{";
  Map_string.iter v (fun k m ->
      Format.fprintf fmt "%s: @[%a@],@ " k print_int_map m);
  Format.fprintf fmt "}@]"

and print_int_map fmt m =
  Map_int.iter m (fun k v -> Format.fprintf fmt "%d - %d" k v)

let add_ident ~mangled:name (stamp : int) (cxt : t) : int * t =
  match Map_string.find_opt cxt name with
  | None -> (0, Map_string.add cxt name (Map_int.add Map_int.empty stamp 0))
  | Some imap -> (
    match Map_int.find_opt imap stamp with
    | None ->
      let v = Map_int.cardinal imap in
      (v, Map_string.add cxt name (Map_int.add imap stamp v))
    | Some i -> (i, cxt))

(**
   same as {!Js_dump.ident} except it generates a string instead of doing the printing
   For fast/debug mode, we can generate the name as 
       [Printf.sprintf "%s$%d" name id.stamp] which is 
       not relevant to the context       

   Attention: 
   - $$Array.length, due to the fact that global module is 
       always printed in the begining(via imports), so you get a gurantee, 
       (global modules will not be printed as [List$1]) 

       However, this means we loose the ability of dynamic loading, is it a big 
       deal? we can fix this by a scanning first, since we already know which 
       modules are global

       check [test/test_global_print.ml] for regression
   - collision
      It is obvious that for the same identifier that they 
      print the same name.

      It also needs to be hold that for two different identifiers,  
      they print different names:
   - This happens when they escape to the same name and 
        share the  same stamp
      So the key has to be mangled name  + stamp
      otherwise, if two identifier happens to have same mangled name,
      if we use the original name as key, they can have same id (like 0).
      then it caused a collision

      Here we can guarantee that if mangled name and stamp are not all the same
      they can not have a collision

*)
let str_of_ident (cxt : t) (id : Ident.t) : string * t =
  if Ext_ident.is_js id then (* reserved by compiler *)
    (id.name, cxt)
  else
    let id_name = id.name in
    let name = Ext_ident.convert id_name in
    let i, new_cxt = add_ident ~mangled:name id.stamp cxt in
    ((if i == 0 then name else Printf.sprintf "%s$%d" name i), new_cxt)

let ident (cxt : t) f (id : Ident.t) : t =
  let str, cxt = str_of_ident cxt id in
  Ext_pp.string f str;
  cxt

let merge (cxt : t) (set : Set_ident.t) =
  Set_ident.fold set cxt (fun ident acc ->
      snd (add_ident ~mangled:(Ext_ident.convert ident.name) ident.stamp acc))

(* Assume that all idents are already in [scope]
   so both [param/0] and [param/1] are in idents, we don't need
   update twice,  once is enough
*)
let sub_scope (scope : t) (idents : Set_ident.t) : t =
  Set_ident.fold idents empty (fun {name} acc ->
      let mangled = Ext_ident.convert name in
      match Map_string.find_exn scope mangled with
      | exception Not_found -> assert false
      | imap ->
        if Map_string.mem acc mangled then acc
        else Map_string.add acc mangled imap)
