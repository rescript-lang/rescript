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

open Lambda

type bindings = (Ident.t * Lambda.t) list

(* [p] may have side effects (masking). Returning true stops the walk. *)
let exists_var (p : Ident.t -> bool) (l : Lambda.t) : bool =
  let rec hit_opt = function
    | None -> false
    | Some a -> hit a
  and hit_list_snd : 'a. ('a * Lambda.t) list -> bool =
   fun x -> Ext_list.exists_snd x hit
  and hit_list xs = Ext_list.exists xs hit
  and hit (l : Lambda.t) =
    match l with
    | Lvar id -> p id
    | Lassign (id, e) -> p id || hit e
    | Lstaticcatch (e1, _, e2)
    | Ltrywith (e1, _, e2)
    | Ltryfinally (e1, e2)
    | Lsequence (e1, e2)
    | Lwhile (e1, e2)
    | Lfor_of (_, e1, e2)
    | Lfor_await_of (_, e1, e2) ->
      hit e1 || hit e2
    | Lfunction {body} -> hit body
    | Llet (_, _, arg, body) -> hit arg || hit body
    | Lletrec (decl, body) -> hit body || hit_list_snd decl
    | Lfor (_, e1, e2, _, e3) | Lifthenelse (e1, e2, e3) ->
      hit e1 || hit e2 || hit e3
    | Lconst _ | Lbreak | Lcontinue -> false
    | Lapply {ap_func; ap_args} -> hit ap_func || hit_list ap_args
    | Lprim {primitive = _; args; loc = _} | Lstaticraise (_, args) ->
      hit_list args
    | Lswitch (arg, sw) ->
      hit arg || hit_list_snd sw.sw_consts || hit_list_snd sw.sw_blocks
      || hit_opt sw.sw_failaction
    | Lstringswitch (arg, cases, default) ->
      hit arg || hit_list_snd cases || hit_opt default
    | Lglobal_module _ -> false
  in
  hit l

(* The bindings of a group as arrays, and for each binding the indices of
   the bindings its body refers to, in binding order. *)
let preprocess_deps (groups : bindings) :
    Ident.t array * Lambda.t array * int array array =
  let ids = Array.of_list (List.map fst groups) in
  let lams = Array.of_list (List.map snd groups) in
  let len = Array.length ids in
  let index = Hash_ident.create len in
  Array.iteri (fun i id -> Hash_ident.replace index id i) ids;
  let edges lam =
    let hit = Array.make len false in
    let remaining = ref len in
    (* stop walking [lam] once it has referred to every binding *)
    ignore
      (exists_var
         (fun id ->
           match Hash_ident.find_opt index id with
           | Some i when not hit.(i) ->
             hit.(i) <- true;
             decr remaining;
             !remaining = 0
           | Some _ | None -> false)
         lam);
    let edges = ref [] in
    for i = len - 1 downto 0 do
      if hit.(i) then edges := i :: !edges
    done;
    Array.of_list !edges
  in
  (ids, lams, Array.map edges lams)

let bind_rec (groups : bindings) (body : Lambda.t) : Lambda.t =
  match groups with
  | [(id, bind)] ->
    if exists_var (Ident.same id) bind then letrec groups body
    else let_ Strict id bind body
  | _ ->
    let ids, lams, node_edges = preprocess_deps groups in
    let clusters = Ext_scc.graph node_edges in
    if List.compare_length_with clusters 1 <= 0 then letrec groups body
    else
      List.fold_right
        (fun (cluster : int array) acc ->
          let bindings =
            List.map (fun i -> (ids.(i), lams.(i))) (Array.to_list cluster)
          in
          match cluster with
          | [|i|] ->
            if Array.mem i node_edges.(i) then letrec bindings acc
            else let_ Strict ids.(i) lams.(i) acc
          | _ -> letrec bindings acc)
        clusters body
