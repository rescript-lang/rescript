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

(** The identifiers that the toplevel [block] needs: the exports, everything a
    statement with side effects refers to, and transitively the free variables
    of the initializers of needed declarations. Each initializer's free
    variables are computed once, and a worklist follows the dependencies. *)
let live_idents (export_set : Set_ident.t) (block : J.block) : Set_ident.t =
  (* free variables of the initializer of each toplevel declaration *)
  let deps = Hash_ident.create 64 in
  let live = ref Set_ident.empty in
  let worklist = ref [] in
  let mark id =
    if not (Set_ident.mem !live id) then (
      live := Set_ident.add !live id;
      worklist := id :: !worklist)
  in
  Ext_list.iter block (fun (st : J.statement) ->
      match st.statement_desc with
      | Variable {ident; value = Some x; _} ->
        let fv = Js_analyzer.free_variables_of_expression x in
        let fv =
          match Hash_ident.find_opt deps ident with
          | None -> fv
          | Some other -> Set_ident.union fv other
        in
        Hash_ident.replace deps ident fv;
        if not (Js_analyzer.no_side_effect_expression x) then mark ident
      | Variable {value = None; _} -> ()
      | _ ->
        if not (Js_analyzer.no_side_effect_statement st) then
          Set_ident.iter (Js_analyzer.free_variables_of_statement st) mark);
  Set_ident.iter export_set mark;
  let rec drain () =
    match !worklist with
    | [] -> ()
    | id :: rest ->
      worklist := rest;
      (match Hash_ident.find_opt deps id with
      | Some fv -> Set_ident.iter fv mark
      | None -> ());
      drain ()
  in
  drain ();
  !live

let shake_program (program : J.program) =
  let shake_block block export_set =
    let block = List.rev @@ Js_analyzer.rev_toplevel_flatten block in
    let really_set = live_idents export_set block in
    Ext_list.fold_right block [] (fun (st : J.statement) acc ->
        match st.statement_desc with
        | Variable {ident; value; _} -> (
          if Set_ident.mem really_set ident then st :: acc
          else
            match value with
            | None -> acc
            | Some x ->
              if Js_analyzer.no_side_effect_expression x then acc else st :: acc
          )
        | _ ->
          if Js_analyzer.no_side_effect_statement st then acc else st :: acc)
  in

  {program with block = shake_block program.block program.export_set}
