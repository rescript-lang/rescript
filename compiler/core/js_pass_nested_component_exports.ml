(* Copyright (C) 2026 - Authors of ReScript
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

module E = Js_exp_make

module StringSet = Set.Make (String)

type candidate = {
  module_ident: Ident.t;
  component_ident: Ident.t;
  hidden_export_name: string;
}

let marker_name hidden_export_name = hidden_export_name ^ "$jsx"

let hidden_component_suffix (module_ident : Ident.t) =
  "$" ^ Ident.name module_ident

let is_hidden_component_name_for module_ident ident =
  Ext_string.ends_with (Ident.name ident) (hidden_component_suffix module_ident)

let find_hidden_alias_by_value block module_ident value_ident =
  List.find_map (fun (st : J.statement) ->
      match st.statement_desc with
      | Variable
          {
            ident;
            value = Some {expression_desc = Var (Id target); _};
            property = _;
            ident_info = _;
          }
        when Ident.same target value_ident
             && is_hidden_component_name_for module_ident ident ->
        Some ident
      | _ -> None)
    block

let has_export_name exports name =
  List.exists (fun (ident : Ident.t) -> String.equal (Ident.name ident) name) exports

let candidate_of_statement block exports (st : J.statement) =
  match st.statement_desc with
  | Variable
      {
        ident = module_ident;
        value =
          Some
            {
              expression_desc =
                Caml_block
                  ([{expression_desc = Var (Id value_ident); _}], Immutable, _, Blk_module ["make"]);
              _;
            };
        property = _;
        ident_info = _;
      } -> (
      let hidden_ident =
        if is_hidden_component_name_for module_ident value_ident then
          Some value_ident
        else find_hidden_alias_by_value block module_ident value_ident
      in
      match hidden_ident with
      | Some hidden_ident
        when has_export_name exports hidden_ident.name ->
        Some
          {
            module_ident;
            component_ident = value_ident;
            hidden_export_name = hidden_ident.name;
          }
      | Some _ | None -> None)
  | _ -> None

let collect_candidates block exports =
  Ext_list.filter_map block (candidate_of_statement block exports)

let hidden_export_names_to_remove candidates =
  Ext_list.fold_left candidates StringSet.empty (fun acc candidate ->
      acc |> StringSet.add candidate.hidden_export_name
      |> StringSet.add (marker_name candidate.hidden_export_name))

let marker_names_to_remove_from_block candidates =
  Ext_list.fold_left candidates StringSet.empty (fun acc candidate ->
      StringSet.add (marker_name candidate.hidden_export_name) acc)

let candidate_by_module_ident candidates module_ident =
  List.find_map (fun candidate ->
      if Ident.same candidate.module_ident module_ident then Some candidate
      else None)
    candidates

let rewrite_block block candidates removed_marker_names =
  List.concat_map (fun (st : J.statement) ->
      match st.statement_desc with
      | Variable {ident; value; property; ident_info} -> (
        match candidate_by_module_ident candidates ident with
        | Some candidate ->
          let module_value = E.var candidate.component_ident in
          [
            {
              st with
              statement_desc =
                Variable {ident; value = Some module_value; property; ident_info};
            };
          ]
        | None ->
          if StringSet.mem (Ident.name ident) removed_marker_names then []
          else [st])
      | _ -> [st])
    block

let program (js : J.program) : J.program =
  let candidates = collect_candidates js.block js.exports in
  match candidates with
  | [] -> js
  | _ ->
    let removed_export_names = hidden_export_names_to_remove candidates in
    let removed_marker_names = marker_names_to_remove_from_block candidates in
    let exports =
      Ext_list.filter js.exports (fun (ident : Ident.t) ->
          not (StringSet.mem (Ident.name ident) removed_export_names))
    in
    let export_set = Set_ident.of_list exports in
    let block = rewrite_block js.block candidates removed_marker_names in
    {J.block; exports; export_set}
