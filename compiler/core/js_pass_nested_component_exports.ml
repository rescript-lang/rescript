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

type candidate = {module_ident: Ident.t}

let hidden_component_suffix (module_ident : Ident.t) =
  "$" ^ Ident.name module_ident

let is_hidden_component_name_for module_ident ident =
  Ext_string.ends_with (Ident.name ident) (hidden_component_suffix module_ident)

let find_hidden_alias_by_value block module_ident value_ident =
  List.find_map
    (fun (st : J.statement) ->
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
  List.exists
    (fun (ident : Ident.t) -> String.equal (Ident.name ident) name)
    exports

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
                  ( [{expression_desc = Var (Id value_ident); _}],
                    Immutable,
                    _,
                    Blk_module ["make"] );
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
    | Some hidden_ident when has_export_name exports hidden_ident.name ->
      Some {module_ident}
    | Some _ | None -> None)
  | _ -> None

let collect_candidates block exports =
  Ext_list.filter_map block (candidate_of_statement block exports)

let hidden_export_names_to_remove candidates =
  Ext_list.fold_left candidates StringSet.empty (fun acc candidate ->
      StringSet.add (Ident.name candidate.module_ident) acc)

let dynamic_import_module_root (expr : J.expression) =
  match expr.expression_desc with
  | Await
      {
        expression_desc =
          Call ({expression_desc = Var (Id import_ident); _}, [arg], _);
        _;
      }
    when String.equal import_ident.name "import" -> (
    match arg.expression_desc with
    | Str {txt; _} ->
      let basename = Filename.basename txt in
      let suffixes = [".res.mjs"; ".res.js"; ".mjs"; ".js"] in
      let rec strip_suffix = function
        | [] -> basename
        | suffix :: rest -> (
          match Ext_string.ends_with_then_chop basename suffix with
          | Some basename -> basename
          | None -> strip_suffix rest)
      in
      Some (strip_suffix suffixes)
    | _ -> None)
  | _ -> None

let known_hidden_component_exports block =
  let hidden_exports = ref StringSet.empty in
  let mapper =
    {
      Js_record_map.super with
      expression =
        (fun self expr ->
          (match expr.expression_desc with
          | Var (Qualified (_, Some name)) ->
            hidden_exports := StringSet.add name !hidden_exports
          | Static_index (_, name, _) when String.contains name '$' ->
            hidden_exports := StringSet.add name !hidden_exports
          | _ -> ());
          Js_record_map.super.expression self expr);
    }
  in
  ignore (mapper.block mapper block);
  !hidden_exports

let dynamic_import_aliases block =
  List.fold_left
    (fun aliases (st : J.statement) ->
      match st.statement_desc with
      | Variable {ident; value = Some expr; _} -> (
        match dynamic_import_module_root expr with
        | Some module_root -> Map_ident.add aliases ident module_root
        | None -> aliases)
      | _ -> aliases)
    Map_ident.empty block

let rewrite_dynamic_import_component_access aliases known_hidden_exports
    (expr : J.expression) =
  let rec collect_segments segments (expr : J.expression) =
    match expr.expression_desc with
    | Static_index (inner, field, _) ->
      collect_segments (field :: segments) inner
    | Var (Id id) -> (
      match Map_ident.find_opt aliases id with
      | Some module_root when segments <> [] -> Some (id, module_root, segments)
      | Some _ | None -> None)
    | _ -> None
  in
  match expr.expression_desc with
  | Static_index (inner, "make", _) -> (
    match collect_segments [] inner with
    | Some (id, module_root, segments) ->
      let segments = List.rev segments in
      let hidden_name =
        match segments with
        | first :: _ when Ext_string.starts_with first (module_root ^ "$") ->
          String.concat "$" segments
        | _ -> String.concat "$" (module_root :: segments)
      in
      if StringSet.mem hidden_name known_hidden_exports then
        {expr with expression_desc = Static_index (E.var id, hidden_name, None)}
      else expr
    | None -> expr)
  | _ -> expr

let rewrite_dynamic_import_block block =
  let aliases = dynamic_import_aliases block in
  if Map_ident.is_empty aliases then block
  else
    let known_hidden_exports = known_hidden_component_exports block in
    let mapper =
      {
        Js_record_map.super with
        expression =
          (fun self expr ->
            let expr = Js_record_map.super.expression self expr in
            rewrite_dynamic_import_component_access aliases known_hidden_exports
              expr);
      }
    in
    mapper.block mapper block

let program (js : J.program) : J.program =
  let candidates = collect_candidates js.block js.exports in
  let removed_export_names = hidden_export_names_to_remove candidates in
  let exports =
    Ext_list.filter js.exports (fun (ident : Ident.t) ->
        not (StringSet.mem (Ident.name ident) removed_export_names))
  in
  let export_set = Set_ident.of_list exports in
  let block = rewrite_dynamic_import_block js.block in
  {J.block; exports; export_set}
