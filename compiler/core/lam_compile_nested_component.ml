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

let root_module_name (id : Ident.t) =
  match Ext_namespace.try_split_module_name id.name with
  | Some (_namespace, module_name) -> module_name
  | None -> (
    match
      String.index_opt id.name Ext_modulename.nested_component_separator_char
    with
    | Some index -> String.sub id.name 0 index
    | None -> id.name)

let nested_component_path id dynamic_import segments =
  let root_name = root_module_name id in
  let denamespace_segment segment =
    let namespaced_prefix = Ext_modulename.nested_component_prefix root_name in
    if Ext_string.starts_with segment namespaced_prefix then
      match
        String.split_on_char Ext_modulename.nested_component_separator_char
          segment
      with
      | root :: _namespace :: rest when rest <> [] ->
        Ext_modulename.concat_nested_component_name (root :: rest)
      | _ -> segment
    else segment
  in
  let segments =
    match segments with
    | head :: rest
      when head = id.name || head = root_name
           || Ext_string.starts_with head
                (Ext_modulename.nested_component_prefix root_name) ->
      rest
    | _ -> segments
  in
  match segments with
  | [] -> None
  | head :: rest ->
    Some
      ( id,
        dynamic_import,
        Ext_modulename.concat_nested_component_name
          (root_name :: denamespace_segment head :: rest) )

let rec extract_component_segments aliases ~allow_import ~allow_unbound_var
    segments (lam : Lam.t) : (Ident.t * bool * string list) option =
  match lam with
  | Lprim
      {
        primitive = Pfield (_, Fld_module {name; jsx_component = _});
        args = [arg];
        _;
      } ->
    extract_component_segments aliases ~allow_import ~allow_unbound_var
      (name :: segments) arg
  | Lprim {primitive = Pawait; args = [arg]; _} ->
    extract_component_segments aliases ~allow_import ~allow_unbound_var segments
      arg
  | Lprim {primitive = Pimport; args = [arg]; _} when allow_import ->
    extract_component_segments aliases ~allow_import ~allow_unbound_var segments
      arg
  | Lvar id -> (
    match Map_ident.find_opt !aliases id with
    | Some alias_lam ->
      extract_component_segments aliases ~allow_import ~allow_unbound_var
        segments alias_lam
    | None ->
      if allow_unbound_var then Some (id, false, List.rev segments) else None)
  | Lglobal_module (id, dynamic_import) ->
    Some (id, dynamic_import, List.rev segments)
  | _ -> None

let rec is_module_alias_candidate aliases (lam : Lam.t) =
  match lam with
  | Lglobal_module _ -> true
  | Lvar id -> Map_ident.mem !aliases id
  | Lprim {primitive = Pfield (_, Fld_module _); args = [arg]; _}
  | Lprim {primitive = Pawait | Pimport; args = [arg]; _} ->
    is_module_alias_candidate aliases arg
  | _ -> false

let extract_nested_external_component_path aliases (lam : Lam.t) :
    (Ident.t * bool * string) option =
  match
    extract_component_segments aliases ~allow_import:false
      ~allow_unbound_var:true [] lam
  with
  | Some (id, dynamic_import, segments) ->
    nested_component_path id dynamic_import segments
  | None -> None

let extract_nested_external_component_field aliases (lam : Lam.t) :
    (Ident.t * bool * string) option =
  match lam with
  | Lprim
      {
        primitive = Pfield (_, Fld_module {name = "make"; jsx_component = _});
        args = [arg];
        _;
      } ->
    extract_nested_external_component_path aliases arg
  | _ -> None

let extract_static_nested_external_component_path aliases (lam : Lam.t) :
    (Ident.t * bool * string) option =
  match
    extract_component_segments aliases ~allow_import:true
      ~allow_unbound_var:false [] lam
  with
  | Some (id, dynamic_import, segments) ->
    nested_component_path id dynamic_import segments
  | None -> None

let normalize_hidden_component_name (id : Ident.t) (hidden_name : string) =
  let root_name = root_module_name id in
  let id_parts =
    String.split_on_char Ext_modulename.nested_component_separator_char id.name
  in
  let namespace_parts =
    match id_parts with
    | _root :: rest -> rest
    | [] -> []
  in
  let hidden_parts =
    String.split_on_char Ext_modulename.nested_component_separator_char
      hidden_name
  in
  let hidden_parts_without_root =
    match hidden_parts with
    | first :: rest when String.equal first root_name -> rest
    | _ -> hidden_parts
  in
  let rec drop_prefix prefix parts =
    match (prefix, parts) with
    | [], _ -> parts
    | x :: xs, y :: ys when String.equal x y -> drop_prefix xs ys
    | _ -> parts
  in
  let tail = drop_prefix namespace_parts hidden_parts_without_root in
  match tail with
  | [] -> hidden_name
  | _ -> Ext_modulename.concat_nested_component_name (root_name :: tail)

let hidden_component_name_candidates (id : Ident.t) (hidden_name : string) =
  let candidates = ref [] in
  let push candidate =
    if not (List.mem candidate !candidates) then
      candidates := candidate :: !candidates
  in
  (match
     String.split_on_char Ext_modulename.nested_component_separator_char
       hidden_name
   with
  | root :: _namespace :: rest when rest <> [] ->
    push (Ext_modulename.concat_nested_component_name (root :: rest))
  | _ -> ());
  push (normalize_hidden_component_name id hidden_name);
  push hidden_name;
  List.rev !candidates

let exported_hidden_component_name ~(id : Ident.t) ~(dynamic_import : bool)
    hidden_name_candidates =
  let rec loop = function
    | [] -> None
    | candidate :: rest -> (
      match
        Lam_compile_env.query_external_id_info ~dynamic_import id candidate
      with
      | _ -> Some candidate
      | exception Not_found -> loop rest)
  in
  loop hidden_name_candidates

let rec extract_root_expr (expr : J.expression) =
  match expr.expression_desc with
  | Var (Qualified (module_id, Some _)) ->
    Some {expr with expression_desc = Var (Qualified (module_id, None))}
  | Static_index (inner, _, _) -> extract_root_expr inner
  | Var _ -> Some expr
  | _ -> None

let hidden_component_access (root_expr : J.expression) hidden_name =
  match root_expr.expression_desc with
  | Var (Qualified (module_id, None)) ->
    {
      root_expr with
      expression_desc = Var (Qualified (module_id, Some hidden_name));
    }
  | _ -> E.dot root_expr hidden_name

let rewrite_jsx_component_expr aliases (jsx_tag : Lam.t)
    (compiled_expr : J.expression) : J.expression =
  match extract_nested_external_component_field aliases jsx_tag with
  | Some (id, dynamic_import, hidden_name) -> (
    let hidden_name_candidates =
      hidden_component_name_candidates id hidden_name
    in
    match extract_root_expr compiled_expr with
    | Some root_expr -> (
      match
        exported_hidden_component_name ~id ~dynamic_import
          hidden_name_candidates
      with
      | Some hidden_name -> hidden_component_access root_expr hidden_name
      | None -> compiled_expr)
    | None -> compiled_expr)
  | None -> compiled_expr

let rewrite_component_make_expr aliases (lam : Lam.t)
    (compiled_expr : J.expression) : J.expression =
  match extract_static_nested_external_component_path aliases lam with
  | Some (id, dynamic_import, hidden_name) -> (
    let hidden_name_candidates =
      hidden_component_name_candidates id hidden_name
    in
    match extract_root_expr compiled_expr with
    | Some root_expr -> (
      match
        exported_hidden_component_name ~id ~dynamic_import
          hidden_name_candidates
      with
      | Some hidden_name -> hidden_component_access root_expr hidden_name
      | None -> compiled_expr)
    | None -> compiled_expr)
  | None -> compiled_expr
