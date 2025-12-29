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

module P = Ext_pp
module L = Js_dump_lit

let empty_explanation =
  "/* This output is empty. Its source's type definitions, externals and/or \
   unused code got optimized away. */\n"

let program_is_empty (x : J.program) =
  match x with
  | {block = []; exports = []; export_set = _; type_exports = _} -> true
  | _ -> false

let deps_program_is_empty (x : J.deps_program) =
  match x with
  | {modules = []; program; side_effect = None} -> program_is_empty program
  | _ -> false

let rec extract_block_comments acc (x : J.block) =
  match x with
  | {
      statement_desc =
        Exp
          {
            expression_desc =
              Raw_js_code {code; code_info = Stmt Js_stmt_comment};
          };
    }
    :: rest ->
    extract_block_comments (code :: acc) rest
  | _ -> (acc, x)

let extract_file_comments (x : J.deps_program) =
  let comments, new_block = extract_block_comments [] x.program.block in
  (comments, {x with program = {x.program with block = new_block}})

let program f cxt (x : J.program) =
  P.at_least_two_lines f;
  let cxt = Js_dump.statements true cxt f x.block in
  Js_dump_import_export.exports cxt f x.exports

let dump_program (x : J.program) oc =
  ignore (program (P.from_channel oc) Ext_pp_scope.empty x)

let[@inline] is_default (x : Js_op.kind) =
  match x with
  | External {default} -> default
  | _ -> false

let node_program ~output_dir f (x : J.deps_program) =
  P.string f L.strict_directive;
  P.newline f;
  let cxt =
    Js_dump_import_export.requires L.require Ext_pp_scope.empty f
      (* Not be emitted in require statements *)
      (Ext_list.filter_map x.modules (fun x ->
           match x.dynamic_import with
           | true -> None
           | false ->
             Some
               ( x.id,
                 Js_name_of_module_id.string_of_module_id x ~output_dir Commonjs,
                 is_default x.kind )))
  in
  program f cxt x.program

let es6_program ~output_dir ~module_name fmt f (x : J.deps_program) =
  (* For TypeScript mode, initialize type state first to collect runtime types *)
  let () =
    match !Js_config.ts_output with
    | Js_config.Ts_typescript ->
      Ts.init_type_decls ~module_name x.program.type_exports;
      (* Also collect runtime types from value exports for variable type annotations *)
      Ts.collect_runtime_types_from_value_exports x.program.value_exports
    | Js_config.Ts_none -> ()
  in
  (* Print runtime type import first (before regular imports) *)
  let () =
    match !Js_config.ts_output with
    | Js_config.Ts_typescript -> Ts.pp_runtime_type_import f
    | Js_config.Ts_none -> ()
  in
  (* Build a map of module names to their import paths for value imports *)
  let value_module_paths =
    Ext_list.fold_left x.modules Map_string.empty (fun acc m ->
        let name = Ident.name m.id in
        let path = Js_name_of_module_id.string_of_module_id m ~output_dir fmt in
        Map_string.add acc name path)
  in
  let value_imported_modules =
    Map_string.fold value_module_paths Ts.StringSet.empty (fun k _ acc ->
        Ts.StringSet.add k acc)
  in
  (* Get locally defined modules from type exports *)
  let local_modules = Ts.collect_local_module_names x.program.type_exports in
  (* Print type-only imports for modules needed by type annotations but not imported for values *)
  let () =
    match !Js_config.ts_output with
    | Js_config.Ts_typescript ->
      let type_only_modules =
        Ts.get_type_only_modules ~value_imported_modules ~local_modules
      in
      (* Add blank line before type-only imports if no runtime types were printed *)
      if type_only_modules <> [] && not (Ts.RuntimeTypes.has_any ()) then
        P.at_least_two_lines f;
      List.iter
        (fun mod_name ->
          (* Try to find the path from value modules first *)
          let path =
            match Map_string.find_opt value_module_paths mod_name with
            | Some p -> p
            | None ->
              (* For modules not in value imports, use runtime package path.
                 This handles stdlib modules like Stdlib, Pervasives, Js, etc. *)
              let js_file = mod_name ^ ".js" in
              Js_packages_info.runtime_package_path fmt js_file
          in
          P.string f "import type * as ";
          P.string f (Ext_ident.convert mod_name);
          P.string f " from \"";
          P.string f path;
          P.string f "\";";
          P.newline f)
        type_only_modules
    | Js_config.Ts_none -> ()
  in
  (* Print regular imports *)
  let cxt =
    Js_dump_import_export.imports Ext_pp_scope.empty f
      (* Not be emitted in import statements *)
      (Ext_list.filter_map x.modules (fun x ->
           match x.dynamic_import with
           | true -> None
           | false ->
             Some
               ( x.id,
                 Js_name_of_module_id.string_of_module_id x ~output_dir fmt,
                 is_default x.kind,
                 match x.kind with
                 | External {import_attributes} -> import_attributes
                 | _ -> None )))
  in
  (* Emit type declarations for TypeScript mode *)
  let has_type_exports =
    match !Js_config.ts_output with
    | Js_config.Ts_typescript ->
      Ts.pp_type_decls_only f x.program.type_exports;
      (* Set up exported value types and module paths for variable annotation lookup *)
      Ts.set_exported_modules x.program.type_exports;
      Ts.set_exported_types x.program.value_exports;
      x.program.type_exports <> []
    | Js_config.Ts_none -> false
  in
  (* Add blank line after imports/type exports before code *)
  let () = if not has_type_exports then P.at_least_two_lines f in
  let cxt = Js_dump.statements true cxt f x.program.block in
  Js_dump_import_export.es6_export cxt f x.program.exports

let pp_deps_program ~(output_prefix : string)
    (kind : Js_packages_info.module_system) (program : J.deps_program)
    (f : Ext_pp.t) =
  !Js_config.directives
  |> List.iter (fun prim ->
         P.string f prim;
         P.newline f);
  if not !Js_config.no_version_header then (
    P.string f Bs_version.header;
    P.newline f);

  if deps_program_is_empty program then P.string f empty_explanation
    (* This is empty module, it won't be referred anywhere *)
  else
    let comments, program = extract_file_comments program in
    Ext_list.rev_iter comments (fun comment ->
        P.string f comment;
        P.newline f);
    let output_dir = Filename.dirname output_prefix in
    let module_name = Filename.basename output_prefix in
    ignore
      (match kind with
      | Esmodule | Es6_global | Typescript ->
        es6_program ~output_dir ~module_name kind f program
      | Commonjs -> node_program ~output_dir f program);
    P.newline f;
    P.string f
      (match program.side_effect with
      | None -> "/* No side effect */"
      | Some v -> Printf.sprintf "/* %s Not a pure module */" v);
    P.newline f;
    P.flush f ()

let dump_deps_program ~output_prefix kind x (oc : out_channel) =
  pp_deps_program ~output_prefix kind x (P.from_channel oc)
