(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
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

(* module E = Js_exp_make  *)
(* module S = Js_stmt_make   *)

let compile_group output_prefix (meta : Lam_stats.t) (x : Lam_group.t) :
    Js_output.t =
  match x with
  (*
        We need

        2. [E.builtin_dot] for javascript builtin
        3. [E.mldot]
     *)
  (* ATTENTION: check {!Lam_compile_global} for consistency  *)
  (* Special handling for values in [Pervasives] *)
  (*
         we delegate [stdout, stderr, and stdin] into [caml_io] module,
         the motivation is to help dead code eliminatiion, it's helpful
         to make those parts pure (not a function call), then it can be removed
         if unused
      *)

  (* QUICK hack to make hello world example nicer,
     Note the arity of [print_endline] is already analyzed before,
     so it should be safe
  *)
  | Single (kind, id, lam) ->
    (* let lam = Optimizer.simplify_lets [] lam in  *)
    (* can not apply again, it's wrong USE it with care*)
    (* ([Js_stmt_make.comment (Gen_of_env.query_type id  env )], None)  ++ *)
    Lam_compile.compile_lambda ~output_prefix
      {
        continuation = Declare (kind, id);
        jmp_table = Lam_compile_context.empty_handler_map;
        switch_depth = 0;
        loop_stack = [];
        loop_label_counter = ref 0;
        meta;
      }
      lam
  | Recursive id_lams ->
    Lam_compile.compile_recursive_lets ~output_prefix
      {
        continuation = EffectCall Not_tail;
        jmp_table = Lam_compile_context.empty_handler_map;
        switch_depth = 0;
        loop_stack = [];
        loop_label_counter = ref 0;
        meta;
      }
      id_lams
  | Nop lam ->
    (* TODO: Side effect callls, log and see statistics *)
    Lam_compile.compile_lambda ~output_prefix
      {
        continuation = EffectCall Not_tail;
        jmp_table = Lam_compile_context.empty_handler_map;
        switch_depth = 0;
        loop_stack = [];
        loop_label_counter = ref 0;
        meta;
      }
      lam

(** Also need analyze its depenency is pure or not *)
let no_side_effects (rest : Lam_group.t list) : string option =
  Ext_list.find_opt rest (fun x ->
      match x with
      | Single (kind, id, body) -> (
        match kind with
        | Strict | Variable ->
          if not @@ Lam_analysis.no_side_effects body then
            Some (Printf.sprintf "%s" id.name)
          else None
        | _ -> None)
      | Recursive bindings ->
        Ext_list.find_opt bindings (fun (id, lam) ->
            if not @@ Lam_analysis.no_side_effects lam then
              Some (Printf.sprintf "%s" id.Ident.name)
            else None)
      | Nop lam ->
        if not @@ Lam_analysis.no_side_effects lam then
          (*  (Lam_util.string_of_lambda lam) *)
          Some ""
        else None (* TODO :*))

(* Materialize JS-hoisted values as root-level aliases and exports. The source
   value still lives at its normal module path, but downstream tools can import
   the flat name directly when the .cmj metadata marks it as hoisted. *)
let js_hoisted_aliases (export_idents : Set_ident.t) (groups : Lam_group.t list)
    =
  let has_hoisted_binding =
    List.exists
      (function
        | Lam_group.Single (_, id, _) -> Ident.js_hoisted id
        | Lam_group.Recursive bindings ->
          List.exists (fun (id, _) -> Ident.js_hoisted id) bindings
        | Lam_group.Nop _ -> false)
      groups
  in
  if not has_hoisted_binding then []
  else
    let group_map =
      Ext_list.fold_left groups Map_ident.empty (fun group_map group ->
          match group with
          | Single (_, id, lam) -> Map_ident.add group_map id lam
          | Recursive bindings ->
            Ext_list.fold_left bindings group_map (fun group_map (id, lam) ->
                Map_ident.add group_map id lam)
          | Nop _ -> group_map)
    in
    let rec access loc base fields =
      match fields with
      | [] -> base
      | (pos, name) :: fields ->
        access loc
          (Lam.prim
             ~primitive:
               (Lam_primitive.Pfield (pos, Lam_compat.Fld_module {name}))
             ~args:[base] loc)
          fields
    in
    let resolve = function
      | Lam.Lvar id as lam -> (
        match Map_ident.find_opt group_map id with
        | Some resolved -> resolved
        | None -> lam)
      | lam -> lam
    in
    let rec fold_module_fields fields args index acc f =
      match (fields, args) with
      | [], [] -> acc
      | field :: fields, arg :: args ->
        fold_module_fields fields args (index + 1) (f index field arg acc) f
      | _, _ -> invalid_arg "fold_module_fields"
    in
    let occupied_names =
      Ext_list.fold_left groups Set_string.empty (fun occupied group ->
          match group with
          | Single (_, id, _) ->
            Set_string.add occupied (Ext_ident.convert id.Ident.name)
          | Recursive bindings ->
            Ext_list.fold_left bindings occupied (fun occupied (id, _) ->
                Set_string.add occupied (Ext_ident.convert id.Ident.name))
          | Nop _ -> occupied)
    in
    let rec scan top_id path ((aliases, occupied_names) as state) lam =
      match resolve lam with
      | Lam.Lprim
          {
            primitive = Lam_primitive.Pmakeblock (_, Blk_module fields, _);
            args;
            loc;
          } ->
        fold_module_fields fields args 0 state (fun pos field arg state ->
            let path = path @ [(pos, field)] in
            let aliases, occupied_names = scan top_id path state arg in
            match arg with
            | Lam.Lvar id when Ident.js_hoisted id ->
              let segments =
                top_id.Ident.name
                :: Ext_list.map path (fun (_pos, name) -> name)
              in
              let name =
                segments
                |> List.map Ext_ident.unwrap_uppercase_exotic
                |> String.concat "$"
              in
              let js_name = Ext_ident.convert name in
              if Set_string.mem occupied_names js_name then
                let loc =
                  match resolve arg with
                  | Lam.Lfunction {loc} -> loc
                  | _ -> loc
                in
                Location.raise_errorf ~loc
                  "Cannot hoist this function as `%s` because that name is \
                   already used by a top-level binding."
                  name
              else
                let alias_id = Ident.create name in
                let alias = access loc (Lam.var top_id) path in
                ( ( Lam_group.Single (Alias, alias_id, alias),
                    alias_id,
                    alias,
                    segments,
                    name )
                  :: aliases,
                  Set_string.add occupied_names js_name )
            | _ -> (aliases, occupied_names))
      | _ -> state
    in
    fst
      (Ext_list.fold_left groups ([], occupied_names) (fun state group ->
           match group with
           | Single (_, id, lam) when Set_ident.mem export_idents id ->
             scan id [] state lam
           | Single _ | Recursive _ | Nop _ -> state))

(** Actually simplify_lets is kind of global optimization since it requires you to know whether
    it's used or not
*)
let compile (output_prefix : string) export_idents (lam : Lambda.lambda) =
  let debug_ir = !Js_config.debug_ir in
  let diagnostics =
    if debug_ir then Some (Ir_diagnostics.create ~output_prefix) else None
  in
  let d pass lam =
    (match diagnostics with
    | Some diagnostics ->
      Ir_diagnostics.dump_lam diagnostics ~pass lam;
      Ext_log.dwarn ~__POS__ "START CHECKING PASS %s@." pass
    | None -> ());
    if !Js_config.check_lam || debug_ir then (
      ignore @@ Lam_check.check ~file:!Location.input_name ~pass lam;
      if debug_ir then Ext_log.dwarn ~__POS__ "FINISH CHECKING PASS %s@." pass);
    lam
  in
  let j pass program =
    Ext_option.iter diagnostics (fun diagnostics ->
        Ir_diagnostics.dump_js diagnostics ~pass program);
    program
  in
  let export_ident_sets = Set_ident.of_list export_idents in
  (* To make toplevel happy - reentrant for js-demo *)
  let () =
    if debug_ir then
      Ext_list.iter export_idents (fun id ->
          Ext_log.dwarn ~__POS__ "export idents: %s/%d" id.name id.stamp);
    Lam_compile_env.reset ()
  in
  let lam, may_required_modules = Lam_convert.convert export_ident_sets lam in

  let lam = d "initial" lam in
  let lam = Lam_pass_deep_flatten.deep_flatten lam in
  let lam = d "flatten0" lam in
  let meta : Lam_stats.t = Lam_stats.make ~export_idents ~export_ident_sets in
  let () = Lam_pass_collect.collect_info meta lam in
  let lam =
    let lam =
      lam |> d "flatten1" |> Lam_pass_exits.simplify_exits |> d "simplify_exits"
      |> (fun lam ->
      Lam_pass_collect.collect_info meta lam;
      if debug_ir then
        Ext_log.dwarn ~__POS__ "Before simplify_alias: %a@." Lam_stats.print
          meta;
      lam)
      |> Lam_pass_remove_alias.simplify_alias meta
      |> d "simplify_alias" |> Lam_pass_deep_flatten.deep_flatten
      |> d "flatten2"
    in
    (* Inling happens*)

    let () = Lam_pass_collect.collect_info meta lam in
    let lam = Lam_pass_remove_alias.simplify_alias meta lam in
    let lam = Lam_pass_deep_flatten.deep_flatten lam in
    let () = Lam_pass_collect.collect_info meta lam in
    let lam =
      lam |> d "alpha_before"
      |> Lam_pass_alpha_conversion.alpha_conversion meta
      |> d "alpha_after" |> Lam_pass_exits.simplify_exits
    in
    let () = Lam_pass_collect.collect_info meta lam in

    lam |> d "simplify_alias_before"
    |> Lam_pass_remove_alias.simplify_alias meta
    |> d "alpha_conversion"
    |> Lam_pass_alpha_conversion.alpha_conversion meta
    |> d "before-simplify_lets"
    (* we should investigate a better way to put different passes : )*)
    |> Lam_pass_lets_dce.simplify_lets
    |> d "before-simplify-exits"
    (* |> (fun lam -> Lam_pass_collect.collect_info meta lam
       ; Lam_pass_remove_alias.simplify_alias meta lam) *)
    (* |> Lam_group_pass.scc_pass
       |> d "scc" *)
    |> Lam_pass_exits.simplify_exits
    |> d "simplify_lets"
    |> fun lam ->
    if debug_ir then
      Ext_log.dwarn ~__POS__ "Before coercion: %a@." Lam_stats.print meta;
    lam
  in

  let ({Lam_coercion.groups} as coerced_input), meta =
    Lam_coercion.coerce_and_group_big_lambda meta lam
  in

  let () =
    if debug_ir then (
      Ext_log.dwarn ~__POS__ "After coercion: %a@." Lam_stats.print meta;
      Ext_option.iter diagnostics (fun diagnostics ->
          Ir_diagnostics.dump_groups diagnostics coerced_input.groups))
  in
  let maybe_pure = no_side_effects groups in
  (* Add the generated alias groups before JS lowering so regular export
     printing, tree shaking, and .cmj metadata all see the flat runtime value. *)
  let hoisted_aliases = js_hoisted_aliases meta.export_idents groups in
  let hoisted_groups, hoisted_exports, hoisted_export_map, hoisted_metadata =
    Ext_list.fold_left hoisted_aliases ([], [], Map_ident.empty, [])
      (fun
        (groups, exports, export_map, hoisted_metadata)
        (group, id, lam, path, name)
      ->
        ( group :: groups,
          id :: exports,
          Map_ident.add export_map id lam,
          {Js_cmj_format.path; name} :: hoisted_metadata ))
  in
  let groups = groups @ List.rev hoisted_groups in
  let meta =
    {
      meta with
      exports = meta.exports @ List.rev hoisted_exports;
      export_idents =
        Ext_list.fold_left hoisted_exports meta.export_idents (fun acc id ->
            Set_ident.add acc id);
    }
  in
  let export_map =
    Map_ident.fold hoisted_export_map coerced_input.export_map
      (fun id lam acc -> Map_ident.add acc id lam)
  in
  let () =
    if debug_ir then
      Ext_log.dwarn ~__POS__ "\n@[[TIME:]Pre-compile: %f@]@."
        (Sys.time () *. 1000.)
  in
  let body =
    Ext_list.map groups (fun group -> compile_group output_prefix meta group)
    |> Js_output.concat |> Js_output.output_as_block
  in
  let () =
    if debug_ir then
      Ext_log.dwarn ~__POS__ "\n@[[TIME:]Post-compile: %f@]@."
        (Sys.time () *. 1000.)
  in
  (* The file is not big at all compared with [cmo] *)
  (* Ext_marshal.to_file (Ext_path.chop_extension filename ^ ".mj")  js; *)
  let meta_exports = meta.exports in
  let export_set = Set_ident.of_list meta_exports in
  let js : J.program = {exports = meta_exports; export_set; block = body} in
  js |> j "initial" |> Js_pass_flatten.program |> j "flatten"
  |> Js_pass_external_shadow.program |> j "external_shadow"
  |> Js_pass_tailcall_inline.tailcall_inline |> j "inline_and_shake"
  |> Js_pass_record_rest.program |> j "record_rest"
  |> Js_pass_flatten_and_mark_dead.program |> j "flatten_and_mark_dead"
  (* |> Js_inline_and_eliminate.inline_and_shake *)
  (* |> j "inline_and_shake" *)
  |> (fun js ->
  ignore @@ Js_pass_scope.program js;
  js)
  |> Js_shake.shake_program |> j "shake"
  |> fun (program : J.program) ->
  let external_module_ids : Lam_module_ident.t list =
    if !Js_config.all_module_aliases then []
    else
      let hard_deps = Js_fold_basic.calculate_hard_dependencies program.block in
      Lam_compile_env.populate_required_modules may_required_modules hard_deps;
      Ext_list.sort_via_array (Lam_module_ident.Hash_set.to_list hard_deps)
        (fun id1 id2 ->
          Ext_string.compare
            (Lam_module_ident.name id1)
            (Lam_module_ident.name id2))
  in
  Warnings.check_fatal ();
  let effect_ =
    Lam_stats_export.get_dependent_module_effect maybe_pure external_module_ids
  in
  let v : Js_cmj_format.t =
    Lam_stats_export.export_to_cmj meta effect_ export_map hoisted_metadata
      (if Ext_char.is_lower_case (Filename.basename output_prefix).[0] then
         Little
       else Upper)
  in
  if not !Clflags.dont_write_files then
    Js_cmj_format.to_file ~check_exists:(not !Js_config.force_cmj)
      (output_prefix ^ Literals.suffix_cmj)
      v;
  {J.program; side_effect = effect_; modules = external_module_ids}

let ( // ) = Filename.concat

let remove_stale_source_map ?(remove_stale_map = true) target_file =
  if remove_stale_map && not !Clflags.dont_write_files then
    Misc.remove_file (target_file ^ ".map")

let dump_deps_program_with_source_map ?(remove_stale_map = true) ~target_file
    ~output_prefix module_system lambda_output chan =
  let builder =
    Js_source_map.make ~generated_file:target_file
      ~source_root:!Js_config.source_map_root
      ~sources_content:!Js_config.source_map_sources_content
  in
  Js_source_map.with_builder builder (fun () ->
      Js_dump_program.pp_deps_program ~output_prefix module_system lambda_output
        (Ext_pp.from_channel chan));
  match !Js_config.source_map with
  | Linked ->
    let json = Js_source_map.json builder in
    output_string chan
      (Js_source_map.linked_comment ~map_file:(target_file ^ ".map"));
    Ext_io.write_file (target_file ^ ".map") json
  | Hidden ->
    Ext_io.write_file (target_file ^ ".map") (Js_source_map.json builder)
  | Inline ->
    output_string chan
      (Js_source_map.inline_comment ~json:(Js_source_map.json builder));
    remove_stale_source_map ~remove_stale_map target_file
  | No_source_map -> ()

let lambda_as_module (lambda_output : J.deps_program) (output_prefix : string) :
    unit =
  let package_info = Js_packages_state.get_packages_info () in
  if Js_packages_info.is_empty package_info && !Js_config.js_stdout then
    match !Js_config.source_map with
    | Inline ->
      let target_file =
        Ext_namespace.change_ext_ns_suffix
          (Filename.basename output_prefix)
          Literals.suffix_js
      in
      dump_deps_program_with_source_map ~remove_stale_map:false ~target_file
        ~output_prefix Commonjs lambda_output stdout
    | _ ->
      Js_dump_program.dump_deps_program ~output_prefix Commonjs lambda_output
        stdout
  else
    Js_packages_info.iter package_info (fun {module_system; path; suffix} ->
        let basename =
          Ext_namespace.change_ext_ns_suffix
            (Filename.basename output_prefix)
            suffix
        in
        let target_file =
          Ext_path.package_dir () // path
          // basename (* #913 only generate little-case js file *)
        in
        let output_chan chan =
          match !Js_config.source_map with
          | No_source_map ->
            Js_dump_program.dump_deps_program ~output_prefix module_system
              lambda_output chan;
            remove_stale_source_map target_file
          | Linked | Inline | Hidden ->
            dump_deps_program_with_source_map ~target_file ~output_prefix
              module_system lambda_output chan
        in
        if not !Clflags.dont_write_files then
          Ext_pervasives.with_file_as_chan target_file output_chan;
        if !Warnings.has_warnings then (
          Warnings.has_warnings := false;
          (* 5206: When there were warnings found during the compilation, we want the file
             to be rebuilt on the next "rescript build" so that the warnings keep being shown.
             Set the timestamp of the ast file to 1970-01-01 to make this rebuild happen.
             (Do *not* set the timestamp of the JS output file instead
             as that does not play well with every bundler.) *)
          let ast_file = output_prefix ^ Literals.suffix_ast in
          if Sys.file_exists ast_file then Build_artifact.mark_stale ast_file))

(* We can use {!Env.current_unit = "Pervasives"} to tell if it is some specific module,
    We need handle some definitions in standard libraries in a special way, most are io specific,
    includes {!Pervasives.stdin, Pervasives.stdout, Pervasives.stderr}

    However, use filename instead of {!Env.current_unit} is more honest, since node-js module system is coupled with the file name
*)
