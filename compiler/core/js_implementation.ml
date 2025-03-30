(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* adapted by rescript from [driver/compile.ml] for convenience    *)

let module_of_filename outputprefix =
  let basename = Filename.basename outputprefix in
  let name =
    try
      let pos = String.index basename '.' in
      String.sub basename 0 pos
    with Not_found -> basename
  in
  String.capitalize_ascii name

let fprintf = Format.fprintf

let print_if_pipe ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let print_if ppf flag printer arg = if !flag then fprintf ppf "%a@." printer arg

let process_with_gentype cmt_file =
  if !Clflags.bs_gentype then GenTypeMain.process_cmt_file cmt_file

let after_parsing_sig ppf outputprefix ast =
  if !Clflags.only_parse = false then (
    Ast_config.process_sig ast;
    (if !Js_config.binary_ast then
       let sourcefile = !Location.input_name in
       Binary_ast.write_ast Mli ~sourcefile
         ~output:(outputprefix ^ Literals.suffix_iast)
         (* to support relocate to another directory *)
         ast);
    if !Js_config.as_pp then (
      output_string stdout Config.ast_intf_magic_number;
      output_value stdout (!Location.input_name : string);
      output_value stdout ast);
    if !Js_config.syntax_only then Warnings.check_fatal ()
    else
      let modulename = module_of_filename outputprefix in
      Lam_compile_env.reset ();
      let initial_env = Res_compmisc.initial_env ~modulename () in
      Env.set_unit_name modulename;
      let tsg = Typemod.transl_signature initial_env ast in
      if !Clflags.dump_typedtree then
        fprintf ppf "%a@." Printtyped.interface tsg;
      let sg = tsg.sig_type in
      ignore (Includemod.signatures initial_env sg sg);
      Delayed_checks.force_delayed_checks ();
      Warnings.check_fatal ();
      let deprecated = Builtin_attributes.deprecated_of_sig ast in
      let sg =
        Env.save_signature ~deprecated sg modulename (outputprefix ^ ".cmi")
      in
      Typemod.save_signature modulename tsg outputprefix !Location.input_name
        initial_env sg;
      process_with_gentype (outputprefix ^ ".cmti"))

let interface ~parser ppf ?outputprefix fname =
  let outputprefix =
    match outputprefix with
    | None -> Config_util.output_prefix fname
    | Some x -> x
  in
  Res_compmisc.init_path ();
  parser fname
  |> Cmd_ppx_apply.apply_rewriters ~restore:false ~tool_name:Js_config.tool_name
       Mli
  |> Ppx_entry.rewrite_signature
  |> print_if_pipe ppf Clflags.dump_parsetree Printast.interface
  |> print_if_pipe ppf Clflags.dump_source Pprintast.signature
  |> after_parsing_sig ppf outputprefix

let interface_mliast ppf fname =
  Res_compmisc.init_path ();
  Binary_ast.read_ast_exn ~fname Mli
  |> print_if_pipe ppf Clflags.dump_parsetree Printast.interface
  |> print_if_pipe ppf Clflags.dump_source Pprintast.signature
  |> after_parsing_sig ppf (Config_util.output_prefix fname)

let all_module_alias (ast : Parsetree.structure) =
  Ext_list.for_all ast (fun {pstr_desc} ->
      match pstr_desc with
      | Pstr_module {pmb_expr = {pmod_desc = Pmod_ident _}} -> true
      | Pstr_attribute _ -> true
      | Pstr_eval _ | Pstr_value _ | Pstr_primitive _ | Pstr_type _
      | Pstr_typext _ | Pstr_exception _ | Pstr_module _ | Pstr_recmodule _
      | Pstr_modtype _ | Pstr_open _ | Pstr_include _ | Pstr_extension _ ->
        false)

let no_export (rest : Parsetree.structure) : Parsetree.structure =
  match rest with
  | head :: _ ->
    let loc = head.pstr_loc in
    Ast_helper.
      [
        Str.include_ ~loc
          (Incl.mk ~loc
             (Mod.constraint_ ~loc (Mod.structure ~loc rest)
                (Mty.signature ~loc [])));
      ]
  | _ -> rest

let after_parsing_impl ppf outputprefix (ast : Parsetree.structure) =
  if !Clflags.only_parse = false then (
    Js_config.all_module_aliases :=
      !Clflags.assume_no_mli = Mli_non_exists && all_module_alias ast;
    Ast_config.process_str ast;
    let ast = if !Js_config.no_export then no_export ast else ast in
    (if !Js_config.binary_ast then
       let sourcefile = !Location.input_name in
       Binary_ast.write_ast ~sourcefile Ml
         ~output:(outputprefix ^ Literals.suffix_ast)
         ast);
    if !Js_config.as_pp then (
      output_string stdout Config.ast_impl_magic_number;
      output_value stdout (!Location.input_name : string);
      output_value stdout ast);
    if !Js_config.syntax_only then Warnings.check_fatal ()
    else
      let modulename = Ext_filename.module_name outputprefix in
      Lam_compile_env.reset ();
      let env = Res_compmisc.initial_env ~modulename () in
      Env.set_unit_name modulename;
      let typedtree, coercion, _, _ =
        Typemod.type_implementation_more
          ?check_exists:(if !Js_config.force_cmi then None else Some ())
          !Location.input_name outputprefix modulename env ast
      in
      let typedtree_coercion = (typedtree, coercion) in
      print_if ppf Clflags.dump_typedtree
        Printtyped.implementation_with_coercion typedtree_coercion;
      (if !Js_config.cmi_only then Warnings.check_fatal ()
       else
         let lambda, exports =
           Translmod.transl_implementation modulename typedtree_coercion
         in
         let js_program =
           print_if_pipe ppf Clflags.dump_rawlambda Printlambda.lambda lambda
           |> Lam_compile_main.compile outputprefix exports
         in
         if not !Js_config.cmj_only then
           Lam_compile_main.lambda_as_module js_program outputprefix);
      process_with_gentype (outputprefix ^ ".cmt"))

let implementation ~parser ppf ?outputprefix fname =
  let outputprefix =
    match outputprefix with
    | None -> Config_util.output_prefix fname
    | Some x -> x
  in
  Res_compmisc.init_path ();
  parser fname
  |> Cmd_ppx_apply.apply_rewriters ~restore:false ~tool_name:Js_config.tool_name
       Ml
  |> Ppx_entry.rewrite_implementation
  |> print_if_pipe ppf Clflags.dump_parsetree Printast.implementation
  |> print_if_pipe ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf outputprefix

let implementation_mlast ppf fname =
  Res_compmisc.init_path ();
  Binary_ast.read_ast_exn ~fname Ml
  |> print_if_pipe ppf Clflags.dump_parsetree Printast.implementation
  |> print_if_pipe ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf (Config_util.output_prefix fname)

let make_structure_item ~ns cunit : Parsetree.structure_item =
  let open Ast_helper in
  let loc = Location.none in
  Str.module_
    (Mb.mk {txt = cunit; loc}
       (Mod.ident {txt = Lident (Ext_namespace_encode.make ~ns cunit); loc}))

(** decoding [.mlmap]
    keep in sync {!Bsb_namespace_map_gen.output}
*)
let implementation_map ppf sourcefile =
  let () = Js_config.cmj_only := true in
  let ichan = open_in_bin sourcefile in
  seek_in ichan (Ext_digest.length + 1);
  let list_of_modules = Ext_io.rev_lines_of_chann ichan in
  close_in ichan;
  let ns = Ext_filename.module_name sourcefile in
  let ml_ast =
    Ext_list.fold_left list_of_modules [] (fun acc line ->
        if Ext_string.is_empty line then acc
        else make_structure_item ~ns line :: acc)
  in
  Res_compmisc.init_path ();
  ml_ast
  |> print_if_pipe ppf Clflags.dump_parsetree Printast.implementation
  |> print_if_pipe ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf (Config_util.output_prefix sourcefile)
