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

(*
 * The API version is giving information about the feature set
 * of the resulting ReScript JS bundle API.
 *
 * It follows the semver format {major.minor} which means:
 * - Whenever there is a breaking change, raise the major version
 * - Whenever there is a feature addition, raise the minor version
 *
 * Whenever you are changing functionality in here, please double check
 * if you are breaking any APIs. If yes, make sure to update this apiVersion
 * value accordingly.
 *
 * Rationale:
 * We ship ReScript bindings that bind to this API. To be able to handle
 * different bundles with different API versions, we need a way to tell the
 * consumer on what interface the bundle provides.
 *
 * This will allow the frontend to have different sets of the same bindings,
 * and use the proper interfaces as stated by the apiVersion.
 *
 * -----------------------------
 * Version History: * v2: Remove refmt support (removes compiler.reason apis)
 * v3: Switched to Uncurried mode by default (requires third party packages
 to be built with uncurried: true in bsconfig.json). Also added
 `config.uncurried` to the BundleConfig.
 * v4: Added `config.open_modules` to the BundleConfig to enable implicitly opened
 * modules in the playground.
 * *)
let api_version = "5"

module Js = Js_of_ocaml.Js

let export (field : string) v = Js.Unsafe.set Js.Unsafe.global field v

module Lang = struct
  type t = Res

  let from_string t =
    match t with
    | "res" -> Some Res
    | _ -> None

  let to_string t =
    match t with
    | Res -> "res"
end

module BundleConfig = struct
  type t = {
    mutable module_system: Ext_module_system.t;
    mutable filename: string option;
    mutable warn_flags: string;
    mutable open_modules: string list;
  }

  let make () =
    {
      module_system = Ext_module_system.Commonjs;
      filename = None;
      warn_flags = Bsc_warnings.defaults_w;
      open_modules = [];
    }

  let default_filename (lang : Lang.t) = "playground." ^ Lang.to_string lang

  let string_of_module_system m =
    match m with
    | Ext_module_system.Commonjs -> "nodejs"
    | Esmodule -> "es6"
    | Es6_global -> "es6_global"
end

type loc_err_info = {
  full_msg: string; (* Full report string with all context *)
  short_msg: string; (* simple explain message without any extra context *)
  loc: Location.t;
}

module LocWarnInfo = struct
  type t = {
    full_msg: string; (* Full super_error related warn string *)
    short_msg: string; (* Plain warn message without any context *)
    warn_number: int;
    is_error: bool;
    loc: Location.t;
  }
end

exception RescriptParsingErrors of loc_err_info list

module ErrorRet = struct
  let loc_error_attributes ~(type_ : string) ~(full_msg : string)
      ~(short_msg : string) (loc : Location.t) =
    let _file, line, startchar = Location.get_pos_info loc.Location.loc_start in
    let _file, endline, endchar = Location.get_pos_info loc.Location.loc_end in
    Js.Unsafe.
      [|
        ("fullMsg", inject @@ Js.string full_msg);
        ("row", inject line);
        ("column", inject startchar);
        ("endRow", inject endline);
        ("endColumn", inject endchar);
        ("shortMsg", inject @@ Js.string short_msg);
        ("type", inject @@ Js.string type_);
      |]

  let make_warning (e : LocWarnInfo.t) =
    let loc_attrs =
      loc_error_attributes ~type_:"warning" ~full_msg:e.full_msg
        ~short_msg:e.short_msg e.loc
    in
    let warn_attrs =
      Js.Unsafe.
        [|
          ( "warnNumber",
            inject @@ (e.warn_number |> float_of_int |> Js.number_of_float) );
          ("isError", inject @@ Js.bool e.is_error);
        |]
    in
    let attrs = Array.append loc_attrs warn_attrs in
    Js.Unsafe.obj attrs

  let from_loc_errors ?(warnings : LocWarnInfo.t array option) ~(type_ : string)
      (errors : loc_err_info array) =
    let js_errors =
      Array.map
        (fun (e : loc_err_info) ->
          Js.Unsafe.(
            obj
              (loc_error_attributes ~type_ ~full_msg:e.full_msg
                 ~short_msg:e.short_msg e.loc)))
        errors
    in
    let loc_err_attrs =
      Js.Unsafe.
        [|
          ("errors", inject @@ Js.array js_errors);
          ("type", inject @@ Js.string type_);
        |]
    in
    let warning_attr =
      match warnings with
      | Some warnings ->
        Js.Unsafe.
          [|("warnings", inject @@ Js.array (Array.map make_warning warnings))|]
      | None -> [||]
    in
    let attrs = Array.append loc_err_attrs warning_attr in
    Js.Unsafe.(obj attrs)

  let from_syntax_errors (errors : loc_err_info array) =
    from_loc_errors ~type_:"syntax_error" errors

  (* for raised errors caused by malformed warning / warning_error flags *)
  let make_warning_flag_error ~(warn_flags : string) (msg : string) =
    Js.Unsafe.(
      obj
        [|
          ("msg", inject @@ Js.string msg);
          ("warn_flags", inject @@ Js.string warn_flags);
          ("type", inject @@ Js.string "warning_flag_error");
        |])

  let make_warning_error (errors : LocWarnInfo.t array) =
    let type_ = "warning_error" in
    let js_errors = Array.map make_warning errors in
    Js.Unsafe.(
      obj
        [|
          ("errors", inject @@ Js.array js_errors);
          ("type", inject @@ Js.string type_);
        |])

  let make_unexpected_error msg =
    Js.Unsafe.(
      obj
        [|
          ("msg", inject @@ Js.string msg);
          ("type", inject @@ Js.string "unexpected_error");
        |])
end

(* One time setup for all relevant modules *)
let () =
  Bs_conditional_initial.setup_env ();
  Clflags.binary_annotations := false;
  Clflags.color := Some Always;
  Lazy.force Res_outcome_printer.setup

let error_of_exn e =
  match Location.error_of_exn e with
  | Some (`Ok e) -> Some e
  | Some `Already_displayed | None -> None

(* Returns a default filename in case given value opt is not set *)
let get_filename ~(lang : Lang.t) opt =
  match opt with
  | Some fname -> fname
  | None -> BundleConfig.default_filename lang

module ResDriver = struct
  (* For now we are basically overriding functionality from Res_driver *)
  open Res_driver

  (* adds ~src parameter *)
  let setup ~src ~filename ~for_printer () =
    let mode =
      if for_printer then Res_parser.Default else ParseForTypeChecker
    in
    Res_parser.make ~mode src filename

  (* get full super error message *)
  let diagnostic_to_string ~(src : string) (d : Res_diagnostics.t) =
    let start_pos = Res_diagnostics.get_start_pos d in
    let end_pos = Res_diagnostics.get_end_pos d in
    let msg = Res_diagnostics.explain d in
    let loc =
      {loc_start = start_pos; Location.loc_end = end_pos; loc_ghost = false}
    in
    let err = {Location.loc; msg; sub = []; if_highlight = ""} in
    Location.default_error_reporter ~src:(Some src) Format.str_formatter err;
    Format.flush_str_formatter ()

  let parse_implementation ~sourcefile ~for_printer ~src =
    Location.input_name := sourcefile;
    let parse_result =
      let engine = setup ~filename:sourcefile ~for_printer ~src () in
      let structure = Res_core.parse_implementation engine in
      let invalid, diagnostics =
        match engine.diagnostics with
        | [] as diagnostics -> (false, diagnostics)
        | _ as diagnostics -> (true, diagnostics)
      in
      {
        filename = engine.scanner.filename;
        source = engine.scanner.src;
        parsetree = structure;
        diagnostics;
        invalid;
        comments = List.rev engine.comments;
      }
    in
    let () =
      if parse_result.invalid then
        let errors =
          parse_result.diagnostics
          |> List.map (fun d ->
                 let full_msg =
                   diagnostic_to_string ~src:parse_result.source d
                 in
                 let short_msg = Res_diagnostics.explain d in
                 let loc =
                   {
                     Location.loc_start = Res_diagnostics.get_start_pos d;
                     Location.loc_end = Res_diagnostics.get_end_pos d;
                     loc_ghost = false;
                   }
                 in
                 {full_msg; short_msg; loc})
          |> List.rev
        in
        raise (RescriptParsingErrors errors)
    in
    (parse_result.parsetree, parse_result.comments)
end

let rescript_parse ~filename src =
  let structure, _ =
    ResDriver.parse_implementation ~for_printer:false ~sourcefile:filename ~src
  in
  structure

module Printer = struct
  let print_expr typ =
    Printtyp.reset_names ();
    Printtyp.reset_and_mark_loops typ;
    Res_doc.to_string ~width:60
      (Res_outcome_printer.print_out_type_doc
         (Printtyp.tree_of_typexp false typ))

  let print_decl ~rec_status name decl =
    Printtyp.reset_names ();
    Res_doc.to_string ~width:60
      (Res_outcome_printer.print_out_sig_item_doc
         (Printtyp.tree_of_type_declaration (Ident.create name) decl rec_status))
end

module Compile = struct
  (* Apparently it's not possible to retrieve the loc info from
   * Location.error_of_exn properly, so we need to do some extra
   * overloading action
   *)
  let warning_infos : LocWarnInfo.t array ref = ref [||]
  let warning_buffer = Buffer.create 512
  let warning_ppf = Format.formatter_of_buffer warning_buffer

  let flush_warning_buffer () =
    Format.pp_print_flush warning_ppf ();
    let str = Buffer.contents warning_buffer in
    Buffer.reset warning_buffer;
    str

  (* We need to overload the original warning printer to capture the warnings
     as an array *)
  let playground_warning_printer loc ppf w =
    match Warnings.report w with
    | `Inactive -> ()
    | `Active {Warnings.number; is_error} ->
      Location.default_warning_printer loc ppf w;
      let open LocWarnInfo in
      let full_msg = flush_warning_buffer () in
      let short_msg = Warnings.message w in
      let info = {full_msg; short_msg; warn_number = number; is_error; loc} in
      warning_infos := Array.append !warning_infos [|info|]

  let () =
    Location.formatter_for_warnings := warning_ppf;
    Location.warning_printer := playground_warning_printer

  let handle_err e =
    match error_of_exn e with
    | Some error ->
      (* This branch handles all
       * errors handled by the Location error reporting
       * system.
       *
       * Here we can differentiate between the different kinds
       * of error types just by looking at the raised exn names *)
      let type_ =
        match e with
        | Typetexp.Error _ | Typecore.Error _ | Typemod.Error _ -> "type_error"
        | _ -> "other_error"
      in
      let full_msg =
        Location.report_error Format.str_formatter error;
        Format.flush_str_formatter ()
      in
      let err = {full_msg; short_msg = error.msg; loc = error.loc} in
      ErrorRet.from_loc_errors ~type_ [|err|]
    | None -> (
      match e with
      | RescriptParsingErrors errors ->
        ErrorRet.from_syntax_errors (Array.of_list errors)
      | _ -> (
        let msg = Printexc.to_string e in
        match e with
        | Warnings.Errors -> ErrorRet.make_warning_error !warning_infos
        | _ -> ErrorRet.make_unexpected_error msg))

  (* Responsible for resetting all compiler state as if it were a new instance *)
  let reset_compiler () =
    warning_infos := [||];
    flush_warning_buffer () |> ignore;
    Warnings.reset_fatal ();
    Env.reset_cache_toplevel ()

  (* Collects the type information from the typed_tree, so we can use that
   * data to display types on hover etc.
   *
   * Note: start / end positions
   *)
  let collect_type_hints typed_tree =
    let open Typedtree in
    let create_type_hint_obj loc kind hint =
      let open Location in
      let _, startline, startcol = Location.get_pos_info loc.loc_start in
      let _, endline, endcol = Location.get_pos_info loc.loc_end in
      Js.Unsafe.(
        obj
          [|
            ( "start",
              inject
              @@ obj
                   [|
                     ( "line",
                       inject
                       @@ (startline |> float_of_int |> Js.number_of_float) );
                     ( "col",
                       inject @@ (startcol |> float_of_int |> Js.number_of_float)
                     );
                   |] );
            ( "end",
              inject
              @@ obj
                   [|
                     ( "line",
                       inject @@ (endline |> float_of_int |> Js.number_of_float)
                     );
                     ( "col",
                       inject @@ (endcol |> float_of_int |> Js.number_of_float)
                     );
                   |] );
            ("kind", inject @@ Js.string kind);
            ("hint", inject @@ Js.string hint);
          |])
    in
    let structure, _ = typed_tree in
    let acc = ref [] in
    let module Iter = TypedtreeIter.MakeIterator (struct
      include TypedtreeIter.DefaultIteratorArgument

      let cur_rec_status = ref None

      let enter_expression expr =
        let hint = Printer.print_expr expr.exp_type in
        let obj = create_type_hint_obj expr.exp_loc "expression" hint in
        acc := obj :: !acc

      let enter_binding binding =
        let hint = Printer.print_expr binding.vb_expr.exp_type in
        let obj = create_type_hint_obj binding.vb_loc "binding" hint in
        acc := obj :: !acc

      let enter_core_type ct =
        let hint = Printer.print_expr ct.ctyp_type in
        let obj = create_type_hint_obj ct.ctyp_loc "core_type" hint in
        acc := obj :: !acc

      let enter_type_declarations rec_flag =
        let status =
          match rec_flag with
          | Asttypes.Nonrecursive -> Types.Trec_not
          | Recursive -> Trec_first
        in
        cur_rec_status := Some status

      let enter_type_declaration tdecl =
        let open Types in
        match !cur_rec_status with
        | Some rec_status -> (
          let hint =
            Printer.print_decl ~rec_status tdecl.typ_name.Asttypes.txt
              tdecl.typ_type
          in
          let obj =
            create_type_hint_obj tdecl.typ_loc "type_declaration" hint
          in
          acc := obj :: !acc;
          match rec_status with
          | Trec_not | Trec_first -> cur_rec_status := Some Trec_next
          | _ -> ())
        | None -> ()
    end) in
    List.iter Iter.iter_structure_item structure.str_items;
    Js.array (!acc |> Array.of_list)

  let implementation ~(config : BundleConfig.t) ~lang str =
    let {BundleConfig.module_system; warn_flags; open_modules} = config in
    try
      reset_compiler ();
      Warnings.parse_options false warn_flags;
      let filename = get_filename ~lang config.filename in
      let modulename = "Playground" in
      let impl = rescript_parse ~filename in
      Clflags.open_modules := open_modules;
      (* let env = !Toploop.toplevel_env in *)
      (* Res_compmisc.init_path (); *)
      (* let modulename = module_of_filename ppf sourcefile outputprefix in *)
      (* Env.set_unit_name modulename; *)
      Lam_compile_env.reset ();
      let env = Res_compmisc.initial_env () in
      (* Question ?? *)
      (* let finalenv = ref Env.empty in *)
      let types_signature = ref [] in
      Js_config.jsx_version := Some Js_config.Jsx_v4;
      (* default *)
      let ast = impl str in
      let ast = Ppx_entry.rewrite_implementation ast in
      let typed_tree =
        let a, b, _, signature =
          Typemod.type_implementation_more modulename modulename modulename env
            ast
        in
        (* finalenv := c ; *)
        types_signature := signature;
        (a, b)
      in
      typed_tree |> Translmod.transl_implementation modulename
      |> (* Printlambda.lambda ppf *) fun (lam, exports) ->
      let buffer = Buffer.create 1000 in
      let () =
        Js_dump_program.pp_deps_program ~output_prefix:""
          (* does not matter here *) module_system
          (Lam_compile_main.compile "" exports lam)
          (Ext_pp.from_buffer buffer)
      in
      let v = Buffer.contents buffer in
      let type_hints = collect_type_hints typed_tree in
      Js.Unsafe.(
        obj
          [|
            ("js_code", inject @@ Js.string v);
            ( "warnings",
              inject
              @@ (!warning_infos
                 |> Array.map ErrorRet.make_warning
                 |> Js.array |> inject) );
            ("type_hints", inject @@ type_hints);
            ("type", inject @@ Js.string "success");
          |])
    with e -> (
      match e with
      | Arg.Bad msg -> ErrorRet.make_warning_flag_error ~warn_flags msg
      | _ -> handle_err e)

  let syntax_format ?(filename : string option) ~(from : Lang.t) ~(to_ : Lang.t)
      (src : string) =
    let filename = get_filename ~lang:from filename in
    try
      let code =
        match (from, to_) with
        | Res, Res ->
          (* Essentially pretty printing.
           * IMPORTANT: we need forPrinter:true when parsing code here,
           * otherwise we will loose some information for the ReScript printer *)
          let structure, comments =
            ResDriver.parse_implementation ~for_printer:true
              ~sourcefile:filename ~src
          in
          Res_printer.print_implementation ~width:80 structure ~comments
      in
      Js.Unsafe.(
        obj
          [|
            ("code", inject @@ Js.string code);
            ("fromLang", inject @@ Js.string (Lang.to_string from));
            ("toLang", inject @@ Js.string (Lang.to_string to_));
            ("type", inject @@ Js.string "success");
          |])
    with e -> handle_err e
end

(* To add a directory to the load path *)
let dir_directory d = Config.load_path := d :: !Config.load_path
let () = dir_directory "/static"

module Export = struct
  let make_compiler ~config ~lang =
    let open Js.Unsafe in
    let base_attrs =
      [|
        ( "compile",
          inject
          @@ Js.wrap_meth_callback (fun _ code ->
                 Compile.implementation ~config ~lang (Js.to_string code)) );
        ("version", inject @@ Js.string Bs_version.version);
      |]
    in
    let attrs =
      Array.append base_attrs
        [|
          ( "format",
            inject
            @@ Js.wrap_meth_callback (fun _ code ->
                   Compile.syntax_format ?filename:config.filename ~from:lang
                     ~to_:lang (Js.to_string code)) );
        |]
    in
    obj attrs

  (* Creates a "compiler instance" binding the configuration context to the specific compile / formatter functions *)
  let make () =
    let open Lang in
    let config = BundleConfig.make () in
    let set_module_system value =
      match value with
      | "esmodule" | "es6" ->
        config.module_system <- Ext_module_system.Esmodule;
        true
      | "commonjs" | "nodejs" ->
        config.module_system <- Commonjs;
        true
      | _ -> false
    in
    let set_filename value =
      config.filename <- Some value;
      true
    in
    let set_warn_flags value =
      config.warn_flags <- value;
      true
    in
    let set_open_modules value =
      config.open_modules <- value;
      true
    in
    let convert_syntax ~(from_lang : string) ~(to_lang : string) (src : string)
        =
      let open Lang in
      match (from_string from_lang, from_string to_lang) with
      | Some from, Some to_ ->
        Compile.syntax_format ?filename:config.filename ~from ~to_ src
      | other ->
        let msg =
          match other with
          | None, None ->
            "Unknown from / to language: " ^ from_lang ^ ", " ^ to_lang
          | None, Some _ -> "Unknown from language: " ^ from_lang
          | Some _, None -> "Unknown to language: " ^ to_lang
          | Some _, Some _ ->
            "Can't convert from " ^ from_lang ^ " to " ^ to_lang
        in
        ErrorRet.make_unexpected_error msg
    in
    Js.Unsafe.(
      obj
        [|
          ("version", inject @@ Js.string Bs_version.version);
          ("rescript", inject @@ make_compiler ~config ~lang:Res);
          ( "convertSyntax",
            inject
            @@ Js.wrap_meth_callback (fun _ from_lang to_lang src ->
                   convert_syntax ~from_lang:(Js.to_string from_lang)
                     ~to_lang:(Js.to_string to_lang) (Js.to_string src)) );
          ( "setModuleSystem",
            inject
            @@ Js.wrap_meth_callback (fun _ value ->
                   Js.bool (set_module_system (Js.to_string value))) );
          ( "setFilename",
            inject
            @@ Js.wrap_meth_callback (fun _ value ->
                   Js.bool (set_filename (Js.to_string value))) );
          ( "setWarnFlags",
            inject
            @@ Js.wrap_meth_callback (fun _ value ->
                   Js.bool (set_warn_flags (Js.to_string value))) );
          ( "setOpenModules",
            inject
            @@ Js.wrap_meth_callback (fun _ value ->
                   Js.bool
                     (set_open_modules
                        (value |> Js.to_array |> Array.map Js.to_string
                       |> Array.to_list))) );
          ( "getConfig",
            inject
            @@ Js.wrap_meth_callback (fun _ ->
                   Js.Unsafe.(
                     obj
                       [|
                         ( "module_system",
                           inject
                           @@ (config.module_system
                             |> BundleConfig.string_of_module_system
                             |> Js.string) );
                         ("warn_flags", inject @@ Js.string config.warn_flags);
                         ( "open_modules",
                           inject
                           @@ (config.open_modules |> Array.of_list |> Js.array)
                         );
                       |])) );
        |])
end

let () =
  export "rescript_compiler"
    Js.Unsafe.(
      obj
        [|
          ("api_version", inject @@ Js.string api_version);
          ("version", inject @@ Js.string Bs_version.version);
          ("make", inject @@ Export.make);
        |])
