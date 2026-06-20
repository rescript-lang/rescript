(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open Dead_common

let check_any_value_binding_with_no_side_effects ~config ~decls ~file
    ~(module_path : Module_path.t)
    ({vb_pat = {pat_desc}; vb_expr = expr; vb_loc = loc} :
      Typedtree.value_binding) =
  match pat_desc with
  | Tpat_any when (not (Side_effects.check_expr expr)) && not loc.loc_ghost ->
    let name = "_" |> Name.create ~is_interface:false in
    let path = module_path.path @ [File_context.module_name_tagged file] in
    name
    |> add_value_declaration ~config ~decls ~file ~path ~loc
         ~module_loc:module_path.loc ~side_effects:false
  | _ -> ()

let collect_value_binding ~config ~decls ~file ~(current_binding : Location.t)
    ~(module_path : Module_path.t) (vb : Typedtree.value_binding) =
  let old_last_binding = current_binding in
  check_any_value_binding_with_no_side_effects ~config ~decls ~file ~module_path
    vb;
  let loc =
    match vb.vb_pat.pat_desc with
    | Tpat_var (id, {loc = {loc_start; loc_ghost} as loc})
    | Tpat_alias
        ({pat_desc = Tpat_any}, id, {loc = {loc_start; loc_ghost} as loc})
      when (not loc_ghost) && not vb.vb_loc.loc_ghost ->
      let name = Ident.name id |> Name.create ~is_interface:false in
      let optional_args, optional_args_report =
        let open Decl.Kind in
        match vb.vb_expr.exp_desc with
        | Texp_function {arity = Some arity; _} ->
          ( vb.vb_expr.exp_type
            |> (fun texpr ->
            Dead_optional_args.from_type_expr_with_arity texpr arity)
            |> Optional_args.from_list,
            ReportOptionalArgs )
        | Texp_function _ ->
          ( vb.vb_expr.exp_type |> Dead_optional_args.from_type_expr
            |> Optional_args.from_list,
            ReportOptionalArgs )
        | _ ->
          ( vb.vb_expr.exp_type |> Dead_optional_args.from_type_expr
            |> Optional_args.from_list,
            NoOptionalArgReport )
      in
      let exists =
        match Declarations.find_opt_builder decls loc_start with
        | Some {decl_kind = Value r} ->
          r.optional_args_report <- optional_args_report;
          r.optional_args <- optional_args;
          true
        | _ -> false
      in
      let path = module_path.path @ [File_context.module_name_tagged file] in
      let is_first_class_module =
        match vb.vb_expr.exp_type.desc with
        | Tpackage _ -> true
        | _ -> false
      in
      (if (not exists) && not is_first_class_module then
         (* This is never toplevel currently *)
         let is_toplevel = old_last_binding = Location.none in
         let side_effects = Side_effects.check_expr vb.vb_expr in
         name
         |> add_value_declaration ~config ~decls ~file ~is_toplevel ~loc
              ~module_loc:module_path.loc ~optional_args_report ~optional_args
              ~path ~side_effects);
      (match Declarations.find_opt_builder decls loc_start with
      | None -> ()
      | Some decl ->
        (* Value bindings contain the correct location for the entire declaration: update final position.
           The previous value was taken from the signature, which only has positions for the id. *)
        let decl_kind =
          match decl.decl_kind with
          | Value vk ->
            Decl.Kind.Value
              {vk with side_effects = Side_effects.check_expr vb.vb_expr}
          | dk -> dk
        in
        Declarations.replace_builder decls loc_start
          {
            decl with
            decl_kind;
            pos_end = vb.vb_loc.loc_end;
            pos_start = vb.vb_loc.loc_start;
          });
      loc
    | _ -> current_binding
  in
  loc

let process_optional_args ~config ~cross_file ~exp_type ~(loc_from : Location.t)
    ~(binding : Location.t) ~loc_to ~path args =
  if exp_type |> Dead_optional_args.has_optional_args then (
    let supplied = ref [] in
    let supplied_maybe = ref [] in
    args
    |> List.iter (fun (lbl, arg) ->
           let arg_is_supplied =
             match arg with
             | Some
                 {
                   Typedtree.exp_desc =
                     Texp_construct (_, {cstr_name = "Some"}, _);
                 } ->
               Some true
             | Some
                 {
                   Typedtree.exp_desc =
                     Texp_construct (_, {cstr_name = "None"}, _);
                 } ->
               Some false
             | Some _ -> None
             | None -> Some false
           in
           match lbl with
           | Asttypes.Optional {txt = s} when not loc_from.loc_ghost ->
             if arg_is_supplied <> Some false then supplied := s :: !supplied;
             if arg_is_supplied = None then
               supplied_maybe := s :: !supplied_maybe
           | _ -> ());
    (!supplied, !supplied_maybe)
    |> Dead_optional_args.add_references ~config ~cross_file ~loc_from ~loc_to
         ~binding ~path)

let value_use_target_pos (e : Typedtree.expression) =
  match e.exp_desc with
  | Texp_ident (_, _, {Types.val_loc = {loc_ghost = false; _} as loc_to}) ->
    Some loc_to.loc_start
  | Texp_let
      ( Nonrecursive,
        [
          {
            vb_pat = {pat_desc = Tpat_var (id_arg, _)};
            vb_expr =
              {
                Typedtree.exp_desc =
                  Texp_ident
                    (_, _, {Types.val_loc = {loc_ghost = false; _} as loc_to});
                _;
              };
            _;
          };
        ],
        {
          Typedtree.exp_desc =
            Texp_function
              {
                case =
                  {
                    c_lhs = {pat_desc = Tpat_var (eta_arg, _)};
                    c_rhs =
                      {
                        Typedtree.exp_desc =
                          Texp_apply
                            {funct = {exp_desc = Texp_ident (id_arg2, _, _)}; _};
                        _;
                      };
                    _;
                  };
                _;
              };
          _;
        } )
    when Ident.name id_arg = "arg"
         && Ident.name eta_arg = "eta"
         && Path.name id_arg2 = "arg" ->
    Some loc_to.loc_start
  | _ -> None

let rec collect_expr ~config ~decls ~refs ~file_deps ~cross_file
    ~(last_binding : Location.t) super self (e : Typedtree.expression) =
  let loc_from = e.exp_loc in
  let binding = last_binding in
  let require_direct_optional_arg_call pos =
    match Declarations.find_opt_builder decls pos with
    | Some
        ({
           decl_kind =
             Value
               ({optional_args_report = Decl.Kind.ReportOptionalArgs} as
                value_kind);
         } as decl) ->
      Declarations.replace_builder decls pos
        {
          decl with
          decl_kind =
            Value
              {
                value_kind with
                optional_args_report = Decl.Kind.ReportOptionalArgsIfDirectCall;
              };
        }
    | _ -> ()
  in
  (match e.exp_desc with
  | Texp_ident (_path, _, {Types.val_loc = {loc_ghost = false; _} as loc_to}) ->
    (* if Path.name _path = "rc" then assert false; *)
    if loc_from = loc_to && _path |> Path.name = "emptyArray" then (
      (* Work around lowercase jsx with no children producing an artifact `emptyArray`
         which is called from its own location as many things are generated on the same location. *)
      if config.Dce_config.cli.debug then
        Log_.item "addDummyReference %s --> %s@."
          (Location.none.loc_start |> Pos.to_string)
          (loc_to.loc_start |> Pos.to_string);
      References.add_value_ref refs ~pos_to:loc_to.loc_start
        ~pos_from:Location.none.loc_start)
    else
      add_value_reference ~config ~refs ~file_deps ~binding
        ~add_file_reference:true ~loc_from ~loc_to
  | Texp_apply
      {
        funct =
          {
            exp_desc =
              Texp_ident
                (path, _, {Types.val_loc = {loc_ghost = false; _} as loc_to});
            exp_type;
          };
        args;
      } ->
    args
    |> process_optional_args ~config ~cross_file ~exp_type
         ~loc_from:(loc_from : Location.t)
         ~binding:last_binding ~loc_to ~path;
    args
    |> List.iter (fun (_, arg) ->
           match arg with
           | Some arg ->
             arg |> value_use_target_pos
             |> Option.iter require_direct_optional_arg_call
           | _ -> ())
  | Texp_let
      ( (* generated for functions with optional args *)
        Nonrecursive,
        [
          {
            vb_pat = {pat_desc = Tpat_var (id_arg, _)};
            vb_expr =
              {
                exp_desc =
                  Texp_ident
                    (path, _, {Types.val_loc = {loc_ghost = false; _} as loc_to});
                exp_type;
              };
          };
        ],
        {
          exp_desc =
            Texp_function
              {
                case =
                  {
                    c_lhs = {pat_desc = Tpat_var (eta_arg, _)};
                    c_rhs =
                      {
                        exp_desc =
                          Texp_apply
                            {
                              funct = {exp_desc = Texp_ident (id_arg2, _, _)};
                              args;
                            };
                      };
                  };
              };
        } )
    when Ident.name id_arg = "arg"
         && Ident.name eta_arg = "eta"
         && Path.name id_arg2 = "arg" ->
    args
    |> process_optional_args ~config ~cross_file ~exp_type
         ~loc_from:(loc_from : Location.t)
         ~binding:last_binding ~loc_to ~path
  | Texp_field
      (_, _, {lbl_loc = {Location.loc_start = pos_to; loc_ghost = false}; _}) ->
    if !Config.analyze_types then
      Dead_type.add_type_reference ~config ~refs ~pos_to
        ~pos_from:loc_from.loc_start
  | Texp_construct
      ( _,
        {
          cstr_loc = {Location.loc_start = pos_to; loc_ghost} as loc_to;
          cstr_tag;
        },
        _ ) ->
    (match cstr_tag with
    | Cstr_extension path ->
      path
      |> Dead_exception.mark_as_used ~config ~refs ~file_deps ~cross_file
           ~binding ~loc_from ~loc_to
    | _ -> ());
    if !Config.analyze_types && not loc_ghost then
      Dead_type.add_type_reference ~config ~refs ~pos_to
        ~pos_from:loc_from.loc_start
  | Texp_record {fields} ->
    fields
    |> Array.iter (fun (_, record_label_definition, _) ->
           match record_label_definition with
           | Typedtree.Overridden (_, e) -> (
             e |> value_use_target_pos
             |> Option.iter require_direct_optional_arg_call;
             match e with
             | {exp_loc; _} when exp_loc.loc_ghost ->
               (* Punned field in OCaml projects has ghost location in expression *)
               let e = {e with exp_loc = {exp_loc with loc_ghost = false}} in
               collect_expr ~config ~decls ~refs ~file_deps ~cross_file
                 ~last_binding super self e
               |> ignore
             | _ -> ())
           | _ -> ())
  | _ -> ());
  super.Tast_mapper.expr self e

(*
  type k. is a locally abstract type
  https://caml.inria.fr/pub/docs/manual-ocaml/locallyabstract.html
  it is required because in ocaml >= 4.11 Typedtree.pattern and ADT is converted
  in a GADT
  https://github.com/ocaml/ocaml/commit/312253ce822c32740349e572498575cf2a82ee96
  in short: all branches of pattern matches aren't the same type.
  With this annotation we declare a new type for each branch to allow the
  function to be typed.
  *)
let type_path_candidates ~file ~(module_path : Module_path.t) path =
  let path = Dce_path.from_path_t path in
  let module_context =
    module_path.path @ [File_context.module_name_tagged file]
  in
  let add_unique paths path =
    if List.exists (fun existing -> existing = path) paths then paths
    else path :: paths
  in
  [path; path @ module_context]
  |> List.fold_left
       (fun paths path ->
         [
           path;
           Dce_path.module_to_implementation path;
           Dce_path.module_to_interface path;
         ]
         |> List.fold_left add_unique paths)
       []

let add_record_label_type_references ~config ~refs ~pos_from labels =
  labels
  |> List.iter (fun {Types.ld_loc = {loc_start = pos_to; loc_ghost}; _} ->
         if not loc_ghost then
           Dead_type.add_type_reference ~config ~refs ~pos_from ~pos_to)

let add_record_rest_type_references_from_path ~config ~decls ~refs ~file
    ~module_path ~pos_from rest =
  if !Config.analyze_types then
    match (Ctype.repr rest.Typedtree.rest_type).desc with
    | Types.Tconstr (path, _, _) ->
      let type_paths = type_path_candidates ~file ~module_path path in
      decls |> Declarations.builder_to_list
      |> List.iter (fun (_, decl) ->
             match (decl.Decl.decl_kind, decl.path) with
             | RecordLabel, _label :: type_path
               when List.exists
                      (fun candidate -> candidate = type_path)
                      type_paths ->
               Dead_type.add_type_reference ~config ~refs ~pos_from
                 ~pos_to:decl.pos
             | _ -> ())
    | _ -> ()

let add_record_rest_type_references ~config ~decls ~refs ~file ~module_path
    ~pos_from ~env rest =
  if !Config.analyze_types then
    match
      try Some (Ctype.extract_concrete_typedecl env rest.Typedtree.rest_type)
      with Not_found -> None
    with
    | Some (_, _, {Types.type_kind = Type_record (labels, _)}) ->
      add_record_label_type_references ~config ~refs ~pos_from labels
    | _ ->
      add_record_rest_type_references_from_path ~config ~decls ~refs ~file
        ~module_path ~pos_from rest

let collect_pattern ~config ~decls ~refs ~file ~module_path :
    _ -> _ -> Typedtree.pattern -> Typedtree.pattern =
 fun super self pat ->
  let pos_from = pat.Typedtree.pat_loc.loc_start in
  (match pat.pat_desc with
  | Typedtree.Tpat_record (cases, _clodsedFlag, rest) -> (
    cases
    |> List.iter (fun (_loc, {Types.lbl_loc = {loc_start = pos_to}}, _pat, _) ->
           if !Config.analyze_types then
             Dead_type.add_type_reference ~config ~refs ~pos_from ~pos_to);
    match rest with
    | None -> ()
    | Some rest ->
      add_record_rest_type_references ~config ~decls ~refs ~file ~module_path
        ~pos_from:rest.rest_name.loc.loc_start ~env:pat.pat_env rest)
  | _ -> ());
  super.Tast_mapper.pat self pat

let rec get_signature (module_type : Types.module_type) =
  match module_type with
  | Mty_signature signature -> signature
  | Mty_functor (_, _mtParam, mt) -> get_signature mt
  | _ -> []

let rec process_signature_item ~config ~decls ~file ~do_types ~do_values
    ~module_loc ~(module_path : Module_path.t) ~path (si : Types.signature_item)
    =
  match si with
  | Sig_type (id, t, _) when do_types ->
    if !Config.analyze_types then
      (* Extract manifest type path for type re-exports (type y = x = {...}).
         Use full Path.t so cross-module re-exports work (Path.Pdot, aliases, etc.). *)
      let manifest_type_path =
        match t.type_manifest with
        | Some {desc = Tconstr (path, _, _)} -> (
          let p = path |> Dce_path.from_path_t in
          match p with
          | [type_name] ->
            let module_context =
              module_path.path @ [File_context.module_name_tagged file]
            in
            Some (type_name :: module_context)
          | _ ->
            Some
              (if File_context.is_interface file then
                 Dce_path.module_to_interface p
               else Dce_path.module_to_implementation p))
        | _ -> None
      in
      Dead_type.add_declaration ~config ~decls ~file ~module_path ~type_id:id
        ~type_kind:t.type_kind ~manifest_type_path
  | Sig_value (id, {Types.val_loc = loc; val_kind = kind; val_type})
    when do_values ->
    if not loc.Location.loc_ghost then
      let is_primitive =
        match kind with
        | Val_prim _ -> true
        | _ -> false
      in
      if (not is_primitive) || !Config.analyze_externals then
        let optional_args =
          val_type |> Dead_optional_args.from_type_expr
          |> Optional_args.from_list
        in
        let optional_args_report =
          if Optional_args.is_empty optional_args then
            Decl.Kind.NoOptionalArgReport
          else Decl.Kind.ReportOptionalArgs
        in

        (* if Ident.name id = "someValue" then
           Printf.printf "XXX %s\n" (Ident.name id); *)
        Ident.name id
        |> Name.create ~is_interface:false
        |> add_value_declaration ~config ~decls ~file ~loc ~module_loc
             ~optional_args_report ~optional_args ~path ~side_effects:false
  | Sig_module (id, {Types.md_type = module_type; md_loc = module_loc}, _)
  | Sig_modtype (id, {Types.mtd_type = Some module_type; mtd_loc = module_loc})
    ->
    let modulePath' =
      Module_path.enter_module module_path
        ~name:(id |> Ident.name |> Name.create)
        ~loc:module_loc
    in
    let collect =
      match si with
      | Sig_modtype _ -> false
      | _ -> true
    in
    if collect then
      get_signature module_type
      |> List.iter
           (process_signature_item ~config ~decls ~file ~do_types ~do_values
              ~module_loc ~module_path:modulePath'
              ~path:((id |> Ident.name |> Name.create) :: path))
  | _ -> ()

(* Traverse the AST *)
let traverse_structure ~config ~decls ~refs ~file_deps ~cross_file ~file
    ~do_types ~do_externals (structure : Typedtree.structure) : unit =
  let rec create_mapper (last_binding : Location.t)
      (module_path : Module_path.t) =
    let super = Tast_mapper.default in
    let rec mapper =
      {
        super with
        expr =
          (fun _self e ->
            e
            |> collect_expr ~config ~decls ~refs ~file_deps ~cross_file
                 ~last_binding super mapper);
        pat =
          (fun _self p ->
            p
            |> collect_pattern ~config ~decls ~refs ~file ~module_path super
                 mapper);
        structure_item =
          (fun _self (structure_item : Typedtree.structure_item) ->
            let modulePath_for_item_opt =
              match structure_item.str_desc with
              | Tstr_module {mb_expr; mb_id; mb_loc} ->
                let has_interface =
                  match mb_expr.mod_desc with
                  | Tmod_constraint _ -> true
                  | _ -> false
                in
                let modulePath' =
                  Module_path.enter_module module_path
                    ~name:(mb_id |> Ident.name |> Name.create)
                    ~loc:mb_loc
                in
                if has_interface then
                  match mb_expr.mod_type with
                  | Mty_signature signature ->
                    signature
                    |> List.iter
                         (process_signature_item ~config ~decls ~file ~do_types
                            ~do_values:false ~module_loc:mb_expr.mod_loc
                            ~module_path:modulePath'
                            ~path:
                              (modulePath'.path
                              @ [File_context.module_name_tagged file]))
                  | _ -> ()
                else ();
                Some modulePath'
              | Tstr_primitive vd when do_externals && !Config.analyze_externals
                ->
                let path =
                  module_path.path @ [File_context.module_name_tagged file]
                in
                let exists =
                  match
                    Declarations.find_opt_builder decls vd.val_loc.loc_start
                  with
                  | Some {decl_kind = Value _} -> true
                  | _ -> false
                in
                let id = vd.val_id |> Ident.name in
                Printf.printf "Primitive %s\n" id;
                if
                  (not exists) && id <> "unsafe_expr"
                  (* see https://github.com/BuckleScript/bucklescript/issues/4532 *)
                then
                  id
                  |> Name.create ~is_interface:false
                  |> add_value_declaration ~config ~decls ~file ~path
                       ~loc:vd.val_loc ~module_loc:module_path.loc
                       ~side_effects:false;
                None
              | Tstr_type (_recFlag, type_declarations) when do_types ->
                if !Config.analyze_types then
                  type_declarations
                  |> List.iter
                       (fun (type_declaration : Typedtree.type_declaration) ->
                         (* Extract manifest type path for type re-exports (type y = x = {...}). *)
                         let manifest_type_path =
                           match type_declaration.typ_manifest with
                           | Some {ctyp_desc = Ttyp_constr (path, _, _)} -> (
                             let p = path |> Dce_path.from_path_t in
                             match p with
                             | [type_name] ->
                               let module_context =
                                 module_path.path
                                 @ [File_context.module_name_tagged file]
                               in
                               Some (type_name :: module_context)
                             | _ ->
                               Some
                                 (if File_context.is_interface file then
                                    Dce_path.module_to_interface p
                                  else Dce_path.module_to_implementation p))
                           | _ -> None
                         in
                         Dead_type.add_declaration ~config ~decls ~file
                           ~module_path ~type_id:type_declaration.typ_id
                           ~type_kind:type_declaration.typ_type.type_kind
                           ~manifest_type_path);
                None
              | Tstr_include {incl_mod; incl_type} ->
                (match incl_mod.mod_desc with
                | Tmod_ident (_path, _lid) ->
                  let current_path =
                    module_path.path @ [File_context.module_name_tagged file]
                  in
                  incl_type
                  |> List.iter
                       (process_signature_item ~config ~decls ~file ~do_types
                          ~do_values:false (* TODO: also values? *)
                          ~module_loc:incl_mod.mod_loc ~module_path
                          ~path:current_path)
                | _ -> ());
                None
              | Tstr_exception {ext_id = id; ext_loc = loc} ->
                let path =
                  module_path.path @ [File_context.module_name_tagged file]
                in
                let name = id |> Ident.name |> Name.create in
                ignore
                  (Dead_exception.add ~config ~decls ~file ~path ~loc
                     ~str_loc:structure_item.str_loc ~module_loc:module_path.loc
                     name);
                None
              | _ -> None
            in
            let mapper_for_item =
              match modulePath_for_item_opt with
              | None -> mapper
              | Some modulePath_for_item ->
                create_mapper last_binding modulePath_for_item
            in
            super.structure_item mapper_for_item structure_item);
        value_binding =
          (fun _self vb ->
            let loc =
              vb
              |> collect_value_binding ~config ~decls ~file
                   ~current_binding:last_binding ~module_path
            in
            let nested_mapper = create_mapper loc module_path in
            super.Tast_mapper.value_binding nested_mapper vb);
      }
    in
    mapper
  in
  let mapper = create_mapper Location.none Module_path.initial in
  mapper.structure mapper structure |> ignore

(* Merge a location's references to another one's *)
let process_value_dependency ~config ~decls ~refs ~file_deps ~cross_file
    ( ({
         val_loc =
           {loc_start = {pos_fname = fn_to} as pos_to; loc_ghost = ghost1} as
           loc_to;
       } :
        Types.value_description),
      ({
         val_loc =
           {loc_start = {pos_fname = fn_from} as pos_from; loc_ghost = ghost2}
           as loc_from;
       } :
        Types.value_description) ) =
  if (not ghost1) && (not ghost2) && pos_to <> pos_from then (
    let add_file_reference = file_is_implementation_of fn_to fn_from in
    add_value_reference ~config ~refs ~file_deps ~binding:Location.none
      ~add_file_reference ~loc_from ~loc_to;
    Dead_optional_args.add_function_reference ~config ~decls ~cross_file
      ~loc_from ~loc_to)

let process_structure ~config ~decls ~refs ~file_deps ~cross_file ~file
    ~cmt_value_dependencies ~do_types ~do_externals
    (structure : Typedtree.structure) =
  traverse_structure ~config ~decls ~refs ~file_deps ~cross_file ~file ~do_types
    ~do_externals structure;
  let value_dependencies = cmt_value_dependencies |> List.rev in
  value_dependencies
  |> List.iter
       (process_value_dependency ~config ~decls ~refs ~file_deps ~cross_file)
