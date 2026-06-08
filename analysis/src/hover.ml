open Shared_types

module String_set = Set.Make (String)

let show_module_top_level ~docstring ~is_type ~name
    (top_level : Module.item list) =
  let contents =
    top_level
    |> List.map (fun item ->
           match item.Module.kind with
           (* TODO pretty print module contents *)
           | Type ({decl}, rec_status) ->
             "  " ^ (decl |> Shared.decl_to_string ~rec_status item.name)
           | Module _ -> "  module " ^ item.name
           | Value typ ->
             "  let " ^ item.name ^ ": " ^ (typ |> Shared.type_to_string))
    (* TODO indent *)
    |> String.concat "\n"
  in
  let name = Utils.cut_after_dash name in
  let full =
    Markdown.code_block
      ("module "
      ^ (if is_type then "type " ^ name ^ " = " else name ^ ": ")
      ^ "{" ^ "\n" ^ contents ^ "\n}")
  in
  let doc =
    match docstring with
    | [] -> ""
    | _ :: _ ->
      "\n"
      ^ (docstring |> String.concat "\n")
      ^ Markdown.divider ^ Markdown.spacing
  in
  Some (doc ^ full)

let rec show_module ~state ~docstring ~(file : File.t) ~package ~name
    (declared : Module.t Declared.t option) =
  match declared with
  | None ->
    show_module_top_level ~docstring ~is_type:false ~name file.structure.items
  | Some {item = Structure {items}; module_path} ->
    let is_type =
      match module_path with
      | ExportedModule {is_type} -> is_type
      | _ -> false
    in
    show_module_top_level ~docstring ~is_type ~name items
  | Some ({item = Constraint (_moduleItem, module_type_item)} as declared) ->
    (* show the interface *)
    show_module ~state ~docstring ~file ~name ~package
      (Some {declared with item = module_type_item})
  | Some ({item = Ident path} as declared) -> (
    match
      References.resolve_module_reference ~state ~file ~package declared
    with
    | None -> Some ("Unable to resolve module reference " ^ Path.name path)
    | Some (_, declared) ->
      show_module ~state ~docstring ~file ~name ~package declared)

type extracted_type = {
  name: string;
  path: Path.t;
  decl: Types.type_declaration;
  env: Shared_types.Query_env.t;
  loc: Warnings.loc;
}

let find_relevant_types_from_type ~state ~file ~package typ =
  (* Expand definitions of types mentioned in typ.
     If typ itself is a record or variant, search its body *)
  let env = Query_env.from_file file in
  let env_to_search, types_to_search =
    match typ |> Shared.dig_constructor with
    | Some path -> (
      let label_declarations_types lds =
        lds |> List.map (fun (ld : Types.label_declaration) -> ld.ld_type)
      in
      match References.dig_constructor ~state ~env ~package path with
      | None -> (env, [typ])
      | Some (env1, {item = {decl}}) -> (
        match decl.type_kind with
        | Type_record (lds, _) ->
          (env1, typ :: (lds |> label_declarations_types))
        | Type_variant cds ->
          ( env1,
            cds
            |> List.map (fun (cd : Types.constructor_declaration) ->
                   let from_args =
                     match cd.cd_args with
                     | Cstr_tuple ts -> ts
                     | Cstr_record lds -> lds |> label_declarations_types
                   in
                   typ
                   ::
                   (match cd.cd_res with
                   | None -> from_args
                   | Some t -> t :: from_args))
            |> List.flatten )
        | _ -> (env, [typ])))
    | None -> (env, [typ])
  in
  let from_constructor_path ~env path =
    match References.dig_constructor ~state ~env ~package path with
    | None -> None
    | Some (env, {name = {txt}; extent_loc; item = {decl}}) ->
      if Utils.is_uncurried_internal path then None
      else Some {name = txt; env; loc = extent_loc; decl; path}
  in
  let constructors = Shared.find_type_constructors types_to_search in
  constructors |> List.filter_map (from_constructor_path ~env:env_to_search)

let expand_types ~state ~file ~package ~supports_markdown_links typ =
  match find_relevant_types_from_type ~state typ ~file ~package with
  | {decl; path} :: _
    when Res_parsetree_viewer.has_inline_record_definition_attribute
           decl.type_attributes ->
    (* We print inline record types just with their definition, not the constr pointing
    to them, since that doesn't make sense to show the user. *)
    ( [
        Markdown.code_block
          (decl
          |> Shared.decl_to_string ~print_name_as_is:true
               (Shared_types.path_ident_to_string path));
      ],
      `InlineType )
  | all ->
    let types_seen = ref String_set.empty in
    let type_id ~(env : Query_env.t) ~name =
      env.file.module_name :: List.rev (name :: env.path_rev)
      |> String.concat "."
    in
    ( all
      (* Don't produce duplicate type definitions for recursive types *)
      |> List.filter (fun {env; name} ->
             let type_id = type_id ~env ~name in
             if String_set.mem type_id !types_seen then false
             else (
               types_seen := String_set.add type_id !types_seen;
               true))
      |> List.map (fun {decl; env; loc; path} ->
             let link_to_type_definition_str =
               if
                 supports_markdown_links
                 && not
                      (Res_parsetree_viewer
                       .has_inline_record_definition_attribute
                         decl.type_attributes)
               then
                 Markdown.go_to_definition_text ~env ~pos:loc.Warnings.loc_start
               else ""
             in
             Markdown.divider
             ^ (if supports_markdown_links then Markdown.spacing else "")
             ^ Markdown.code_block
                 (decl
                 |> Shared.decl_to_string ~print_name_as_is:true
                      (Shared_types.path_ident_to_string path))
             ^ link_to_type_definition_str ^ "\n"),
      `Default )

(* Produces a hover with relevant types expanded in the main type being hovered. *)
let hover_with_expanded_types ~state ~file ~package ~supports_markdown_links
    ?docstring ?constructor typ =
  let expanded_types, expansion_type =
    expand_types ~state ~file ~package ~supports_markdown_links typ
  in
  match expansion_type with
  | `Default ->
    let type_string = Shared.type_to_string typ in
    let type_string =
      match constructor with
      | Some constructor ->
        type_string ^ "\n" ^ Completion_back_end.show_constructor constructor
      | None -> type_string
    in
    let type_string =
      match docstring with
      | Some [] | None -> Markdown.code_block type_string
      | Some docstring ->
        Markdown.code_block type_string
        ^ Markdown.divider
        ^ (docstring |> String.concat "\n")
    in
    type_string :: expanded_types |> String.concat "\n"
  | `InlineType -> expanded_types |> String.concat "\n"

(* Leverages autocomplete functionality to produce a hover for a position. This
   makes it (most often) work with unsaved content. *)
let get_hover_via_completions ~state ~debug ~source ~kind_file ~pos ~for_hover
    ~supports_markdown_links ~full =
  match
    Completions.get_completions ~debug ~source ~kind_file ~pos ~for_hover ~full
      ~state
  with
  | None -> None
  | Some (completions, ({file; package} as full), scope) -> (
    let raw_opens = Scope.get_raw_opens scope in
    match completions with
    | {kind = Label typ_string; docstring} :: _ ->
      let parts =
        docstring
        @ if typ_string = "" then [] else [Markdown.code_block typ_string]
      in

      Some (String.concat "\n\n" parts)
    | {kind = Field _; env; docstring} :: _ -> (
      let opens =
        Completion_back_end.get_opens ~state ~debug ~raw_opens ~package ~env
      in
      match
        Completion_back_end.completions_get_type_env2 ~state ~debug ~full
          ~raw_opens ~opens ~pos completions
      with
      | Some (typ, _env) ->
        let type_string =
          hover_with_expanded_types ~state ~file ~package ~docstring
            ~supports_markdown_links typ
        in
        Some type_string
      | None -> None)
    | {env} :: _ -> (
      let opens =
        Completion_back_end.get_opens ~state ~debug ~raw_opens ~package ~env
      in
      match
        Completion_back_end.completions_get_type_env2 ~state ~debug ~full
          ~raw_opens ~opens ~pos completions
      with
      | Some (typ, _env) ->
        let type_string =
          hover_with_expanded_types ~state ~file ~package
            ~supports_markdown_links typ
        in
        Some type_string
      | None -> None)
    | _ -> None)

let new_hover ~state ~full:{file; package} ~supports_markdown_links loc_item =
  match loc_item.loc_type with
  | TypeDefinition (name, decl, _stamp) -> (
    let type_def = Markdown.code_block (Shared.decl_to_string name decl) in
    match decl.type_manifest with
    | None -> Some type_def
    | Some typ -> (
      let expanded_types, expansion_type =
        expand_types ~state ~file ~package ~supports_markdown_links typ
      in
      match expansion_type with
      | `Default -> Some (type_def :: expanded_types |> String.concat "\n")
      | `InlineType -> Some (expanded_types |> String.concat "\n")))
  | LModule (Definition (stamp, _tip)) | LModule (LocalReference (stamp, _tip))
    -> (
    match Stamps.find_module file.stamps stamp with
    | None -> None
    | Some md -> (
      match References.resolve_module_reference ~state ~file ~package md with
      | None -> None
      | Some (file, declared) ->
        let name, docstring =
          match declared with
          | Some d -> (d.name.txt, d.docstring)
          | None -> (file.module_name, file.structure.docstring)
        in
        show_module ~state ~docstring ~name ~file declared ~package))
  | LModule (GlobalReference (module_name, path, tip)) -> (
    match Process_cmt.file_for_module ~state ~package module_name with
    | None -> None
    | Some file -> (
      let env = Query_env.from_file file in
      match References.exported_for_tip ~state ~env ~path ~package ~tip with
      | None -> None
      | Some (_env, _name, stamp) -> (
        match Stamps.find_module file.stamps stamp with
        | None -> None
        | Some md -> (
          match
            References.resolve_module_reference ~state ~file ~package md
          with
          | None -> None
          | Some (file, declared) ->
            let name, docstring =
              match declared with
              | Some d -> (d.name.txt, d.docstring)
              | None -> (file.module_name, file.structure.docstring)
            in
            show_module ~state ~docstring ~name ~file ~package declared))))
  | LModule NotFound -> None
  | TopLevelModule name -> (
    match Process_cmt.file_for_module ~state ~package name with
    | None -> None
    | Some file ->
      show_module ~state ~docstring:file.structure.docstring
        ~name:file.module_name ~file ~package None)
  | Typed (_, _, Definition (_, (Field _ | Constructor _))) -> None
  | Constant t ->
    Some
      (Markdown.code_block
         (match t with
         | Const_int _ -> "int"
         | Const_char _ -> "char"
         | Const_string _ -> "string"
         | Const_float _ -> "float"
         | Const_int32 _ -> "int32"
         | Const_int64 _ -> "int64"
         | Const_bigint _ -> "bigint"))
  | Typed (_, t, loc_kind) -> (
    let from_type ?docstring ?constructor typ =
      hover_with_expanded_types ~state ~file ~package ~supports_markdown_links
        ?docstring ?constructor typ
    in
    (* Expand first-class modules to the underlying module type signature. *)
    let t = Shared.dig t in
    match t.desc with
    | Tpackage (path, _lids, _tys) -> (
      let env = Query_env.from_file file in
      match
        Resolve_path.resolve_module_from_compiler_path ~state ~env ~package path
      with
      | None -> Some (from_type t)
      | Some (env_for_module, Some declared) ->
        let name = Path.name path in
        show_module ~state ~docstring:declared.docstring ~name
          ~file:env_for_module.file ~package (Some declared)
      | Some (_, None) -> Some (from_type t))
    | _ ->
      Some
        (match References.defined_for_loc ~state ~file ~package loc_kind with
        | None -> t |> from_type
        | Some (docstring, res) -> (
          match res with
          | `Declared | `Field -> t |> from_type ~docstring
          | `Constructor constructor ->
            t |> from_type ~docstring:constructor.docstring ~constructor)))
