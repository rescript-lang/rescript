open Shared_types

let show_constructor {Constructor.cname = {txt}; args; res} =
  txt
  ^ (match args with
    | Args [] -> ""
    | InlineRecord fields ->
      "({"
      ^ (fields
        |> List.map (fun (field : field) ->
               Printf.sprintf "%s%s: %s" field.fname.txt
                 (if field.optional then "?" else "")
                 (Shared.type_to_string
                    (if field.optional then Utils.unwrap_if_option field.typ
                     else field.typ)))
        |> String.concat ", ")
      ^ "})"
    | Args args ->
      "("
      ^ (args
        |> List.map (fun (typ, _) -> typ |> Shared.type_to_string)
        |> String.concat ", ")
      ^ ")")
  ^
  match res with
  | None -> ""
  | Some typ -> "\n" ^ (typ |> Shared.type_to_string)

(* TODO: local opens *)
let resolve_opens ~env opens ~package ~state =
  List.fold_left
    (fun previous path ->
      (* Finding an open, first trying to find it in previoulsly resolved opens *)
      let rec loop prev =
        match prev with
        | [] -> (
          match path with
          | [] | [_] -> previous
          | name :: path -> (
            match Process_cmt.file_for_module ~state ~package name with
            | None ->
              Log.log ("Could not get module " ^ name);
              previous (* TODO: warn? *)
            | Some file -> (
              match
                Resolve_path.resolve_path ~env:(Query_env.from_file file) ~state
                  ~package ~path
              with
              | None ->
                Log.log ("Could not resolve in " ^ name);
                previous
              | Some (env, _placeholder) -> previous @ [env])))
        | env :: rest -> (
          match Resolve_path.resolve_path ~state ~env ~package ~path with
          | None -> loop rest
          | Some (env, _placeholder) -> previous @ [env])
      in
      Log.log ("resolving open " ^ path_to_string path);
      match Resolve_path.resolve_path ~state ~env ~package ~path with
      | None ->
        Log.log "Not local";
        loop previous
      | Some (env, _) ->
        Log.log "Was local";
        previous @ [env])
    (* loop(previous) *)
    [] opens

let completion_for_exporteds iter_exported get_declared ~prefix ~exact ~env
    ~names_used transform_contents =
  let res = ref [] in
  iter_exported (fun name stamp ->
      (* Log.log("checking exported: " ++ name); *)
      if Utils.check_name name ~prefix ~exact then
        match get_declared stamp with
        | Some (declared : _ Declared.t)
          when not (Hashtbl.mem names_used declared.name.txt) ->
          Hashtbl.add names_used declared.name.txt ();
          res :=
            {
              (Completion.create declared.name.txt ~env
                 ~kind:(transform_contents declared))
              with
              deprecated = declared.deprecated;
              docstring = declared.docstring;
            }
            :: !res
        | _ -> ());
  !res

let completion_for_exported_modules ~env ~prefix ~exact ~names_used =
  completion_for_exporteds
    (Exported.iter env.Query_env.exported Exported.Module)
    (Stamps.find_module env.file.stamps) ~prefix ~exact ~env ~names_used
    (fun declared ->
      Completion.Module
        {docstring = declared.docstring; module_ = declared.item})

let completion_for_exported_values ~env ~prefix ~exact ~names_used =
  completion_for_exporteds (Exported.iter env.Query_env.exported Exported.Value)
    (Stamps.find_value env.file.stamps) ~prefix ~exact ~env ~names_used
    (fun declared -> Completion.Value declared.item)

let completion_for_exported_types ~env ~prefix ~exact ~names_used =
  completion_for_exporteds (Exported.iter env.Query_env.exported Exported.Type)
    (Stamps.find_type env.file.stamps) ~prefix ~exact ~env ~names_used
    (fun declared -> Completion.Type declared.item)

let completions_for_exported_constructors ~(env : Query_env.t) ~prefix ~exact
    ~names_used =
  let res = ref [] in
  Exported.iter env.exported Exported.Type (fun _name stamp ->
      match Stamps.find_type env.file.stamps stamp with
      | Some ({item = {kind = Type.Variant constructors}} as t) ->
        res :=
          (constructors
          |> List.filter (fun c ->
                 Utils.check_name c.Constructor.cname.txt ~prefix ~exact)
          |> Utils.filter_map (fun c ->
                 let name = c.Constructor.cname.txt in
                 if not (Hashtbl.mem names_used name) then
                   let () = Hashtbl.add names_used name () in
                   Some
                     (Completion.create name ~env ~docstring:c.docstring
                        ?deprecated:c.deprecated
                        ~kind:
                          (Completion.Constructor
                             (c, t.item.decl |> Shared.decl_to_string t.name.txt)))
                 else None))
          @ !res
      | _ -> ());
  !res

let completion_for_exported_fields ~(env : Query_env.t) ~prefix ~exact
    ~names_used =
  let res = ref [] in
  Exported.iter env.exported Exported.Type (fun _name stamp ->
      match Stamps.find_type env.file.stamps stamp with
      | Some ({item = {kind = Record fields}} as t) ->
        res :=
          (fields
          |> List.filter (fun f -> Utils.check_name f.fname.txt ~prefix ~exact)
          |> Utils.filter_map (fun f ->
                 let name = f.fname.txt in
                 if not (Hashtbl.mem names_used name) then
                   let () = Hashtbl.add names_used name () in
                   Some
                     (Completion.create name ~env ~docstring:f.docstring
                        ?deprecated:f.deprecated
                        ~kind:
                          (Completion.Field
                             (f, t.item.decl |> Shared.decl_to_string t.name.txt)))
                 else None))
          @ !res
      | _ -> ());
  !res

let find_module_in_scope ~env ~module_name ~scope =
  let modules_table = Hashtbl.create 10 in
  env.Query_env.file.stamps
  |> Stamps.iter_modules (fun _ declared ->
         Hashtbl.replace modules_table
           (declared.name.txt, declared.extent_loc |> Loc.start)
           declared);
  let result = ref None in
  let process_module name loc =
    if name = module_name && !result = None then
      match Hashtbl.find_opt modules_table (name, Loc.start loc) with
      | Some declared -> result := Some declared
      | None ->
        Log.log
          (Printf.sprintf "Module Not Found %s loc:%s\n" name
             (Loc.to_string loc))
  in
  scope |> Scope.iter_modules_before_first_open process_module;
  scope |> Scope.iter_modules_after_first_open process_module;
  !result

let rec module_item_to_structure_env ~(env : Query_env.t) ~state ~package
    (item : Module.t) =
  match item with
  | Module.Structure structure -> Some (env, structure)
  | Module.Constraint (_, module_type) ->
    module_item_to_structure_env ~state ~env ~package module_type
  | Module.Ident p -> (
    match
      Resolve_path.resolve_module_from_compiler_path ~state ~env ~package p
    with
    | Some (env2, Some declared2) ->
      module_item_to_structure_env ~state ~env:env2 ~package declared2.item
    | _ -> None)

(* Given a declared module, return the env entered into its concrete structure
   and the structure itself. Follows constraints and aliases *)
let enter_structure_from_declared ~state ~(env : Query_env.t) ~package
    (declared : Module.t Declared.t) =
  match module_item_to_structure_env ~state ~env ~package declared.item with
  | Some (env, s) -> Some (Query_env.enter_structure env s, s)
  | None -> None

let completions_from_structure_items ~(env : Query_env.t)
    (structure : Module.structure) =
  Structure_utils.unique_items structure
  |> List.filter_map (fun (it : Module.item) ->
         match it.kind with
         | Module.Value typ ->
           Some
             (Completion.create ~env ~docstring:it.docstring
                ~kind:(Completion.Value typ) it.name)
         | Module.Module {type_ = m} ->
           Some
             (Completion.create ~env ~docstring:it.docstring
                ~kind:
                  (Completion.Module {docstring = it.docstring; module_ = m})
                it.name)
         | Module.Type (t, _recStatus) ->
           Some
             (Completion.create ~env ~docstring:it.docstring
                ~kind:(Completion.Type t) it.name))

let resolve_path_from_stamps ~state ~(env : Query_env.t) ~package ~scope
    ~module_name ~path =
  (* Log.log("Finding from stamps " ++ name); *)
  match find_module_in_scope ~env ~module_name ~scope with
  | None -> None
  | Some declared -> (
    (* Log.log("found it"); *)
    (* [""] means completion after `ModuleName.` (trailing dot). *)
    match path with
    | [""] -> (
      match module_item_to_structure_env ~state ~env ~package declared.item with
      | Some (env, structure) ->
        Some (Query_env.enter_structure env structure, "")
      | None -> None)
    | _ -> (
      match Resolve_path.find_in_module ~env declared.item path with
      | None -> None
      | Some res -> (
        match res with
        | `Local (env, name) -> Some (env, name)
        | `Global (module_name, full_path) -> (
          match Process_cmt.file_for_module ~state ~package module_name with
          | None -> None
          | Some file ->
            Resolve_path.resolve_path ~env:(Query_env.from_file file) ~state
              ~path:full_path ~package))))

let resolve_module_with_opens ~state ~opens ~package ~module_name =
  let rec loop opens =
    match opens with
    | (env : Query_env.t) :: rest -> (
      Log.log ("Looking for env in " ^ Uri.to_string env.file.uri);
      match
        Resolve_path.resolve_path ~state ~env ~package ~path:[module_name; ""]
      with
      | Some (env, _) -> Some env
      | None -> loop rest)
    | [] -> None
  in
  loop opens

let resolve_file_module ~state ~module_name ~package =
  Log.log ("Getting module " ^ module_name);
  match Process_cmt.file_for_module ~state ~package module_name with
  | None -> None
  | Some file ->
    Log.log "got it";
    let env = Query_env.from_file file in
    Some env

let get_env_with_opens ~scope ~(env : Query_env.t) ~state ~package
    ~(opens : Query_env.t list) ~module_name (path : string list) =
  (* TODO: handle interleaving of opens and local modules correctly *)
  match
    resolve_path_from_stamps ~state ~env ~scope ~module_name ~path ~package
  with
  | Some x -> Some x
  | None -> (
    let env_opt =
      match resolve_module_with_opens ~state ~opens ~package ~module_name with
      | Some env_opens -> Some env_opens
      | None -> resolve_file_module ~state ~module_name ~package
    in
    match env_opt with
    | None -> None
    | Some env -> (
      match path with
      | [""] -> Some (env, "")
      | _ -> Resolve_path.resolve_path ~state ~env ~package ~path))

let rec expand_type_expr ~state ~env ~package type_expr =
  match type_expr |> Shared.dig_constructor with
  | Some path -> (
    match References.dig_constructor ~state ~env ~package path with
    | None -> None
    | Some (env, {item = {decl = {type_manifest = Some t}}}) ->
      expand_type_expr ~state ~env ~package t
    | Some (_, {docstring; item}) -> Some (docstring, item))
  | None -> None

let kind_to_documentation ~state ~env ~full ~current_docstring name
    (kind : Completion.kind) =
  let docs_from_kind =
    match kind with
    | ObjLabel _ | Label _ | FileModule _ | Snippet _ | FollowContextPath _ ->
      []
    | Module {docstring} -> docstring
    | Type {decl; name} ->
      [decl |> Shared.decl_to_string name |> Markdown.code_block]
    | Value typ -> (
      match expand_type_expr ~state ~env ~package:full.package typ with
      | None -> []
      | Some (docstrings, {decl; name; kind}) ->
        docstrings
        @ [
            (match kind with
            | Record _ | Tuple _ | Variant _ ->
              Markdown.code_block (Shared.decl_to_string name decl)
            | _ -> "");
          ])
    | Field ({typ; optional; docstring}, s) ->
      (* Handle optional fields. Checking for "?" is because sometimes optional
         fields are prefixed with "?" when completing, and at that point we don't
         need to _also_ add a "?" after the field name, as that looks weird. *)
      docstring
      @ [
          Markdown.code_block
            (if optional && Utils.starts_with name "?" = false then
               name ^ "?: "
               ^ (typ |> Utils.unwrap_if_option |> Shared.type_to_string)
             else name ^ ": " ^ (typ |> Shared.type_to_string));
          Markdown.code_block s;
        ]
    | Constructor (c, s) ->
      [Markdown.code_block (show_constructor c); Markdown.code_block s]
    | PolyvariantConstructor ({display_name; args}, s) ->
      [
        Markdown.code_block
          ("#" ^ display_name
          ^
          match args with
          | [] -> ""
          | type_exprs ->
            "("
            ^ (type_exprs
              |> List.map (fun type_expr -> type_expr |> Shared.type_to_string)
              |> String.concat ", ")
            ^ ")");
        Markdown.code_block s;
      ]
    | ExtractedType (extracted_type, _) ->
      [Markdown.code_block (Type_utils.extracted_type_to_string extracted_type)]
  in
  current_docstring @ docs_from_kind
  |> List.filter (fun s -> s <> "")
  |> String.concat "\n\n"

let kind_to_detail name (kind : Completion.kind) =
  match kind with
  | Type {name} -> "type " ^ name
  | Value typ -> typ |> Shared.type_to_string
  | ObjLabel typ -> typ |> Shared.type_to_string
  | Label typ_string -> typ_string
  | Module _ -> "module " ^ name
  | FileModule f -> "module " ^ f
  | Field ({typ; optional}, _) ->
    (* Handle optional fields. Checking for "?" is because sometimes optional
       fields are prefixed with "?" when completing, and at that point we don't
       need to _also_ add a "?" after the field name, as that looks weird. *)
    if optional && Utils.starts_with name "?" = false then
      typ |> Utils.unwrap_if_option |> Shared.type_to_string
    else typ |> Shared.type_to_string
  | Constructor (c, _) -> show_constructor c
  | PolyvariantConstructor ({display_name; args}, _) -> (
    "#" ^ display_name
    ^
    match args with
    | [] -> ""
    | type_exprs ->
      "("
      ^ (type_exprs
        |> List.map (fun type_expr -> type_expr |> Shared.type_to_string)
        |> String.concat ", ")
      ^ ")")
  | Snippet s -> s
  | FollowContextPath _ -> ""
  | ExtractedType (extracted_type, _) ->
    Type_utils.extracted_type_to_string ~name_only:true extracted_type

let kind_to_data file_path (kind : Completion.kind) =
  match kind with
  | FileModule f ->
    Some (`Assoc [("modulePath", `String f); ("filePath", `String file_path)])
  | _ -> Some `Null

let find_all_completions ~(env : Query_env.t) ~prefix ~exact ~names_used
    ~(completion_context : Completable.completion_context) =
  Log.log ("findAllCompletions uri:" ^ Uri.to_string env.file.uri);
  match completion_context with
  | Value ->
    completion_for_exported_values ~env ~prefix ~exact ~names_used
    @ completions_for_exported_constructors ~env ~prefix ~exact ~names_used
    @ completion_for_exported_modules ~env ~prefix ~exact ~names_used
  | Type ->
    completion_for_exported_types ~env ~prefix ~exact ~names_used
    @ completion_for_exported_modules ~env ~prefix ~exact ~names_used
  | Module -> completion_for_exported_modules ~env ~prefix ~exact ~names_used
  | Field ->
    completion_for_exported_fields ~env ~prefix ~exact ~names_used
    @ completion_for_exported_modules ~env ~prefix ~exact ~names_used
  | ValueOrField ->
    completion_for_exported_values ~env ~prefix ~exact ~names_used
    @ completion_for_exported_fields ~env ~prefix ~exact ~names_used
    @ completion_for_exported_modules ~env ~prefix ~exact ~names_used

let process_local_value name loc context_path scope ~prefix ~exact ~env
    ~(local_tables : Local_tables.t) =
  if Utils.check_name name ~prefix ~exact then
    match Hashtbl.find_opt local_tables.value_table (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem local_tables.names_used name) then (
        Hashtbl.add local_tables.names_used name ();
        local_tables.result_rev <-
          {
            (Completion.create declared.name.txt ~env ~kind:(Value declared.item))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: local_tables.result_rev)
    | None ->
      if !Cfg.debug_follow_ctx_path then
        Printf.printf "Completion Value Not Found %s loc:%s\n" name
          (Loc.to_string loc);
      local_tables.result_rev <-
        Completion.create name ~env
          ~kind:
            (match context_path with
            | Some context_path -> FollowContextPath (context_path, scope)
            | None ->
              Value
                (Ctype.newconstr
                   (Path.Pident (Ident.create "Type Not Known"))
                   []))
        :: local_tables.result_rev

let process_local_constructor name loc ~prefix ~exact ~env
    ~(local_tables : Local_tables.t) =
  if Utils.check_name name ~prefix ~exact then
    match
      Hashtbl.find_opt local_tables.constructor_table (name, Loc.start loc)
    with
    | Some declared ->
      if not (Hashtbl.mem local_tables.names_used name) then (
        Hashtbl.add local_tables.names_used name ();
        local_tables.result_rev <-
          {
            (Completion.create declared.name.txt ~env
               ~kind:
                 (Constructor
                    ( declared.item,
                      snd declared.item.type_decl
                      |> Shared.decl_to_string (fst declared.item.type_decl) )))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: local_tables.result_rev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Constructor Not Found %s loc:%s\n" name
           (Loc.to_string loc))

let process_local_type name loc ~prefix ~exact ~env
    ~(local_tables : Local_tables.t) =
  if Utils.check_name name ~prefix ~exact then
    match Hashtbl.find_opt local_tables.types_table (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem local_tables.names_used name) then (
        Hashtbl.add local_tables.names_used name ();
        local_tables.result_rev <-
          {
            (Completion.create declared.name.txt ~env ~kind:(Type declared.item))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: local_tables.result_rev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Type Not Found %s loc:%s\n" name
           (Loc.to_string loc))

let process_local_module name loc ~prefix ~exact ~env
    ~(local_tables : Local_tables.t) =
  if Utils.check_name name ~prefix ~exact then
    match Hashtbl.find_opt local_tables.modules_table (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem local_tables.names_used name) then (
        Hashtbl.add local_tables.names_used name ();
        local_tables.result_rev <-
          {
            (Completion.create declared.name.txt ~env
               ~kind:
                 (Module
                    {docstring = declared.docstring; module_ = declared.item}))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: local_tables.result_rev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Module Not Found %s loc:%s\n" name
           (Loc.to_string loc))

let process_local_include include_path _loc ~prefix ~exact ~(env : Query_env.t)
    ~(local_tables : Local_tables.t) =
  (* process only values for now *)
  local_tables.included_value_table
  |> Hashtbl.iter
       (fun (name, _) (declared : (string * Types.type_expr) Declared.t) ->
         (* We check all the values if their origin is the same as the include path. *)
         let source_module_path = fst declared.item in
         if String.ends_with ~suffix:include_path source_module_path then
           (* If this is the case we perform a similar check for the prefix *)
           if Utils.check_name name ~prefix ~exact then
             if not (Hashtbl.mem local_tables.names_used name) then (
               Hashtbl.add local_tables.names_used name ();
               local_tables.result_rev <-
                 {
                   (Completion.create declared.name.txt ~env
                      ~kind:(Value (snd declared.item)))
                   with
                   deprecated = declared.deprecated;
                   docstring = declared.docstring;
                   synthetic = true;
                 }
                 :: local_tables.result_rev))

let get_items_from_opens ~opens ~local_tables ~prefix ~exact ~completion_context
    =
  opens
  |> List.fold_left
       (fun results env ->
         let completions_from_this_open =
           find_all_completions ~env ~prefix ~exact
             ~names_used:local_tables.Local_tables.names_used
             ~completion_context
         in
         completions_from_this_open @ results)
       []

let find_local_completions_for_values_and_constructors
    ~(local_tables : Local_tables.t) ~env ~prefix ~exact ~opens ~scope =
  local_tables |> Local_tables.populate_values ~env;
  local_tables |> Local_tables.populate_included_values ~env;
  local_tables |> Local_tables.populate_constructors ~env;
  local_tables |> Local_tables.populate_modules ~env;

  scope
  |> Scope.iter_values_before_first_open
       (process_local_value ~prefix ~exact ~env ~local_tables);
  scope
  |> Scope.iter_constructors_before_first_open
       (process_local_constructor ~prefix ~exact ~env ~local_tables);
  scope
  |> Scope.iter_modules_before_first_open
       (process_local_module ~prefix ~exact ~env ~local_tables);

  let values_from_opens =
    get_items_from_opens ~opens ~local_tables ~prefix ~exact
      ~completion_context:Value
  in

  scope
  |> Scope.iter_values_after_first_open
       (process_local_value ~prefix ~exact ~env ~local_tables);
  scope
  |> Scope.iter_constructors_after_first_open
       (process_local_constructor ~prefix ~exact ~env ~local_tables);
  scope
  |> Scope.iter_modules_after_first_open
       (process_local_module ~prefix ~exact ~env ~local_tables);

  scope
  |> Scope.iter_includes
       (process_local_include ~prefix ~exact ~env ~local_tables);

  List.rev_append local_tables.result_rev values_from_opens

let find_local_completions_for_values ~(local_tables : Local_tables.t) ~env
    ~prefix ~exact ~opens ~scope =
  local_tables |> Local_tables.populate_values ~env;
  local_tables |> Local_tables.populate_included_values ~env;
  local_tables |> Local_tables.populate_modules ~env;
  scope
  |> Scope.iter_values_before_first_open
       (process_local_value ~prefix ~exact ~env ~local_tables);
  scope
  |> Scope.iter_modules_before_first_open
       (process_local_module ~prefix ~exact ~env ~local_tables);

  let values_from_opens =
    get_items_from_opens ~opens ~local_tables ~prefix ~exact
      ~completion_context:Value
  in

  scope
  |> Scope.iter_values_after_first_open
       (process_local_value ~prefix ~exact ~env ~local_tables);
  scope
  |> Scope.iter_modules_after_first_open
       (process_local_module ~prefix ~exact ~env ~local_tables);

  scope
  |> Scope.iter_includes
       (process_local_include ~prefix ~exact ~env ~local_tables);

  List.rev_append local_tables.result_rev values_from_opens

let find_local_completions_for_types ~(local_tables : Local_tables.t) ~env
    ~prefix ~exact ~opens ~scope =
  local_tables |> Local_tables.populate_types ~env;
  local_tables |> Local_tables.populate_modules ~env;
  scope
  |> Scope.iter_types_before_first_open
       (process_local_type ~prefix ~exact ~env ~local_tables);
  scope
  |> Scope.iter_modules_before_first_open
       (process_local_module ~prefix ~exact ~env ~local_tables);

  let values_from_opens =
    get_items_from_opens ~opens ~local_tables ~prefix ~exact
      ~completion_context:Type
  in

  scope
  |> Scope.iter_types_after_first_open
       (process_local_type ~prefix ~exact ~env ~local_tables);
  scope
  |> Scope.iter_modules_after_first_open
       (process_local_module ~prefix ~exact ~env ~local_tables);
  List.rev_append local_tables.result_rev values_from_opens

let find_local_completions_for_modules ~(local_tables : Local_tables.t) ~env
    ~prefix ~exact ~opens ~scope =
  local_tables |> Local_tables.populate_modules ~env;
  scope
  |> Scope.iter_modules_before_first_open
       (process_local_module ~prefix ~exact ~env ~local_tables);

  let values_from_opens =
    get_items_from_opens ~opens ~local_tables ~prefix ~exact
      ~completion_context:Module
  in

  scope
  |> Scope.iter_modules_after_first_open
       (process_local_module ~prefix ~exact ~env ~local_tables);
  List.rev_append local_tables.result_rev values_from_opens

let find_local_completions_with_opens ~pos ~(env : Query_env.t) ~prefix ~exact
    ~opens ~scope ~(completion_context : Completable.completion_context) =
  (* TODO: handle arbitrary interleaving of opens and local bindings correctly *)
  Log.log
    ("findLocalCompletionsWithOpens uri:" ^ Uri.to_string env.file.uri ^ " pos:"
   ^ Pos.to_string pos);
  let local_tables = Local_tables.create () in
  match completion_context with
  | Value | ValueOrField ->
    find_local_completions_for_values_and_constructors ~local_tables ~env
      ~prefix ~exact ~opens ~scope
  | Type ->
    find_local_completions_for_types ~local_tables ~env ~prefix ~exact ~opens
      ~scope
  | Module ->
    find_local_completions_for_modules ~local_tables ~env ~prefix ~exact ~opens
      ~scope
  | Field ->
    (* There's no local completion for fields *)
    []

let get_complementary_completions_for_typed_value ~opens ~all_files ~scope ~env
    prefix =
  let exact = false in
  let local_completions_with_opens =
    let local_tables = Local_tables.create () in
    find_local_completions_for_values ~local_tables ~env ~prefix ~exact ~opens
      ~scope
  in
  let file_modules =
    all_files |> File_set.elements
    |> Utils.filter_map (fun name ->
           if
             Utils.check_name name ~prefix ~exact
             && not
                  (* TODO complete the namespaced name too *)
                  (Utils.file_name_has_unallowed_chars name)
           then
             Some
               (Completion.create name ~synthetic:true ~env
                  ~kind:(Completion.FileModule name))
           else None)
  in
  local_completions_with_opens @ file_modules

let get_completions_for_path ~state ~debug ~opens ~full ~pos ~exact ~scope
    ~completion_context ~env path =
  if debug then Printf.printf "Path %s\n" (path |> String.concat ".");
  let all_files = all_files_in_package full.package in
  match path with
  | [] -> []
  | [prefix] ->
    let local_completions_with_opens =
      find_local_completions_with_opens ~pos ~env ~prefix ~exact ~opens ~scope
        ~completion_context
    in
    let file_modules =
      all_files |> File_set.elements
      |> Utils.filter_map (fun name ->
             if
               Utils.check_name name ~prefix ~exact
               && not
                    (* TODO complete the namespaced name too *)
                    (Utils.file_name_has_unallowed_chars name)
             then
               Some
                 (Completion.create name ~env ~kind:(Completion.FileModule name))
             else None)
    in
    local_completions_with_opens @ file_modules
  | module_name :: path -> (
    Log.log ("Path " ^ path_to_string path);
    (* [""] is trailing dot completion (`ModuleName.<com>`). *)
    match path with
    | [""] -> (
      let env_file = env in
      let declared_opt =
        match find_module_in_scope ~env:env_file ~module_name ~scope with
        | Some d -> Some d
        | None -> (
          match Exported.find env_file.exported Exported.Module module_name with
          | Some stamp -> Stamps.find_module env_file.file.stamps stamp
          | None -> None)
      in
      match declared_opt with
      | Some (declared : Module.t Declared.t) when declared.is_exported = false
        -> (
        match
          enter_structure_from_declared ~state ~env:env_file
            ~package:full.package declared
        with
        | None -> []
        | Some (env_in_module, structure) ->
          completions_from_structure_items ~env:env_in_module structure)
      | _ -> (
        match
          get_env_with_opens ~state ~scope ~env ~package:full.package ~opens
            ~module_name path
        with
        | Some (env, prefix) ->
          Log.log "Got the env";
          let names_used = Hashtbl.create 10 in
          find_all_completions ~env ~prefix ~exact ~names_used
            ~completion_context
        | None -> []))
    | _ -> (
      match
        get_env_with_opens ~state ~scope ~env ~package:full.package ~opens
          ~module_name path
      with
      | Some (env, prefix) ->
        Log.log "Got the env";
        let names_used = Hashtbl.create 10 in
        find_all_completions ~env ~prefix ~exact ~names_used ~completion_context
      | None -> []))

(** Completions intended for piping, from a completion path. *)
let completions_for_pipe_from_completion_path ~state
    ~env_completion_is_made_from ~opens ~pos ~scope ~debug ~prefix ~env
    ~raw_opens ~full completion_path =
  let completion_path_without_current_module =
    Type_utils.remove_current_module_if_needed ~env_completion_is_made_from
      completion_path
  in
  let completion_path_minus_opens =
    Type_utils.remove_opens_from_completion_path ~raw_opens
      ~package:full.package completion_path_without_current_module
    |> String.concat "."
  in
  let completion_name name =
    if completion_path_minus_opens = "" then name
    else completion_path_minus_opens ^ "." ^ name
  in
  let completions =
    completion_path @ [prefix]
    |> get_completions_for_path ~state ~debug ~completion_context:Value
         ~exact:false ~opens ~full ~pos ~env ~scope
  in
  let completions =
    completions
    |> List.map (fun (completion : Completion.t) ->
           {completion with name = completion_name completion.name})
  in
  completions

let rec dig_to_record_fields_for_completion ~state ~debug ~package ~opens ~full
    ~pos ~env ~scope path =
  match
    path
    |> get_completions_for_path ~state ~debug ~completion_context:Type
         ~exact:true ~opens ~full ~pos ~env ~scope
  with
  | {kind = Type {kind = Abstract (Some (p, _))}} :: _ ->
    (* This case happens when what we're looking for is a type alias.
       This is the case in newer rescript-react versions where
       ReactDOM.domProps is an alias for JsxEvent.t. *)
    let path_rev = p |> Utils.expand_path in
    path_rev |> List.rev
    |> dig_to_record_fields_for_completion ~state ~debug ~package ~opens ~full
         ~pos ~env ~scope
  | {kind = Type {kind = Record fields}} :: _ -> Some fields
  | _ -> None

let mk_item ?data ?additional_text_edits name ~kind ~detail ~deprecated
    ~docstring =
  let doc_content =
    (match deprecated with
    | None -> ""
    | Some s -> "Deprecated: " ^ s ^ "\n\n")
    ^
    match docstring with
    | [] -> ""
    | _ :: _ -> docstring |> String.concat "\n"
  in
  let tags =
    match deprecated with
    | None -> []
    | Some _ -> [Lsp.Types.CompletionItemTag.Deprecated (* deprecated *)]
  in

  let documentation =
    match String.length doc_content > 0 with
    | true ->
      Some
        (`MarkupContent
           (Lsp.Types.MarkupContent.create ~kind:Lsp.Types.MarkupKind.Markdown
              ~value:doc_content))
    | false -> None
  in

  let deprecated = if Option.is_some deprecated then Some true else None in
  let data =
    match data with
    | Some `Null | None -> None
    | Some other -> Some other
  in

  Lsp.Types.CompletionItem.create ~label:name ~kind ~tags ~detail ?documentation
    ?deprecated ?data ?additionalTextEdits:additional_text_edits ?sortText:None
    ?insertText:None ?insertTextFormat:None ?filterText:None ()

let completion_to_item ~state
    {
      Completion.name;
      deprecated;
      docstring;
      kind;
      sort_text;
      insert_text;
      insert_text_format;
      filter_text;
      detail;
      env;
      additional_text_edits;
    } ~full =
  let item =
    mk_item name ?additional_text_edits
      ?data:(kind_to_data (full.file.uri |> Uri.to_path) kind)
      ~kind:(Completion.kind_to_lsp_completion_item kind)
      ~deprecated
      ~detail:
        (match detail with
        | None -> kind_to_detail name kind
        | Some detail -> detail)
      ~docstring:
        (match
           kind_to_documentation ~state ~current_docstring:docstring ~full ~env
             name kind
         with
        | "" -> []
        | docstring -> [docstring])
  in
  {
    item with
    sortText = sort_text;
    insertText = insert_text;
    insertTextFormat = insert_text_format;
    filterText = filter_text;
  }

let completions_get_type_env = function
  | {Completion.kind = Value typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = ObjLabel typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = Field ({typ}, _); env} :: _ -> Some (typ, env)
  | _ -> None

type get_completions_for_context_path_mode = Regular | Pipe

let completions_get_completion_type ~full ~state completions =
  let first_non_synthetic_completion =
    List.find_opt (fun c -> not c.Completion.synthetic) completions
  in
  match first_non_synthetic_completion with
  | Some {Completion.kind = Value typ; env}
  | Some {Completion.kind = ObjLabel typ; env}
  | Some {Completion.kind = Field ({typ}, _); env} ->
    typ
    |> Type_utils.extract_type ~state ~env ~package:full.package
    |> Option.map (fun (typ, _) -> (typ, env))
  | Some {Completion.kind = Type typ; env} -> (
    match Type_utils.extract_type_from_resolved_type typ ~state ~env ~full with
    | None -> None
    | Some extracted_type -> Some (extracted_type, env))
  | Some {Completion.kind = ExtractedType (typ, _); env} -> Some (typ, env)
  | _ -> None

let rec completions_get_completion_type2 ~state ~debug ~full ~opens ~raw_opens
    ~pos completions =
  let first_non_synthetic_completion =
    List.find_opt (fun c -> not c.Completion.synthetic) completions
  in
  match first_non_synthetic_completion with
  | Some
      ( {Completion.kind = Value typ; env}
      | {Completion.kind = ObjLabel typ; env}
      | {Completion.kind = Field ({typ}, _); env} ) ->
    Some (TypeExpr typ, env)
  | Some {Completion.kind = FollowContextPath (ctx_path, scope); env} ->
    ctx_path
    |> get_completions_for_context_path ~state ~debug ~full ~env ~exact:true
         ~opens ~raw_opens ~pos ~scope
    |> completions_get_completion_type2 ~state ~debug ~full ~opens ~raw_opens
         ~pos
  | Some {Completion.kind = Type typ; env} -> (
    match Type_utils.extract_type_from_resolved_type typ ~state ~env ~full with
    | None -> None
    | Some extracted_type -> Some (ExtractedType extracted_type, env))
  | Some {Completion.kind = ExtractedType (typ, _); env} ->
    Some (ExtractedType typ, env)
  | _ -> None

and completions_get_type_env2 ~state ~debug (completions : Completion.t list)
    ~full ~opens ~raw_opens ~pos =
  let first_non_synthetic_completion =
    List.find_opt (fun c -> not c.Completion.synthetic) completions
  in
  match first_non_synthetic_completion with
  | Some {Completion.kind = Value typ; env} -> Some (typ, env)
  | Some {Completion.kind = ObjLabel typ; env} -> Some (typ, env)
  | Some {Completion.kind = Field ({typ}, _); env} -> Some (typ, env)
  | Some {Completion.kind = FollowContextPath (ctx_path, scope); env} ->
    ctx_path
    |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
         ~pos ~env ~exact:true ~scope
    |> completions_get_type_env2 ~state ~debug ~full ~opens ~raw_opens ~pos
  | _ -> None

and get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens ~pos
    ~env ~exact ~scope ?(mode = Regular) context_path =
  let env_completion_is_made_from = env in
  if debug then
    Printf.printf "ContextPath %s\n"
      (Completable.context_path_to_string context_path);
  let package = full.package in
  match context_path with
  | CPString ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPString";
    [Completion.create "dummy" ~env ~kind:(Completion.Value Predef.type_string)]
  | CPBool ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPBool";
    [Completion.create "dummy" ~env ~kind:(Completion.Value Predef.type_bool)]
  | CPInt ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPInt";
    [Completion.create "dummy" ~env ~kind:(Completion.Value Predef.type_int)]
  | CPFloat ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPFloat";
    [Completion.create "dummy" ~env ~kind:(Completion.Value Predef.type_float)]
  | CPArray None ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPArray (no payload)";
    [
      Completion.create "array" ~env
        ~kind:(Completion.Value (Ctype.newconstr Predef.path_array []));
    ]
  | CPArray (Some cp) -> (
    if Debug.verbose () then
      print_endline "[ctx_path]--> CPArray (with payload)";
    match mode with
    | Regular -> (
      match
        cp
        |> get_completions_for_context_path ~state ~debug ~full ~opens
             ~raw_opens ~pos ~env ~exact:true ~scope
        |> completions_get_completion_type ~full ~state
      with
      | None -> []
      | Some (typ, env) ->
        [
          Completion.create "dummy" ~env
            ~kind:
              (Completion.ExtractedType (Tarray (env, ExtractedType typ), `Type));
        ])
    | Pipe ->
      (* Pipe completion with array just needs to know that it's an array, not
         what inner type it has. *)
      [
        Completion.create "dummy" ~env
          ~kind:(Completion.Value (Ctype.newconstr Predef.path_array []));
      ])
  | CPOption cp -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CPOption";
    match
      cp
      |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
           ~pos ~env ~exact:true ~scope
      |> completions_get_completion_type ~state ~full
    with
    | None -> []
    | Some (typ, env) ->
      [
        Completion.create "dummy" ~env
          ~kind:
            (Completion.ExtractedType (Toption (env, ExtractedType typ), `Type));
      ])
  | CPAwait cp -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CPAwait";
    match
      cp
      |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
           ~pos ~env ~exact:true ~scope
      |> completions_get_completion_type ~state ~full
    with
    | Some (Tpromise (env, typ), _env) ->
      [Completion.create "dummy" ~env ~kind:(Completion.Value typ)]
    | _ -> [])
  | CPId {path; completion_context; loc} ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPId";
    (* Looks up the type of an identifier.

       Because of reasons we sometimes don't get enough type
       information when looking up identifiers where the type
       has type parameters. This in turn means less completions.

       There's a heuristic below that tries to look up the type
       of the ID in the usual way first. But if the type found
       still has uninstantiated type parameters, we check the
       location for the identifier from the compiler type artifacts.
       That type usually has the type params instantiated, if they are.
       This leads to better completion.

       However, we only do it in incremental type checking mode,
       because more type information is always available in that mode. *)
    let use_tvar_lookup = !Cfg.in_incremental_typechecking_mode in
    let by_path =
      path
      |> get_completions_for_path ~state ~debug ~opens ~full ~pos ~exact
           ~completion_context ~env ~scope
    in
    let has_tvars =
      if use_tvar_lookup then
        match by_path with
        | [{kind = Value typ}] when Type_utils.has_tvar typ -> true
        | _ -> false
      else false
    in
    let result =
      if has_tvars then
        let by_loc = Type_utils.find_type_via_loc loc ~full ~debug in
        match (by_loc, by_path) with
        | Some t, [({kind = Value _} as item)] -> [{item with kind = Value t}]
        | _ -> by_path
      else by_path
    in
    result
  | CPApply (cp, labels) -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CPApply";
    match
      cp
      |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
           ~pos ~env ~exact:true ~scope
      |> completions_get_completion_type2 ~state ~debug ~full ~opens ~raw_opens
           ~pos
    with
    | Some ((TypeExpr typ | ExtractedType (Tfunction {typ})), env) -> (
      let rec reconstruct_function_type args t_ret =
        match args with
        | [] -> t_ret
        | (label, t_arg) :: rest ->
          let rest_type = reconstruct_function_type rest t_ret in
          {
            typ with
            desc = Tarrow ({lbl = label; typ = t_arg}, rest_type, Cok, None);
          }
      in
      let rec process_apply args labels =
        match (args, labels) with
        | _, [] -> args
        | _, label :: (_ :: _ as next_labels) ->
          (* compute the application of the first label, then the next ones *)
          let args = process_apply args [label] in
          process_apply args next_labels
        | (Asttypes.Nolabel, _) :: next_args, [Asttypes.Nolabel] -> next_args
        | ((Labelled _, _) as arg) :: next_args, [Nolabel] ->
          arg :: process_apply next_args labels
        | (Optional _, _) :: next_args, [Nolabel] ->
          process_apply next_args labels
        | ( (((Labelled {txt = s1} | Optional {txt = s1}), _) as arg)
            :: next_args,
            [(Labelled {txt = s2} | Optional {txt = s2})] ) ->
          if s1 = s2 then next_args else arg :: process_apply next_args labels
        | ((Nolabel, _) as arg) :: next_args, [(Labelled _ | Optional _)] ->
          arg :: process_apply next_args labels
        | [], [(Nolabel | Labelled _ | Optional _)] ->
          (* should not happen, but just ignore extra arguments *) []
      in

      match
        Type_utils.extract_function_type ~env ~state ~package ~dig_into:false
          typ
      with
      | args, t_ret when args <> [] ->
        let args = process_apply args labels in
        let ret_type = reconstruct_function_type args t_ret in
        [Completion.create "dummy" ~env ~kind:(Completion.Value ret_type)]
      | _ -> (
        (* A tagged template tag has type taggedTemplate<'param, 'output>;
           applying it (via backtick syntax) yields the 'output type. *)
        let rec unwrap (t : Types.type_expr) =
          match t.desc with
          | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> unwrap t1
          | _ -> t
        in
        match (unwrap typ).desc with
        | Tconstr (path, [_param; output_type], _)
          when Path.name path = "taggedTemplate" ->
          [Completion.create "dummy" ~env ~kind:(Completion.Value output_type)]
        | _ -> []))
    | _ -> [])
  | CPField
      {context_path = CPId {path; completion_context = Module}; field_name} ->
    if Debug.verbose () then print_endline "[ctx_path]--> CPField: M.field";
    (* M.field *)
    path @ [field_name]
    |> get_completions_for_path ~state ~debug ~opens ~full ~pos ~exact
         ~completion_context:Field ~env ~scope
  | CPField {context_path = cp; field_name; pos_of_dot; expr_loc; in_jsx} -> (
    if Debug.verbose () then print_endline "[dot_completion]--> Triggered";
    let completions_from_ctx_path =
      cp
      |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
           ~pos ~env ~exact:true ~scope
    in
    let main_type_completion_env =
      completions_from_ctx_path
      |> completions_get_type_env2 ~state ~debug ~full ~opens ~raw_opens ~pos
    in
    match main_type_completion_env with
    | None ->
      if Debug.verbose () then
        Printf.printf
          "[dot_completion] Could not extract main type completion env.\n";
      []
    | Some (typ, env) ->
      let field_completions =
        Dot_completion_utils.field_completions_for_dot_completion typ ~state
          ~env ~package ~prefix:field_name ?pos_of_dot ~exact
      in
      (* Get additional completions acting as if this field completion was actually a pipe completion. *)
      let cp_as_pipe_completion =
        Completable.CPPipe
          {
            synthetic = true;
            context_path =
              (match cp with
              | CPApply (c, args) -> CPApply (c, args @ [Asttypes.Nolabel])
              | CPId _ when Type_utils.is_function_type ~state ~env ~package typ
                ->
                CPApply (cp, [Asttypes.Nolabel])
              | _ -> cp);
            id = field_name;
            in_jsx;
            lhs_loc = expr_loc;
          }
      in
      let pipe_completions =
        cp_as_pipe_completion
        |> get_completions_for_context_path ~state ~debug ~full ~opens
             ~raw_opens ~pos ~env:env_completion_is_made_from ~exact ~scope
        |> List.filter_map (fun c ->
               Type_utils.transform_completion_to_pipe_completion
                 ~synthetic:true ~env ?pos_of_dot c)
      in
      field_completions @ pipe_completions)
  | CPObj (cp, label) -> (
    (* TODO: Also needs to support ExtractedType *)
    if Debug.verbose () then print_endline "[ctx_path]--> CPObj";
    match
      cp
      |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
           ~pos ~env ~exact:true ~scope
      |> completions_get_type_env2 ~state ~debug ~full ~opens ~raw_opens ~pos
    with
    | Some (typ, env) -> (
      match typ |> Type_utils.extract_object_type ~state ~env ~package with
      | Some (env, t_obj) ->
        t_obj |> Type_utils.get_obj_fields
        |> Utils.filter_map (fun (field, typ) ->
               if Utils.check_name field ~prefix:label ~exact then
                 Some
                   (Completion.create field ~env ~kind:(Completion.ObjLabel typ))
               else None)
      | None -> [])
    | None -> [])
  | CPPipe {context_path = cp; id = prefix; lhs_loc; in_jsx; synthetic} -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CPPipe";
    (* The environment at the cursor is the environment we're completing from. *)
    let env_at_cursor = env in
    match
      cp
      |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
           ~pos ~env ~exact:true ~scope ~mode:Pipe
      |> completions_get_type_env2 ~state ~debug ~full ~opens ~raw_opens ~pos
    with
    | None ->
      if Debug.verbose () then
        print_endline "[CPPipe]--> Could not resolve type env";
      []
    | Some (typ, env) -> (
      let env, typ =
        typ
        |> Type_utils.resolve_type_for_pipe_completion ~env ~state
             ~package:full.package ~full ~lhs_loc
      in
      let main_type_id = Type_utils.find_root_type_id ~state ~full ~env typ in
      let type_path = Type_utils.path_from_type_expr typ in
      match main_type_id with
      | None ->
        if Debug.verbose () then
          Printf.printf
            "[pipe_completion] Could not find mainTypeId. Aborting pipe \
             completions.\n";
        []
      | Some main_type_id ->
        if Debug.verbose () then
          Printf.printf "[pipe_completion] mainTypeId: %s\n" main_type_id;
        let pipe_completions =
          (* We now need a completion path from where to look up the module for our dot completion type.
              This is from where we pull all of the functions we want to complete for the pipe.

              A completion path here could be one of two things:
              1. A module path to the main module for the type we've found
              2. A module path to a builtin module, like `Int` for `int`, or `Array` for `array`

             The below code will deliberately _not_ dig into type aliases for the main type when we're looking
             for what _module_ to complete from. This is because you should be able to control where completions
             come from even if your type is an alias.
          *)
          let complete_as_builtin =
            match type_path with
            | Some t -> Type_utils.completion_path_from_maybe_builtin t
            | None -> None
          in
          let completion_path =
            match (complete_as_builtin, type_path) with
            | Some completion_path_for_builtin, _ ->
              Some (false, completion_path_for_builtin)
            | _, Some p -> (
              (* If this isn't a builtin, but we have a path, we try to resolve the
                 module path relative to the env we're completing from. This ensures that
                 what we get here is a module path we can find completions for regardless of
                 of the current scope for the position we're at.*)
              match
                Type_utils.get_module_path_relative_to_env ~debug
                  ~env:env_completion_is_made_from ~env_from_item:env
                  (Utils.expand_path p)
              with
              | None -> Some (true, [env.file.module_name])
              | Some p -> Some (false, p))
            | _ -> None
          in
          match completion_path with
          | None -> []
          | Some (is_from_current_module, completion_path) ->
            completions_for_pipe_from_completion_path ~state
              ~env_completion_is_made_from ~opens ~pos ~scope ~debug ~prefix
              ~env ~raw_opens ~full completion_path
            |> Type_utils.filter_pipeable_functions ~env ~state ~full ~synthetic
                 ~target_type_id:main_type_id
            |> List.filter (fun (c : Completion.t) ->
                   (* If we're completing from the current module then we need to care about scope.
                      This is automatically taken care of in other cases. *)
                   if is_from_current_module then
                     match c.kind with
                     | Value _ ->
                       scope
                       |> List.find_opt (fun (item : Scope_types.item) ->
                              match item with
                              | Value (scope_item_name, _, _, _) ->
                                scope_item_name = c.name
                              | _ -> false)
                       |> Option.is_some
                     | _ -> false
                   else true)
        in

        let globally_configured_completions_for_type =
          match
            package.autocomplete |> Misc.String_map.find_opt main_type_id
          with
          | None -> []
          | Some completion_paths ->
            completion_paths |> List.map (fun p -> String.split_on_char '.' p)
        in

        let globally_configured_completions =
          globally_configured_completions_for_type
          |> List.map (fun completion_path ->
                 completions_for_pipe_from_completion_path ~state
                   ~env_completion_is_made_from ~opens ~pos ~scope ~debug
                   ~prefix ~env ~raw_opens ~full completion_path)
          |> List.flatten
          |> Type_utils.filter_pipeable_functions ~synthetic:true ~state ~env
               ~full ~target_type_id:main_type_id
        in

        (* Extra completions can be drawn from the @editor.completeFrom attribute. Here we
           find and add those completions as well. *)
        let extra_completions =
          Type_utils.get_extra_modules_to_complete_from_for_type ~state ~env
            ~full typ
          |> List.map (fun completion_path ->
                 completions_for_pipe_from_completion_path ~state
                   ~env_completion_is_made_from ~opens ~pos ~scope ~debug
                   ~prefix ~env ~raw_opens ~full completion_path)
          |> List.flatten
          |> Type_utils.filter_pipeable_functions ~synthetic:true ~state ~env
               ~full ~target_type_id:main_type_id
        in
        (* Add JSX completion items if we're in a JSX context. *)
        let jsx_completions =
          if in_jsx then
            Pipe_completion_utils.add_jsx_completion_items ~env ~main_type_id
              ~prefix ~full ~raw_opens typ
          else []
        in
        (* Add completions from the current module. *)
        let current_module_completions =
          get_completions_for_path ~state ~debug ~completion_context:Value
            ~exact:false ~opens:[] ~full ~pos ~env:env_at_cursor ~scope [prefix]
          |> Type_utils.filter_pipeable_functions ~synthetic:true ~state ~env
               ~full ~target_type_id:main_type_id
        in
        jsx_completions @ pipe_completions @ extra_completions
        @ current_module_completions @ globally_configured_completions))
  | CTuple ctx_paths ->
    if Debug.verbose () then print_endline "[ctx_path]--> CTuple";
    (* Turn a list of context paths into a list of type expressions. *)
    let type_exrps =
      ctx_paths
      |> List.map (fun context_path ->
             context_path
             |> get_completions_for_context_path ~state ~debug ~full ~opens
                  ~raw_opens ~pos ~env ~exact:true ~scope)
      |> List.filter_map (fun completion_items ->
             match completion_items with
             | {Completion.kind = Value typ} :: _ -> Some typ
             | _ -> None)
    in
    if List.length ctx_paths = List.length type_exrps then
      [
        Completion.create "dummy" ~env
          ~kind:(Completion.Value (Ctype.newty (Ttuple type_exrps)));
      ]
    else []
  | CJsxPropValue {path_to_component; prop_name; empty_jsx_prop_name_hint} -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CJsxPropValue";
    let find_type_of_value path =
      path
      |> get_completions_for_path ~state ~debug ~completion_context:Value
           ~exact:true ~opens ~full ~pos ~env ~scope
      |> completions_get_type_env2 ~state ~debug ~full ~opens ~raw_opens ~pos
    in
    let lowercase_component =
      match path_to_component with
      | [el_name] when Char.lowercase_ascii el_name.[0] = el_name.[0] -> true
      | _ -> false
    in
    (* TODO(env-stuff) Does this need to potentially be instantiated with type args too? *)
    let labels =
      if lowercase_component then
        let rec dig_to_type_for_completion path =
          match
            path
            |> get_completions_for_path ~state ~debug ~completion_context:Type
                 ~exact:true ~opens ~full ~pos ~env ~scope
          with
          | {kind = Type {kind = Abstract (Some (p, _))}} :: _ ->
            (* This case happens when what we're looking for is a type alias.
               This is the case in newer rescript-react versions where
               ReactDOM.domProps is an alias for JsxEvent.t. *)
            let path_rev = p |> Utils.expand_path in
            path_rev |> List.rev |> dig_to_type_for_completion
          | {kind = Type {kind = Record fields}} :: _ ->
            fields |> List.map (fun f -> (f.fname.txt, f.typ, env))
          | _ -> []
        in
        Type_utils.path_to_element_props package |> dig_to_type_for_completion
      else
        Completion_jsx.get_jsx_labels ~component_path:path_to_component
          ~find_type_of_value ~package ~state
    in
    (* We have a heuristic that kicks in when completing empty prop expressions in the middle of a JSX element,
       like <SomeComp firstProp=test second=<com> third=123 />.
       The parser turns that broken JSX into: <SomeComp firstProp=test second=<com>third />, 123.

       So, we use a heuristic that covers this scenario by picking up on the cursor being between
       the prop name and the prop expression, and the prop expression being an ident that's a
       _valid prop name_ for that JSX element.

       This works because the ident itself will always be the next prop name (since that's what the
       parser eats). So, we do a simple lookup of that hint here if it exists, to make sure the hint
       is indeed a valid label for this JSX element. *)
    let empty_jsx_prop_name_hint_is_correct =
      match empty_jsx_prop_name_hint with
      | Some ident_name when ident_name != prop_name ->
        labels
        |> List.find_opt (fun (f, _, _) -> f = ident_name)
        |> Option.is_some
      | Some _ -> false
      | None -> true
    in
    let target_label =
      if empty_jsx_prop_name_hint_is_correct then
        labels |> List.find_opt (fun (f, _, _) -> f = prop_name)
      else None
    in
    match target_label with
    | None -> []
    | Some (_, typ, env) ->
      [
        Completion.create "dummy" ~env
          ~kind:(Completion.Value (Utils.unwrap_if_option typ));
      ])
  | CArgument {function_context_path; argument_label} -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CArgument";
    if Debug.verbose () then
      Printf.printf "--> function argument: %s\n"
        (match argument_label with
        | Labelled n | Optional n -> n
        | Unlabelled {argument_position} ->
          "$" ^ string_of_int argument_position);

    let labels, env =
      match
        function_context_path
        |> get_completions_for_context_path ~state ~debug ~full ~opens
             ~raw_opens ~pos ~env ~exact:true ~scope
        |> completions_get_completion_type2 ~state ~debug ~full ~opens
             ~raw_opens ~pos
      with
      | Some ((TypeExpr typ | ExtractedType (Tfunction {typ})), env) ->
        if Debug.verbose () then print_endline "--> found function type";
        (typ |> Type_utils.get_args ~full ~env ~state, env)
      | _ ->
        if Debug.verbose () then
          print_endline "--> could not find function type";
        ([], env)
    in
    let target_label =
      labels
      |> List.find_opt (fun (label, _) ->
             match (argument_label, label) with
             | ( Unlabelled {argument_position = pos1},
                 Completable.Unlabelled {argument_position = pos2} ) ->
               pos1 = pos2
             | ( (Labelled name1 | Optional name1),
                 (Labelled name2 | Optional name2) ) ->
               name1 = name2
             | _ -> false)
    in
    let expand_option =
      match target_label with
      | None | Some ((Unlabelled _ | Labelled _), _) -> false
      | Some (Optional _, _) -> true
    in
    match target_label with
    | None ->
      if Debug.verbose () then
        print_endline "--> could not look up function argument";
      []
    | Some (_, typ) ->
      if Debug.verbose () then print_endline "--> found function argument!";
      [
        Completion.create "dummy" ~env
          ~kind:
            (Completion.Value
               (if expand_option then Utils.unwrap_if_option typ else typ));
      ])
  | CPatternPath {root_ctx_path; nested} -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CPatternPath";
    (* TODO(env-stuff) Get rid of innerType etc *)
    match
      root_ctx_path
      |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
           ~pos ~env ~exact:true ~scope
      |> completions_get_completion_type2 ~state ~debug ~full ~opens ~raw_opens
           ~pos
    with
    | Some (typ, env) -> (
      match
        typ |> Type_utils.resolve_nested_pattern_path ~env ~full ~state ~nested
      with
      | Some (typ, env) ->
        [Completion.create "dummy" ~env ~kind:(kind_from_inner_type typ)]
      | None -> [])
    | None -> [])
  | CTypeAtPos loc -> (
    if Debug.verbose () then print_endline "[ctx_path]--> CTypeAtPos";
    match Type_utils.find_type_via_loc loc ~full ~debug with
    | None -> []
    | Some typ_expr -> [Completion.create "dummy" ~env ~kind:(Value typ_expr)])

let get_opens ~state ~debug ~raw_opens ~package ~env =
  if debug && raw_opens <> [] then
    Printf.printf "%s\n"
      ("Raw opens: "
      ^ string_of_int (List.length raw_opens)
      ^ " "
      ^ String.concat " ... " (raw_opens |> List.map path_to_string));
  let package_opens = package.opens in
  if debug && package_opens <> [] then
    Printf.printf "%s\n"
      ("Package opens "
      ^ String.concat " "
          (package_opens |> List.map (fun p -> p |> path_to_string)));
  let resolved_opens =
    resolve_opens ~state ~env (List.rev (raw_opens @ package_opens)) ~package
  in
  if debug && resolved_opens <> [] then
    Printf.printf "%s\n"
      ("Resolved opens "
      ^ string_of_int (List.length resolved_opens)
      ^ " "
      ^ String.concat " "
          (resolved_opens
          |> List.map (fun (e : Query_env.t) -> e.file.module_name)));
  (* Last open takes priority *)
  List.rev resolved_opens

let filter_items items ~prefix =
  if prefix = "" then items
  else
    items
    |> List.filter (fun (item : Completion.t) ->
           Utils.starts_with item.name prefix)

type completion_mode = Pattern of Completable.pattern_mode | Expression

let empty_case ~mode num =
  match mode with
  | Expression -> "$" ^ string_of_int (num - 1)
  | Pattern _ -> "${" ^ string_of_int num ^ ":_}"

let print_constructor_args ~mode ~as_snippet args_len =
  let args = ref [] in
  for arg_num = 1 to args_len do
    args :=
      !args
      @ [
          (match (as_snippet, args_len) with
          | true, l when l > 1 -> Printf.sprintf "${%i:_}" arg_num
          | true, l when l > 0 -> empty_case ~mode arg_num
          | _ -> "_");
        ]
  done;
  if List.length !args > 0 then "(" ^ (!args |> String.concat ", ") ^ ")"
  else ""

let rec complete_typed_value ?(type_arg_context : type_arg_context option)
    ~raw_opens ~full ~state ~prefix ~completion_context ~mode
    (t : Shared_types.completion_type) =
  let empty_case = empty_case ~mode in
  let print_constructor_args = print_constructor_args ~mode in
  let create = Completion.create ?type_arg_context in
  let get_record_completions ~env ~fields ~extracted_type =
    (* As we're completing for a record, we'll need a hint (completionContext)
       here to figure out whether we should complete for a record field, or
       the record body itself. *)
    match completion_context with
    | Some (Completable.RecordField {seen_fields}) ->
      fields
      |> List.filter (fun (field : field) ->
             List.mem field.fname.txt seen_fields = false)
      |> List.map (fun (field : field) ->
             match (field.optional, mode) with
             | true, Pattern Destructuring ->
               create ("?" ^ field.fname.txt) ?deprecated:field.deprecated
                 ~docstring:
                   [
                     field.fname.txt
                     ^ " is an optional field, and needs to be destructured \
                        using '?'.";
                   ]
                 ~kind:
                   (Field
                      (field, Type_utils.extracted_type_to_string extracted_type))
                 ~env
             | _ ->
               create field.fname.txt ?deprecated:field.deprecated
                 ~kind:
                   (Field
                      (field, Type_utils.extracted_type_to_string extracted_type))
                 ~env)
      |> filter_items ~prefix
    | _ ->
      if prefix = "" then
        [
          create "{}" ~includes_snippets:true ~insert_text:"{$0}" ~sort_text:"A"
            ~kind:
              (ExtractedType
                 ( extracted_type,
                   match mode with
                   | Pattern _ -> `Type
                   | Expression -> `Value ))
            ~env;
        ]
      else []
  in
  match t with
  | TtypeT {env; path} when mode = Expression ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> TtypeT (Expression)";
    (* Find all values in the module with type t *)
    let value_with_type_t t =
      match t.Types.desc with
      | Tconstr (Pident {name = "t"}, [], _) -> true
      | _ -> false
    in
    (* Find all functions in the module that returns type t *)
    let rec fn_returns_type_t t =
      match t.Types.desc with
      | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> fn_returns_type_t t1
      | Tarrow _ -> (
        match
          Type_utils.extract_function_type ~env ~state ~package:full.package t
        with
        | ( (Nolabel, {desc = Tconstr (Path.Pident {name = "t"}, _, _)}) :: _,
            {desc = Tconstr (Path.Pident {name = "t"}, _, _)} ) ->
          (* Filter out functions that take type t first. These are often
             @send style functions that we don't want to have here because
             they usually aren't meant to create a type t from scratch. *)
          false
        | _args, {desc = Tconstr (Path.Pident {name = "t"}, _, _)} -> true
        | _ -> false)
      | _ -> false
    in
    let get_completion_name exported_value_name =
      let fn_nname =
        Type_utils.get_module_path_relative_to_env ~debug:false
          ~env:(Query_env.from_file full.file)
          ~env_from_item:env (Utils.expand_path path)
      in
      match fn_nname with
      | None -> None
      | Some base ->
        let base =
          Type_utils.remove_opens_from_completion_path ~raw_opens
            ~package:full.package base
        in
        Some ((base |> String.concat ".") ^ "." ^ exported_value_name)
    in
    let get_exported_value_completion name
        (declared : Types.type_expr Declared.t) =
      let type_expr = declared.item in
      if value_with_type_t type_expr then
        get_completion_name name
        |> Option.map (fun name ->
               create name ~includes_snippets:true ~insert_text:name
                 ~kind:(Value type_expr) ~env)
      else if fn_returns_type_t type_expr then
        get_completion_name name
        |> Option.map (fun name ->
               create
                 (Printf.sprintf "%s()" name)
                 ~includes_snippets:true ~insert_text:(name ^ "($0)")
                 ~kind:(Value type_expr) ~env)
      else None
    in
    let completion_items =
      Hashtbl.fold
        (fun name stamp all ->
          match Stamps.find_value env.file.stamps stamp with
          | None -> all
          | Some declared_type_expr -> (
            match get_exported_value_completion name declared_type_expr with
            | None -> all
            | Some completion -> completion :: all))
        env.exported.values_ []
    in

    (* Special casing for things where we want extra things in the completions *)
    let completion_items =
      match path with
      | Pdot (Pdot (Pident {name = "Js"}, "Re", _), "t", _)
      | Pdot (Pdot (Pident {name = "Stdlib"}, "RegExp", _), "t", _)
      | Pdot (Pident {name = "RegExp"}, "t", _) ->
        (* regexps *)
        create "/<regexp>/g" ~insert_text:"/$0/g" ~includes_snippets:true
          ~kind:(Label "Regular expression") ~env
        :: completion_items
      | _ -> completion_items
    in
    completion_items
  | Tbool env ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tbool";
    [
      create "true" ~kind:(Label "bool") ~env;
      create "false" ~kind:(Label "bool") ~env;
    ]
    |> filter_items ~prefix
  | TtypeT {env; path} ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> TtypeT (Pattern)";
    (* This is in patterns. Emit an alias/binding with the module name as a value name. *)
    if prefix <> "" then []
    else
      let module_name =
        match path |> Utils.expand_path with
        | _t :: module_name :: _rest -> String.uncapitalize_ascii module_name
        | _ -> "value"
      in
      [
        create module_name ~kind:(Label module_name) ~env
          ~insert_text:("${0:" ^ module_name ^ "}")
          ~includes_snippets:true;
      ]
  | Tvariant {env; constructors; variant_decl; variant_name} ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tvariant";
    constructors
    |> List.map (fun (constructor : Constructor.t) ->
           let num_args =
             match constructor.args with
             | InlineRecord _ -> 1
             | Args args -> List.length args
           in
           create ?deprecated:constructor.deprecated ~includes_snippets:true
             (constructor.cname.txt
             ^ print_constructor_args num_args ~as_snippet:false)
             ~insert_text:
               (constructor.cname.txt
               ^ print_constructor_args num_args ~as_snippet:true)
             ~kind:
               (Constructor
                  ( constructor,
                    variant_decl |> Shared.decl_to_string variant_name ))
             ~env)
    |> filter_items ~prefix
  | Tpolyvariant {env; constructors; type_expr} ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> Tpolyvariant";
    constructors
    |> List.map (fun (constructor : poly_variant_constructor) ->
           create
             ("#" ^ constructor.display_name
             ^ print_constructor_args
                 (List.length constructor.args)
                 ~as_snippet:false)
             ~includes_snippets:true
             ~insert_text:
               ((if Utils.starts_with prefix "#" then "" else "#")
               ^ constructor.display_name
               ^ print_constructor_args
                   (List.length constructor.args)
                   ~as_snippet:true)
             ~kind:
               (PolyvariantConstructor
                  (constructor, type_expr |> Shared.type_to_string))
             ~env)
    |> filter_items
         ~prefix:(if Utils.starts_with prefix "#" then prefix else "#" ^ prefix)
  | Toption (env, t) ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Toption";
    let inner_type =
      match t with
      | ExtractedType t -> Some (t, None)
      | TypeExpr t ->
        t |> Type_utils.extract_type ~env ~package:full.package ~state
    in
    let expanded_completions =
      match inner_type with
      | None -> []
      | Some (inner_type, _typeArgsContext) ->
        inner_type
        |> complete_typed_value ~raw_opens ~full ~state ~prefix
             ~completion_context ~mode
        |> List.map (fun (c : Completion.t) ->
               {
                 c with
                 name = "Some(" ^ c.name ^ ")";
                 sort_text = None;
                 insert_text =
                   (match c.insert_text with
                   | None -> None
                   | Some insert_text -> Some ("Some(" ^ insert_text ^ ")"));
               })
    in
    let none_case =
      Completion.create "None" ~kind:(kind_from_inner_type t) ~env
    in
    let some_any_case =
      create "Some(_)" ~includes_snippets:true ~kind:(kind_from_inner_type t)
        ~env
        ~insert_text:(Printf.sprintf "Some(%s)" (empty_case 1))
    in
    let completions =
      match completion_context with
      | Some (Completable.CameFromRecordField field_name) ->
        [
          create
            ("Some(" ^ field_name ^ ")")
            ~includes_snippets:true ~kind:(kind_from_inner_type t) ~env
            ~insert_text:("Some(" ^ field_name ^ ")$0");
          some_any_case;
          none_case;
        ]
      | _ -> [none_case; some_any_case]
    in
    completions @ expanded_completions |> filter_items ~prefix
  | Tresult {env; ok_type; error_type} ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tresult";
    let ok_inner_type =
      ok_type |> Type_utils.extract_type ~env ~package:full.package ~state
    in
    let error_inner_type =
      error_type |> Type_utils.extract_type ~env ~package:full.package ~state
    in
    let expanded_ok_completions =
      match ok_inner_type with
      | None -> []
      | Some (inner_type, _) ->
        inner_type
        |> complete_typed_value ~raw_opens ~full ~prefix ~completion_context
             ~mode ~state
        |> List.map (fun (c : Completion.t) ->
               {
                 c with
                 name = "Ok(" ^ c.name ^ ")";
                 sort_text = None;
                 insert_text =
                   (match c.insert_text with
                   | None -> None
                   | Some insert_text -> Some ("Ok(" ^ insert_text ^ ")"));
               })
    in
    let expanded_error_completions =
      match error_inner_type with
      | None -> []
      | Some (inner_type, _) ->
        inner_type
        |> complete_typed_value ~raw_opens ~full ~prefix ~completion_context
             ~mode ~state
        |> List.map (fun (c : Completion.t) ->
               {
                 c with
                 name = "Error(" ^ c.name ^ ")";
                 sort_text = None;
                 insert_text =
                   (match c.insert_text with
                   | None -> None
                   | Some insert_text -> Some ("Error(" ^ insert_text ^ ")"));
               })
    in
    let ok_any_case =
      create "Ok(_)" ~includes_snippets:true ~kind:(Value ok_type) ~env
        ~insert_text:(Printf.sprintf "Ok(%s)" (empty_case 1))
    in
    let error_any_case =
      create "Error(_)" ~includes_snippets:true ~kind:(Value error_type) ~env
        ~insert_text:(Printf.sprintf "Error(%s)" (empty_case 1))
    in
    let completions =
      match completion_context with
      | Some (Completable.CameFromRecordField field_name) ->
        [
          create
            ("Ok(" ^ field_name ^ ")")
            ~includes_snippets:true ~kind:(Value ok_type) ~env
            ~insert_text:("Ok(" ^ field_name ^ ")$0");
          ok_any_case;
          error_any_case;
        ]
      | _ -> [ok_any_case; error_any_case]
    in
    completions @ expanded_ok_completions @ expanded_error_completions
    |> filter_items ~prefix
  | Tuple (env, exprs, typ) ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tuple";
    let num_exprs = List.length exprs in
    [
      create
        (print_constructor_args num_exprs ~as_snippet:false)
        ~includes_snippets:true
        ~insert_text:(print_constructor_args num_exprs ~as_snippet:true)
        ~kind:(Value typ) ~env;
    ]
  | Trecord {env; fields} as extracted_type ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Trecord";
    get_record_completions ~env ~fields ~extracted_type
  | TinlineRecord {env; fields} as extracted_type ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> TinlineRecord";
    get_record_completions ~env ~fields ~extracted_type
  | Tarray (env, typ) ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tarray";
    if prefix = "" then
      [
        create "[]" ~includes_snippets:true ~insert_text:"[$0]" ~sort_text:"A"
          ~kind:
            (match typ with
            | ExtractedType typ ->
              ExtractedType
                ( typ,
                  match mode with
                  | Pattern _ -> `Type
                  | Expression -> `Value )
            | TypeExpr typ -> Value typ)
          ~env;
      ]
    else []
  | Tstring env ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tstring";
    if prefix = "" then
      [
        create "\"\"" ~includes_snippets:true ~insert_text:"\"$0\""
          ~sort_text:"A" ~kind:(Value Predef.type_string) ~env;
      ]
    else []
  | Tfunction {env; typ; args; return_type}
    when prefix = "" && mode = Expression ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> Tfunction #1";
    let mk_fn_args ~as_snippet =
      match args with
      | [(Nolabel, arg_typ)] when Type_utils.type_is_unit arg_typ -> "()"
      | [(Nolabel, arg_typ)] ->
        let var_name =
          Completion_expressions.pretty_print_fn_template_arg_name ~env ~full
            ~state arg_typ
        in
        if as_snippet then "${1:" ^ var_name ^ "}" else var_name
      | _ ->
        let current_unlabelled_index = ref 0 in
        let args_text =
          args
          |> List.map (fun ((label, typ) : typed_fn_arg) ->
                 match label with
                 | Optional {txt = name} -> "~" ^ name ^ "=?"
                 | Labelled {txt = name} -> "~" ^ name
                 | Nolabel ->
                   if Type_utils.type_is_unit typ then "()"
                   else (
                     current_unlabelled_index := !current_unlabelled_index + 1;
                     let num = !current_unlabelled_index in
                     let var_name =
                       Completion_expressions.pretty_print_fn_template_arg_name
                         ~current_index:num ~env ~full ~state typ
                     in
                     if as_snippet then
                       "${" ^ string_of_int num ^ ":" ^ var_name ^ "}"
                     else var_name))
          |> String.concat ", "
        in
        "(" ^ args_text ^ ")"
    in
    let is_async =
      match
        Type_utils.extract_type ~env ~package:full.package ~state return_type
      with
      | Some (Tpromise _, _) -> true
      | _ -> false
    in
    let async_prefix = if is_async then "async " else "" in
    let function_body, function_body_insert_text =
      match args with
      | [(Nolabel, arg_typ)] ->
        let var_name =
          Completion_expressions.pretty_print_fn_template_arg_name ~env ~full
            ~state arg_typ
        in
        ( (" => " ^ if var_name = "()" then "{}" else var_name),
          " => ${0:" ^ var_name ^ "}" )
      | _ -> (" => {}", " => {${0:()}}")
    in
    [
      create
        (async_prefix ^ mk_fn_args ~as_snippet:false ^ function_body)
        ~includes_snippets:true
        ~insert_text:
          (async_prefix
          ^ mk_fn_args ~as_snippet:true
          ^ function_body_insert_text)
        ~sort_text:"A" ~kind:(Value typ) ~env;
    ]
  | Tfunction _ ->
    if Debug.verbose () then
      print_endline "[complete_typed_value]--> Tfunction #other";
    []
  | Texn env ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Texn";
    [
      create
        (["Exn"; "Error(error)"] |> ident)
        ~kind:(Label "Catches errors from JavaScript errors.")
        ~docstring:
          [
            "Matches on a JavaScript error. Read more in the [documentation on \
             catching JS \
             exceptions](https://rescript-lang.org/docs/manual/latest/exception#catching-js-exceptions).";
          ]
        ~env;
    ]
  | Tpromise _ ->
    if Debug.verbose () then print_endline "[complete_typed_value]--> Tpromise";
    []

module String_set = Set.Make (String)

let rec process_completable ~state ~debug ~full ~scope ~env ~pos ~for_hover
    completable =
  if debug then
    Printf.printf "Completable: %s\n" (Completable.to_string completable);
  let package = full.package in
  let raw_opens = Scope.get_raw_opens scope in
  let opens = get_opens ~state ~debug ~raw_opens ~package ~env in
  let all_files = all_files_in_package package in
  let find_type_of_value path =
    path
    |> get_completions_for_path ~state ~debug ~completion_context:Value
         ~exact:true ~opens ~full ~pos ~env ~scope
    |> completions_get_type_env2 ~state ~debug ~full ~opens ~raw_opens ~pos
  in
  match completable with
  | Cnone -> []
  | Cpath context_path ->
    context_path
    |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
         ~pos ~env ~exact:for_hover ~scope
  | Cjsx ([id], prefix, idents_seen) when String.uncapitalize_ascii id = id -> (
    (* Lowercase JSX tag means builtin *)
    let mk_label (name, typ_string) =
      Completion.create name ~kind:(Label typ_string) ~env
    in
    let key_labels =
      if Utils.starts_with "key" prefix then [mk_label ("key", "string")]
      else []
    in
    let path_to_element_props = Type_utils.path_to_element_props package in
    if Debug.verbose () then
      Printf.printf
        "[completing-lowercase-jsx] Attempting to complete from type at %s\n"
        (path_to_element_props |> String.concat ".");
    let from_element_props =
      match
        path_to_element_props
        |> dig_to_record_fields_for_completion ~state ~debug ~package ~opens
             ~full ~pos ~env ~scope
      with
      | None -> None
      | Some fields ->
        Some
          (fields
          |> List.filter_map (fun (f : field) ->
                 if
                   Utils.starts_with f.fname.txt prefix
                   && (for_hover || not (List.mem f.fname.txt idents_seen))
                 then
                   Some
                     ( f.fname.txt,
                       Shared.type_to_string (Utils.unwrap_if_option f.typ) )
                 else None)
          |> List.map mk_label)
    in
    match from_element_props with
    | Some element_props -> element_props
    | None ->
      if debug then
        Printf.printf
          "[completing-lowercase-jsx] could not find element props to complete \
           from.\n";
      key_labels)
  | Cjsx (component_path, prefix, idents_seen) ->
    let labels =
      Completion_jsx.get_jsx_labels ~component_path ~find_type_of_value ~package
        ~state
    in
    let mkLabel_ name typ_string =
      Completion.create name ~kind:(Label typ_string) ~env
    in
    let mk_label (name, typ, _env) =
      mkLabel_ name (typ |> Shared.type_to_string)
    in
    let key_labels =
      if Utils.starts_with "key" prefix then [mkLabel_ "key" "string"] else []
    in
    if labels = [] then []
    else
      (labels
      |> List.filter (fun (name, _t, _env) ->
             Utils.starts_with name prefix
             && name <> "key"
             && (for_hover || not (List.mem name idents_seen)))
      |> List.map mk_label)
      @ key_labels
  | CdecoratorPayload (JsxConfig {prefix; nested}) -> (
    let mk_field ~name ~primitive =
      {
        stamp = -1;
        fname = {loc = Location.none; txt = name};
        optional = true;
        typ = Ctype.newconstr primitive [];
        docstring = [];
        deprecated = None;
      }
    in
    let typ : completion_type =
      Trecord
        {
          env;
          definition = `NameOnly "jsxConfig";
          fields =
            [
              mk_field ~name:"version" ~primitive:Predef.path_int;
              mk_field ~name:"module_" ~primitive:Predef.path_string;
              mk_field ~name:"mode" ~primitive:Predef.path_string;
            ];
        }
    in
    match typ |> Type_utils.resolve_nested ~env ~full ~nested ~state with
    | None -> []
    | Some (typ, _env, completion_context, type_arg_context) ->
      typ
      |> complete_typed_value ?type_arg_context ~raw_opens ~mode:Expression
           ~full ~prefix ~completion_context ~state)
  | CdecoratorPayload (ModuleWithImportAttributes {prefix; nested}) -> (
    let mk_field ~name ~primitive =
      {
        stamp = -1;
        fname = {loc = Location.none; txt = name};
        optional = true;
        typ = Ctype.newconstr primitive [];
        docstring = [];
        deprecated = None;
      }
    in
    let import_attributes_config : completion_type =
      Trecord
        {
          env;
          definition = `NameOnly "importAttributesConfig";
          fields = [mk_field ~name:"type_" ~primitive:Predef.path_string];
        }
    in
    let root_config : completion_type =
      Trecord
        {
          env;
          definition = `NameOnly "moduleConfig";
          fields =
            [
              mk_field ~name:"from" ~primitive:Predef.path_string;
              mk_field ~name:"with" ~primitive:Predef.path_string;
            ];
        }
    in
    let nested, typ =
      match nested with
      | NFollowRecordField {field_name = "with"} :: rest ->
        (rest, import_attributes_config)
      | _ -> (nested, root_config)
    in
    match typ |> Type_utils.resolve_nested ~env ~full ~nested ~state with
    | None -> []
    | Some (typ, _env, completion_context, type_arg_context) ->
      typ
      |> complete_typed_value ?type_arg_context ~raw_opens ~mode:Expression
           ~full ~prefix ~completion_context ~state)
  | CdecoratorPayload (Module prefix) ->
    let package_json_path =
      Utils.find_package_json (full.package.root_path |> Uri.from_path)
    in
    let items_from_package_json =
      match package_json_path with
      | None ->
        if debug then
          Printf.printf
            "Did not find package.json, started looking (going upwards) from: %s\n"
            full.package.root_path;
        []
      | Some path -> (
        match Files.read_file path with
        | None ->
          if debug then print_endline "Could not read package.json";
          []
        | Some s -> (
          match Yojson_helpers.from_string_opt s with
          | Some (`Assoc items) ->
            items
            |> List.filter_map (fun (key, t) ->
                   match (key, t) with
                   | ("dependencies" | "devDependencies"), `Assoc o ->
                     Some
                       (o
                       |> List.filter_map (fun (pkg_name, _) ->
                              match pkg_name with
                              | "rescript" -> None
                              | pkg_name -> Some pkg_name))
                   | _ -> None)
            |> List.flatten
          | _ ->
            if debug then print_endline "Could not parse package.json";
            []))
    in
    (* TODO: Resolve relatives? *)
    let local_items =
      try
        let files =
          Sys.readdir (Filename.dirname (env.file.uri |> Uri.to_path))
          |> Array.to_list
        in
        (* Filter out generated build artifacts from in-source builds. *)
        let res_files =
          String_set.of_list
            (files
            |> List.filter_map (fun f ->
                   if Filename.extension f = ".res" then
                     Some (try Filename.chop_extension f with _ -> f)
                   else None))
        in
        let is_internal_artifact_extension = function
          | ".ast" | ".cmi" | ".cmj" | ".cmt" | ".cmti" | ".iast" -> true
          | _ -> false
        in
        files
        |> List.filter_map (fun file_name ->
               let without_extension =
                 try Filename.chop_extension file_name with _ -> file_name
               in
               if
                 String.ends_with file_name ~suffix:package.suffix
                 && res_files |> String_set.mem without_extension
               then None
               else
                 match Filename.extension file_name with
                 | ".res" | ".resi" | "" -> None
                 | ext when is_internal_artifact_extension ext -> None
                 | _ -> Some ("./" ^ file_name))
        |> List.sort String.compare
      with _ ->
        if debug then print_endline "Could not read relative directory";
        []
    in
    let items = items_from_package_json @ local_items in
    items
    |> List.filter (fun name -> Utils.starts_with name prefix)
    |> List.map (fun name ->
           let is_local = Utils.starts_with name "./" in
           Completion.create name
             ~kind:(Label (if is_local then "Local file" else "Package"))
             ~env)
  | Cdecorator prefix ->
    let mk_decorator (name, docstring, maybe_insert_text) =
      {
        (Completion.create name ~synthetic:true ~includes_snippets:true
           ~kind:(Label "") ~env ?insert_text:maybe_insert_text)
        with
        docstring;
      }
    in
    let is_top_level = String.starts_with ~prefix:"@" prefix in
    let prefix =
      if is_top_level then String.sub prefix 1 (String.length prefix - 1)
      else prefix
    in
    let decorators =
      if is_top_level then Completion_decorators.toplevel
      else Completion_decorators.local
    in
    decorators
    |> List.filter (fun (decorator, _, _) -> Utils.starts_with decorator prefix)
    |> List.map (fun (decorator, maybe_insert_text, doc) ->
           let parts = String.split_on_char '.' prefix in
           let len = String.length prefix in
           let dec2 =
             if List.length parts > 1 then
               String.sub decorator len (String.length decorator - len)
             else decorator
           in
           (dec2, doc, maybe_insert_text))
    |> List.map mk_decorator
  | CnamedArg (cp, prefix, idents_seen) ->
    let labels =
      match
        cp
        |> get_completions_for_context_path ~state ~debug ~full ~opens
             ~raw_opens ~pos ~env ~exact:true ~scope
        |> completions_get_type_env2 ~state ~debug ~full ~opens ~raw_opens ~pos
      with
      | Some (typ, _env) ->
        if debug then
          Printf.printf "Found type for function %s\n"
            (typ |> Shared.type_to_string);

        typ
        |> Type_utils.get_args ~full ~env ~state
        |> List.filter_map (fun arg ->
               match arg with
               | Shared_types.Completable.Labelled name, a -> Some (name, a)
               | Optional name, a -> Some (name, a)
               | _ -> None)
      | None -> []
    in
    let mk_label (name, typ) =
      Completion.create name ~kind:(Label (typ |> Shared.type_to_string)) ~env
    in
    labels
    |> List.filter (fun (name, _t) ->
           Utils.starts_with name prefix
           && (for_hover || not (List.mem name idents_seen)))
    |> List.map mk_label
  | Cpattern {context_path; prefix; nested; fallback; pattern_mode} -> (
    let fallback_or_empty ?items () =
      match (fallback, items) with
      | Some fallback, (None | Some []) ->
        fallback
        |> process_completable ~state ~debug ~full ~scope ~env ~pos ~for_hover
      | _, Some items -> items
      | None, None -> []
    in
    match
      context_path
      |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
           ~pos ~env ~exact:true ~scope
      |> completions_get_type_env2 ~state ~debug ~full ~opens ~raw_opens ~pos
    with
    | Some (typ, env) -> (
      match
        typ
        |> Type_utils.extract_type ~env ~package:full.package ~state
        |> Utils.Option.flat_map (fun (typ, type_arg_context) ->
               typ
               |> Type_utils.resolve_nested ?type_arg_context ~env ~full ~nested
                    ~state)
      with
      | None -> fallback_or_empty ()
      | Some (typ, _env, completion_context, type_arg_context) ->
        let items =
          typ
          |> complete_typed_value ?type_arg_context ~raw_opens
               ~mode:(Pattern pattern_mode) ~full ~prefix ~completion_context
               ~state
        in
        fallback_or_empty ~items ())
    | None -> fallback_or_empty ())
  | Cexpression {context_path; prefix; nested} -> (
    let is_ambigious_record_body_or_jsx_wrap =
      match (context_path, nested) with
      | CJsxPropValue _, [NRecordBody _] -> true
      | _ -> false
    in
    if Debug.verbose () then
      (* This happens in this scenario: `<SomeComponent someProp={<com>}`
           Here, we don't know whether `{}` is just wraps for the type of
           `someProp`, or if it's a record body where we want to complete
            for the fields in the record. We need to look up what the type is
           first before deciding what completions to show. So we do that here.*)
      if is_ambigious_record_body_or_jsx_wrap then
        print_endline
          "[process_completable]--> Cexpression special case: JSX prop value \
           that might be record body or JSX wrap"
      else print_endline "[process_completable]--> Cexpression";
    (* Completions for local things like variables in scope, modules in the
       project, etc. We only add completions when there's a prefix of some sort
       we can filter on, since we know we're in some sort of context, and
       therefore don't want to overwhelm the user with completion items. *)
    let regular_completions =
      if prefix = "" then []
      else
        prefix
        |> get_complementary_completions_for_typed_value ~opens ~all_files ~env
             ~scope
    in
    match
      context_path
      |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
           ~pos ~env ~exact:true ~scope
      |> completions_get_completion_type ~full ~state
    with
    | None ->
      if Debug.verbose () then
        print_endline
          "[process_completable]--> could not get completions for context path";
      regular_completions
    | Some (typ, env) -> (
      match typ |> Type_utils.resolve_nested ~env ~full ~nested ~state with
      | None ->
        if Debug.verbose () then
          print_endline
            "[process_completable]--> could not resolve nested expression path";
        if is_ambigious_record_body_or_jsx_wrap then (
          if Debug.verbose () then
            print_endline
              "[process_completable]--> case is ambigious Jsx prop vs record \
               body case, complete also for the JSX prop value directly";
          let items_for_raw_jsx_prop_value =
            typ
            |> complete_typed_value ~raw_opens ~mode:Expression ~full ~prefix
                 ~completion_context:None ~state
          in
          items_for_raw_jsx_prop_value @ regular_completions)
        else regular_completions
      | Some (typ, _env, completion_context, type_arg_context) -> (
        if Debug.verbose () then
          print_endline
            "[process_completable]--> found type in nested expression \
             completion";
        (* Wrap the insert text in braces when we're completing the root of a
           JSX prop value. *)
        let wrap_insert_text_in_braces =
          if List.length nested > 0 then false
          else
            match context_path with
            | CJsxPropValue _ -> true
            | _ -> false
        in
        let items =
          typ
          |> complete_typed_value ?type_arg_context ~raw_opens ~mode:Expression
               ~full ~prefix ~completion_context ~state
          |> List.map (fun (c : Completion.t) ->
                 if wrap_insert_text_in_braces then
                   {
                     c with
                     insert_text =
                       (match c.insert_text with
                       | None -> None
                       | Some text -> Some ("{" ^ text ^ "}"));
                   }
                 else c)
        in
        match (prefix, completion_context) with
        | "", _ -> items
        | _, None ->
          let items =
            if List.length regular_completions > 0 then
              (* The client will occasionally sort the list of completions alphabetically, disregarding the order
                 in which we send it. This fixes that by providing a sort text making the typed completions
                 guaranteed to end up on top. *)
              items
              |> List.map (fun (c : Completion.t) ->
                     {c with sort_text = Some ("A" ^ " " ^ c.name)})
            else items
          in
          items @ regular_completions
        | _ -> items)))
  | CexhaustiveSwitch {context_path; expr_loc} ->
    let range = Utils.range_of_loc expr_loc in
    let print_failwith_str num = "${" ^ string_of_int num ^ ":%todo}" in
    let with_exhaustive_item ~cases ?(start_index = 0) (c : Completion.t) =
      (* We don't need to write out `switch` here since we know that's what the
         user has already written. Just complete for the rest. *)
      let new_text =
        c.name ^ " {\n"
        ^ (cases
          |> List.mapi (fun index case_text ->
                 "| " ^ case_text ^ " => "
                 ^ print_failwith_str (start_index + index + 1))
          |> String.concat "\n")
        ^ "\n}"
        |> Utils.indent range.start.character
      in
      [
        c;
        {
          c with
          name = c.name ^ " (exhaustive switch)";
          filter_text = Some c.name;
          insert_text_format = Some Snippet;
          insert_text = Some new_text;
          kind = Snippet "insert exhaustive switch for value";
        };
      ]
    in
    let completions_for_context_path =
      context_path
      |> get_completions_for_context_path ~state ~debug ~full ~opens ~raw_opens
           ~pos ~env ~exact:for_hover ~scope
    in
    completions_for_context_path
    |> List.map (fun (c : Completion.t) ->
           match c.kind with
           | Value typ_expr -> (
             match
               typ_expr |> Type_utils.extract_type ~env:c.env ~package ~state
             with
             | Some (Tvariant v, _) ->
               with_exhaustive_item c
                 ~cases:
                   (v.constructors
                   |> List.map (fun (constructor : Constructor.t) ->
                          constructor.cname.txt
                          ^
                          match constructor.args with
                          | Args [] -> ""
                          | _ -> "(_)"))
             | Some (Tpolyvariant v, _) ->
               with_exhaustive_item c
                 ~cases:
                   (v.constructors
                   |> List.map (fun (constructor : poly_variant_constructor) ->
                          "#" ^ constructor.display_name
                          ^
                          match constructor.args with
                          | [] -> ""
                          | _ -> "(_)"))
             | Some (Toption (_env, _typ), _) ->
               with_exhaustive_item c ~cases:["Some($1)"; "None"] ~start_index:1
             | Some (Tresult _, _) ->
               with_exhaustive_item c ~cases:["Ok($1)"; "Error($1)"]
                 ~start_index:1
             | Some (Tbool _, _) ->
               with_exhaustive_item c ~cases:["true"; "false"]
             | _ -> [c])
           | _ -> [c])
    |> List.flatten
  | ChtmlElement {prefix} ->
    Completion_jsx.html_elements
    |> List.filter_map (fun (element_name, description, deprecated) ->
           if Utils.starts_with element_name prefix then
             let name = "<" ^ element_name ^ ">" in
             Some
               (Completion.create name ~synthetic:true ~kind:(Label name)
                  ~detail:description ~env ~docstring:[description]
                  ~insert_text:element_name
                  ?deprecated:
                    (match deprecated with
                    | true -> Some "true"
                    | false -> None))
           else None)
  | CextensionNode prefix ->
    if Utils.starts_with "todo" prefix then
      let detail =
        "`%todo` is used to tell the compiler that some code still needs to be \
         implemented."
      in
      [
        Completion.create "todo" ~synthetic:true ~kind:(Label "todo") ~detail
          ~env ~insert_text:"todo";
        Completion.create "todo (with payload)" ~synthetic:true
          ~includes_snippets:true ~kind:(Label "todo")
          ~detail:(detail ^ " With a payload.")
          ~env ~insert_text:"todo(\"${0:TODO}\")";
      ]
    else []
