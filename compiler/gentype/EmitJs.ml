open GenTypeCommon

type env = {
  requires_early: ImportPath.t Config.ModuleNameMap.t;
  requires: ImportPath.t Config.ModuleNameMap.t;
      (**  For each .cmt we import types from, keep the map of exported types *)
  cmt_to_export_type_map: CodeItem.export_type_map StringMap.t;
      (**  Map of types imported from other files *)
  export_type_map_from_other_files: CodeItem.export_type_map;
  imported_value_or_component: bool;
}

let require_module ~import ~env ~import_path module_name =
  let requires =
    match import with
    | true -> env.requires_early
    | false -> env.requires
  in
  let requires_new =
    requires |> Config.ModuleNameMap.add module_name import_path
  in
  match import with
  | true -> {env with requires_early = requires_new}
  | false -> {env with requires = requires_new}

let create_export_type_map ~config ~file ~from_cmt_read_recursively
    (type_declarations : CodeItem.type_declaration list) :
    CodeItem.export_type_map =
  if !Debug.code_items then Log_.item "Create Type Map for %s\n" file;
  let update_export_type_map (export_type_map : CodeItem.export_type_map)
      (type_declaration : CodeItem.type_declaration) : CodeItem.export_type_map
      =
    let add_export_type ~annotation
        ({resolved_type_name; type_; type_vars} : CodeItem.export_type) =
      let annotation =
        match annotation with
        | Annotation.NoGenType when from_cmt_read_recursively ->
          Annotation.GenType
        | _ -> annotation
      in
      if !Debug.code_items then
        Log_.item "Type Map: %s%s%s\n"
          (resolved_type_name |> ResolvedName.to_string)
          (match type_vars = [] with
          | true -> ""
          | false -> "(" ^ (type_vars |> String.concat ",") ^ ")")
          (" "
          ^ (annotation |> Annotation.to_string |> EmitText.comment)
          ^ " = "
          ^ (type_
            |> EmitType.type_to_string ~config ~type_name_is_interface:(fun _ ->
                   false)));
      export_type_map
      |> StringMap.add
           (resolved_type_name |> ResolvedName.to_string)
           {CodeItem.type_vars; type_; annotation}
    in
    match type_declaration.export_from_type_declaration with
    | {export_type; annotation} -> export_type |> add_export_type ~annotation
  in
  type_declarations |> List.fold_left update_export_type_map StringMap.empty

let code_item_to_string ~config ~type_name_is_interface (code_item : CodeItem.t)
    =
  match code_item with
  | ExportValue {resolved_name; type_} ->
    "ExportValue" ^ " resolvedName:"
    ^ ResolvedName.to_string resolved_name
    ^ " type:"
    ^ EmitType.type_to_string ~config ~type_name_is_interface type_
  | ImportValue {import_annotation} ->
    "ImportValue " ^ (import_annotation.import_path |> ImportPath.dump)

let emit_export_type ~emitters ~config ~type_name_is_interface
    {
      CodeItem.loc;
      name_as;
      opaque;
      type_;
      type_vars;
      resolved_type_name;
      doc_string;
    } =
  let free_type_vars = TypeVars.free type_ in
  let is_gadt =
    free_type_vars |> List.exists (fun s -> not (List.mem s type_vars))
  in
  let opaque =
    match opaque with
    | Some true -> opaque
    | _ when is_gadt ->
      Log_.Color.setup ();
      Log_.info ~loc ~name:"Warning genType" (fun ppf () ->
          Format.fprintf ppf
            "GADT types are not supported: exporting %s as opaque type"
            (resolved_type_name |> ResolvedName.to_string));
      Some true
    | _ -> opaque
  in
  let opaque =
    match opaque with
    | Some opaque -> opaque
    | None -> false
  in
  resolved_type_name |> ResolvedName.to_string
  |> EmitType.emit_export_type ~config ~emitters ~name_as ~opaque ~type_
       ~type_name_is_interface ~type_vars ~doc_string

let type_name_is_interface ~(export_type_map : CodeItem.export_type_map)
    ~(export_type_map_from_other_files : CodeItem.export_type_map) type_name =
  let type_is_interface type_ =
    match type_ with
    | Object _ -> true
    | _ -> false
  in
  match export_type_map |> StringMap.find type_name with
  | {type_} -> type_ |> type_is_interface
  | exception Not_found -> (
    match export_type_map_from_other_files |> StringMap.find type_name with
    | {type_} -> type_ |> type_is_interface
    | exception Not_found -> false)

let emit_export_from_type_declaration ~config ~emitters ~env
    ~type_name_is_interface
    (export_from_type_declaration : CodeItem.export_from_type_declaration) =
  ( env,
    export_from_type_declaration.export_type
    |> emit_export_type ~emitters ~config ~type_name_is_interface )

let emit_export_from_type_declarations ~config ~emitters ~env
    ~type_name_is_interface export_from_type_declarations =
  export_from_type_declarations
  |> List.fold_left
       (fun (env, emitters) ->
         emit_export_from_type_declaration ~config ~emitters ~env
           ~type_name_is_interface)
       (env, emitters)

let emit_code_item ~config ~emitters ~module_items_emitter ~env ~file_name
    ~output_file_relative ~resolver ~inline_one_level ~type_name_is_interface
    code_item =
  if !Debug.code_items then
    Log_.item "Code Item: %s\n"
      (code_item |> code_item_to_string ~config ~type_name_is_interface);
  match code_item with
  | ImportValue {as_path; import_annotation; type_; value_name} ->
    let import_path = import_annotation.import_path in
    let first_name_in_path, rest_of_path =
      match value_name = as_path with
      | true -> (value_name, "")
      | false -> (
        match as_path |> String.split_on_char '.' with
        | x :: y -> (x, "" :: y |> String.concat ".")
        | _ -> (as_path, ""))
    in
    let emitters, imported_as_name, env =
      (* emit an import {... as ...} immediately *)
      let value_name_not_checked = value_name ^ "NotChecked" in
      let emitters =
        import_path
        |> EmitType.emit_import_value_as_early ~emitters
             ~name:first_name_in_path ~name_as:(Some value_name_not_checked)
      in
      (emitters, value_name_not_checked, env)
    in
    let type_ =
      match type_ with
      | Function
          ({
             arg_types = [{a_type = Object (closed_flag, fields); a_name}];
             ret_type;
           } as function_)
        when ret_type |> EmitType.is_type_function_component ~fields ->
        (* JSX V3 *)
        let fields =
          fields
          |> List.map (fun (field : field) ->
                 match
                   field.name_js = "children"
                   && field.type_ |> EmitType.is_type_react_element
                 with
                 | true -> {field with type_ = EmitType.type_react_child}
                 | false -> field)
        in
        let function_ =
          {
            function_ with
            arg_types = [{a_type = Object (closed_flag, fields); a_name}];
          }
        in
        Function function_
      | Function
          ({
             arg_types = [{a_type = Ident {name} as props_type; a_name}];
             ret_type;
           } as function_)
        when Filename.check_suffix name "props"
             && ret_type |> EmitType.is_type_function_component ~fields:[] -> (
        match inline_one_level props_type with
        | Object (closed_flags, fields) ->
          (* JSX V3 *)
          let fields =
            Ext_list.filter_map fields (fun (field : field) ->
                match field.name_js with
                | "children" when field.type_ |> EmitType.is_type_react_element
                  ->
                  Some {field with type_ = EmitType.type_react_child}
                | "key" ->
                  (* Filter out key, which is added to the props type definition in V4 *)
                  None
                | _ -> Some field)
          in
          let function_ =
            {
              function_ with
              arg_types = [{a_type = Object (closed_flags, fields); a_name}];
            }
          in
          Function function_
        | _ -> type_)
      | _ -> type_
    in
    let value_name_type_checked = value_name ^ "TypeChecked" in
    let emitters =
      imported_as_name ^ rest_of_path
      |> EmitType.emit_export_const ~config
           ~comment:
             ("In case of type error, check the type of '" ^ value_name
            ^ "' in '"
             ^ (file_name |> ModuleName.to_string)
             ^ ".res'" ^ " and '"
             ^ (import_path |> ImportPath.emit)
             ^ "'.")
           ~early:true ~emitters ~name:value_name_type_checked ~type_
           ~type_name_is_interface
    in
    let value_name_not_default =
      match value_name = "default" with
      | true -> Runtime.default
      | false -> value_name
    in
    let emitters =
      value_name_type_checked
      |> EmitType.emit_type_cast ~config ~type_ ~type_name_is_interface
      |> EmitType.emit_export_const
           ~comment:
             ("Export '" ^ value_name_not_default
            ^ "' early to allow circular import from the '.bs.js' file.")
           ~config ~early:true ~emitters ~name:value_name_not_default
           ~type_:unknown ~type_name_is_interface
    in
    let emitters =
      match value_name = "default" with
      | true -> EmitType.emit_export_default ~emitters value_name_not_default
      | false -> emitters
    in
    ({env with imported_value_or_component = true}, emitters)
  | ExportValue
      {doc_string; module_access_path; original_name; resolved_name; type_} ->
    let resolved_name_str = ResolvedName.to_string resolved_name in
    let import_path =
      file_name
      |> ModuleResolver.resolve_module ~config ~import_extension:config.suffix
           ~output_file_relative ~resolver ~use_bs_dependencies:false
    in
    let file_name_js = file_name |> ModuleName.for_js_file in
    let env_with_requires =
      file_name_js |> require_module ~import:false ~env ~import_path
    in
    let default = "default" in
    let make = "make" in
    let name =
      match original_name = default with
      | true -> Runtime.default
      | false -> resolved_name_str
    in
    let module HookType = struct
      type t = {
        props_type: type_;
        resolved_type_name: ResolvedName.t;
        type_vars: string list;
      }
    end in
    let type_, hook_type =
      match type_ with
      | Function
          ({
             arg_types = [{a_type = Object (closed_flags, fields)}];
             ret_type;
             type_vars;
           } as function_)
        when ret_type |> EmitType.is_type_function_component ~fields ->
        (* JSX V3 *)
        let props_type =
          let fields =
            fields
            |> List.map (fun (field : field) ->
                   match
                     field.name_js = "children"
                     && field.type_ |> EmitType.is_type_react_element
                   with
                   | true -> {field with type_ = EmitType.type_react_child}
                   | false -> field)
          in
          Object (closed_flags, fields)
        in
        let function_ =
          {function_ with arg_types = [{a_name = ""; a_type = props_type}]}
        in
        let resolved_type_name =
          if
            (not config.emit_type_prop_done)
            && (original_name = default || original_name = make)
          then (
            config.emit_type_prop_done <- true;
            ResolvedName.from_string "Props")
          else ResolvedName.from_string name |> ResolvedName.dot "Props"
        in
        ( Function function_,
          Some {HookType.props_type; resolved_type_name; type_vars} )
      | Function
          ({arg_types = [{a_type = Ident {name} as props_type}]; ret_type} as
           function_)
        when Filename.check_suffix name "props"
             && ret_type |> EmitType.is_type_function_component ~fields:[] ->
        let comp_type =
          match inline_one_level props_type with
          | Object (closed_flags, fields) ->
            (* JSX V4 *)
            let props_type =
              let fields =
                Ext_list.filter_map fields (fun (field : field) ->
                    match field.name_js with
                    | "children"
                      when field.type_ |> EmitType.is_type_react_element ->
                      Some {field with type_ = EmitType.type_react_child}
                    | "key" ->
                      (* Filter out key, which is added to the props type definition in V4 *)
                      None
                    | _ -> Some field)
              in
              Object (closed_flags, fields)
            in
            let function_ =
              {function_ with arg_types = [{a_name = ""; a_type = props_type}]}
            in
            Function function_
          | _ -> type_
        in
        (comp_type, None)
      | _ -> (type_, None)
    in

    resolved_name
    |> ExportModule.extend_export_modules ~doc_string ~module_items_emitter
         ~type_;
    let emitters =
      match hook_type with
      | Some {props_type; resolved_type_name; type_vars} ->
        let export_type =
          ({
             loc = Location.none;
             name_as = None;
             opaque = Some false;
             type_ = props_type;
             type_vars;
             resolved_type_name;
             doc_string;
           }
            : CodeItem.export_type)
        in
        (* For doc gen (https://github.com/cristianoc/genType/issues/342) *)
        config.emit_import_react <- true;
        emit_export_type ~emitters ~config ~type_name_is_interface export_type
      | _ -> emitters
    in
    let emitters =
      (file_name_js |> ModuleName.to_string)
      ^ "."
      ^ (module_access_path |> Runtime.emit_module_access_path ~config)
      |> EmitType.emit_export_const ~config ~doc_string ~early:false ~emitters
           ~name ~type_ ~type_name_is_interface
    in
    let emitters =
      match original_name = default with
      | true -> EmitType.emit_export_default ~emitters Runtime.default
      | false -> emitters
    in
    (env_with_requires, emitters)

let emit_code_items ~config ~output_file_relative ~emitters
    ~module_items_emitter ~env ~file_name ~resolver ~type_name_is_interface
    ~inline_one_level code_items =
  code_items
  |> List.fold_left
       (fun (env, emitters) ->
         emit_code_item ~config ~emitters ~module_items_emitter ~env ~file_name
           ~output_file_relative ~resolver ~inline_one_level
           ~type_name_is_interface)
       (env, emitters)

let emit_requires ~imported_value_or_component ~early ~config ~requires emitters
    =
  Config.ModuleNameMap.fold
    (fun module_name import_path emitters ->
      import_path
      |> EmitType.emit_require ~imported_value_or_component ~early ~emitters
           ~config ~module_name)
    requires emitters

let type_get_inlined ~config ~export_type_map type_ =
  type_
  |> Converter.type_get_inlined ~config
       ~lookup_id:(fun s -> export_type_map |> StringMap.find s)
       ~type_name_is_interface:(fun _ -> false)

(** Read the cmt file referenced in an import type,
   and recursively for the import types obtained from reading the cmt file. *)
let rec read_cmt_files_recursively ~config ~env
    ~input_cmt_translate_type_declarations ~output_file_relative ~resolver
    {CodeItem.type_name; as_type_name; import_path} =
  let update_type_map_from_other_files ~as_type ~export_type_map_from_cmt env =
    match export_type_map_from_cmt |> StringMap.find type_name with
    | (export_type_item : CodeItem.export_type_item) ->
      let type_ =
        export_type_item.type_
        |> type_get_inlined ~config ~export_type_map:export_type_map_from_cmt
      in
      {
        env with
        export_type_map_from_other_files =
          env.export_type_map_from_other_files
          |> StringMap.add as_type {export_type_item with type_};
      }
    | exception Not_found -> env
  in
  let cmt_file =
    import_path
    |> ImportPath.to_cmt ~config ~output_file_relative
    |> Paths.get_cmt_file
  in
  match as_type_name with
  | Some as_type when cmt_file <> "" -> (
    match env.cmt_to_export_type_map |> StringMap.find cmt_file with
    | export_type_map_from_cmt ->
      env |> update_type_map_from_other_files ~as_type ~export_type_map_from_cmt
    | exception Not_found ->
      (* cmt file not read before: this ensures termination  *)
      let type_declarations =
        Cmt_format.read_cmt cmt_file
        |> input_cmt_translate_type_declarations ~config ~output_file_relative
             ~resolver
        |> fun (x : CodeItem.translation) -> x.type_declarations
      in
      let export_type_map_from_cmt =
        type_declarations
        |> create_export_type_map ~config ~from_cmt_read_recursively:true
             ~file:
               (cmt_file |> Filename.basename
              |> (Filename.chop_extension [@doesNotRaise]))
      in
      let cmt_to_export_type_map =
        env.cmt_to_export_type_map
        |> StringMap.add cmt_file export_type_map_from_cmt
      in
      let env =
        {env with cmt_to_export_type_map}
        |> update_type_map_from_other_files ~as_type ~export_type_map_from_cmt
      in
      let new_import_types =
        type_declarations
        |> List.map (fun (type_declaration : CodeItem.type_declaration) ->
               type_declaration.import_types)
        |> List.concat
      in
      new_import_types
      |> List.fold_left
           (fun env new_import_type ->
             new_import_type
             |> read_cmt_files_recursively ~config ~env
                  ~input_cmt_translate_type_declarations ~output_file_relative
                  ~resolver)
           env)
  | _ -> env

let emit_import_type ~config ~emitters ~env
    ~input_cmt_translate_type_declarations ~output_file_relative ~resolver
    ~type_name_is_interface
    ({CodeItem.type_name; as_type_name; import_path} as import_type) =
  let env =
    import_type
    |> read_cmt_files_recursively ~config ~env
         ~input_cmt_translate_type_declarations ~output_file_relative ~resolver
  in
  let emitters =
    EmitType.emit_import_type_as ~emitters ~config ~type_name ~as_type_name
      ~type_name_is_interface:(type_name_is_interface ~env)
      ~import_path
  in
  (env, emitters)

let emit_import_types ~config ~emitters ~env
    ~input_cmt_translate_type_declarations ~output_file_relative ~resolver
    ~type_name_is_interface import_types =
  import_types
  |> List.fold_left
       (fun (env, emitters) ->
         emit_import_type ~config ~emitters ~env
           ~input_cmt_translate_type_declarations ~output_file_relative
           ~resolver ~type_name_is_interface)
       (env, emitters)

let get_annotated_typed_declarations ~annotated_set type_declarations =
  type_declarations
  |> List.map (fun type_declaration ->
         let name_in_annotated_set =
           annotated_set
           |> StringSet.mem
                (type_declaration.CodeItem.export_from_type_declaration
                   .export_type
                   .resolved_type_name |> ResolvedName.to_string)
         in
         if name_in_annotated_set then
           {
             type_declaration with
             export_from_type_declaration =
               {
                 type_declaration.export_from_type_declaration with
                 annotation = GenType;
               };
           }
         else type_declaration)
  |> List.filter
       (fun
         ({export_from_type_declaration = {annotation}} :
           CodeItem.type_declaration)
       -> annotation <> NoGenType)

let propagate_annotation_to_sub_types ~code_items
    (type_map : CodeItem.export_type_map) =
  let annotated_set = ref StringSet.empty in
  let initial_annotated_types =
    type_map |> StringMap.bindings
    |> List.filter (fun (_, {CodeItem.annotation}) ->
           annotation = Annotation.GenType)
    |> List.map (fun (_, {CodeItem.type_}) -> type_)
  in
  let types_of_exported_value (code_item : CodeItem.t) =
    match code_item with
    | ExportValue {type_} | ImportValue {type_} -> [type_]
  in
  let types_of_exported_values =
    code_items |> List.map types_of_exported_value |> List.concat
  in
  let visit_typ_and_update_marked type0 =
    let visited = ref StringSet.empty in
    let rec visit type_ =
      match type_ with
      | Ident {name = type_name; type_args} ->
        if !visited |> StringSet.mem type_name then ()
        else (
          visited := !visited |> StringSet.add type_name;
          type_args |> List.iter visit;
          match type_map |> StringMap.find type_name with
          | {annotation = GenType | GenTypeOpaque} -> ()
          | {type_ = type1; annotation = NoGenType} ->
            if !Debug.translation then
              Log_.item "Marking Type As Annotated %s\n" type_name;
            annotated_set := !annotated_set |> StringSet.add type_name;
            type1 |> visit
          | exception Not_found ->
            annotated_set := !annotated_set |> StringSet.add type_name)
      | Array (t, _) | Dict t -> t |> visit
      | Function {arg_types; ret_type} ->
        arg_types |> List.iter (fun {a_type} -> visit a_type);
        ret_type |> visit
      | Object (_, fields) ->
        fields |> List.iter (fun {type_} -> type_ |> visit)
      | Option t | Null t | Nullable t | Promise t -> t |> visit
      | Tuple inner_types -> inner_types |> List.iter visit
      | TypeVar _ -> ()
      | Variant {inherits; payloads} ->
        inherits |> List.iter visit;
        payloads |> List.iter (fun {t} -> t |> visit)
    in
    type0 |> visit
  in
  initial_annotated_types @ types_of_exported_values
  |> List.iter visit_typ_and_update_marked;
  let new_type_map =
    type_map
    |> StringMap.mapi
         (fun type_name (export_type_item : CodeItem.export_type_item) ->
           {
             export_type_item with
             annotation =
               (match !annotated_set |> StringSet.mem type_name with
               | true -> Annotation.GenType
               | false -> export_type_item.annotation);
           })
  in
  (new_type_map, !annotated_set)

let emit_translation_as_string ~config ~file_name
    ~input_cmt_translate_type_declarations ~output_file_relative ~resolver
    (translation : Translation.t) =
  let initial_env =
    {
      requires = Config.ModuleNameMap.empty;
      requires_early = Config.ModuleNameMap.empty;
      cmt_to_export_type_map = StringMap.empty;
      export_type_map_from_other_files = StringMap.empty;
      imported_value_or_component = false;
    }
  in
  let export_type_map, annotated_set =
    translation.type_declarations
    |> create_export_type_map ~config
         ~file:(file_name |> ModuleName.to_string)
         ~from_cmt_read_recursively:false
    |> propagate_annotation_to_sub_types ~code_items:translation.code_items
  in
  let annotated_type_declarations =
    translation.type_declarations
    |> get_annotated_typed_declarations ~annotated_set
  in
  let import_types_from_type_declarations =
    annotated_type_declarations
    |> List.map (fun (type_declaration : CodeItem.type_declaration) ->
           type_declaration.import_types)
    |> List.concat
  in
  let export_from_type_declarations =
    annotated_type_declarations
    |> List.map (fun (type_declaration : CodeItem.type_declaration) ->
           type_declaration.export_from_type_declaration)
  in
  let type_name_is_interface ~env =
    type_name_is_interface ~export_type_map
      ~export_type_map_from_other_files:env.export_type_map_from_other_files
  in
  let lookupId_ ~env s =
    try export_type_map |> StringMap.find s
    with Not_found -> env.export_type_map_from_other_files |> StringMap.find s
  in
  let emitters = Emitters.initial
  and module_items_emitter = ExportModule.create_module_items_emitter ()
  and env = initial_env in
  let env, emitters =
    (* imports from type declarations go first to build up type tables *)
    import_types_from_type_declarations @ translation.import_types
    |> List.sort_uniq Translation.import_type_compare
    |> emit_import_types ~config ~emitters ~env
         ~input_cmt_translate_type_declarations ~output_file_relative ~resolver
         ~type_name_is_interface
  in
  let env, emitters =
    export_from_type_declarations
    |> emit_export_from_type_declarations ~config ~emitters ~env
         ~type_name_is_interface:(type_name_is_interface ~env)
  in
  let inline_one_level type_ =
    match type_ with
    | Ident {builtin = false; name; type_args} -> (
      match name |> lookupId_ ~env with
      | {type_; type_vars} ->
        let pairs =
          try List.combine type_vars type_args with Invalid_argument _ -> []
        in
        let f type_var =
          match
            pairs |> List.find (fun (type_var1, _) -> type_var = type_var1)
          with
          | _, type_argument -> Some type_argument
          | exception Not_found -> None
        in
        type_ |> TypeVars.substitute ~f
      | exception Not_found -> type_)
    | _ -> type_
  in
  let env, emitters =
    translation.code_items
    |> emit_code_items ~config ~emitters ~module_items_emitter ~env ~file_name
         ~output_file_relative ~resolver ~inline_one_level
         ~type_name_is_interface:(type_name_is_interface ~env)
  in
  let emitters =
    match config.emit_import_react with
    | true -> EmitType.emit_import_react ~emitters
    | false -> emitters
  in
  let env =
    match config.emit_import_curry with
    | true ->
      ModuleName.curry
      |> require_module ~import:true ~env
           ~import_path:(ImportPath.bs_curry_path ~config)
    | false -> env
  in
  let final_env = env in
  let emitters =
    module_items_emitter
    |> ExportModule.emit_all_module_items ~config ~emitters ~file_name
  in
  emitters
  |> emit_requires ~imported_value_or_component:false ~early:true ~config
       ~requires:final_env.requires_early
  |> emit_requires
       ~imported_value_or_component:final_env.imported_value_or_component
       ~early:false ~config ~requires:final_env.requires
  |> Emitters.to_string ~separator:"\n\n"
