open Gen_type_common

let rec handle_namespace ~name dep =
  match dep with
  | External _ | Internal _ -> dep
  | Dot (External s, module_name) when s = name -> External module_name
  | Dot (dep1, s) -> Dot (dep1 |> handle_namespace ~name, s)

let rec from_path1 ~config ~type_env (path : Path.t) =
  match path with
  | Pident id -> (
    let name = id |> Ident.name in
    match type_env |> Type_env.lookup ~name with
    | None -> (type_env, External name)
    | Some type_env1 -> (
      let type_env2 =
        match type_env |> Type_env.get_module ~name with
        | Some type_env2 -> type_env2
        | None -> type_env1
      in
      match type_env1 |> Type_env.expand_alias_to_external_module ~name with
      | Some dep -> (type_env2, dep)
      | None ->
        let resolved_name =
          name |> Type_env.add_module_path ~type_env:type_env1
        in
        (type_env2, Internal resolved_name)))
  | Pdot (Pident id, s, _pos)
    when id |> Scoped_package.is_generated_module ~config ->
    ( type_env,
      External (s |> Scoped_package.add_generated_module ~generated_module:id)
    )
  | Pdot (p, s, _pos) -> (
    let type_env_from_p, dep = p |> from_path1 ~config ~type_env in
    match
      type_env_from_p |> Type_env.expand_alias_to_external_module ~name:s
    with
    | Some dep -> (type_env_from_p, dep)
    | None -> (type_env_from_p, Dot (dep, s)))
  | Papply _ ->
    ( type_env,
      Internal ("__Papply_unsupported_genType__" |> Resolved_name.from_string)
    )

let rec is_internal dep =
  match dep with
  | External _ -> false
  | Internal _ -> true
  | Dot (d, _) -> d |> is_internal

let from_path ~config ~type_env path =
  let _, dep = path |> from_path1 ~config ~type_env in
  if !Debug.type_resolution then
    Log_.item "fromPath path:%s typeEnv:%s %s resolved:%s\n" (path |> Path.name)
      (type_env |> Type_env.to_string)
      (match dep |> is_internal with
      | true -> "Internal"
      | false -> "External")
      (dep |> dep_to_string);
  match config.namespace with
  | None -> dep
  | Some name -> dep |> handle_namespace ~name

let rec get_outer_module_name dep =
  match dep with
  | External name -> name |> Module_name.from_string_unsafe
  | Internal resolved_name ->
    resolved_name |> Resolved_name.to_string |> Module_name.from_string_unsafe
  | Dot (dep1, _) -> dep1 |> get_outer_module_name

let rec remove_external_outer_module dep =
  match dep with
  | External _ | Internal _ -> dep
  | Dot (External _, s) -> External s
  | Dot (dep1, s) -> Dot (dep1 |> remove_external_outer_module, s)
