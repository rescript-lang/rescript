let ( /+ ) = Filename.concat

let package_path name =
  let scope = Filename.dirname name in
  let name = Filename.basename name in
  match scope.[0] with
  | '@' -> scope /+ name
  | _ -> name

let resolve_deno_pnpm ~start_path name =
  let create_path folder =
    start_path /+ "node_modules" /+ folder /+ "node_modules"
    /+ package_path name
  in
  let deno = create_path ".deno" in
  let pnpm = create_path ".pnpm" in
  match (Files.if_exists deno, Files.if_exists pnpm) with
  | Some deno, None -> Some deno
  | None, Some pnpm -> Some pnpm
  | Some deno, Some pnpm ->
    failwith
      (Printf.sprintf
         "Failed to resolve path for %s. Conflict: two directories found. one \
          for Deno (%s) and one for pnpm (%s)."
         name deno pnpm)
  | _ -> None

let rec resolve_from_node_modules ~start_path name =
  let path = start_path /+ "node_modules" /+ package_path name in
  if Files.exists path then Some path
  else
    match resolve_deno_pnpm ~start_path name with
    | Some p -> Some p
    | None ->
      if Filename.dirname start_path = start_path then None
      else
        resolve_from_node_modules ~start_path:(Filename.dirname start_path) name

let resolve_node_module_path ?(fallback_to_runtime_package = true) ~start_path
    name =
  match resolve_from_node_modules ~start_path name with
  | Some path -> Some path
  | None
    when fallback_to_runtime_package
         && name = Runtime_package.name
         && Files.exists !Runtime_package.path ->
    (* Compiler-local fallback for development builds where the runtime package
       is not installed in the target workspace. Real project installs must win
       so LSP definition locations point at the workspace's node_modules. *)
    Some !Runtime_package.path
  | None -> None
