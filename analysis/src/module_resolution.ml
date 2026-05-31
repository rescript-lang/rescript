let ( /+ ) = Filename.concat

let rec resolve_node_module_path ~start_path name =
  if name = "@rescript/runtime" then
    (* Hack: we need a reliable way to resolve modules in monorepos. *)
    Some !Runtime_package.path
  else
    let scope = Filename.dirname name in
    let name = Filename.basename name in
    let name =
      match scope.[0] with
      | '@' -> scope /+ name
      | _ -> name
    in
    let path = start_path /+ "node_modules" /+ name in
    if Files.exists path then Some path
    else if Filename.dirname start_path = start_path then None
    else resolve_node_module_path ~start_path:(Filename.dirname start_path) name
