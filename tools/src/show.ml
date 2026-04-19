open Analysis
open SharedTypes

type show_kind = Lint_support.SymbolKind.t = Auto | Module | Value | Type

type comments_mode = Include | Omit

type run_result = {output: string}

let show_kind_to_string = Lint_support.SymbolKind.to_string
let show_kind_of_string = Lint_support.SymbolKind.of_string

let comments_mode_of_string = function
  | "include" -> Some Include
  | "omit" -> Some Omit
  | _ -> None

let normalize_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let package_for_context path =
  let uri = Uri.fromPath path in
  Packages.getPackage ~uri

let display_name path =
  match List.rev path with
  | name :: _ -> name
  | [] -> ""

let trim_trailing_horizontal_whitespace value =
  let rec loop last =
    if last < 0 then ""
    else
      match value.[last] with
      | ' ' | '\t' -> loop (last - 1)
      | _ -> String.sub value 0 (last + 1)
  in
  loop (String.length value - 1)

let normalize_output output =
  let rec drop_trailing_blank_lines_rev = function
    | "" :: rest -> drop_trailing_blank_lines_rev rest
    | lines -> lines
  in
  output |> String.split_on_char '\n'
  |> List.map trim_trailing_horizontal_whitespace
  |> List.rev |> drop_trailing_blank_lines_rev |> List.rev |> String.concat "\n"

let docstring_for_mode ~comments_mode docstring =
  match comments_mode with
  | Include -> docstring
  | Omit -> []

let resolve_module_hover ~package ~comments_mode path =
  let name = display_name path in
  match Lint_support.SymbolPath.resolve_top_level_module ~package path with
  | Some file ->
    Hover.showModule
      ~docstring:(docstring_for_mode ~comments_mode file.structure.docstring)
      ~name ~file ~package None
  | None -> (
    match
      Lint_support.SymbolPath.resolve_exported ~package ~tip:Tip.Module path
    with
    | None -> None
    | Some (env, stamp) -> (
      match Stamps.findModule env.file.stamps stamp with
      | None -> None
      | Some declared ->
        Hover.showModule
          ~docstring:(docstring_for_mode ~comments_mode declared.docstring)
          ~name ~file:env.file ~package (Some declared)))

let resolve_value_hover ~package ~comments_mode path =
  match
    Lint_support.SymbolPath.resolve_exported ~package ~tip:Tip.Value path
  with
  | None -> None
  | Some (env, stamp) -> (
    match Stamps.findValue env.file.stamps stamp with
    | None -> None
    | Some declared ->
      Some
        (Hover.hoverWithExpandedTypes ~file:env.file ~package
           ~supportsMarkdownLinks:false
           ~docstring:(docstring_for_mode ~comments_mode declared.docstring)
           declared.item))

let resolve_type_hover ~package path =
  match
    Lint_support.SymbolPath.resolve_exported ~package ~tip:Tip.Type path
  with
  | None -> None
  | Some (env, stamp) -> (
    match Stamps.findType env.file.stamps stamp with
    | None -> None
    | Some declared -> (
      let type_definition =
        Markdown.codeBlock
          (Shared.declToString declared.name.txt declared.item.decl)
      in
      match declared.item.decl.type_manifest with
      | None -> Some type_definition
      | Some typ -> (
        let expanded_types, expansion_type =
          Hover.expandTypes ~file:env.file ~package ~supportsMarkdownLinks:false
            typ
        in
        match expansion_type with
        | `Default ->
          Some (String.concat "\n" (type_definition :: expanded_types))
        | `InlineType -> Some (String.concat "\n" expanded_types))))

let try_resolvers resolvers =
  resolvers
  |> List.find_map (fun (kind, resolver) ->
         resolver () |> Option.map (fun output -> (kind, output)))

let resolve_hover ~package ~kind ~comments_mode path =
  match kind with
  | Auto ->
    try_resolvers
      [
        (Module, fun () -> resolve_module_hover ~package ~comments_mode path);
        (Value, fun () -> resolve_value_hover ~package ~comments_mode path);
        (Type, fun () -> resolve_type_hover ~package path);
      ]
  | Module ->
    resolve_module_hover ~package ~comments_mode path
    |> Option.map (fun output -> (Module, output))
  | Value ->
    resolve_value_hover ~package ~comments_mode path
    |> Option.map (fun output -> (Value, output))
  | Type ->
    resolve_type_hover ~package path
    |> Option.map (fun output -> (Type, output))

let run ?context_path ?(kind = Auto) ?(comments_mode = Include) path =
  let context_path =
    match context_path with
    | Some path -> normalize_path path
    | None -> Sys.getcwd ()
  in
  if not (Files.exists context_path) then
    Error ("error: no such file or directory: " ^ context_path)
  else
    let path = Lint_support.SymbolPath.split path in
    match path with
    | [] -> Error "error: expected a symbol path like String.localeCompare"
    | _ -> (
      match package_for_context context_path with
      | None ->
        Error
          ("error: failed to load ReScript project context from " ^ context_path)
      | Some package -> (
        match resolve_hover ~package ~kind ~comments_mode path with
        | Some (_resolved_kind, output) -> Ok {output = normalize_output output}
        | None ->
          Error
            (Printf.sprintf "error: could not resolve %s as %s"
               (String.concat "." path) (show_kind_to_string kind))))
