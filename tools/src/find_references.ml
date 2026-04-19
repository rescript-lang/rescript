open Analysis
open SharedTypes

type symbol_kind = Lint_support.SymbolKind.t =
  | Auto
  | Module
  | Value
  | Type

type query =
  | Symbol of {
      symbol_path: string;
      kind: symbol_kind;
      context_path: string option;
    }
  | Location of {file_path: string; line: int; col: int}

type raw_reference = {abs_path: string; loc: Location.t option}

type query_info =
  | SymbolQuery of {symbol_path: string; resolved_kind: symbol_kind}
  | LocationQuery of {abs_path: string; line: int; col: int}

type run_result = {output: string; count: int}

let symbol_kind_of_string = Lint_support.SymbolKind.of_string

let normalize_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let package_for_path path =
  let uri = Uri.fromPath path in
  Packages.getPackage ~uri

let display_base_for_path path =
  match package_for_path path with
  | Some package -> package.rootPath
  | None ->
    if Lint_support.Path.is_directory path then path else Filename.dirname path

let raw_reference_of_analysis_reference ({References.uri; locOpt} :
                                         References.references) =
  {abs_path = Uri.toPath uri; loc = locOpt}

let compare_loc_opt left right =
  match (left, right) with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some left, Some right ->
    compare (Lint_support.Range.of_loc left) (Lint_support.Range.of_loc right)

let compare_raw_reference left right =
  compare (left.abs_path, left.loc |> Option.map Lint_support.Range.of_loc)
    (right.abs_path, right.loc |> Option.map Lint_support.Range.of_loc)

let sort_and_dedupe references =
  let same_reference left right =
    left.abs_path = right.abs_path && compare_loc_opt left.loc right.loc = 0
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | reference :: rest -> (
      match acc with
      | seen :: _ when same_reference reference seen -> loop acc rest
      | _ -> loop (reference :: acc) rest)
  in
  references |> List.sort compare_raw_reference |> loop []

let references_from_loc_item full loc_item =
  References.allReferencesForLocItem ~full loc_item
  |> List.map raw_reference_of_analysis_reference
  |> sort_and_dedupe

let references_for_top_level_module (file : File.t) =
  match Cmt.fullFromUri ~uri:file.uri with
  | None ->
    Error
      (Printf.sprintf "error: failed to load typed info for %s"
         (Uri.toPath file.uri))
  | Some full ->
    Ok
      (references_from_loc_item full
         {loc = Location.none; locType = TopLevelModule file.moduleName})

let references_for_exported ~(env : QueryEnv.t) ~tip ~stamp =
  match Cmt.fullFromUri ~uri:env.file.uri with
  | None ->
    Error
      (Printf.sprintf "error: failed to load typed info for %s"
         (Uri.toPath env.file.uri))
  | Some full ->
    Ok
      (References.forLocalStamp ~full stamp tip
      |> List.map raw_reference_of_analysis_reference
      |> sort_and_dedupe)

let resolve_module_references ~package path =
  match Lint_support.SymbolPath.resolve_top_level_module ~package path with
  | Some file ->
    Some
      (references_for_top_level_module file
      |> Result.map (fun refs -> (Module, refs)))
  | None -> (
    match
      Lint_support.SymbolPath.resolve_exported ~package ~tip:Tip.Module path
    with
    | None -> None
    | Some (env, stamp) ->
      Some
        (references_for_exported ~env ~tip:Tip.Module ~stamp
        |> Result.map (fun refs -> (Module, refs))))

let resolve_value_references ~package path =
  match
    Lint_support.SymbolPath.resolve_exported ~package ~tip:Tip.Value path
  with
  | None -> None
  | Some (env, stamp) ->
    Some
      (references_for_exported ~env ~tip:Tip.Value ~stamp
      |> Result.map (fun refs -> (Value, refs)))

let resolve_type_references ~package path =
  match Lint_support.SymbolPath.resolve_exported ~package ~tip:Tip.Type path with
  | None -> None
  | Some (env, stamp) ->
    Some
      (references_for_exported ~env ~tip:Tip.Type ~stamp
      |> Result.map (fun refs -> (Type, refs)))

let rec try_symbol_resolvers = function
  | [] -> Ok None
  | resolver :: rest -> (
    match resolver () with
    | None -> try_symbol_resolvers rest
    | Some (Ok resolved) -> Ok (Some resolved)
    | Some (Error _ as error) -> error |> Result.map Option.some)

let resolve_symbol_references ~package ~kind path =
  match kind with
  | Auto ->
    try_symbol_resolvers
      [
        (fun () -> resolve_module_references ~package path);
        (fun () -> resolve_value_references ~package path);
        (fun () -> resolve_type_references ~package path);
      ]
  | Module -> try_symbol_resolvers [fun () -> resolve_module_references ~package path]
  | Value -> try_symbol_resolvers [fun () -> resolve_value_references ~package path]
  | Type -> try_symbol_resolvers [fun () -> resolve_type_references ~package path]

let resolve_location_references ~file_path ~line ~col =
  match Cmt.loadFullCmtFromPath ~path:file_path with
  | None ->
    Error
      (Printf.sprintf "error: failed to load typed info for %s" file_path)
  | Some full -> (
    match References.getLocItem ~full ~pos:(line - 1, col - 1) ~debug:false with
    | None ->
      Error
        (Printf.sprintf "error: could not resolve a symbol at %s:%d:%d" file_path
           line col)
    | Some loc_item -> Ok (references_from_loc_item full loc_item))

let snippet_of_reference ~source_cache (reference : raw_reference) =
  match reference.loc with
  | None -> None
  | Some loc ->
    Lint_support.Snippet.of_loc ~source_cache ~path:reference.abs_path ~loc ()

let stringify_text_reference ~display_base ~source_cache
    (reference : raw_reference) =
  let path = Lint_support.Path.display ~base:display_base reference.abs_path in
  let lines = [Printf.sprintf "path: %s" path] in
  let lines =
    match reference.loc with
    | None -> lines
    | Some loc ->
      let range = Lint_support.Range.of_loc loc |> Lint_support.Range.to_text in
      lines @ [Printf.sprintf "range: %s" range]
  in
  match snippet_of_reference ~source_cache reference with
  | None -> String.concat "\n" lines
  | Some snippet -> String.concat "\n" (lines @ ["snippet:"; snippet])

let render_text ~display_base ~query_info references =
  let header =
    match query_info with
    | SymbolQuery {symbol_path; resolved_kind} ->
      [
        "mode: symbol";
        Printf.sprintf "symbol: %s" symbol_path;
        Printf.sprintf "kind: %s"
          (Lint_support.SymbolKind.to_string resolved_kind);
        Printf.sprintf "count: %d" (List.length references);
      ]
    | LocationQuery {abs_path; line; col} ->
      [
        "mode: location";
        Printf.sprintf "path: %s"
          (Lint_support.Path.display ~base:display_base abs_path);
        Printf.sprintf "position: %d:%d" line col;
        Printf.sprintf "count: %d" (List.length references);
      ]
  in
  match references with
  | [] -> String.concat "\n" header
  | _ ->
    let source_cache = Lint_support.Snippet.create_cache () in
    let body =
      references
      |> List.map (stringify_text_reference ~display_base ~source_cache)
      |> String.concat "\n\n"
    in
    String.concat "\n" header ^ "\n\n" ^ body

let run_symbol ~symbol_path ~kind ?context_path () =
  let context_path =
    match context_path with
    | Some path -> normalize_path path
    | None -> Sys.getcwd ()
  in
  if not (Files.exists context_path) then
    Error ("error: no such file or directory: " ^ context_path)
  else
    match Lint_support.SymbolPath.split symbol_path with
    | [] -> Error "error: expected a symbol path like String.localeCompare"
    | path -> (
      match package_for_path context_path with
      | None ->
        Error
          ("error: failed to load ReScript project context from " ^ context_path)
      | Some package -> (
        match resolve_symbol_references ~package ~kind path with
        | Error _ as error -> error
        | Ok None ->
          Error
            (Printf.sprintf "error: could not resolve %s as %s" symbol_path
               (Lint_support.SymbolKind.to_string kind))
        | Ok (Some (resolved_kind, references)) ->
          let display_base = display_base_for_path context_path in
          Ok
            {
              output =
                render_text ~display_base
                  ~query_info:(SymbolQuery {symbol_path; resolved_kind})
                  references;
              count = List.length references;
            }))

let run_location ~file_path ~line ~col =
  let file_path = normalize_path file_path in
  if not (Files.exists file_path) then
    Error ("error: no such file or directory: " ^ file_path)
  else if line <= 0 || col <= 0 then
    Error "error: line and col must be 1-based positive integers"
  else
    resolve_location_references ~file_path ~line ~col
    |> Result.map (fun references ->
           let display_base = display_base_for_path file_path in
           {
             output =
               render_text ~display_base
                 ~query_info:(LocationQuery {abs_path = file_path; line; col})
                 references;
             count = List.length references;
           })

let run (query : query) =
  match query with
  | Symbol {symbol_path; kind; context_path} ->
    run_symbol ?context_path ~symbol_path ~kind ()
  | Location {file_path; line; col} -> run_location ~file_path ~line ~col
