let getString key obj =
  match Json.get key obj with
  | Some (Json.String s) -> s
  | _ -> ""

let parsePathsForModule json =
  let pathsForModule = Hashtbl.create 30 in
  (match json with
  | Json.Object items ->
    List.iter
      (fun (moduleName, value) ->
        let paths =
          match Json.get "impl" value with
          | Some impl ->
            Some
              (SharedTypes.Impl
                 {cmt = getString "cmt" impl; res = getString "res" impl})
          | None -> (
            match Json.get "intfAndImpl" value with
            | Some ii ->
              Some
                (SharedTypes.IntfAndImpl
                   {
                     cmti = getString "cmti" ii;
                     resi = getString "resi" ii;
                     cmt = getString "cmt" ii;
                     res = getString "res" ii;
                   })
            | None -> (
              match Json.get "namespace" value with
              | Some ns ->
                Some (SharedTypes.Namespace {cmt = getString "cmt" ns})
              | None -> None))
        in
        match paths with
        | Some p -> Hashtbl.replace pathsForModule moduleName p
        | None -> ())
      items
  | _ -> ());
  pathsForModule

let parseFileSet json =
  match json with
  | Some (Json.Array items) ->
    items
    |> List.filter_map (fun item ->
           match item with
           | Json.String s -> Some s
           | _ -> None)
    |> SharedTypes.FileSet.of_list
  | _ -> SharedTypes.FileSet.empty

let parseOpens json =
  match json with
  | Some (Json.Array items) ->
    items
    |> List.filter_map (fun item ->
           match item with
           | Json.Array strings ->
             Some
               (List.filter_map
                  (fun s ->
                    match s with
                    | Json.String s -> Some s
                    | _ -> None)
                  strings)
           | _ -> None)
  | _ -> []

type rewatch_context = {
  source: string;
  path: string;
  pos: int * int;
  package: SharedTypes.package;
}

let parseRewatchContext json =
  let source = getString "source" json in
  let path = getString "path" json in
  let pos =
    match Json.get "pos" json with
    | Some (Json.Array [Json.Number line; Json.Number col]) ->
      (int_of_float line, int_of_float col)
    | _ -> (0, 0)
  in
  let rootPath = getString "rootPath" json in
  let namespace =
    match Json.get "namespace" json with
    | Some (Json.String s) -> Some s
    | _ -> None
  in
  let suffix =
    match Json.get "suffix" json with
    | Some (Json.String s) -> s
    | _ -> ".js"
  in
  let rescriptVersion =
    match Json.get "rescriptVersion" json with
    | Some (Json.Array [Json.Number major; Json.Number minor]) ->
      (int_of_float major, int_of_float minor)
    | _ -> (13, 0)
  in
  let genericJsxModule =
    match Json.get "genericJsxModule" json with
    | Some (Json.String s) -> Some s
    | _ -> None
  in
  let opens = parseOpens (Json.get "opens" json) in
  let pathsForModule =
    match Json.get "pathsForModule" json with
    | Some obj -> parsePathsForModule obj
    | None -> Hashtbl.create 0
  in
  let projectFiles = parseFileSet (Json.get "projectFiles" json) in
  let dependenciesFiles = parseFileSet (Json.get "dependenciesFiles" json) in
  let package : SharedTypes.package =
    {
      genericJsxModule;
      suffix;
      rootPath;
      projectFiles;
      dependenciesFiles;
      pathsForModule;
      namespace;
      opens;
      rescriptVersion;
      autocomplete = Misc.StringMap.empty;
    }
  in
  {source; path; pos; package}

let completion () =
  let input = In_channel.input_all In_channel.stdin in
  match Json.parse input with
  | None ->
    prerr_endline "completion-rewatch: failed to parse JSON from stdin";
    print_endline "[]"
  | Some json ->
    let {source; path; pos; package} = parseRewatchContext json in
    let completions =
      match
        Completions.getCompletionsFromSource ~debug:false ~path ~pos ~source
          ~package ()
      with
      | None -> []
      | Some (completions, full, _) ->
        completions
        |> List.map (CompletionBackEnd.completionToItem ~full)
        |> List.map Protocol.stringifyCompletionItem
    in
    completions |> Protocol.array |> print_endline

let hover () =
  let input = In_channel.input_all In_channel.stdin in
  match Json.parse input with
  | None ->
    prerr_endline "hover-rewatch: failed to parse JSON from stdin";
    print_endline Protocol.null
  | Some json ->
    let {source; path; pos; package} = parseRewatchContext json in
    let result =
      match Cmt.loadFullCmtWithPackage ~path ~package with
      | None -> Protocol.null
      | Some full -> (
        match References.getLocItem ~full ~pos ~debug:false with
        | Some locItem -> (
          match Hover.newHover ~supportsMarkdownLinks:true ~full locItem with
          | Some s -> Protocol.stringifyHover s
          | None -> Protocol.null)
        | None -> (
          (* Fall back to completion-based hover *)
          match
            Completions.getCompletionsFromSource ~debug:false ~path ~pos ~source
              ~package ~forHover:true ()
          with
          | None -> Protocol.null
          | Some (completions, ({file; package} as full), scope) -> (
            let rawOpens = Scope.getRawOpens scope in
            match completions with
            | {kind = Label typString; docstring} :: _ ->
              let parts =
                docstring
                @ if typString = "" then [] else [Markdown.codeBlock typString]
              in
              Protocol.stringifyHover (String.concat "\n\n" parts)
            | {env} :: _ -> (
              let opens =
                CompletionBackEnd.getOpens ~debug:false ~rawOpens ~package ~env
              in
              match
                CompletionBackEnd.completionsGetTypeEnv2 ~debug:false ~full
                  ~rawOpens ~opens ~pos completions
              with
              | Some (typ, _env) ->
                let typeString =
                  Hover.hoverWithExpandedTypes ~file ~package
                    ~supportsMarkdownLinks:true typ
                in
                Protocol.stringifyHover typeString
              | None -> Protocol.null)
            | _ -> Protocol.null)))
    in
    print_endline result
