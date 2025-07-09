open Analysis

module StringSet = Set.Make (String)

type fieldDoc = {
  fieldName: string;
  docstrings: string list;
  signature: string;
  optional: bool;
  deprecated: string option;
}

type constructorPayload = InlineRecord of {fieldDocs: fieldDoc list}

type constructorDoc = {
  constructorName: string;
  docstrings: string list;
  signature: string;
  deprecated: string option;
  items: constructorPayload option;
}

type typeDoc = {path: string; genericParameters: typeDoc list}
type valueSignature = {parameters: typeDoc list; returnType: typeDoc}

type source = {filepath: string; line: int; col: int}

type docItemDetail =
  | Record of {fieldDocs: fieldDoc list}
  | Variant of {constructorDocs: constructorDoc list}
  | Signature of valueSignature

type docItem =
  | Value of {
      id: string;
      docstring: string list;
      signature: string;
      name: string;
      deprecated: string option;
      detail: docItemDetail option;
      source: source;
    }
  | Type of {
      id: string;
      docstring: string list;
      signature: string;
      name: string;
      deprecated: string option;
      detail: docItemDetail option;
      source: source;
          (** Additional documentation for constructors and record fields, if available. *)
    }
  | Module of docsForModule
  | ModuleType of {
      id: string;
      docstring: string list;
      deprecated: string option;
      name: string;
      source: source;
      items: docItem list;
    }
  | ModuleAlias of {
      id: string;
      docstring: string list;
      name: string;
      source: source;
      items: docItem list;
    }
and docsForModule = {
  id: string;
  docstring: string list;
  deprecated: string option;
  name: string;
  moduletypeid: string option;
  source: source;
  items: docItem list;
}

let stringifyDocstrings docstrings =
  let open Protocol in
  docstrings
  |> List.map (fun docstring -> docstring |> String.trim |> wrapInQuotes)
  |> array

let stringifyFieldDoc ~indentation (fieldDoc : fieldDoc) =
  let open Protocol in
  stringifyObject ~indentation:(indentation + 1)
    [
      ("name", Some (wrapInQuotes fieldDoc.fieldName));
      ( "deprecated",
        match fieldDoc.deprecated with
        | Some d -> Some (wrapInQuotes d)
        | None -> None );
      ("optional", Some (string_of_bool fieldDoc.optional));
      ("docstrings", Some (stringifyDocstrings fieldDoc.docstrings));
      ("signature", Some (wrapInQuotes fieldDoc.signature));
    ]

let stringifyConstructorPayload ~indentation
    (constructorPayload : constructorPayload) =
  let open Protocol in
  match constructorPayload with
  | InlineRecord {fieldDocs} ->
    stringifyObject ~indentation:(indentation + 1)
      [
        ("kind", Some (wrapInQuotes "inlineRecord"));
        ( "fields",
          Some
            (fieldDocs
            |> List.map (stringifyFieldDoc ~indentation:(indentation + 1))
            |> array) );
      ]

let rec stringifyTypeDoc ~indentation (td : typeDoc) : string =
  let open Protocol in
  let ps =
    match td.genericParameters with
    | [] -> None
    | ts ->
      ts |> List.map (stringifyTypeDoc ~indentation:(indentation + 1))
      |> fun ts -> Some (array ts)
  in

  stringifyObject ~indentation:(indentation + 1)
    [("path", Some (wrapInQuotes td.path)); ("genericTypeParameters", ps)]

let stringifyDetail ?(indentation = 0) (detail : docItemDetail) =
  let open Protocol in
  match detail with
  | Record {fieldDocs} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("kind", Some (wrapInQuotes "record"));
        ( "items",
          Some (fieldDocs |> List.map (stringifyFieldDoc ~indentation) |> array)
        );
      ]
  | Variant {constructorDocs} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("kind", Some (wrapInQuotes "variant"));
        ( "items",
          Some
            (constructorDocs
            |> List.map (fun constructorDoc ->
                   stringifyObject ~startOnNewline:true
                     ~indentation:(indentation + 1)
                     [
                       ( "name",
                         Some (wrapInQuotes constructorDoc.constructorName) );
                       ( "deprecated",
                         match constructorDoc.deprecated with
                         | Some d -> Some (wrapInQuotes d)
                         | None -> None );
                       ( "docstrings",
                         Some (stringifyDocstrings constructorDoc.docstrings) );
                       ( "signature",
                         Some (wrapInQuotes constructorDoc.signature) );
                       ( "payload",
                         match constructorDoc.items with
                         | None -> None
                         | Some constructorPayload ->
                           Some
                             (stringifyConstructorPayload
                                ~indentation:(indentation + 1)
                                constructorPayload) );
                     ])
            |> array) );
      ]
  | Signature {parameters; returnType} ->
    let ps =
      match parameters with
      | [] -> None
      | ps ->
        ps |> List.map (stringifyTypeDoc ~indentation:(indentation + 1))
        |> fun ps -> Some (array ps)
    in
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("kind", Some (wrapInQuotes "signature"));
        ( "details",
          Some
            (stringifyObject ~startOnNewline:false ~indentation
               [
                 ("parameters", ps);
                 ("returnType", Some (stringifyTypeDoc ~indentation returnType));
               ]) );
      ]

let stringifySource ~indentation source =
  let open Protocol in
  stringifyObject ~startOnNewline:false ~indentation
    [
      ("filepath", Some (source.filepath |> wrapInQuotes));
      ("line", Some (source.line |> string_of_int));
      ("col", Some (source.col |> string_of_int));
    ]

let rec stringifyDocItem ?(indentation = 0) ~originalEnv (item : docItem) =
  let open Protocol in
  match item with
  | Value {id; docstring; signature; name; deprecated; source; detail} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes id));
        ("kind", Some (wrapInQuotes "value"));
        ("name", Some (name |> wrapInQuotes));
        ( "deprecated",
          match deprecated with
          | Some d -> Some (wrapInQuotes d)
          | None -> None );
        ("signature", Some (signature |> String.trim |> wrapInQuotes));
        ("docstrings", Some (stringifyDocstrings docstring));
        ("source", Some (stringifySource ~indentation:(indentation + 1) source));
        ( "detail",
          match detail with
          | None -> None
          | Some detail ->
            Some (stringifyDetail ~indentation:(indentation + 1) detail) );
      ]
  | Type {id; docstring; signature; name; deprecated; detail; source} ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes id));
        ("kind", Some (wrapInQuotes "type"));
        ("name", Some (name |> wrapInQuotes));
        ( "deprecated",
          match deprecated with
          | Some d -> Some (wrapInQuotes d)
          | None -> None );
        ("signature", Some (signature |> wrapInQuotes));
        ("docstrings", Some (stringifyDocstrings docstring));
        ("source", Some (stringifySource ~indentation:(indentation + 1) source));
        ( "detail",
          match detail with
          | None -> None
          | Some detail ->
            Some (stringifyDetail ~indentation:(indentation + 1) detail) );
      ]
  | Module m ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes m.id));
        ("name", Some (wrapInQuotes m.name));
        ("kind", Some (wrapInQuotes "module"));
        ( "deprecated",
          match m.deprecated with
          | Some d -> Some (wrapInQuotes d)
          | None -> None );
        ( "moduletypeid",
          match m.moduletypeid with
          | Some path -> Some (wrapInQuotes path)
          | None -> None );
        ("docstrings", Some (stringifyDocstrings m.docstring));
        ( "source",
          Some (stringifySource ~indentation:(indentation + 1) m.source) );
        ( "items",
          Some
            (m.items
            |> List.map
                 (stringifyDocItem ~originalEnv ~indentation:(indentation + 1))
            |> array) );
      ]
  | ModuleType m ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes m.id));
        ("name", Some (wrapInQuotes m.name));
        ("kind", Some (wrapInQuotes "moduleType"));
        ( "deprecated",
          match m.deprecated with
          | Some d -> Some (wrapInQuotes d)
          | None -> None );
        ("docstrings", Some (stringifyDocstrings m.docstring));
        ( "source",
          Some (stringifySource ~indentation:(indentation + 1) m.source) );
        ( "items",
          Some
            (m.items
            |> List.map
                 (stringifyDocItem ~originalEnv ~indentation:(indentation + 1))
            |> array) );
      ]
  | ModuleAlias m ->
    stringifyObject ~startOnNewline:true ~indentation
      [
        ("id", Some (wrapInQuotes m.id));
        ("kind", Some (wrapInQuotes "moduleAlias"));
        ("name", Some (wrapInQuotes m.name));
        ("docstrings", Some (stringifyDocstrings m.docstring));
        ( "source",
          Some (stringifySource ~indentation:(indentation + 1) m.source) );
        ( "items",
          Some
            (m.items
            |> List.map
                 (stringifyDocItem ~originalEnv ~indentation:(indentation + 1))
            |> array) );
      ]

and stringifyDocsForModule ?(indentation = 0) ~originalEnv (d : docsForModule) =
  let open Protocol in
  stringifyObject ~startOnNewline:true ~indentation
    [
      ("name", Some (wrapInQuotes d.name));
      ( "deprecated",
        match d.deprecated with
        | Some d -> Some (wrapInQuotes d)
        | None -> None );
      ("docstrings", Some (stringifyDocstrings d.docstring));
      ("source", Some (stringifySource ~indentation:(indentation + 1) d.source));
      ( "items",
        Some
          (d.items
          |> List.map
               (stringifyDocItem ~originalEnv ~indentation:(indentation + 1))
          |> array) );
    ]

let fieldToFieldDoc (field : SharedTypes.field) : fieldDoc =
  {
    fieldName = field.fname.txt;
    docstrings = field.docstring;
    optional = field.optional;
    signature = Shared.typeToString field.typ;
    deprecated = field.deprecated;
  }

let typeDetail typ ~env ~full =
  let open SharedTypes in
  match TypeUtils.extractTypeFromResolvedType ~env ~full typ with
  | Some (Trecord {fields}) ->
    Some (Record {fieldDocs = fields |> List.map fieldToFieldDoc})
  | Some (Tvariant {constructors}) ->
    Some
      (Variant
         {
           constructorDocs =
             constructors
             |> List.map (fun (c : Constructor.t) ->
                    {
                      constructorName = c.cname.txt;
                      docstrings = c.docstring;
                      signature = CompletionBackEnd.showConstructor c;
                      deprecated = c.deprecated;
                      items =
                        (match c.args with
                        | InlineRecord fields ->
                          Some
                            (InlineRecord
                               {fieldDocs = fields |> List.map fieldToFieldDoc})
                        | _ -> None);
                    });
         })
  | _ -> None

(* split a list into two parts all the items except the last one and the last item *)
let splitLast l =
  let rec splitLast' acc = function
    | [] -> failwith "splitLast: empty list"
    | [x] -> (List.rev acc, x)
    | x :: xs -> splitLast' (x :: acc) xs
  in
  splitLast' [] l

let path_to_string path =
  let buf = Buffer.create 64 in
  let rec aux = function
    | Path.Pident id -> Buffer.add_string buf (Ident.name id)
    | Path.Pdot (p, s, _) ->
      aux p;
      Buffer.add_char buf '.';
      Buffer.add_string buf s
    | Path.Papply (p1, p2) ->
      aux p1;
      Buffer.add_char buf '(';
      aux p2;
      Buffer.add_char buf ')'
  in
  aux path;
  Buffer.contents buf

let valueDetail (typ : Types.type_expr) =
  let rec collectSignatureTypes (typ : Types.type_expr) =
    match typ.desc with
    | Tlink t | Tsubst t | Tpoly (t, []) -> collectSignatureTypes t
    | Tconstr (path, ts, _) -> (
      let p = path_to_string path in
      match ts with
      | [] -> [{path = p; genericParameters = []}]
      | ts ->
        let ts =
          ts
          |> List.concat_map (fun (t : Types.type_expr) ->
                 collectSignatureTypes t)
        in
        [{path = p; genericParameters = ts}])
    | Tarrow (_, t1, t2, _, _) ->
      collectSignatureTypes t1 @ collectSignatureTypes t2
    | Tvar None -> [{path = "_"; genericParameters = []}]
    | _ -> []
  in
  match collectSignatureTypes typ with
  | [] -> None
  | ts ->
    let parameters, returnType = splitLast ts in
    Some (Signature {parameters; returnType})

let makeId modulePath ~identifier =
  identifier :: modulePath |> List.rev |> SharedTypes.ident

let getSource ~rootPath ({loc_start} : Location.t) =
  let line, col = Pos.ofLexing loc_start in
  let filepath =
    Files.relpath rootPath loc_start.pos_fname
    |> Files.split Filename.dir_sep
    |> String.concat "/"
  in
  {filepath; line = line + 1; col = col + 1}

let extractDocs ~entryPointFile ~debug =
  let path =
    match Filename.is_relative entryPointFile with
    | true -> Unix.realpath entryPointFile
    | false -> entryPointFile
  in
  if debug then Printf.printf "extracting docs for %s\n" path;
  let result =
    match
      FindFiles.isImplementation path = false
      && FindFiles.isInterface path = false
    with
    | false -> (
      let path =
        if FindFiles.isImplementation path then
          let pathAsResi =
            (path |> Filename.dirname) ^ "/"
            ^ (path |> Filename.basename |> Filename.chop_extension)
            ^ ".resi"
          in
          if Sys.file_exists pathAsResi then (
            if debug then
              Printf.printf "preferring found resi file for impl: %s\n"
                pathAsResi;
            pathAsResi)
          else path
        else path
      in
      match Cmt.loadFullCmtFromPath ~path with
      | None ->
        Error
          (Printf.sprintf
             "error: failed to generate doc for %s, try to build the project"
             path)
      | Some full ->
        let file = full.file in
        let structure = file.structure in
        let rootPath = full.package.rootPath in
        let open SharedTypes in
        let env = QueryEnv.fromFile file in
        let rec extractDocsForModule ?(modulePath = [env.file.moduleName])
            (structure : Module.structure) =
          let valuesSeen = ref StringSet.empty in
          {
            id = modulePath |> List.rev |> ident;
            docstring = structure.docstring |> List.map String.trim;
            name = structure.name;
            moduletypeid = None;
            deprecated = structure.deprecated;
            source =
              {
                filepath =
                  (match rootPath = "." with
                  | true -> file.uri |> Uri.toPath
                  | false ->
                    Files.relpath rootPath (file.uri |> Uri.toPath)
                    |> Files.split Filename.dir_sep
                    |> String.concat "/");
                line = 1;
                col = 1;
              };
            items =
              structure.items
              |> List.filter_map (fun (item : Module.item) ->
                     let item =
                       {
                         item with
                         name = Ext_ident.unwrap_uppercase_exotic item.name;
                       }
                     in
                     let source = getSource ~rootPath item.loc in
                     match item.kind with
                     | Value typ ->
                       Some
                         (Value
                            {
                              id = modulePath |> makeId ~identifier:item.name;
                              docstring = item.docstring |> List.map String.trim;
                              signature =
                                "let " ^ item.name ^ ": "
                                ^ Shared.typeToString typ;
                              name = item.name;
                              deprecated = item.deprecated;
                              detail = valueDetail typ;
                              source;
                            })
                     | Type (typ, _) ->
                       Some
                         (Type
                            {
                              id = modulePath |> makeId ~identifier:item.name;
                              docstring = item.docstring |> List.map String.trim;
                              signature =
                                typ.decl |> Shared.declToString item.name;
                              name = item.name;
                              deprecated = item.deprecated;
                              detail = typeDetail typ ~full ~env;
                              source;
                            })
                     | Module {type_ = Ident p; isModuleType = false} ->
                       (* module Whatever = OtherModule *)
                       let aliasToModule = p |> pathIdentToString in
                       let id =
                         (modulePath |> List.rev |> List.hd) ^ "." ^ item.name
                       in
                       let items, internalDocstrings =
                         match
                           ProcessCmt.fileForModule ~package:full.package
                             aliasToModule
                         with
                         | None -> ([], [])
                         | Some file ->
                           let docs =
                             extractDocsForModule ~modulePath:[id]
                               file.structure
                           in
                           (docs.items, docs.docstring)
                       in
                       Some
                         (ModuleAlias
                            {
                              id;
                              name = item.name;
                              source;
                              items;
                              docstring =
                                item.docstring @ internalDocstrings
                                |> List.map String.trim;
                            })
                     | Module {type_ = Structure m; isModuleType = false} ->
                       (* module Whatever = {} in res or module Whatever: {} in resi. *)
                       let modulePath = m.name :: modulePath in
                       let docs = extractDocsForModule ~modulePath m in
                       Some
                         (Module
                            {
                              id = modulePath |> List.rev |> ident;
                              name = m.name;
                              moduletypeid = None;
                              docstring = item.docstring @ m.docstring;
                              deprecated = item.deprecated;
                              source;
                              items = docs.items;
                            })
                     | Module {type_ = Structure m; isModuleType = true} ->
                       (* module type Whatever = {} *)
                       let modulePath = m.name :: modulePath in
                       let docs = extractDocsForModule ~modulePath m in
                       Some
                         (ModuleType
                            {
                              id = modulePath |> List.rev |> ident;
                              name = m.name;
                              docstring = item.docstring @ m.docstring;
                              deprecated = item.deprecated;
                              source;
                              items = docs.items;
                            })
                     | Module
                         {
                           type_ =
                             Constraint (Structure _impl, Structure interface);
                         } ->
                       (* module Whatever: { <interface> } = { <impl> }. Prefer the interface. *)
                       Some
                         (Module
                            (extractDocsForModule
                               ~modulePath:(interface.name :: modulePath)
                               interface))
                     | Module {type_ = Constraint (Structure m, Ident p)} ->
                       (* module M: T = { <impl> }. Print M *)
                       let docs =
                         extractDocsForModule ~modulePath:(m.name :: modulePath)
                           m
                       in
                       let identModulePath = p |> Path.head |> Ident.name in

                       let moduleTypeIdPath =
                         match
                           ProcessCmt.fileForModule ~package:full.package
                             identModulePath
                           |> Option.is_none
                         with
                         | false -> []
                         | true -> [modulePath |> List.rev |> List.hd]
                       in

                       Some
                         (Module
                            {
                              docs with
                              moduletypeid =
                                Some
                                  (makeId ~identifier:(Path.name p)
                                     moduleTypeIdPath);
                            })
                     | _ -> None)
              (* Filter out shadowed bindings by keeping only the last value associated with an id *)
              |> List.rev
              |> List.filter_map (fun (docItem : docItem) ->
                     match docItem with
                     | Value {id} ->
                       if StringSet.mem id !valuesSeen then None
                       else (
                         valuesSeen := StringSet.add id !valuesSeen;
                         Some docItem)
                     | _ -> Some docItem)
              |> List.rev;
          }
        in
        let docs = extractDocsForModule structure in
        Ok (stringifyDocsForModule ~originalEnv:env docs))
    | true ->
      Error
        (Printf.sprintf
           "error: failed to read %s, expected an .res or .resi file" path)
  in

  result

let extractEmbedded ~extensionPoints ~filename =
  let {Res_driver.parsetree = structure} =
    Res_driver.parsing_engine.parse_implementation ~for_printer:false ~filename
  in
  let content = ref [] in
  let append item = content := item :: !content in
  let extension (iterator : Ast_iterator.iterator) (ext : Parsetree.extension) =
    (match ext with
    | ( {txt},
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( {
                      pexp_loc;
                      pexp_desc = Pexp_constant (Pconst_string (contents, _));
                    },
                    _ );
            };
          ] )
      when extensionPoints |> List.exists (fun v -> v = txt) ->
      append (pexp_loc, txt, contents)
    | _ -> ());
    Ast_iterator.default_iterator.extension iterator ext
  in
  let iterator = {Ast_iterator.default_iterator with extension} in
  iterator.structure iterator structure;
  let open Analysis.Protocol in
  !content
  |> List.map (fun (loc, extensionName, contents) ->
         stringifyObject
           [
             ("extensionName", Some (wrapInQuotes extensionName));
             ("contents", Some (wrapInQuotes contents));
             ("loc", Some (Analysis.Utils.cmtLocToRange loc |> stringifyRange));
           ])
  |> List.rev |> array

module FormatDocstrings = struct
  let mapRescriptCodeBlocks ~colIndent ~(mapper : string -> int -> string)
      (doc : string) =
    let indent = String.make colIndent ' ' in
    let len = String.length doc in
    let buf = Buffer.create len in
    let addIndent () = Buffer.add_string buf indent in
    let currentCodeBlockContents = ref None in
    let lines = String.split_on_char '\n' doc in
    let lineCount = ref (-1) in
    let rec processLines lines =
      let currentLine = !lineCount in
      lineCount := currentLine + 1;
      match (lines, !currentCodeBlockContents) with
      | l :: rest, None ->
        if String.trim l = "```rescript" then (
          currentCodeBlockContents := Some [];
          processLines rest)
        else (
          Buffer.add_string buf l;
          Buffer.add_char buf '\n';
          processLines rest)
      | l :: rest, Some codeBlockContents ->
        if String.trim l = "```" then (
          let codeBlockContents =
            codeBlockContents |> List.rev |> String.concat "\n"
          in
          let mappedCodeBlockContents =
            mapper codeBlockContents currentLine
            |> String.split_on_char '\n'
            |> List.map (fun line -> indent ^ line)
            |> String.concat "\n"
          in
          addIndent ();
          Buffer.add_string buf "```rescript\n";
          Buffer.add_string buf mappedCodeBlockContents;
          Buffer.add_char buf '\n';
          addIndent ();
          Buffer.add_string buf "```";
          Buffer.add_char buf '\n';
          currentCodeBlockContents := None;
          processLines rest)
        else (
          currentCodeBlockContents := Some (l :: codeBlockContents);
          processLines rest)
      | [], Some codeBlockContents ->
        (* EOF, broken, do not format*)
        let codeBlockContents =
          codeBlockContents |> List.rev |> String.concat "\n"
        in
        addIndent ();
        Buffer.add_string buf "```rescript\n";
        Buffer.add_string buf codeBlockContents
      | [], None -> ()
    in
    processLines lines;

    (* Normalize newlines at start/end of the content. *)
    let initialWhitespace =
      let rec findFirstNonWhitespace i =
        if i >= String.length doc then ""
        else if not (String.contains " \t\n\r" doc.[i]) then String.sub doc 0 i
        else findFirstNonWhitespace (i + 1)
      in
      findFirstNonWhitespace 0
    in

    initialWhitespace ^ (buf |> Buffer.contents |> String.trim) ^ indent ^ "\n"

  let formatRescriptCodeBlocks content ~displayFilename ~addError
      ~(payloadLoc : Location.t) =
    let newContent =
      mapRescriptCodeBlocks
        ~colIndent:(payloadLoc.loc_start.pos_cnum - payloadLoc.loc_start.pos_bol)
        ~mapper:(fun code currentLine ->
          let codeLines = String.split_on_char '\n' code in
          let n = List.length codeLines in
          let newlinesNeeded =
            max 0 (payloadLoc.loc_start.pos_lnum + currentLine - n)
          in
          let codeWithOffset = String.make newlinesNeeded '\n' ^ code in
          let formatted_code =
            let {Res_driver.parsetree; comments; invalid; diagnostics} =
              Res_driver.parse_implementation_from_source ~for_printer:true
                ~display_filename:displayFilename ~source:codeWithOffset
            in
            if invalid then (
              let buf = Buffer.create 32 in
              let formatter = Format.formatter_of_buffer buf in
              Res_diagnostics.print_report ~formatter
                ~custom_intro:(Some "Syntax error in code block in docstring")
                diagnostics codeWithOffset;
              addError (Buffer.contents buf);
              code)
            else
              Res_printer.print_implementation
                ~width:Res_multi_printer.default_print_width parsetree ~comments
              |> String.trim
          in
          formatted_code)
        content
    in
    newContent

  let formatDocstrings ~outputMode ~entryPointFile =
    let path =
      match Filename.is_relative entryPointFile with
      | true -> Unix.realpath entryPointFile
      | false -> entryPointFile
    in
    let errors = ref [] in
    let addError error = errors := error :: !errors in

    let makeMapper ~displayFilename =
      {
        Ast_mapper.default_mapper with
        attribute =
          (fun mapper ((name, payload) as attr) ->
            match (name, Ast_payload.is_single_string payload, payload) with
            | ( {txt = "res.doc"},
                Some (contents, None),
                PStr [{pstr_desc = Pstr_eval ({pexp_loc}, _)}] ) ->
              let formatted_contents =
                formatRescriptCodeBlocks ~addError ~displayFilename
                  ~payloadLoc:pexp_loc contents
              in
              if formatted_contents <> contents then
                ( name,
                  PStr
                    [
                      Ast_helper.Str.eval
                        (Ast_helper.Exp.constant
                           (Pconst_string (formatted_contents, None)));
                    ] )
              else attr
            | _ -> Ast_mapper.default_mapper.attribute mapper attr);
      }
    in
    let formatted_content, source =
      if Filename.check_suffix path ".res" then
        let parser =
          Res_driver.parsing_engine.parse_implementation ~for_printer:true
        in
        let {Res_driver.parsetree = structure; comments; source; filename} =
          parser ~filename:path
        in

        let mapper = makeMapper ~displayFilename:filename in
        let astMapped = mapper.structure mapper structure in
        ( Res_printer.print_implementation
            ~width:Res_multi_printer.default_print_width astMapped ~comments,
          source )
      else
        let parser =
          Res_driver.parsing_engine.parse_interface ~for_printer:true
        in
        let {Res_driver.parsetree = signature; comments; source; filename} =
          parser ~filename:path
        in
        let mapper = makeMapper ~displayFilename:filename in
        let astMapped = mapper.signature mapper signature in
        ( Res_printer.print_interface
            ~width:Res_multi_printer.default_print_width astMapped ~comments,
          source )
    in
    let errors = !errors in
    if not (List.is_empty errors) then (
      errors |> String.concat "\n" |> print_endline;
      Error (Printf.sprintf "Error formatting docstrings."))
    else if formatted_content <> source then (
      match outputMode with
      | `Stdout -> Ok formatted_content
      | `File ->
        let oc = open_out path in
        Printf.fprintf oc "%s" formatted_content;
        close_out oc;
        Ok "Formatted docstrings successfully")
    else Ok "No formatting needed"
end
