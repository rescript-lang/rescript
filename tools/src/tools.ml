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
  `List
    (docstrings
    |> List.map (fun docstring -> `String (docstring |> String.trim)))

let stringifyFieldDoc (fieldDoc : fieldDoc) =
  `Assoc
    ([
       ("name", `String fieldDoc.fieldName);
       ("optional", `Bool fieldDoc.optional);
       ("docstrings", stringifyDocstrings fieldDoc.docstrings);
       ("signature", `String fieldDoc.signature);
     ]
    @
    match fieldDoc.deprecated with
    | Some d -> [("deprecated", `String d)]
    | None -> [])

let stringifyConstructorPayload (constructorPayload : constructorPayload) =
  match constructorPayload with
  | InlineRecord {fieldDocs} ->
    `Assoc
      [
        ("kind", `String "inlineRecord");
        ("fields", `List (fieldDocs |> List.map stringifyFieldDoc));
      ]

let rec stringifyTypeDoc (td : typeDoc) =
  let ps =
    match td.genericParameters with
    | [] -> `List []
    | ts -> ts |> List.map stringifyTypeDoc |> fun ts -> `List ts
  in
  `Assoc [("path", `String td.path); ("genericTypeParameters", ps)]

let stringifyDetail (detail : docItemDetail) =
  match detail with
  | Record {fieldDocs} ->
    `Assoc
      [
        ("kind", `String "record");
        ("items", `List (fieldDocs |> List.map stringifyFieldDoc));
      ]
  | Variant {constructorDocs} ->
    `Assoc
      [
        ("kind", `String "variant");
        ( "items",
          `List
            (constructorDocs
            |> List.map (fun constructorDoc ->
                   `Assoc
                     ([
                        ("name", `String constructorDoc.constructorName);
                        ( "docstrings",
                          stringifyDocstrings constructorDoc.docstrings );
                        ("signature", `String constructorDoc.signature);
                      ]
                     @ (match constructorDoc.deprecated with
                       | Some d -> [("deprecated", `String d)]
                       | None -> [])
                     @
                     match constructorDoc.items with
                     | Some constructorPayload ->
                       [
                         ( "payload",
                           stringifyConstructorPayload constructorPayload );
                       ]
                     | None -> []))) );
      ]
  | Signature {parameters; returnType} ->
    let ps =
      match parameters with
      | [] -> `List []
      | ps -> ps |> List.map stringifyTypeDoc |> fun ps -> `List ps
    in
    `Assoc
      [
        ("kind", `String "signature");
        ( "details",
          `Assoc
            [("parameters", ps); ("returnType", stringifyTypeDoc returnType)] );
      ]

let stringifySource source =
  `Assoc
    [
      ("filepath", `String source.filepath);
      ("line", `Int source.line);
      ("col", `Int source.col);
    ]

let rec stringifyDocItem ~originalEnv (item : docItem) =
  match item with
  | Value {id; docstring; signature; name; deprecated; source; detail} ->
    `Assoc
      ([
         ("id", `String id);
         ("kind", `String "value");
         ("name", `String name);
         ("signature", `String (signature |> String.trim));
         ("docstrings", stringifyDocstrings docstring);
         ("source", stringifySource source);
       ]
      @ (match deprecated with
        | Some d -> [("deprecated", `String d)]
        | None -> [])
      @
      match detail with
      | Some detail -> [("detail", stringifyDetail detail)]
      | None -> [])
  | Type {id; docstring; signature; name; deprecated; detail; source} ->
    `Assoc
      ([
         ("id", `String id);
         ("kind", `String "type");
         ("name", `String name);
         ("signature", `String signature);
         ("docstrings", stringifyDocstrings docstring);
         ("source", stringifySource source);
       ]
      @ (match deprecated with
        | Some d -> [("deprecated", `String d)]
        | None -> [])
      @
      match detail with
      | Some detail -> [("detail", stringifyDetail detail)]
      | None -> [])
  | Module m ->
    `Assoc
      ([
         ("id", `String m.id);
         ("name", `String m.name);
         ("kind", `String "module");
         ("docstrings", stringifyDocstrings m.docstring);
         ("source", stringifySource m.source);
         ( "items",
           `List
             (m.items
             |> List.map (fun item -> stringifyDocItem ~originalEnv item)) );
       ]
      @ (match m.deprecated with
        | Some d -> [("deprecated", `String d)]
        | None -> [])
      @
      match m.moduletypeid with
      | Some path -> [("moduletypeid", `String path)]
      | None -> [])
  | ModuleType m ->
    `Assoc
      ([
         ("id", `String m.id);
         ("name", `String m.name);
         ("kind", `String "moduleType");
         ("docstrings", stringifyDocstrings m.docstring);
         ("source", stringifySource m.source);
         ( "items",
           `List
             (m.items
             |> List.map (fun item -> stringifyDocItem ~originalEnv item)) );
       ]
      @
      match m.deprecated with
      | Some d -> [("deprecated", `String d)]
      | None -> [])
  | ModuleAlias m ->
    `Assoc
      [
        ("id", `String m.id);
        ("kind", `String "moduleAlias");
        ("name", `String m.name);
        ("docstrings", stringifyDocstrings m.docstring);
        ("source", stringifySource m.source);
        ("items", `List (m.items |> List.map (stringifyDocItem ~originalEnv)));
      ]

and stringifyDocsForModule ~originalEnv (d : docsForModule) =
  `Assoc
    ([
       ("name", `String d.name);
       ("docstrings", stringifyDocstrings d.docstring);
       ("source", stringifySource d.source);
       ( "items",
         `List
           (d.items |> List.map (fun item -> stringifyDocItem ~originalEnv item))
       );
     ]
    @
    match d.deprecated with
    | Some d -> [("deprecated", `String d)]
    | None -> [])

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
    | Tarrow (arg, ret, _, _) ->
      collectSignatureTypes arg.typ @ collectSignatureTypes ret
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
        Ok
          (stringifyDocsForModule ~originalEnv:env docs
          |> Yojson.Safe.pretty_to_string))
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
  let result =
    !content
    |> List.map (fun (loc, extensionName, contents) ->
           `Assoc
             [
               ("extensionName", `String extensionName);
               ("contents", `String contents);
               ( "loc",
                 Analysis.Utils.cmtLocToRange loc |> Lsp.Types.Range.yojson_of_t
               );
             ])
    |> List.rev
  in
  Yojson.Safe.pretty_to_string (`List result)

let readFile path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let isResLang lang =
  match String.lowercase_ascii lang with
  | "res" | "rescript" | "resi" -> true
  | lang ->
    (* Cover ```res example, and similar *)
    String.starts_with ~prefix:"res " lang
    || String.starts_with ~prefix:"rescript " lang
    || String.starts_with ~prefix:"resi " lang

module FormatCodeblocks = struct
  module Transform = struct
    type transform = AssertEqualFnToEquals  (** assertEqual(a, b) -> a == b *)

    (** Transforms for the code blocks themselves. *)
    let transform ~transforms ast =
      match transforms with
      | [] -> ast
      | transforms ->
        let hasTransform transform = transforms |> List.mem transform in
        let mapper =
          {
            Ast_mapper.default_mapper with
            expr =
              (fun mapper exp ->
                match exp.pexp_desc with
                | Pexp_apply
                    {
                      funct =
                        {
                          pexp_desc =
                            Pexp_ident
                              ({txt = Lident "assertEqual"} as identTxt);
                        } as ident;
                      partial = false;
                      args = [(Nolabel, _); (Nolabel, _)] as args;
                    }
                  when hasTransform AssertEqualFnToEquals ->
                  {
                    exp with
                    pexp_desc =
                      Pexp_apply
                        {
                          funct =
                            {
                              ident with
                              pexp_desc =
                                Pexp_ident {identTxt with txt = Lident "=="};
                            };
                          args;
                          partial = false;
                          transformed_jsx = false;
                        };
                  }
                  (* Piped *)
                | Pexp_apply
                    {
                      funct = {pexp_desc = Pexp_ident {txt = Lident "->"}};
                      partial = false;
                      args =
                        [
                          (_, lhs);
                          ( Nolabel,
                            {
                              pexp_desc =
                                Pexp_apply
                                  {
                                    funct =
                                      {
                                        pexp_desc =
                                          Pexp_ident
                                            ({txt = Lident "assertEqual"} as
                                             identTxt);
                                      } as ident;
                                    partial = false;
                                    args = [rhs];
                                  };
                            } );
                        ];
                    }
                  when hasTransform AssertEqualFnToEquals ->
                  {
                    exp with
                    pexp_desc =
                      Pexp_apply
                        {
                          funct =
                            {
                              ident with
                              pexp_desc =
                                Pexp_ident {identTxt with txt = Lident "=="};
                            };
                          args = [(Nolabel, lhs); rhs];
                          partial = false;
                          transformed_jsx = false;
                        };
                  }
                | _ -> Ast_mapper.default_mapper.expr mapper exp);
          }
        in
        mapper.structure mapper ast
  end

  let formatRescriptCodeBlocks content ~transformAssertEqual ~displayFilename
      ~addError ~markdownBlockStartLine =
    (* Detect ReScript code blocks. *)
    let hadCodeBlocks = ref false in
    let block _m = function
      | Cmarkit.Block.Code_block (codeBlock, meta) -> (
        match Cmarkit.Block.Code_block.info_string codeBlock with
        | Some ((lang, _) as info_string) when isResLang lang ->
          hadCodeBlocks := true;

          let currentLine =
            meta |> Cmarkit.Meta.textloc |> Cmarkit.Textloc.first_line |> fst
          in
          (* Account for 0-based line numbers *)
          let currentLine = currentLine + 1 in
          let layout = Cmarkit.Block.Code_block.layout codeBlock in
          let code = Cmarkit.Block.Code_block.code codeBlock in
          let codeText =
            code |> List.map Cmarkit.Block_line.to_string |> String.concat "\n"
          in

          let n = List.length code in
          let newlinesNeeded =
            max 0 (markdownBlockStartLine + currentLine - n)
          in
          let codeWithOffset = String.make newlinesNeeded '\n' ^ codeText in
          let reportParseError diagnostics =
            let buf = Buffer.create 1000 in
            let formatter = Format.formatter_of_buffer buf in
            Res_diagnostics.print_report ~formatter
              ~custom_intro:(Some "Syntax error in code block in docstring")
              diagnostics codeWithOffset;
            addError (Buffer.contents buf)
          in
          let formattedCode =
            if lang |> String.split_on_char ' ' |> List.hd = "resi" then
              let {Res_driver.parsetree; comments; invalid; diagnostics} =
                Res_driver.parse_interface_from_source ~for_printer:true
                  ~display_filename:displayFilename ~source:codeWithOffset
              in
              if invalid then (
                reportParseError diagnostics;
                code)
              else
                Res_printer.print_interface parsetree ~comments
                |> String.trim |> Cmarkit.Block_line.list_of_string
            else
              let {Res_driver.parsetree; comments; invalid; diagnostics} =
                Res_driver.parse_implementation_from_source ~for_printer:true
                  ~display_filename:displayFilename ~source:codeWithOffset
              in
              if invalid then (
                reportParseError diagnostics;
                code)
              else
                let parsetree =
                  if transformAssertEqual then
                    Transform.transform ~transforms:[AssertEqualFnToEquals]
                      parsetree
                  else parsetree
                in
                Res_printer.print_implementation parsetree ~comments
                |> String.trim |> Cmarkit.Block_line.list_of_string
          in

          let mappedCodeBlock =
            Cmarkit.Block.Code_block.make ~layout ~info_string formattedCode
          in
          Cmarkit.Mapper.ret (Cmarkit.Block.Code_block (mappedCodeBlock, meta))
        | _ -> Cmarkit.Mapper.default)
      | _ -> Cmarkit.Mapper.default
    in
    let mapper = Cmarkit.Mapper.make ~block () in
    let newContent =
      content
      |> Cmarkit.Doc.of_string ~locs:true
      |> Cmarkit.Mapper.map_doc mapper
      |> Cmarkit_commonmark.of_doc
    in
    (newContent, !hadCodeBlocks)

  let formatCodeBlocksInFile ~outputMode ~transformAssertEqual ~entryPointFile =
    let path =
      match Filename.is_relative entryPointFile with
      | true -> Unix.realpath entryPointFile
      | false -> entryPointFile
    in
    let errors = ref [] in
    let addError error = errors := error :: !errors in

    let makeMapper ~transformAssertEqual ~displayFilename =
      {
        Ast_mapper.default_mapper with
        attribute =
          (fun mapper ((name, payload) as attr) ->
            match (name, Ast_payload.is_single_string payload, payload) with
            | ( {txt = "res.doc"},
                Some (contents, None),
                PStr [{pstr_desc = Pstr_eval ({pexp_loc}, _)}] ) ->
              let formattedContents, hadCodeBlocks =
                formatRescriptCodeBlocks ~transformAssertEqual ~addError
                  ~displayFilename
                  ~markdownBlockStartLine:pexp_loc.loc_start.pos_lnum contents
              in
              if hadCodeBlocks && formattedContents <> contents then
                ( name,
                  PStr
                    [
                      Ast_helper.Str.eval
                        (Ast_helper.Exp.constant
                           (Pconst_string (formattedContents, None)));
                    ] )
              else attr
            | _ -> Ast_mapper.default_mapper.attribute mapper attr);
      }
    in
    let content =
      if Filename.check_suffix path ".md" then
        let content = readFile path in
        let displayFilename = Filename.basename path in
        let formattedContents, hadCodeBlocks =
          formatRescriptCodeBlocks ~transformAssertEqual ~addError
            ~displayFilename ~markdownBlockStartLine:1 content
        in
        if hadCodeBlocks && formattedContents <> content then
          Ok (formattedContents, content)
        else Ok (content, content)
      else if Filename.check_suffix path ".res" then
        let parser =
          Res_driver.parsing_engine.parse_implementation ~for_printer:true
        in
        let {Res_driver.parsetree = structure; comments; source; filename} =
          parser ~filename:path
        in
        let filename = Filename.basename filename in
        let mapper =
          makeMapper ~transformAssertEqual ~displayFilename:filename
        in
        let astMapped = mapper.structure mapper structure in
        Ok (Res_printer.print_implementation astMapped ~comments, source)
      else if Filename.check_suffix path ".resi" then
        let parser =
          Res_driver.parsing_engine.parse_interface ~for_printer:true
        in
        let {Res_driver.parsetree = signature; comments; source; filename} =
          parser ~filename:path
        in
        let mapper =
          makeMapper ~transformAssertEqual ~displayFilename:filename
        in
        let astMapped = mapper.signature mapper signature in
        Ok (Res_printer.print_interface astMapped ~comments, source)
      else
        Error
          (Printf.sprintf
             "File extension not supported. This command accepts .res, .resi, \
              and .md files")
    in
    match content with
    | Error e -> Error e
    | Ok (formatted_content, source) ->
      let errors = !errors in
      if List.length errors > 0 then (
        errors |> List.rev |> String.concat "\n" |> print_endline;
        Error
          (Printf.sprintf "%s: Error formatting docstrings."
             (Filename.basename path)))
      else if formatted_content <> source then (
        match outputMode with
        | `Stdout -> Ok formatted_content
        | `File ->
          let oc = open_out path in
          Printf.fprintf oc "%s" formatted_content;
          close_out oc;
          Ok (Filename.basename path ^ ": formatted successfully"))
      else Ok (Filename.basename path ^ ": needed no formatting")
end

module ExtractCodeblocks = struct
  module Transform = struct
    type transform =
      | EqualsToAssertEqualFn
          (** a == b -> assertEqual(a, b), for structure items only *)

    let transform ~transforms ast =
      match transforms with
      | [] -> ast
      | transforms ->
        let hasTransform transform = transforms |> List.mem transform in
        let mapper =
          {
            Ast_mapper.default_mapper with
            structure_item =
              (fun mapper str_item ->
                match str_item.pstr_desc with
                | Pstr_eval
                    ( ({
                         pexp_desc =
                           Pexp_apply
                             {
                               funct =
                                 {
                                   pexp_desc =
                                     Pexp_ident
                                       ({txt = Lident "=="} as identTxt);
                                 } as ident;
                               partial = false;
                               args = [(Nolabel, _); (Nolabel, _)] as args;
                             };
                       } as exp),
                      x1 )
                  when hasTransform EqualsToAssertEqualFn ->
                  {
                    str_item with
                    pstr_desc =
                      Pstr_eval
                        ( {
                            exp with
                            pexp_desc =
                              Pexp_apply
                                {
                                  funct =
                                    {
                                      ident with
                                      pexp_desc =
                                        Pexp_ident
                                          {
                                            identTxt with
                                            txt = Lident "assertEqual";
                                          };
                                    };
                                  args;
                                  partial = false;
                                  transformed_jsx = false;
                                };
                          },
                          x1 );
                  }
                | _ -> Ast_mapper.default_mapper.structure_item mapper str_item);
          }
        in
        mapper.structure mapper ast
  end

  type codeBlock = {id: string; code: string; name: string}

  let getDocstring = function
    | d :: _ -> d
    | _ -> ""

  let extractCodeBlocks ~entryPointFile
      ~(processDocstrings : id:string -> name:string -> string -> unit) =
    let path =
      match Filename.is_relative entryPointFile with
      | true -> Unix.realpath entryPointFile
      | false -> entryPointFile
    in
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
            if Sys.file_exists pathAsResi then pathAsResi else path
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
          let open SharedTypes in
          let env = QueryEnv.fromFile file in
          let rec extractCodeBlocksForModule
              ?(modulePath = [env.file.moduleName])
              (structure : Module.structure) =
            let id = modulePath |> List.rev |> ident in
            let name = structure.name in
            processDocstrings ~id ~name (getDocstring structure.docstring);

            structure.items
            |> List.iter (fun (item : Module.item) ->
                   match item.kind with
                   | Value _typ ->
                     let id = modulePath |> makeId ~identifier:item.name in
                     let name = item.name in
                     processDocstrings ~id ~name (getDocstring item.docstring)
                   | Type (_typ, _) ->
                     let id = modulePath |> makeId ~identifier:item.name in
                     let name = item.name in
                     processDocstrings ~id ~name (getDocstring item.docstring)
                   | Module {type_ = Ident _p; isModuleType = false} ->
                     (* module Whatever = OtherModule *)
                     let id =
                       (modulePath |> List.rev |> List.hd) ^ "." ^ item.name
                     in
                     let name = item.name in
                     processDocstrings ~id ~name (getDocstring item.docstring)
                   | Module {type_ = Structure m; isModuleType = false} ->
                     (* module Whatever = {} in res or module Whatever: {} in resi. *)
                     let modulePath = m.name :: modulePath in
                     let id = modulePath |> List.rev |> ident in
                     let name = m.name in
                     processDocstrings ~id ~name (getDocstring m.docstring);
                     extractCodeBlocksForModule ~modulePath m
                   | Module {type_ = Structure m; isModuleType = true} ->
                     (* module type Whatever = {} *)
                     let modulePath = m.name :: modulePath in
                     let id = modulePath |> List.rev |> ident in
                     let name = m.name in
                     processDocstrings ~id ~name (getDocstring m.docstring);
                     extractCodeBlocksForModule ~modulePath m
                   | Module
                       {
                         type_ =
                           Constraint (Structure _impl, Structure interface);
                       } ->
                     (* module Whatever: { <interface> } = { <impl> }. Prefer the interface. *)
                     let modulePath = interface.name :: modulePath in
                     let id = modulePath |> List.rev |> ident in
                     let name = interface.name in
                     processDocstrings ~id ~name
                       (getDocstring interface.docstring);
                     extractCodeBlocksForModule ~modulePath interface
                   | Module {type_ = Constraint (Structure m, Ident _p)} ->
                     (* module M: T = { <impl> }. Print M *)
                     let modulePath = m.name :: modulePath in
                     let id = modulePath |> List.rev |> ident in
                     let name = m.name in
                     processDocstrings ~id ~name (getDocstring m.docstring);
                     extractCodeBlocksForModule ~modulePath m
                   | Module.Module _ -> ())
          in
          extractCodeBlocksForModule structure;
          Ok ())
      | true ->
        Error
          (Printf.sprintf
             "error: failed to read %s, expected an .res or .resi file" path)
    in

    result

  let extractRescriptCodeBlocks content ~transformAssertEqual ~displayFilename
      ~addError ~markdownBlockStartLine =
    (* Detect ReScript code blocks. *)
    let codeBlocks = ref [] in
    let addCodeBlock codeBlock = codeBlocks := codeBlock :: !codeBlocks in
    let block _m = function
      | Cmarkit.Block.Code_block (codeBlock, meta) -> (
        match Cmarkit.Block.Code_block.info_string codeBlock with
        | Some (lang, _) when isResLang lang ->
          let currentLine =
            meta |> Cmarkit.Meta.textloc |> Cmarkit.Textloc.first_line |> fst
          in
          (* Account for 0-based line numbers *)
          let currentLine = currentLine + 1 in
          let code = Cmarkit.Block.Code_block.code codeBlock in
          let codeText =
            code |> List.map Cmarkit.Block_line.to_string |> String.concat "\n"
          in
          let n = List.length code in
          let newlinesNeeded =
            max 0 (markdownBlockStartLine + currentLine - n)
          in
          let codeWithOffset = String.make newlinesNeeded '\n' ^ codeText in
          let reportParseError diagnostics =
            let buf = Buffer.create 1000 in
            let formatter = Format.formatter_of_buffer buf in
            Res_diagnostics.print_report ~formatter
              ~custom_intro:(Some "Syntax error in code block in docstring")
              diagnostics codeWithOffset;
            addError (Buffer.contents buf)
          in
          let mappedCode =
            if lang |> String.split_on_char ' ' |> List.hd = "resi" then
              let {Res_driver.parsetree; comments; invalid; diagnostics} =
                Res_driver.parse_interface_from_source ~for_printer:true
                  ~display_filename:displayFilename ~source:codeWithOffset
              in
              if invalid then (
                reportParseError diagnostics;
                codeText)
              else
                Res_printer.print_interface parsetree ~comments |> String.trim
            else
              let {Res_driver.parsetree; comments; invalid; diagnostics} =
                Res_driver.parse_implementation_from_source ~for_printer:true
                  ~display_filename:displayFilename ~source:codeWithOffset
              in
              if invalid then (
                reportParseError diagnostics;
                codeText)
              else
                let parsetree =
                  if transformAssertEqual then
                    Transform.transform ~transforms:[EqualsToAssertEqualFn]
                      parsetree
                  else parsetree
                in
                Res_printer.print_implementation parsetree ~comments
                |> String.trim
          in
          addCodeBlock mappedCode;
          Cmarkit.Mapper.default
        | _ -> Cmarkit.Mapper.default)
      | _ -> Cmarkit.Mapper.default
    in
    let mapper = Cmarkit.Mapper.make ~block () in
    let _ =
      content
      |> Cmarkit.Doc.of_string ~locs:true
      |> Cmarkit.Mapper.map_doc mapper
    in
    !codeBlocks

  let extractCodeblocksFromFile ~transformAssertEqual ~entryPointFile =
    let path =
      match Filename.is_relative entryPointFile with
      | true -> Unix.realpath entryPointFile
      | false -> entryPointFile
    in
    let displayFilename = Filename.basename path in
    let errors = ref [] in
    let addError error = errors := error :: !errors in

    let codeBlocks = ref [] in
    let addCodeBlock codeBlock = codeBlocks := codeBlock :: !codeBlocks in

    let content =
      if Filename.check_suffix path ".md" then
        let content = readFile path in
        let displayFilename = Filename.basename path in
        let codeBlocks =
          extractRescriptCodeBlocks ~transformAssertEqual ~addError
            ~displayFilename ~markdownBlockStartLine:1 content
        in
        Ok
          (codeBlocks
          |> List.mapi (fun index codeBlock ->
                 {
                   id = "codeblock-" ^ string_of_int (index + 1);
                   name = "codeblock-" ^ string_of_int (index + 1);
                   code = codeBlock;
                 }))
      else
        let extracted =
          extractCodeBlocks ~entryPointFile
            ~processDocstrings:(fun ~id ~name code ->
              let codeBlocks =
                code
                |> extractRescriptCodeBlocks ~transformAssertEqual ~addError
                     ~displayFilename ~markdownBlockStartLine:1
              in
              if List.length codeBlocks > 1 then
                codeBlocks |> List.rev
                |> List.iteri (fun index codeBlock ->
                       addCodeBlock
                         {
                           id = id ^ "-" ^ string_of_int (index + 1);
                           name;
                           code = codeBlock;
                         })
              else
                codeBlocks
                |> List.iter (fun codeBlock ->
                       addCodeBlock {id; name; code = codeBlock}))
        in

        match extracted with
        | Ok () -> Ok !codeBlocks
        | Error e -> Error e
    in
    match content with
    | Error e -> Error e
    | Ok codeBlocks ->
      let errors = !errors in
      if List.length errors > 0 then
        let errors = errors |> List.rev |> String.concat "\n" in
        Error errors
      else
        Ok
          (Yojson.Safe.pretty_to_string ~std:true
             (`List
                (codeBlocks
                |> List.map (fun codeBlock ->
                       `Assoc
                         [
                           ("id", `String codeBlock.id);
                           ("name", `String codeBlock.name);
                           ("code", `String codeBlock.code);
                         ]))))
end

module Migrate = Migrate
