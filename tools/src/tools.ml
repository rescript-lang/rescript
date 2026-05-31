open Analysis

module StringSet = Set.Make (String)

type field_doc = {
  field_name: string;
  docstrings: string list;
  signature: string;
  optional: bool;
  deprecated: string option;
}

type constructor_payload = InlineRecord of {field_docs: field_doc list}

type constructor_doc = {
  constructor_name: string;
  docstrings: string list;
  signature: string;
  deprecated: string option;
  items: constructor_payload option;
}

type type_doc = {path: string; generic_parameters: type_doc list}
type value_signature = {parameters: type_doc list; return_type: type_doc}

type source = {filepath: string; line: int; col: int}

type doc_item_detail =
  | Record of {field_docs: field_doc list}
  | Variant of {constructor_docs: constructor_doc list}
  | Signature of value_signature

type doc_item =
  | Value of {
      id: string;
      docstring: string list;
      signature: string;
      name: string;
      deprecated: string option;
      detail: doc_item_detail option;
      source: source;
    }
  | Type of {
      id: string;
      docstring: string list;
      signature: string;
      name: string;
      deprecated: string option;
      detail: doc_item_detail option;
      source: source;
          (** Additional documentation for constructors and record fields, if available. *)
    }
  | Module of docs_for_module
  | ModuleType of {
      id: string;
      docstring: string list;
      deprecated: string option;
      name: string;
      source: source;
      items: doc_item list;
    }
  | ModuleAlias of {
      id: string;
      docstring: string list;
      name: string;
      source: source;
      items: doc_item list;
    }
and docs_for_module = {
  id: string;
  docstring: string list;
  deprecated: string option;
  name: string;
  moduletypeid: string option;
  source: source;
  items: doc_item list;
}

let stringify_docstrings docstrings =
  `List
    (docstrings
    |> List.map (fun docstring -> `String (docstring |> String.trim)))

let stringify_field_doc (field_doc : field_doc) =
  `Assoc
    ([
       ("name", `String field_doc.field_name);
       ("optional", `Bool field_doc.optional);
       ("docstrings", stringify_docstrings field_doc.docstrings);
       ("signature", `String field_doc.signature);
     ]
    @
    match field_doc.deprecated with
    | Some d -> [("deprecated", `String d)]
    | None -> [])

let stringify_constructor_payload (constructor_payload : constructor_payload) =
  match constructor_payload with
  | InlineRecord {field_docs} ->
    `Assoc
      [
        ("kind", `String "inlineRecord");
        ("fields", `List (field_docs |> List.map stringify_field_doc));
      ]

let rec stringify_type_doc (td : type_doc) =
  let ps =
    match td.generic_parameters with
    | [] -> `List []
    | ts -> ts |> List.map stringify_type_doc |> fun ts -> `List ts
  in
  `Assoc [("path", `String td.path); ("genericTypeParameters", ps)]

let stringify_detail (detail : doc_item_detail) =
  match detail with
  | Record {field_docs} ->
    `Assoc
      [
        ("kind", `String "record");
        ("items", `List (field_docs |> List.map stringify_field_doc));
      ]
  | Variant {constructor_docs} ->
    `Assoc
      [
        ("kind", `String "variant");
        ( "items",
          `List
            (constructor_docs
            |> List.map (fun constructor_doc ->
                   `Assoc
                     ([
                        ("name", `String constructor_doc.constructor_name);
                        ( "docstrings",
                          stringify_docstrings constructor_doc.docstrings );
                        ("signature", `String constructor_doc.signature);
                      ]
                     @ (match constructor_doc.deprecated with
                       | Some d -> [("deprecated", `String d)]
                       | None -> [])
                     @
                     match constructor_doc.items with
                     | Some constructor_payload ->
                       [
                         ( "payload",
                           stringify_constructor_payload constructor_payload );
                       ]
                     | None -> []))) );
      ]
  | Signature {parameters; return_type} ->
    let ps =
      match parameters with
      | [] -> `List []
      | ps -> ps |> List.map stringify_type_doc |> fun ps -> `List ps
    in
    `Assoc
      [
        ("kind", `String "signature");
        ( "details",
          `Assoc
            [("parameters", ps); ("returnType", stringify_type_doc return_type)]
        );
      ]

let stringify_source source =
  `Assoc
    [
      ("filepath", `String source.filepath);
      ("line", `Int source.line);
      ("col", `Int source.col);
    ]

let rec stringify_doc_item ~original_env (item : doc_item) =
  match item with
  | Value {id; docstring; signature; name; deprecated; source; detail} ->
    `Assoc
      ([
         ("id", `String id);
         ("kind", `String "value");
         ("name", `String name);
         ("signature", `String (signature |> String.trim));
         ("docstrings", stringify_docstrings docstring);
         ("source", stringify_source source);
       ]
      @ (match deprecated with
        | Some d -> [("deprecated", `String d)]
        | None -> [])
      @
      match detail with
      | Some detail -> [("detail", stringify_detail detail)]
      | None -> [])
  | Type {id; docstring; signature; name; deprecated; detail; source} ->
    `Assoc
      ([
         ("id", `String id);
         ("kind", `String "type");
         ("name", `String name);
         ("signature", `String signature);
         ("docstrings", stringify_docstrings docstring);
         ("source", stringify_source source);
       ]
      @ (match deprecated with
        | Some d -> [("deprecated", `String d)]
        | None -> [])
      @
      match detail with
      | Some detail -> [("detail", stringify_detail detail)]
      | None -> [])
  | Module m ->
    `Assoc
      ([
         ("id", `String m.id);
         ("name", `String m.name);
         ("kind", `String "module");
         ("docstrings", stringify_docstrings m.docstring);
         ("source", stringify_source m.source);
         ( "items",
           `List
             (m.items
             |> List.map (fun item -> stringify_doc_item ~original_env item)) );
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
         ("docstrings", stringify_docstrings m.docstring);
         ("source", stringify_source m.source);
         ( "items",
           `List
             (m.items
             |> List.map (fun item -> stringify_doc_item ~original_env item)) );
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
        ("docstrings", stringify_docstrings m.docstring);
        ("source", stringify_source m.source);
        ("items", `List (m.items |> List.map (stringify_doc_item ~original_env)));
      ]

and stringify_docs_for_module ~original_env (d : docs_for_module) =
  `Assoc
    ([
       ("name", `String d.name);
       ("docstrings", stringify_docstrings d.docstring);
       ("source", stringify_source d.source);
       ( "items",
         `List
           (d.items
           |> List.map (fun item -> stringify_doc_item ~original_env item)) );
     ]
    @
    match d.deprecated with
    | Some d -> [("deprecated", `String d)]
    | None -> [])

let field_to_field_doc (field : SharedTypes.field) : field_doc =
  {
    field_name = field.fname.txt;
    docstrings = field.docstring;
    optional = field.optional;
    signature = Shared.type_to_string field.typ;
    deprecated = field.deprecated;
  }

let type_detail typ ~env ~full =
  let open SharedTypes in
  match TypeUtils.extract_type_from_resolved_type ~env ~full typ with
  | Some (Trecord {fields}) ->
    Some (Record {field_docs = fields |> List.map field_to_field_doc})
  | Some (Tvariant {constructors}) ->
    Some
      (Variant
         {
           constructor_docs =
             constructors
             |> List.map (fun (c : Constructor.t) ->
                    {
                      constructor_name = c.cname.txt;
                      docstrings = c.docstring;
                      signature = CompletionBackEnd.show_constructor c;
                      deprecated = c.deprecated;
                      items =
                        (match c.args with
                        | InlineRecord fields ->
                          Some
                            (InlineRecord
                               {
                                 field_docs =
                                   fields |> List.map field_to_field_doc;
                               })
                        | _ -> None);
                    });
         })
  | _ -> None

(* split a list into two parts all the items except the last one and the last item *)
let split_last l =
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

let value_detail (typ : Types.type_expr) =
  let rec collect_signature_types (typ : Types.type_expr) =
    match typ.desc with
    | Tlink t | Tsubst t | Tpoly (t, []) -> collect_signature_types t
    | Tconstr (path, ts, _) -> (
      let p = path_to_string path in
      match ts with
      | [] -> [{path = p; generic_parameters = []}]
      | ts ->
        let ts =
          ts
          |> List.concat_map (fun (t : Types.type_expr) ->
                 collect_signature_types t)
        in
        [{path = p; generic_parameters = ts}])
    | Tarrow (arg, ret, _, _) ->
      collect_signature_types arg.typ @ collect_signature_types ret
    | Tvar None -> [{path = "_"; generic_parameters = []}]
    | _ -> []
  in
  match collect_signature_types typ with
  | [] -> None
  | ts ->
    let parameters, return_type = split_last ts in
    Some (Signature {parameters; return_type})

let make_id module_path ~identifier =
  identifier :: module_path |> List.rev |> SharedTypes.ident

let get_source ~root_path ({loc_start} : Location.t) =
  let line, col = Pos.of_lexing loc_start in
  let filepath =
    Files.relpath root_path loc_start.pos_fname
    |> Files.split Filename.dir_sep
    |> String.concat "/"
  in
  {filepath; line = line + 1; col = col + 1}

let extract_docs ~entry_point_file ~debug =
  let path =
    match Filename.is_relative entry_point_file with
    | true -> Unix.realpath entry_point_file
    | false -> entry_point_file
  in
  if debug then Printf.printf "extracting docs for %s\n" path;
  let result =
    match
      FindFiles.is_implementation path = false
      && FindFiles.is_interface path = false
    with
    | false -> (
      let path =
        if FindFiles.is_implementation path then
          let path_as_resi =
            (path |> Filename.dirname) ^ "/"
            ^ (path |> Filename.basename |> Filename.chop_extension)
            ^ ".resi"
          in
          if Sys.file_exists path_as_resi then (
            if debug then
              Printf.printf "preferring found resi file for impl: %s\n"
                path_as_resi;
            path_as_resi)
          else path
        else path
      in
      match Cmt.load_full_cmt_from_path ~path with
      | None ->
        Error
          (Printf.sprintf
             "error: failed to generate doc for %s, try to build the project"
             path)
      | Some full ->
        let file = full.file in
        let structure = file.structure in
        let root_path = full.package.root_path in
        let open SharedTypes in
        let env = QueryEnv.from_file file in
        let rec extract_docs_for_module ?(module_path = [env.file.module_name])
            (structure : Module.structure) =
          let values_seen = ref StringSet.empty in
          {
            id = module_path |> List.rev |> ident;
            docstring = structure.docstring |> List.map String.trim;
            name = structure.name;
            moduletypeid = None;
            deprecated = structure.deprecated;
            source =
              {
                filepath =
                  (match root_path = "." with
                  | true -> file.uri |> Uri.to_path
                  | false ->
                    Files.relpath root_path (file.uri |> Uri.to_path)
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
                     let source = get_source ~root_path item.loc in
                     match item.kind with
                     | Value typ ->
                       Some
                         (Value
                            {
                              id = module_path |> make_id ~identifier:item.name;
                              docstring = item.docstring |> List.map String.trim;
                              signature =
                                "let " ^ item.name ^ ": "
                                ^ Shared.type_to_string typ;
                              name = item.name;
                              deprecated = item.deprecated;
                              detail = value_detail typ;
                              source;
                            })
                     | Type (typ, _) ->
                       Some
                         (Type
                            {
                              id = module_path |> make_id ~identifier:item.name;
                              docstring = item.docstring |> List.map String.trim;
                              signature =
                                typ.decl |> Shared.decl_to_string item.name;
                              name = item.name;
                              deprecated = item.deprecated;
                              detail = type_detail typ ~full ~env;
                              source;
                            })
                     | Module {type_ = Ident p; is_module_type = false} ->
                       (* module Whatever = OtherModule *)
                       let alias_to_module = p |> path_ident_to_string in
                       let id =
                         (module_path |> List.rev |> List.hd) ^ "." ^ item.name
                       in
                       let items, internal_docstrings =
                         match
                           ProcessCmt.file_for_module ~package:full.package
                             alias_to_module
                         with
                         | None -> ([], [])
                         | Some file ->
                           let docs =
                             extract_docs_for_module ~module_path:[id]
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
                                item.docstring @ internal_docstrings
                                |> List.map String.trim;
                            })
                     | Module {type_ = Structure m; is_module_type = false} ->
                       (* module Whatever = {} in res or module Whatever: {} in resi. *)
                       let module_path = m.name :: module_path in
                       let docs = extract_docs_for_module ~module_path m in
                       Some
                         (Module
                            {
                              id = module_path |> List.rev |> ident;
                              name = m.name;
                              moduletypeid = None;
                              docstring = item.docstring @ m.docstring;
                              deprecated = item.deprecated;
                              source;
                              items = docs.items;
                            })
                     | Module {type_ = Structure m; is_module_type = true} ->
                       (* module type Whatever = {} *)
                       let module_path = m.name :: module_path in
                       let docs = extract_docs_for_module ~module_path m in
                       Some
                         (ModuleType
                            {
                              id = module_path |> List.rev |> ident;
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
                            (extract_docs_for_module
                               ~module_path:(interface.name :: module_path)
                               interface))
                     | Module {type_ = Constraint (Structure m, Ident p)} ->
                       (* module M: T = { <impl> }. Print M *)
                       let docs =
                         extract_docs_for_module
                           ~module_path:(m.name :: module_path) m
                       in
                       let ident_module_path = p |> Path.head |> Ident.name in

                       let module_type_id_path =
                         match
                           ProcessCmt.file_for_module ~package:full.package
                             ident_module_path
                           |> Option.is_none
                         with
                         | false -> []
                         | true -> [module_path |> List.rev |> List.hd]
                       in

                       Some
                         (Module
                            {
                              docs with
                              moduletypeid =
                                Some
                                  (make_id ~identifier:(Path.name p)
                                     module_type_id_path);
                            })
                     | _ -> None)
              (* Filter out shadowed bindings by keeping only the last value associated with an id *)
              |> List.rev
              |> List.filter_map (fun (doc_item : doc_item) ->
                     match doc_item with
                     | Value {id} ->
                       if StringSet.mem id !values_seen then None
                       else (
                         values_seen := StringSet.add id !values_seen;
                         Some doc_item)
                     | _ -> Some doc_item)
              |> List.rev;
          }
        in
        let docs = extract_docs_for_module structure in
        Ok
          (stringify_docs_for_module ~original_env:env docs
          |> Yojson.Safe.pretty_to_string))
    | true ->
      Error
        (Printf.sprintf
           "error: failed to read %s, expected an .res or .resi file" path)
  in

  result

let extract_embedded ~extension_points ~filename =
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
      when extension_points |> List.exists (fun v -> v = txt) ->
      append (pexp_loc, txt, contents)
    | _ -> ());
    Ast_iterator.default_iterator.extension iterator ext
  in
  let iterator = {Ast_iterator.default_iterator with extension} in
  iterator.structure iterator structure;
  let result =
    !content
    |> List.map (fun (loc, extension_name, contents) ->
           `Assoc
             [
               ("extensionName", `String extension_name);
               ("contents", `String contents);
               ( "loc",
                 Analysis.Utils.cmt_loc_to_range loc
                 |> Lsp.Types.Range.yojson_of_t );
             ])
    |> List.rev
  in
  Yojson.Safe.pretty_to_string (`List result)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let is_res_lang lang =
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
        let has_transform transform = transforms |> List.mem transform in
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
                              ({txt = Lident "assertEqual"} as ident_txt);
                        } as ident;
                      partial = false;
                      args = [(Nolabel, _); (Nolabel, _)] as args;
                    }
                  when has_transform AssertEqualFnToEquals ->
                  {
                    exp with
                    pexp_desc =
                      Pexp_apply
                        {
                          funct =
                            {
                              ident with
                              pexp_desc =
                                Pexp_ident {ident_txt with txt = Lident "=="};
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
                                             ident_txt);
                                      } as ident;
                                    partial = false;
                                    args = [rhs];
                                  };
                            } );
                        ];
                    }
                  when has_transform AssertEqualFnToEquals ->
                  {
                    exp with
                    pexp_desc =
                      Pexp_apply
                        {
                          funct =
                            {
                              ident with
                              pexp_desc =
                                Pexp_ident {ident_txt with txt = Lident "=="};
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

  let format_rescript_code_blocks content ~transform_assert_equal
      ~display_filename ~add_error ~markdown_block_start_line =
    (* Detect ReScript code blocks. *)
    let had_code_blocks = ref false in
    let block _m = function
      | Cmarkit.Block.Code_block (code_block, meta) -> (
        match Cmarkit.Block.Code_block.info_string code_block with
        | Some ((lang, _) as info_string) when is_res_lang lang ->
          had_code_blocks := true;

          let current_line =
            meta |> Cmarkit.Meta.textloc |> Cmarkit.Textloc.first_line |> fst
          in
          (* Account for 0-based line numbers *)
          let current_line = current_line + 1 in
          let layout = Cmarkit.Block.Code_block.layout code_block in
          let code = Cmarkit.Block.Code_block.code code_block in
          let code_text =
            code |> List.map Cmarkit.Block_line.to_string |> String.concat "\n"
          in

          let n = List.length code in
          let newlines_needed =
            max 0 (markdown_block_start_line + current_line - n)
          in
          let code_with_offset = String.make newlines_needed '\n' ^ code_text in
          let report_parse_error diagnostics =
            let buf = Buffer.create 1000 in
            let formatter = Format.formatter_of_buffer buf in
            Res_diagnostics.print_report ~formatter
              ~custom_intro:(Some "Syntax error in code block in docstring")
              diagnostics code_with_offset;
            add_error (Buffer.contents buf)
          in
          let formatted_code =
            if lang |> String.split_on_char ' ' |> List.hd = "resi" then
              let {Res_driver.parsetree; comments; invalid; diagnostics} =
                Res_driver.parse_interface_from_source ~for_printer:true
                  ~display_filename ~source:code_with_offset
              in
              if invalid then (
                report_parse_error diagnostics;
                code)
              else
                Res_printer.print_interface parsetree ~comments
                |> String.trim |> Cmarkit.Block_line.list_of_string
            else
              let {Res_driver.parsetree; comments; invalid; diagnostics} =
                Res_driver.parse_implementation_from_source ~for_printer:true
                  ~display_filename ~source:code_with_offset
              in
              if invalid then (
                report_parse_error diagnostics;
                code)
              else
                let parsetree =
                  if transform_assert_equal then
                    Transform.transform ~transforms:[AssertEqualFnToEquals]
                      parsetree
                  else parsetree
                in
                Res_printer.print_implementation parsetree ~comments
                |> String.trim |> Cmarkit.Block_line.list_of_string
          in

          let mapped_code_block =
            Cmarkit.Block.Code_block.make ~layout ~info_string formatted_code
          in
          Cmarkit.Mapper.ret
            (Cmarkit.Block.Code_block (mapped_code_block, meta))
        | _ -> Cmarkit.Mapper.default)
      | _ -> Cmarkit.Mapper.default
    in
    let mapper = Cmarkit.Mapper.make ~block () in
    let new_content =
      content
      |> Cmarkit.Doc.of_string ~locs:true
      |> Cmarkit.Mapper.map_doc mapper
      |> Cmarkit_commonmark.of_doc
    in
    (new_content, !had_code_blocks)

  let format_code_blocks_in_file ~output_mode ~transform_assert_equal
      ~entry_point_file =
    let path =
      match Filename.is_relative entry_point_file with
      | true -> Unix.realpath entry_point_file
      | false -> entry_point_file
    in
    let errors = ref [] in
    let add_error error = errors := error :: !errors in

    let make_mapper ~transform_assert_equal ~display_filename =
      {
        Ast_mapper.default_mapper with
        attribute =
          (fun mapper ((name, payload) as attr) ->
            match (name, Ast_payload.is_single_string payload, payload) with
            | ( {txt = "res.doc"},
                Some (contents, None),
                PStr [{pstr_desc = Pstr_eval ({pexp_loc}, _)}] ) ->
              let formatted_contents, had_code_blocks =
                format_rescript_code_blocks ~transform_assert_equal ~add_error
                  ~display_filename
                  ~markdown_block_start_line:pexp_loc.loc_start.pos_lnum
                  contents
              in
              if had_code_blocks && formatted_contents <> contents then
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
    let content =
      if Filename.check_suffix path ".md" then
        let content = read_file path in
        let display_filename = Filename.basename path in
        let formatted_contents, had_code_blocks =
          format_rescript_code_blocks ~transform_assert_equal ~add_error
            ~display_filename ~markdown_block_start_line:1 content
        in
        if had_code_blocks && formatted_contents <> content then
          Ok (formatted_contents, content)
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
          make_mapper ~transform_assert_equal ~display_filename:filename
        in
        let ast_mapped = mapper.structure mapper structure in
        Ok (Res_printer.print_implementation ast_mapped ~comments, source)
      else if Filename.check_suffix path ".resi" then
        let parser =
          Res_driver.parsing_engine.parse_interface ~for_printer:true
        in
        let {Res_driver.parsetree = signature; comments; source; filename} =
          parser ~filename:path
        in
        let mapper =
          make_mapper ~transform_assert_equal ~display_filename:filename
        in
        let ast_mapped = mapper.signature mapper signature in
        Ok (Res_printer.print_interface ast_mapped ~comments, source)
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
        match output_mode with
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
        let has_transform transform = transforms |> List.mem transform in
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
                                       ({txt = Lident "=="} as ident_txt);
                                 } as ident;
                               partial = false;
                               args = [(Nolabel, _); (Nolabel, _)] as args;
                             };
                       } as exp),
                      x1 )
                  when has_transform EqualsToAssertEqualFn ->
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
                                            ident_txt with
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

  type code_block = {id: string; code: string; name: string}

  let get_docstring = function
    | d :: _ -> d
    | _ -> ""

  let extract_code_blocks ~entry_point_file
      ~(process_docstrings : id:string -> name:string -> string -> unit) =
    let path =
      match Filename.is_relative entry_point_file with
      | true -> Unix.realpath entry_point_file
      | false -> entry_point_file
    in
    let result =
      match
        FindFiles.is_implementation path = false
        && FindFiles.is_interface path = false
      with
      | false -> (
        let path =
          if FindFiles.is_implementation path then
            let path_as_resi =
              (path |> Filename.dirname) ^ "/"
              ^ (path |> Filename.basename |> Filename.chop_extension)
              ^ ".resi"
            in
            if Sys.file_exists path_as_resi then path_as_resi else path
          else path
        in
        match Cmt.load_full_cmt_from_path ~path with
        | None ->
          Error
            (Printf.sprintf
               "error: failed to generate doc for %s, try to build the project"
               path)
        | Some full ->
          let file = full.file in
          let structure = file.structure in
          let open SharedTypes in
          let env = QueryEnv.from_file file in
          let rec extract_code_blocks_for_module
              ?(module_path = [env.file.module_name])
              (structure : Module.structure) =
            let id = module_path |> List.rev |> ident in
            let name = structure.name in
            process_docstrings ~id ~name (get_docstring structure.docstring);

            structure.items
            |> List.iter (fun (item : Module.item) ->
                   match item.kind with
                   | Value _typ ->
                     let id = module_path |> make_id ~identifier:item.name in
                     let name = item.name in
                     process_docstrings ~id ~name (get_docstring item.docstring)
                   | Type (_typ, _) ->
                     let id = module_path |> make_id ~identifier:item.name in
                     let name = item.name in
                     process_docstrings ~id ~name (get_docstring item.docstring)
                   | Module {type_ = Ident _p; is_module_type = false} ->
                     (* module Whatever = OtherModule *)
                     let id =
                       (module_path |> List.rev |> List.hd) ^ "." ^ item.name
                     in
                     let name = item.name in
                     process_docstrings ~id ~name (get_docstring item.docstring)
                   | Module {type_ = Structure m; is_module_type = false} ->
                     (* module Whatever = {} in res or module Whatever: {} in resi. *)
                     let module_path = m.name :: module_path in
                     let id = module_path |> List.rev |> ident in
                     let name = m.name in
                     process_docstrings ~id ~name (get_docstring m.docstring);
                     extract_code_blocks_for_module ~module_path m
                   | Module {type_ = Structure m; is_module_type = true} ->
                     (* module type Whatever = {} *)
                     let module_path = m.name :: module_path in
                     let id = module_path |> List.rev |> ident in
                     let name = m.name in
                     process_docstrings ~id ~name (get_docstring m.docstring);
                     extract_code_blocks_for_module ~module_path m
                   | Module
                       {
                         type_ =
                           Constraint (Structure _impl, Structure interface);
                       } ->
                     (* module Whatever: { <interface> } = { <impl> }. Prefer the interface. *)
                     let module_path = interface.name :: module_path in
                     let id = module_path |> List.rev |> ident in
                     let name = interface.name in
                     process_docstrings ~id ~name
                       (get_docstring interface.docstring);
                     extract_code_blocks_for_module ~module_path interface
                   | Module {type_ = Constraint (Structure m, Ident _p)} ->
                     (* module M: T = { <impl> }. Print M *)
                     let module_path = m.name :: module_path in
                     let id = module_path |> List.rev |> ident in
                     let name = m.name in
                     process_docstrings ~id ~name (get_docstring m.docstring);
                     extract_code_blocks_for_module ~module_path m
                   | Module.Module _ -> ())
          in
          extract_code_blocks_for_module structure;
          Ok ())
      | true ->
        Error
          (Printf.sprintf
             "error: failed to read %s, expected an .res or .resi file" path)
    in

    result

  let extract_rescript_code_blocks content ~transform_assert_equal
      ~display_filename ~add_error ~markdown_block_start_line =
    (* Detect ReScript code blocks. *)
    let code_blocks = ref [] in
    let add_code_block code_block = code_blocks := code_block :: !code_blocks in
    let block _m = function
      | Cmarkit.Block.Code_block (code_block, meta) -> (
        match Cmarkit.Block.Code_block.info_string code_block with
        | Some (lang, _) when is_res_lang lang ->
          let current_line =
            meta |> Cmarkit.Meta.textloc |> Cmarkit.Textloc.first_line |> fst
          in
          (* Account for 0-based line numbers *)
          let current_line = current_line + 1 in
          let code = Cmarkit.Block.Code_block.code code_block in
          let code_text =
            code |> List.map Cmarkit.Block_line.to_string |> String.concat "\n"
          in
          let n = List.length code in
          let newlines_needed =
            max 0 (markdown_block_start_line + current_line - n)
          in
          let code_with_offset = String.make newlines_needed '\n' ^ code_text in
          let report_parse_error diagnostics =
            let buf = Buffer.create 1000 in
            let formatter = Format.formatter_of_buffer buf in
            Res_diagnostics.print_report ~formatter
              ~custom_intro:(Some "Syntax error in code block in docstring")
              diagnostics code_with_offset;
            add_error (Buffer.contents buf)
          in
          let mapped_code =
            if lang |> String.split_on_char ' ' |> List.hd = "resi" then
              let {Res_driver.parsetree; comments; invalid; diagnostics} =
                Res_driver.parse_interface_from_source ~for_printer:true
                  ~display_filename ~source:code_with_offset
              in
              if invalid then (
                report_parse_error diagnostics;
                code_text)
              else
                Res_printer.print_interface parsetree ~comments |> String.trim
            else
              let {Res_driver.parsetree; comments; invalid; diagnostics} =
                Res_driver.parse_implementation_from_source ~for_printer:true
                  ~display_filename ~source:code_with_offset
              in
              if invalid then (
                report_parse_error diagnostics;
                code_text)
              else
                let parsetree =
                  if transform_assert_equal then
                    Transform.transform ~transforms:[EqualsToAssertEqualFn]
                      parsetree
                  else parsetree
                in
                Res_printer.print_implementation parsetree ~comments
                |> String.trim
          in
          add_code_block mapped_code;
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
    !code_blocks

  let extract_codeblocks_from_file ~transform_assert_equal ~entry_point_file =
    let path =
      match Filename.is_relative entry_point_file with
      | true -> Unix.realpath entry_point_file
      | false -> entry_point_file
    in
    let display_filename = Filename.basename path in
    let errors = ref [] in
    let add_error error = errors := error :: !errors in

    let code_blocks = ref [] in
    let add_code_block code_block = code_blocks := code_block :: !code_blocks in

    let content =
      if Filename.check_suffix path ".md" then
        let content = read_file path in
        let display_filename = Filename.basename path in
        let code_blocks =
          extract_rescript_code_blocks ~transform_assert_equal ~add_error
            ~display_filename ~markdown_block_start_line:1 content
        in
        Ok
          (code_blocks
          |> List.mapi (fun index code_block ->
                 {
                   id = "codeblock-" ^ string_of_int (index + 1);
                   name = "codeblock-" ^ string_of_int (index + 1);
                   code = code_block;
                 }))
      else
        let extracted =
          extract_code_blocks ~entry_point_file
            ~process_docstrings:(fun ~id ~name code ->
              let code_blocks =
                code
                |> extract_rescript_code_blocks ~transform_assert_equal
                     ~add_error ~display_filename ~markdown_block_start_line:1
              in
              if List.length code_blocks > 1 then
                code_blocks |> List.rev
                |> List.iteri (fun index code_block ->
                       add_code_block
                         {
                           id = id ^ "-" ^ string_of_int (index + 1);
                           name;
                           code = code_block;
                         })
              else
                code_blocks
                |> List.iter (fun code_block ->
                       add_code_block {id; name; code = code_block}))
        in

        match extracted with
        | Ok () -> Ok !code_blocks
        | Error e -> Error e
    in
    match content with
    | Error e -> Error e
    | Ok code_blocks ->
      let errors = !errors in
      if List.length errors > 0 then
        let errors = errors |> List.rev |> String.concat "\n" in
        Error errors
      else
        Ok
          (Yojson.Safe.pretty_to_string ~std:true
             (`List
                (code_blocks
                |> List.map (fun code_block ->
                       `Assoc
                         [
                           ("id", `String code_block.id);
                           ("name", `String code_block.name);
                           ("code", `String code_block.code);
                         ]))))
end

module Migrate = Migrate
