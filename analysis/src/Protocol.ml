type position = {line: int; character: int}
type range = {start: position; end_: position}
type markupContent = {kind: string; value: string}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#command *)
type command = {title: string; command: string}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeLens *)
type codeLens = {range: range; command: command option}

type inlayHint = {
  position: position;
  label: string;
  kind: int;
  paddingLeft: bool;
  paddingRight: bool;
}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#parameterInformation *)
type parameterInformation = {label: int * int; documentation: markupContent}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#signatureInformation *)
type signatureInformation = {
  label: string;
  parameters: parameterInformation list;
  documentation: markupContent option;
}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#signatureHelp *)
type signatureHelp = {
  signatures: signatureInformation list;
  activeSignature: int option;
  activeParameter: int option;
}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#insertTextFormat *)
type insertTextFormat = Snippet

let insertTextFormatToInt f =
  match f with
  | Snippet -> 2

type textEdit = {range: range; newText: string}

type completionItem = {
  label: string;
  kind: int;
  tags: int list;
  detail: string;
  sortText: string option;
  filterText: string option;
  insertTextFormat: insertTextFormat option;
  insertText: string option;
  documentation: markupContent option;
  data: (string * string) list option;
  additionalTextEdits: textEdit list option;
}

type location = {uri: string; range: range}
type documentSymbolItem = {
  name: string;
  kind: int;
  range: range;
  children: documentSymbolItem list;
}
type renameFile = {oldUri: string; newUri: string}
type diagnostic = {range: range; message: string; severity: int}

type optionalVersionedTextDocumentIdentifier = {
  version: int option;
  uri: string;
}

type textDocumentEdit = {
  textDocument: optionalVersionedTextDocumentIdentifier;
  edits: textEdit list;
}

type createFileOptions = {overwrite: bool option; ignoreIfExists: bool option}
type createFile = {uri: string; options: createFileOptions option}

type documentChange =
  | TextDocumentEdit of textDocumentEdit
  | CreateFile of createFile

type codeActionEdit = {documentChanges: documentChange list}
type codeActionKind = RefactorRewrite

type codeAction = {
  title: string;
  codeActionKind: codeActionKind;
  edit: codeActionEdit;
}

let wrapInQuotes s = "\"" ^ Json.escape s ^ "\""

let null = "null"
let array l = "[" ^ String.concat ", " l ^ "]"

let stringifyPosition p =
  Printf.sprintf {|{"line": %i, "character": %i}|} p.line p.character

let stringifyRange r =
  Printf.sprintf {|{"start": %s, "end": %s}|}
    (stringifyPosition r.start)
    (stringifyPosition r.end_)

let stringifyTextEdit (te : textEdit) =
  Printf.sprintf
    {|{
      "range": %s,
      "newText": %s
      }|}
    (stringifyRange te.range) (wrapInQuotes te.newText)

let stringifyMarkupContent (m : markupContent) =
  Printf.sprintf {|{"kind": %s, "value": %s}|} (wrapInQuotes m.kind)
    (wrapInQuotes m.value)

(** None values are not emitted in the output. *)
let stringifyObject ?(startOnNewline = false) ?(indentation = 1) properties =
  let indentationStr = String.make (indentation * 2) ' ' in
  (if startOnNewline then "\n" ^ indentationStr else "")
  ^ {|{
|}
  ^ (properties
    |> List.filter_map (fun (key, value) ->
           match value with
           | None -> None
           | Some v ->
             Some (Printf.sprintf {|%s  "%s": %s|} indentationStr key v))
    |> String.concat ",\n")
  ^ "\n" ^ indentationStr ^ "}"

let optWrapInQuotes s =
  match s with
  | None -> None
  | Some s -> Some (wrapInQuotes s)

let stringifyCompletionItem c =
  stringifyObject
    [
      ("label", Some (wrapInQuotes c.label));
      ("kind", Some (string_of_int c.kind));
      ("tags", Some (c.tags |> List.map string_of_int |> array));
      ("detail", Some (wrapInQuotes c.detail));
      ( "documentation",
        Some
          (match c.documentation with
          | None -> null
          | Some doc -> stringifyMarkupContent doc) );
      ("sortText", optWrapInQuotes c.sortText);
      ("filterText", optWrapInQuotes c.filterText);
      ("insertText", optWrapInQuotes c.insertText);
      ( "insertTextFormat",
        match c.insertTextFormat with
        | None -> None
        | Some insertTextFormat ->
          Some (Printf.sprintf "%i" (insertTextFormatToInt insertTextFormat)) );
      ( "data",
        match c.data with
        | None -> None
        | Some fields ->
          Some
            (fields
            |> List.map (fun (key, value) -> (key, Some (wrapInQuotes value)))
            |> stringifyObject ~indentation:2) );
      ( "additionalTextEdits",
        match c.additionalTextEdits with
        | Some additionalTextEdits ->
          Some (additionalTextEdits |> List.map stringifyTextEdit |> array)
        | None -> None );
    ]

let stringifyHover value =
  Printf.sprintf {|{"contents": %s}|}
    (stringifyMarkupContent {kind = "markdown"; value})

let stringifyLocation (h : location) =
  Printf.sprintf {|{"uri": %s, "range": %s}|} (wrapInQuotes h.uri)
    (stringifyRange h.range)

let stringifyDocumentSymbolItems items =
  let buf = Buffer.create 10 in
  let stringifyName name = Printf.sprintf "\"%s\"" (Json.escape name) in
  let stringifyKind kind = string_of_int kind in
  let emitStr = Buffer.add_string buf in
  let emitSep () = emitStr ",\n" in
  let rec emitItem ~indent item =
    let openBrace = Printf.sprintf "%s{\n" indent in
    let closeBrace = Printf.sprintf "\n%s}" indent in
    let indent = indent ^ "  " in
    let emitField name s =
      emitStr (Printf.sprintf "%s\"%s\": %s" indent name s)
    in
    emitStr openBrace;
    emitField "name" (stringifyName item.name);
    emitSep ();
    emitField "kind" (stringifyKind item.kind);
    emitSep ();
    emitField "range" (stringifyRange item.range);
    emitSep ();
    emitField "selectionRange" (stringifyRange item.range);
    if item.children <> [] then (
      emitSep ();
      emitField "children" "[\n";
      emitBody ~indent (List.rev item.children);
      emitStr "]");
    emitStr closeBrace
  and emitBody ~indent items =
    match items with
    | [] -> ()
    | item :: rest ->
      emitItem ~indent item;
      if rest <> [] then emitSep ();
      emitBody ~indent rest
  in
  let indent = "" in
  emitStr "[\n";
  emitBody ~indent (List.rev items);
  emitStr "\n]";
  Buffer.contents buf

let stringifyRenameFile {oldUri; newUri} =
  Printf.sprintf {|{
  "kind": "rename",
  "oldUri": %s,
  "newUri": %s
}|}
    (wrapInQuotes oldUri) (wrapInQuotes newUri)

let stringifyoptionalVersionedTextDocumentIdentifier td =
  Printf.sprintf {|{
  "version": %s,
  "uri": %s
  }|}
    (match td.version with
    | None -> null
    | Some v -> string_of_int v)
    (wrapInQuotes td.uri)

let stringifyTextDocumentEdit tde =
  Printf.sprintf {|{
  "textDocument": %s,
  "edits": %s
  }|}
    (stringifyoptionalVersionedTextDocumentIdentifier tde.textDocument)
    (tde.edits |> List.map stringifyTextEdit |> array)

let stringifyCreateFile cf =
  stringifyObject
    [
      ("kind", Some (wrapInQuotes "create"));
      ("uri", Some (wrapInQuotes cf.uri));
      ( "options",
        match cf.options with
        | None -> None
        | Some options ->
          Some
            (stringifyObject
               [
                 ( "overwrite",
                   match options.overwrite with
                   | None -> None
                   | Some ov -> Some (string_of_bool ov) );
                 ( "ignoreIfExists",
                   match options.ignoreIfExists with
                   | None -> None
                   | Some i -> Some (string_of_bool i) );
               ]) );
    ]

let stringifyDocumentChange dc =
  match dc with
  | TextDocumentEdit tde -> stringifyTextDocumentEdit tde
  | CreateFile cf -> stringifyCreateFile cf

let codeActionKindToString kind =
  match kind with
  | RefactorRewrite -> "refactor.rewrite"

let stringifyCodeActionEdit cae =
  Printf.sprintf {|{"documentChanges": %s}|}
    (cae.documentChanges |> List.map stringifyDocumentChange |> array)

let stringifyCodeAction ca =
  Printf.sprintf {|{"title": %s, "kind": %s, "edit": %s}|}
    (wrapInQuotes ca.title)
    (wrapInQuotes (codeActionKindToString ca.codeActionKind))
    (ca.edit |> stringifyCodeActionEdit)

let stringifyHint (hint : inlayHint) =
  Printf.sprintf
    {|{
    "position": %s,
    "label": %s,
    "kind": %i,
    "paddingLeft": %b,
    "paddingRight": %b
}|}
    (stringifyPosition hint.position)
    (wrapInQuotes hint.label) hint.kind hint.paddingLeft hint.paddingRight

let stringifyCommand (command : command) =
  Printf.sprintf {|{"title": %s, "command": %s}|}
    (wrapInQuotes command.title)
    (wrapInQuotes command.command)

let stringifyCodeLens (codeLens : codeLens) =
  Printf.sprintf
    {|{
        "range": %s,
        "command": %s
    }|}
    (stringifyRange codeLens.range)
    (match codeLens.command with
    | None -> ""
    | Some command -> stringifyCommand command)

let stringifyParameterInformation (parameterInformation : parameterInformation)
    =
  Printf.sprintf {|{"label": %s, "documentation": %s}|}
    (let line, chr = parameterInformation.label in
     "[" ^ string_of_int line ^ ", " ^ string_of_int chr ^ "]")
    (stringifyMarkupContent parameterInformation.documentation)

let stringifySignatureInformation (signatureInformation : signatureInformation)
    =
  Printf.sprintf
    {|{
    "label": %s,
    "parameters": %s%s
  }|}
    (wrapInQuotes signatureInformation.label)
    (signatureInformation.parameters
    |> List.map stringifyParameterInformation
    |> array)
    (match signatureInformation.documentation with
    | None -> ""
    | Some docs ->
      Printf.sprintf ",\n    \"documentation\": %s"
        (stringifyMarkupContent docs))

let stringifySignatureHelp (signatureHelp : signatureHelp) =
  Printf.sprintf
    {|{
  "signatures": %s,
  "activeSignature": %s,
  "activeParameter": %s
}|}
    (signatureHelp.signatures |> List.map stringifySignatureInformation |> array)
    (match signatureHelp.activeSignature with
    | None -> null
    | Some activeSignature -> activeSignature |> string_of_int)
    (match signatureHelp.activeParameter with
    | None -> null
    | Some activeParameter -> activeParameter |> string_of_int)

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic *)
let stringifyDiagnostic d =
  Printf.sprintf
    {|{
  "range": %s,
  "message": %s,
  "severity": %d,
  "source": "ReScript"
}|}
    (stringifyRange d.range) (wrapInQuotes d.message) d.severity
