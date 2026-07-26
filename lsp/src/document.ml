open Lsp
open Types

type t = Text_document.t

type kind = Analysis.Files.classified_file

let kind_of_uri uri =
  let p = Uri.to_path uri in
  match p |> Filename.extension with
  | ".res" -> Analysis.Files.Res
  | ".resi" -> Analysis.Files.Resi
  | other ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:("unsupported file extension for " ^ p)
         ~data:(`Assoc [("extension", `String other)])
         ())

let kind doc = doc |> Text_document.documentUri |> kind_of_uri

let to_string (kind : kind) =
  match kind with
  | Res -> "res"
  | Resi -> "resi"
  | Other -> assert false

let make (doc : DidOpenTextDocumentParams.t) =
  let t_doc = Text_document.make ~position_encoding:`UTF8 doc in
  t_doc

let text t = Text_document.text t

let uri t = Text_document.documentUri t

let update_text ?version t changes =
  match Text_document.apply_content_changes ?version t changes with
  | exception Text_document.Invalid_utf _ ->
    (* TODO: add log *)
    t
  | tdoc -> tdoc

let range_of_text (text : string) : Range.t =
  let text_length = String.length text in
  let rec end_position offset line character =
    if offset = text_length then Position.create ~line ~character
    else
      match String.get text offset with
      | '\n' -> end_position (offset + 1) (line + 1) 0
      | _ -> end_position (offset + 1) line (character + 1)
  in
  Range.create
    ~start:(Position.create ~line:0 ~character:0)
    ~end_:(end_position 0 0 0)

let%expect_test "range_of_text" =
  let print text =
    let range = range_of_text text in
    Yojson.Safe.pretty_to_string
      (`Assoc
         [
           ("start", range.start |> Position.yojson_of_t);
           ("end", range.end_ |> Position.yojson_of_t);
         ])
    |> print_endline
  in
  print "";
  print "let a = 1";
  print "let a = 1\nlet b = 2";
  print "let a = 1\n";
  print "\xC3\xA9";
  print "ab";
  print "ab\ncd";
  [%expect
    {|
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 0, "line": 0 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 9, "line": 0 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 9, "line": 1 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 0, "line": 1 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 2, "line": 0 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 2, "line": 0 }
    }
    {
      "start": { "character": 0, "line": 0 },
      "end": { "character": 2, "line": 1 }
    }
    |}]
