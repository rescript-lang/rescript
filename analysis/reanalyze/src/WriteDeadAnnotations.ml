open Common

type language = Ml | Res

let posLanguage (pos : Lexing.position) =
  if
    Filename.check_suffix pos.pos_fname ".res"
    || Filename.check_suffix pos.pos_fname ".resi"
  then Res
  else Ml

let deadAnnotation = "dead"
let annotateAtEnd ~pos =
  match posLanguage pos with
  | Res -> false
  | Ml -> true

let getPosAnnotation decl =
  match annotateAtEnd ~pos:decl.pos with
  | true -> decl.posEnd
  | false -> decl.posStart

let rec lineToString_ {original; declarations} =
  match declarations with
  | [] -> original
  | ({declKind; path; pos} as decl) :: nextDeclarations ->
    let language = posLanguage pos in
    let annotationStr =
      match language with
      | Res ->
        "@" ^ deadAnnotation ^ "(\"" ^ (path |> Path.withoutHead) ^ "\") "
      | Ml ->
        " " ^ "["
        ^ (match declKind |> DeclKind.isType with
          | true -> "@"
          | false -> "@@")
        ^ deadAnnotation ^ " \"" ^ (path |> Path.withoutHead) ^ "\"] "
    in
    let posAnnotation = decl |> getPosAnnotation in
    let col = posAnnotation.pos_cnum - posAnnotation.pos_bol in
    let originalLen = String.length original in
    {
      original =
        (if String.length original >= col && col > 0 then
           let original1, original2 =
             try
               ( String.sub original 0 col,
                 String.sub original col (originalLen - col) )
             with Invalid_argument _ -> (original, "")
           in
           if language = Res && declKind = VariantCase then
             if
               String.length original2 >= 2
               && (String.sub [@doesNotRaise]) original2 0 2 = "| "
             then
               original1 ^ "| " ^ annotationStr
               ^ (String.sub [@doesNotRaise]) original2 2
                   (String.length original2 - 2)
             else if
               String.length original2 >= 1
               && (String.sub [@doesNotRaise]) original2 0 1 = "|"
             then
               original1 ^ "|" ^ annotationStr
               ^ (String.sub [@doesNotRaise]) original2 1
                   (String.length original2 - 1)
             else original1 ^ "| " ^ annotationStr ^ original2
           else original1 ^ annotationStr ^ original2
         else
           match language = Ml with
           | true -> original ^ annotationStr
           | false -> annotationStr ^ original);
      declarations = nextDeclarations;
    }
    |> lineToString_

let lineToString {original; declarations} =
  let declarations =
    declarations
    |> List.sort (fun decl1 decl2 ->
           (getPosAnnotation decl2).pos_cnum - (getPosAnnotation decl1).pos_cnum)
  in
  lineToString_ {original; declarations}

let currentFile = ref ""
let currentFileLines = (ref [||] : line array ref)

let readFile fileName =
  let channel = open_in fileName in
  let lines = ref [] in
  let rec loop () =
    let line = {original = input_line channel; declarations = []} in
    lines := line :: !lines;
    loop ()
      [@@raises End_of_file]
  in
  try loop ()
  with End_of_file ->
    close_in_noerr channel;
    !lines |> List.rev |> Array.of_list

let writeFile fileName lines =
  if fileName <> "" && !Cli.write then (
    let channel = open_out fileName in
    let lastLine = Array.length lines in
    lines
    |> Array.iteri (fun n line ->
           output_string channel (line |> lineToString);
           if n < lastLine - 1 then output_char channel '\n');
    close_out_noerr channel)

let offsetOfPosAdjustment = function
  | FirstVariant | Nothing -> 0
  | OtherVariant -> 2

let getLineAnnotation ~decl ~line =
  if !Cli.json then
    let posAnnotation = decl |> getPosAnnotation in
    let offset = decl.posAdjustment |> offsetOfPosAdjustment in
    EmitJson.emitAnnotate
      ~pos:
        ( posAnnotation.pos_lnum - 1,
          posAnnotation.pos_cnum - posAnnotation.pos_bol + offset )
      ~text:
        (if decl.posAdjustment = FirstVariant then
           (* avoid syntax error *)
           "| @dead "
         else "@dead ")
      ~action:"Suppress dead code warning"
  else
    Format.asprintf "@.  <-- line %d@.  %s" decl.pos.pos_lnum
      (line |> lineToString)

let cantFindLine () = if !Cli.json then "" else "\n  <-- Can't find line"

let lineAnnotationToString = function
  | None -> cantFindLine ()
  | Some (decl, line) -> getLineAnnotation ~decl ~line

let addLineAnnotation ~decl : lineAnnotation =
  let fileName = decl.pos.pos_fname in
  if Sys.file_exists fileName then (
    if fileName <> !currentFile then (
      writeFile !currentFile !currentFileLines;
      currentFile := fileName;
      currentFileLines := readFile fileName);
    let indexInLines = (decl |> getPosAnnotation).pos_lnum - 1 in
    match !currentFileLines.(indexInLines) with
    | line ->
      line.declarations <- decl :: line.declarations;
      Some (decl, line)
    | exception Invalid_argument _ -> None)
  else None

let write () = writeFile !currentFile !currentFileLines
