module Doc = Res_doc
module CommentTable = Res_comments_table
module Comment = Res_comment
module Token = Res_token
module Parens = Res_parens
module ParsetreeViewer = Res_parsetree_viewer

type callbackStyle =
  (* regular arrow function, example: `let f = x => x + 1` *)
  | NoCallback
  (* `Thing.map(foo, (arg1, arg2) => MyModuleBlah.toList(argument))` *)
  | FitsOnOneLine
  (* Thing.map(longArgumet, veryLooooongArgument, (arg1, arg2) =>
   *   MyModuleBlah.toList(argument)
   * )
   *)
  | ArgumentsFitOnOneLine

(* Since compiler version 8.3, the bs. prefix is no longer needed *)
(* Synced from
   https://github.com/rescript-lang/rescript-compiler/blob/29174de1a5fde3b16cf05d10f5ac109cfac5c4ca/jscomp/frontend/ast_external_process.ml#L291-L367 *)
let convertBsExternalAttribute = function
  | "bs.as" -> "as"
  | "bs.deriving" -> "deriving"
  | "bs.get" -> "get"
  | "bs.get_index" -> "get_index"
  | "bs.ignore" -> "ignore"
  | "bs.inline" -> "inline"
  | "bs.int" -> "int"
  | "bs.meth" -> "meth"
  | "bs.module" -> "module"
  | "bs.new" -> "new"
  | "bs.obj" -> "obj"
  | "bs.optional" -> "optional"
  | "bs.return" -> "return"
  | "bs.send" -> "send"
  | "bs.scope" -> "scope"
  | "bs.set" -> "set"
  | "bs.set_index" -> "set_index"
  | "bs.splice" | "bs.variadic" -> "variadic"
  | "bs.string" -> "string"
  | "bs.this" -> "this"
  | "bs.uncurry" -> "uncurry"
  | "bs.unwrap" -> "unwrap"
  | "bs.val" -> "val"
  (* bs.send.pipe shouldn't be transformed *)
  | txt -> txt

(* These haven't been needed for a long time now *)
(* Synced from
   https://github.com/rescript-lang/rescript-compiler/blob/29174de1a5fde3b16cf05d10f5ac109cfac5c4ca/jscomp/frontend/ast_exp_extension.ml *)
let convertBsExtension = function
  | "bs.debugger" -> "debugger"
  | "bs.external" -> "raw"
  (* We should never see this one since we use the sugared object form, but still *)
  | "bs.obj" -> "obj"
  | "bs.raw" -> "raw"
  | "bs.re" -> "re"
  (* TODO: what about bs.time and bs.node? *)
  | txt -> txt

let addParens doc =
  Doc.group
    (Doc.concat
       [
         Doc.lparen;
         Doc.indent (Doc.concat [Doc.softLine; doc]);
         Doc.softLine;
         Doc.rparen;
       ])

let addBraces doc =
  Doc.group
    (Doc.concat
       [
         Doc.lbrace;
         Doc.indent (Doc.concat [Doc.softLine; doc]);
         Doc.softLine;
         Doc.rbrace;
       ])

let addAsync doc = Doc.concat [Doc.text "async "; doc]

let getFirstLeadingComment tbl loc =
  match Hashtbl.find tbl.CommentTable.leading loc with
  | comment :: _ -> Some comment
  | [] -> None
  | exception Not_found -> None

(* Checks if `loc` has a leading line comment, i.e. `// comment above`*)
let hasLeadingLineComment tbl loc =
  match getFirstLeadingComment tbl loc with
  | Some comment -> Comment.isSingleLineComment comment
  | None -> false

let hasCommentBelow tbl loc =
  match Hashtbl.find tbl.CommentTable.trailing loc with
  | comment :: _ ->
    let commentLoc = Comment.loc comment in
    commentLoc.Location.loc_start.pos_lnum > loc.Location.loc_end.pos_lnum
  | [] -> false
  | exception Not_found -> false

let hasNestedJsxOrMoreThanOneChild expr =
  let rec loop inRecursion expr =
    match expr.Parsetree.pexp_desc with
    | Pexp_construct
        ({txt = Longident.Lident "::"}, Some {pexp_desc = Pexp_tuple [hd; tail]})
      ->
      if inRecursion || ParsetreeViewer.isJsxExpression hd then true
      else loop true tail
    | _ -> false
  in
  loop false expr

let hasCommentsInside tbl loc =
  match Hashtbl.find_opt tbl.CommentTable.inside loc with
  | None -> false
  | _ -> true

let hasTrailingComments tbl loc =
  match Hashtbl.find_opt tbl.CommentTable.trailing loc with
  | None -> false
  | _ -> true

let printMultilineCommentContent txt =
  (* Turns
   *         |* first line
   *  * second line
   *      * third line *|
   * Into
   * |* first line
   *  * second line
   *  * third line *|
   *
   * What makes a comment suitable for this kind of indentation?
   *  ->  multiple lines + every line starts with a star
   *)
  let rec indentStars lines acc =
    match lines with
    | [] -> Doc.nil
    | [lastLine] ->
      let line = String.trim lastLine in
      let doc = Doc.text (" " ^ line) in
      let trailingSpace = if line = "" then Doc.nil else Doc.space in
      List.rev (trailingSpace :: doc :: acc) |> Doc.concat
    | line :: lines ->
      let line = String.trim line in
      if line != "" && String.unsafe_get line 0 == '*' then
        let doc = Doc.text (" " ^ line) in
        indentStars lines (Doc.hardLine :: doc :: acc)
      else
        let trailingSpace =
          let len = String.length txt in
          if len > 0 && String.unsafe_get txt (len - 1) = ' ' then Doc.space
          else Doc.nil
        in
        let content = Comment.trimSpaces txt in
        Doc.concat [Doc.text content; trailingSpace]
  in
  let lines = String.split_on_char '\n' txt in
  match lines with
  | [] -> Doc.text "/* */"
  | [line] ->
    Doc.concat
      [Doc.text "/* "; Doc.text (Comment.trimSpaces line); Doc.text " */"]
  | first :: rest ->
    let firstLine = Comment.trimSpaces first in
    Doc.concat
      [
        Doc.text "/*";
        (match firstLine with
        | "" | "*" -> Doc.nil
        | _ -> Doc.space);
        indentStars rest [Doc.hardLine; Doc.text firstLine];
        Doc.text "*/";
      ]

let printTrailingComment (prevLoc : Location.t) (nodeLoc : Location.t) comment =
  let singleLine = Comment.isSingleLineComment comment in
  let content =
    let txt = Comment.txt comment in
    if singleLine then Doc.text ("//" ^ txt)
    else printMultilineCommentContent txt
  in
  let diff =
    let cmtStart = (Comment.loc comment).loc_start in
    cmtStart.pos_lnum - prevLoc.loc_end.pos_lnum
  in
  let isBelow =
    (Comment.loc comment).loc_start.pos_lnum > nodeLoc.loc_end.pos_lnum
  in
  if diff > 0 || isBelow then
    Doc.concat
      [
        Doc.breakParent;
        Doc.lineSuffix
          (Doc.concat
             [
               Doc.hardLine;
               (if diff > 1 then Doc.hardLine else Doc.nil);
               content;
             ]);
      ]
  else if not singleLine then Doc.concat [Doc.space; content]
  else Doc.lineSuffix (Doc.concat [Doc.space; content])

let printLeadingComment ?nextComment comment =
  let singleLine = Comment.isSingleLineComment comment in
  let content =
    let txt = Comment.txt comment in
    if singleLine then Doc.text ("//" ^ txt)
    else printMultilineCommentContent txt
  in
  let separator =
    Doc.concat
      [
        (if singleLine then Doc.concat [Doc.hardLine; Doc.breakParent]
         else Doc.nil);
        (match nextComment with
        | Some next ->
          let nextLoc = Comment.loc next in
          let currLoc = Comment.loc comment in
          let diff =
            nextLoc.Location.loc_start.pos_lnum
            - currLoc.Location.loc_end.pos_lnum
          in
          let nextSingleLine = Comment.isSingleLineComment next in
          if singleLine && nextSingleLine then
            if diff > 1 then Doc.hardLine else Doc.nil
          else if singleLine && not nextSingleLine then
            if diff > 1 then Doc.hardLine else Doc.nil
          else if diff > 1 then Doc.concat [Doc.hardLine; Doc.hardLine]
          else if diff == 1 then Doc.hardLine
          else Doc.space
        | None -> Doc.nil);
      ]
  in
  Doc.concat [content; separator]

(* This function is used for printing comments inside an empty block *)
let printCommentsInside cmtTbl loc =
  let printComment comment =
    let singleLine = Comment.isSingleLineComment comment in
    let txt = Comment.txt comment in
    if singleLine then Doc.text ("//" ^ txt)
    else printMultilineCommentContent txt
  in
  let forceBreak =
    loc.Location.loc_start.pos_lnum <> loc.Location.loc_end.pos_lnum
  in
  let rec loop acc comments =
    match comments with
    | [] -> Doc.nil
    | [comment] ->
      let cmtDoc = printComment comment in
      let cmtsDoc = Doc.concat (Doc.softLine :: List.rev (cmtDoc :: acc)) in
      let doc =
        Doc.breakableGroup ~forceBreak
          (Doc.concat [Doc.ifBreaks (Doc.indent cmtsDoc) cmtsDoc; Doc.softLine])
      in
      doc
    | comment :: rest ->
      let cmtDoc = Doc.concat [printComment comment; Doc.line] in
      loop (cmtDoc :: acc) rest
  in
  match Hashtbl.find cmtTbl.CommentTable.inside loc with
  | exception Not_found -> Doc.nil
  | comments ->
    Hashtbl.remove cmtTbl.inside loc;
    loop [] comments

(* This function is used for printing comments inside an empty file *)
let printCommentsInsideFile cmtTbl =
  let rec loop acc comments =
    match comments with
    | [] -> Doc.nil
    | [comment] ->
      let cmtDoc = printLeadingComment comment in
      let doc =
        Doc.group (Doc.concat [Doc.concat (List.rev (cmtDoc :: acc))])
      in
      doc
    | comment :: (nextComment :: _comments as rest) ->
      let cmtDoc = printLeadingComment ~nextComment comment in
      loop (cmtDoc :: acc) rest
  in
  match Hashtbl.find cmtTbl.CommentTable.inside Location.none with
  | exception Not_found -> Doc.nil
  | comments ->
    Hashtbl.remove cmtTbl.inside Location.none;
    Doc.group (loop [] comments)

let printLeadingComments node tbl loc =
  let rec loop acc comments =
    match comments with
    | [] -> node
    | [comment] ->
      let cmtDoc = printLeadingComment comment in
      let diff =
        loc.Location.loc_start.pos_lnum
        - (Comment.loc comment).Location.loc_end.pos_lnum
      in
      let separator =
        if Comment.isSingleLineComment comment then
          if diff > 1 then Doc.hardLine else Doc.nil
        else if diff == 0 then Doc.space
        else if diff > 1 then Doc.concat [Doc.hardLine; Doc.hardLine]
        else Doc.hardLine
      in
      let doc =
        Doc.group
          (Doc.concat [Doc.concat (List.rev (cmtDoc :: acc)); separator; node])
      in
      doc
    | comment :: (nextComment :: _comments as rest) ->
      let cmtDoc = printLeadingComment ~nextComment comment in
      loop (cmtDoc :: acc) rest
  in
  match Hashtbl.find tbl loc with
  | exception Not_found -> node
  | comments ->
    (* Remove comments from tbl: Some ast nodes have the same location.
     * We only want to print comments once *)
    Hashtbl.remove tbl loc;
    loop [] comments

let printTrailingComments node tbl loc =
  let rec loop prev acc comments =
    match comments with
    | [] -> Doc.concat (List.rev acc)
    | comment :: comments ->
      let cmtDoc = printTrailingComment prev loc comment in
      loop (Comment.loc comment) (cmtDoc :: acc) comments
  in
  match Hashtbl.find tbl loc with
  | exception Not_found -> node
  | [] -> node
  | _first :: _ as comments ->
    (* Remove comments from tbl: Some ast nodes have the same location.
     * We only want to print comments once *)
    Hashtbl.remove tbl loc;
    let cmtsDoc = loop loc [] comments in
    Doc.concat [node; cmtsDoc]

let printComments doc (tbl : CommentTable.t) loc =
  let docWithLeadingComments = printLeadingComments doc tbl.leading loc in
  printTrailingComments docWithLeadingComments tbl.trailing loc

let printList ~getLoc ~nodes ~print ?(forceBreak = false) t =
  let rec loop (prevLoc : Location.t) acc nodes =
    match nodes with
    | [] -> (prevLoc, Doc.concat (List.rev acc))
    | node :: nodes ->
      let loc = getLoc node in
      let startPos =
        match getFirstLeadingComment t loc with
        | None -> loc.loc_start
        | Some comment -> (Comment.loc comment).loc_start
      in
      let sep =
        if startPos.pos_lnum - prevLoc.loc_end.pos_lnum > 1 then
          Doc.concat [Doc.hardLine; Doc.hardLine]
        else Doc.hardLine
      in
      let doc = printComments (print node t) t loc in
      loop loc (doc :: sep :: acc) nodes
  in
  match nodes with
  | [] -> Doc.nil
  | node :: nodes ->
    let firstLoc = getLoc node in
    let doc = printComments (print node t) t firstLoc in
    let lastLoc, docs = loop firstLoc [doc] nodes in
    let forceBreak =
      forceBreak || firstLoc.loc_start.pos_lnum != lastLoc.loc_end.pos_lnum
    in
    Doc.breakableGroup ~forceBreak docs

let printListi ~getLoc ~nodes ~print ?(forceBreak = false) t =
  let rec loop i (prevLoc : Location.t) acc nodes =
    match nodes with
    | [] -> (prevLoc, Doc.concat (List.rev acc))
    | node :: nodes ->
      let loc = getLoc node in
      let startPos =
        match getFirstLeadingComment t loc with
        | None -> loc.loc_start
        | Some comment -> (Comment.loc comment).loc_start
      in
      let sep =
        if startPos.pos_lnum - prevLoc.loc_end.pos_lnum > 1 then
          Doc.concat [Doc.hardLine; Doc.hardLine]
        else Doc.line
      in
      let doc = printComments (print node t i) t loc in
      loop (i + 1) loc (doc :: sep :: acc) nodes
  in
  match nodes with
  | [] -> Doc.nil
  | node :: nodes ->
    let firstLoc = getLoc node in
    let doc = printComments (print node t 0) t firstLoc in
    let lastLoc, docs = loop 1 firstLoc [doc] nodes in
    let forceBreak =
      forceBreak || firstLoc.loc_start.pos_lnum != lastLoc.loc_end.pos_lnum
    in
    Doc.breakableGroup ~forceBreak docs

let rec printLongidentAux accu = function
  | Longident.Lident s -> Doc.text s :: accu
  | Ldot (lid, s) -> printLongidentAux (Doc.text s :: accu) lid
  | Lapply (lid1, lid2) ->
    let d1 = Doc.join ~sep:Doc.dot (printLongidentAux [] lid1) in
    let d2 = Doc.join ~sep:Doc.dot (printLongidentAux [] lid2) in
    Doc.concat [d1; Doc.lparen; d2; Doc.rparen] :: accu

let printLongident = function
  | Longident.Lident txt -> Doc.text txt
  | lid -> Doc.join ~sep:Doc.dot (printLongidentAux [] lid)

type identifierStyle = ExoticIdent | NormalIdent

let classifyIdentContent ?(allowUident = false) ?(allowHyphen = false) txt =
  if Token.isKeywordTxt txt then ExoticIdent
  else
    let len = String.length txt in
    let rec loop i =
      if i == len then NormalIdent
      else if i == 0 then
        match String.unsafe_get txt i with
        | 'A' .. 'Z' when allowUident -> loop (i + 1)
        | 'a' .. 'z' | '_' -> loop (i + 1)
        | '-' when allowHyphen -> loop (i + 1)
        | _ -> ExoticIdent
      else
        match String.unsafe_get txt i with
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '\'' | '_' -> loop (i + 1)
        | '-' when allowHyphen -> loop (i + 1)
        | _ -> ExoticIdent
    in
    loop 0

let printIdentLike ?allowUident ?allowHyphen txt =
  match classifyIdentContent ?allowUident ?allowHyphen txt with
  | ExoticIdent -> Doc.concat [Doc.text "\\\""; Doc.text txt; Doc.text "\""]
  | NormalIdent -> Doc.text txt

let rec unsafe_for_all_range s ~start ~finish p =
  start > finish
  || p (String.unsafe_get s start)
     && unsafe_for_all_range s ~start:(start + 1) ~finish p

let for_all_from s start p =
  let len = String.length s in
  unsafe_for_all_range s ~start ~finish:(len - 1) p

(* See https://github.com/rescript-lang/rescript-compiler/blob/726cfa534314b586e5b5734471bc2023ad99ebd9/jscomp/ext/ext_string.ml#L510 *)
let isValidNumericPolyvarNumber (x : string) =
  let len = String.length x in
  len > 0
  &&
  let a = Char.code (String.unsafe_get x 0) in
  a <= 57
  &&
  if len > 1 then
    a > 48
    && for_all_from x 1 (function
         | '0' .. '9' -> true
         | _ -> false)
  else a >= 48

(* Exotic identifiers in poly-vars have a "lighter" syntax: #"ease-in" *)
let printPolyVarIdent txt =
  (* numeric poly-vars don't need quotes: #644 *)
  if isValidNumericPolyvarNumber txt then Doc.text txt
  else
    match classifyIdentContent ~allowUident:true txt with
    | ExoticIdent -> Doc.concat [Doc.text "\""; Doc.text txt; Doc.text "\""]
    | NormalIdent -> (
      match txt with
      | "" -> Doc.concat [Doc.text "\""; Doc.text txt; Doc.text "\""]
      | _ -> Doc.text txt)

let polyVarIdentToString polyVarIdent =
  Doc.concat [Doc.text "#"; printPolyVarIdent polyVarIdent]
  |> Doc.toString ~width:80

let printLident l =
  let flatLidOpt lid =
    let rec flat accu = function
      | Longident.Lident s -> Some (s :: accu)
      | Ldot (lid, s) -> flat (s :: accu) lid
      | Lapply (_, _) -> None
    in
    flat [] lid
  in
  match l with
  | Longident.Lident txt -> printIdentLike txt
  | Longident.Ldot (path, txt) ->
    let doc =
      match flatLidOpt path with
      | Some txts ->
        Doc.concat
          [
            Doc.join ~sep:Doc.dot (List.map Doc.text txts);
            Doc.dot;
            printIdentLike txt;
          ]
      | None -> Doc.text "printLident: Longident.Lapply is not supported"
    in
    doc
  | Lapply (_, _) -> Doc.text "printLident: Longident.Lapply is not supported"

let printLongidentLocation l cmtTbl =
  let doc = printLongident l.Location.txt in
  printComments doc cmtTbl l.loc

(* Module.SubModule.x *)
let printLidentPath path cmtTbl =
  let doc = printLident path.Location.txt in
  printComments doc cmtTbl path.loc

(* Module.SubModule.x or Module.SubModule.X *)
let printIdentPath path cmtTbl =
  let doc = printLident path.Location.txt in
  printComments doc cmtTbl path.loc

let printStringLoc sloc cmtTbl =
  let doc = printIdentLike sloc.Location.txt in
  printComments doc cmtTbl sloc.loc

let printStringContents txt =
  let lines = String.split_on_char '\n' txt in
  Doc.join ~sep:Doc.literalLine (List.map Doc.text lines)

let printConstant ?(templateLiteral = false) c =
  match c with
  | Parsetree.Pconst_integer (s, suffix) -> (
    match suffix with
    | Some c -> Doc.text (s ^ Char.escaped c)
    | None -> Doc.text s)
  | Pconst_string (txt, None) ->
    Doc.concat [Doc.text "\""; printStringContents txt; Doc.text "\""]
  | Pconst_string (txt, Some prefix) ->
    if prefix = "INTERNAL_RES_CHAR_CONTENTS" then
      Doc.concat [Doc.text "'"; Doc.text txt; Doc.text "'"]
    else
      let lquote, rquote =
        if templateLiteral then ("`", "`") else ("\"", "\"")
      in
      Doc.concat
        [
          (if prefix = "js" then Doc.nil else Doc.text prefix);
          Doc.text lquote;
          printStringContents txt;
          Doc.text rquote;
        ]
  | Pconst_float (s, _) -> Doc.text s
  | Pconst_char c ->
    let str =
      match Char.unsafe_chr c with
      | '\'' -> "\\'"
      | '\\' -> "\\\\"
      | '\n' -> "\\n"
      | '\t' -> "\\t"
      | '\r' -> "\\r"
      | '\b' -> "\\b"
      | ' ' .. '~' as c ->
        let s = (Bytes.create [@doesNotRaise]) 1 in
        Bytes.unsafe_set s 0 c;
        Bytes.unsafe_to_string s
      | _ -> Res_utf8.encodeCodePoint c
    in
    Doc.text ("'" ^ str ^ "'")

let printOptionalLabel attrs =
  if Res_parsetree_viewer.hasOptionalAttribute attrs then Doc.text "?"
  else Doc.nil

module State = struct
  let customLayoutThreshold = 2

  type t = {customLayout: int; mutable uncurried_config: Config.uncurried}

  let init () = {customLayout = 0; uncurried_config = !Config.uncurried}

  let nextCustomLayout t = {t with customLayout = t.customLayout + 1}

  let shouldBreakCallback t = t.customLayout > customLayoutThreshold
end

let rec printStructure ~state (s : Parsetree.structure) t =
  match s with
  | [] -> printCommentsInsideFile t
  | structure ->
    printList
      ~getLoc:(fun s -> s.Parsetree.pstr_loc)
      ~nodes:structure
      ~print:(printStructureItem ~state)
      t

and printStructureItem ~state (si : Parsetree.structure_item) cmtTbl =
  match si.pstr_desc with
  | Pstr_value (rec_flag, valueBindings) ->
    let recFlag =
      match rec_flag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
    in
    printValueBindings ~state ~recFlag valueBindings cmtTbl
  | Pstr_type (recFlag, typeDeclarations) ->
    let recFlag =
      match recFlag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
    in
    printTypeDeclarations ~state ~recFlag typeDeclarations cmtTbl
  | Pstr_primitive valueDescription ->
    printValueDescription ~state valueDescription cmtTbl
  | Pstr_eval (expr, attrs) ->
    let exprDoc =
      let doc = printExpressionWithComments ~state expr cmtTbl in
      match Parens.structureExpr expr with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc expr braces
      | Nothing -> doc
    in
    Doc.concat [printAttributes ~state attrs cmtTbl; exprDoc]
  | Pstr_attribute attr ->
    fst (printAttribute ~state ~standalone:true attr cmtTbl)
  | Pstr_extension (extension, attrs) ->
    Doc.concat
      [
        printAttributes ~state attrs cmtTbl;
        Doc.concat [printExtension ~state ~atModuleLvl:true extension cmtTbl];
      ]
  | Pstr_include includeDeclaration ->
    printIncludeDeclaration ~state includeDeclaration cmtTbl
  | Pstr_open openDescription ->
    printOpenDescription ~state openDescription cmtTbl
  | Pstr_modtype modTypeDecl ->
    printModuleTypeDeclaration ~state modTypeDecl cmtTbl
  | Pstr_module moduleBinding ->
    printModuleBinding ~state ~isRec:false moduleBinding cmtTbl 0
  | Pstr_recmodule moduleBindings ->
    printListi
      ~getLoc:(fun mb -> mb.Parsetree.pmb_loc)
      ~nodes:moduleBindings
      ~print:(printModuleBinding ~state ~isRec:true)
      cmtTbl
  | Pstr_exception extensionConstructor ->
    printExceptionDef ~state extensionConstructor cmtTbl
  | Pstr_typext typeExtension -> printTypeExtension ~state typeExtension cmtTbl
  | Pstr_class _ | Pstr_class_type _ -> Doc.nil

and printTypeExtension ~state (te : Parsetree.type_extension) cmtTbl =
  let prefix = Doc.text "type " in
  let name = printLidentPath te.ptyext_path cmtTbl in
  let typeParams = printTypeParams ~state te.ptyext_params cmtTbl in
  let extensionConstructors =
    let ecs = te.ptyext_constructors in
    let forceBreak =
      match (ecs, List.rev ecs) with
      | first :: _, last :: _ ->
        first.pext_loc.loc_start.pos_lnum > te.ptyext_path.loc.loc_end.pos_lnum
        || first.pext_loc.loc_start.pos_lnum < last.pext_loc.loc_end.pos_lnum
      | _ -> false
    in
    let privateFlag =
      match te.ptyext_private with
      | Asttypes.Private -> Doc.concat [Doc.text "private"; Doc.line]
      | Public -> Doc.nil
    in
    let rows =
      printListi
        ~getLoc:(fun n -> n.Parsetree.pext_loc)
        ~print:(printExtensionConstructor ~state)
        ~nodes:ecs ~forceBreak cmtTbl
    in
    Doc.breakableGroup ~forceBreak
      (Doc.indent
         (Doc.concat
            [
              Doc.line;
              privateFlag;
              rows;
              (* Doc.join ~sep:Doc.line ( *)
              (* List.mapi printExtensionConstructor ecs *)
              (* ) *)
            ]))
  in
  Doc.group
    (Doc.concat
       [
         printAttributes ~state ~loc:te.ptyext_path.loc te.ptyext_attributes
           cmtTbl;
         prefix;
         name;
         typeParams;
         Doc.text " +=";
         extensionConstructors;
       ])

and printModuleBinding ~state ~isRec moduleBinding cmtTbl i =
  let prefix =
    if i = 0 then
      Doc.concat
        [Doc.text "module "; (if isRec then Doc.text "rec " else Doc.nil)]
    else Doc.text "and "
  in
  let modExprDoc, modConstraintDoc =
    match moduleBinding.pmb_expr with
    | {pmod_desc = Pmod_constraint (modExpr, modType)}
      when not
             (ParsetreeViewer.hasAwaitAttribute
                moduleBinding.pmb_expr.pmod_attributes) ->
      ( printModExpr ~state modExpr cmtTbl,
        Doc.concat [Doc.text ": "; printModType ~state modType cmtTbl] )
    | modExpr -> (printModExpr ~state modExpr cmtTbl, Doc.nil)
  in
  let modExprDocParens =
    if Parens.modExprParens moduleBinding.pmb_expr then
      Doc.concat [Doc.lparen; modExprDoc; Doc.rparen]
    else modExprDoc
  in
  let modName =
    let doc = Doc.text moduleBinding.pmb_name.Location.txt in
    printComments doc cmtTbl moduleBinding.pmb_name.loc
  in
  let doc =
    Doc.concat
      [
        printAttributes ~state ~loc:moduleBinding.pmb_name.loc
          moduleBinding.pmb_attributes cmtTbl;
        prefix;
        modName;
        modConstraintDoc;
        Doc.text " = ";
        modExprDocParens;
      ]
  in
  printComments doc cmtTbl moduleBinding.pmb_loc

and printModuleTypeDeclaration ~state
    (modTypeDecl : Parsetree.module_type_declaration) cmtTbl =
  let modName =
    let doc = Doc.text modTypeDecl.pmtd_name.txt in
    printComments doc cmtTbl modTypeDecl.pmtd_name.loc
  in
  Doc.concat
    [
      printAttributes ~state modTypeDecl.pmtd_attributes cmtTbl;
      Doc.text "module type ";
      modName;
      (match modTypeDecl.pmtd_type with
      | None -> Doc.nil
      | Some modType ->
        Doc.concat [Doc.text " = "; printModType ~state modType cmtTbl]);
    ]

and printModType ~state modType cmtTbl =
  let modTypeDoc =
    match modType.pmty_desc with
    | Parsetree.Pmty_ident longident ->
      Doc.concat
        [
          printAttributes ~state ~loc:longident.loc modType.pmty_attributes
            cmtTbl;
          printLongidentLocation longident cmtTbl;
        ]
    | Pmty_signature [] ->
      if hasCommentsInside cmtTbl modType.pmty_loc then
        let doc = printCommentsInside cmtTbl modType.pmty_loc in
        Doc.concat [Doc.lbrace; doc; Doc.rbrace]
      else
        let shouldBreak =
          modType.pmty_loc.loc_start.pos_lnum
          < modType.pmty_loc.loc_end.pos_lnum
        in
        Doc.breakableGroup ~forceBreak:shouldBreak
          (Doc.concat [Doc.lbrace; Doc.softLine; Doc.softLine; Doc.rbrace])
    | Pmty_signature signature ->
      let signatureDoc =
        Doc.breakableGroup ~forceBreak:true
          (Doc.concat
             [
               Doc.lbrace;
               Doc.indent
                 (Doc.concat [Doc.line; printSignature ~state signature cmtTbl]);
               Doc.line;
               Doc.rbrace;
             ])
      in
      Doc.concat
        [printAttributes ~state modType.pmty_attributes cmtTbl; signatureDoc]
    | Pmty_functor _ ->
      let parameters, returnType = ParsetreeViewer.functorType modType in
      let parametersDoc =
        match parameters with
        | [] -> Doc.nil
        | [(attrs, {Location.txt = "_"; loc}, Some modType)] ->
          let cmtLoc =
            {loc with loc_end = modType.Parsetree.pmty_loc.loc_end}
          in
          let attrs = printAttributes ~state attrs cmtTbl in
          let doc = Doc.concat [attrs; printModType ~state modType cmtTbl] in
          printComments doc cmtTbl cmtLoc
        | params ->
          Doc.group
            (Doc.concat
               [
                 Doc.lparen;
                 Doc.indent
                   (Doc.concat
                      [
                        Doc.softLine;
                        Doc.join
                          ~sep:(Doc.concat [Doc.comma; Doc.line])
                          (List.map
                             (fun (attrs, lbl, modType) ->
                               let cmtLoc =
                                 match modType with
                                 | None -> lbl.Asttypes.loc
                                 | Some modType ->
                                   {
                                     lbl.Asttypes.loc with
                                     loc_end =
                                       modType.Parsetree.pmty_loc.loc_end;
                                   }
                               in
                               let attrs =
                                 printAttributes ~state attrs cmtTbl
                               in
                               let lblDoc =
                                 if lbl.Location.txt = "_" || lbl.txt = "*" then
                                   Doc.nil
                                 else
                                   let doc = Doc.text lbl.txt in
                                   printComments doc cmtTbl lbl.loc
                               in
                               let doc =
                                 Doc.concat
                                   [
                                     attrs;
                                     lblDoc;
                                     (match modType with
                                     | None -> Doc.nil
                                     | Some modType ->
                                       Doc.concat
                                         [
                                           (if lbl.txt = "_" then Doc.nil
                                            else Doc.text ": ");
                                           printModType ~state modType cmtTbl;
                                         ]);
                                   ]
                               in
                               printComments doc cmtTbl cmtLoc)
                             params);
                      ]);
                 Doc.trailingComma;
                 Doc.softLine;
                 Doc.rparen;
               ])
      in
      let returnDoc =
        let doc = printModType ~state returnType cmtTbl in
        if Parens.modTypeFunctorReturn returnType then addParens doc else doc
      in
      Doc.group
        (Doc.concat
           [
             parametersDoc;
             Doc.group (Doc.concat [Doc.text " =>"; Doc.line; returnDoc]);
           ])
    | Pmty_typeof modExpr ->
      Doc.concat
        [Doc.text "module type of "; printModExpr ~state modExpr cmtTbl]
    | Pmty_extension extension ->
      printExtension ~state ~atModuleLvl:false extension cmtTbl
    | Pmty_alias longident ->
      Doc.concat [Doc.text "module "; printLongidentLocation longident cmtTbl]
    | Pmty_with (modType, withConstraints) ->
      let operand =
        let doc = printModType ~state modType cmtTbl in
        if Parens.modTypeWithOperand modType then addParens doc else doc
      in
      Doc.group
        (Doc.concat
           [
             operand;
             Doc.indent
               (Doc.concat
                  [Doc.line; printWithConstraints ~state withConstraints cmtTbl]);
           ])
  in
  let attrsAlreadyPrinted =
    match modType.pmty_desc with
    | Pmty_functor _ | Pmty_signature _ | Pmty_ident _ -> true
    | _ -> false
  in
  let doc =
    Doc.concat
      [
        (if attrsAlreadyPrinted then Doc.nil
         else printAttributes ~state modType.pmty_attributes cmtTbl);
        modTypeDoc;
      ]
  in
  printComments doc cmtTbl modType.pmty_loc

and printWithConstraints ~state withConstraints cmtTbl =
  let rows =
    List.mapi
      (fun i withConstraint ->
        Doc.group
          (Doc.concat
             [
               (if i == 0 then Doc.text "with " else Doc.text "and ");
               printWithConstraint ~state withConstraint cmtTbl;
             ]))
      withConstraints
  in
  Doc.join ~sep:Doc.line rows

and printWithConstraint ~state (withConstraint : Parsetree.with_constraint)
    cmtTbl =
  match withConstraint with
  (* with type X.t = ... *)
  | Pwith_type (longident, typeDeclaration) ->
    Doc.group
      (printTypeDeclaration ~state
         ~name:(printLidentPath longident cmtTbl)
         ~equalSign:"=" ~recFlag:Doc.nil 0 typeDeclaration CommentTable.empty)
  (* with module X.Y = Z *)
  | Pwith_module ({txt = longident1}, {txt = longident2}) ->
    Doc.concat
      [
        Doc.text "module ";
        printLongident longident1;
        Doc.text " =";
        Doc.indent (Doc.concat [Doc.line; printLongident longident2]);
      ]
  (* with type X.t := ..., same format as [Pwith_type] *)
  | Pwith_typesubst (longident, typeDeclaration) ->
    Doc.group
      (printTypeDeclaration ~state
         ~name:(printLidentPath longident cmtTbl)
         ~equalSign:":=" ~recFlag:Doc.nil 0 typeDeclaration CommentTable.empty)
  | Pwith_modsubst ({txt = longident1}, {txt = longident2}) ->
    Doc.concat
      [
        Doc.text "module ";
        printLongident longident1;
        Doc.text " :=";
        Doc.indent (Doc.concat [Doc.line; printLongident longident2]);
      ]

and printSignature ~state signature cmtTbl =
  match signature with
  | [] -> printCommentsInsideFile cmtTbl
  | signature ->
    printList
      ~getLoc:(fun s -> s.Parsetree.psig_loc)
      ~nodes:signature
      ~print:(printSignatureItem ~state)
      cmtTbl

and printSignatureItem ~state (si : Parsetree.signature_item) cmtTbl =
  match si.psig_desc with
  | Parsetree.Psig_value valueDescription ->
    printValueDescription ~state valueDescription cmtTbl
  | Psig_type (recFlag, typeDeclarations) ->
    let recFlag =
      match recFlag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
    in
    printTypeDeclarations ~state ~recFlag typeDeclarations cmtTbl
  | Psig_typext typeExtension -> printTypeExtension ~state typeExtension cmtTbl
  | Psig_exception extensionConstructor ->
    printExceptionDef ~state extensionConstructor cmtTbl
  | Psig_module moduleDeclaration ->
    printModuleDeclaration ~state moduleDeclaration cmtTbl
  | Psig_recmodule moduleDeclarations ->
    printRecModuleDeclarations ~state moduleDeclarations cmtTbl
  | Psig_modtype modTypeDecl ->
    printModuleTypeDeclaration ~state modTypeDecl cmtTbl
  | Psig_open openDescription ->
    printOpenDescription ~state openDescription cmtTbl
  | Psig_include includeDescription ->
    printIncludeDescription ~state includeDescription cmtTbl
  | Psig_attribute attr ->
    fst (printAttribute ~state ~standalone:true attr cmtTbl)
  | Psig_extension (extension, attrs) ->
    Doc.concat
      [
        printAttributes ~state attrs cmtTbl;
        Doc.concat [printExtension ~state ~atModuleLvl:true extension cmtTbl];
      ]
  | Psig_class _ | Psig_class_type _ -> Doc.nil

and printRecModuleDeclarations ~state moduleDeclarations cmtTbl =
  printListi
    ~getLoc:(fun n -> n.Parsetree.pmd_loc)
    ~nodes:moduleDeclarations
    ~print:(printRecModuleDeclaration ~state)
    cmtTbl

and printRecModuleDeclaration ~state md cmtTbl i =
  let body =
    match md.pmd_type.pmty_desc with
    | Parsetree.Pmty_alias longident ->
      Doc.concat [Doc.text " = "; printLongidentLocation longident cmtTbl]
    | _ ->
      let needsParens =
        match md.pmd_type.pmty_desc with
        | Pmty_with _ -> true
        | _ -> false
      in
      let modTypeDoc =
        let doc = printModType ~state md.pmd_type cmtTbl in
        if needsParens then addParens doc else doc
      in
      Doc.concat [Doc.text ": "; modTypeDoc]
  in
  let prefix = if i < 1 then "module rec " else "and " in
  Doc.concat
    [
      printAttributes ~state ~loc:md.pmd_name.loc md.pmd_attributes cmtTbl;
      Doc.text prefix;
      printComments (Doc.text md.pmd_name.txt) cmtTbl md.pmd_name.loc;
      body;
    ]

and printModuleDeclaration ~state (md : Parsetree.module_declaration) cmtTbl =
  let body =
    match md.pmd_type.pmty_desc with
    | Parsetree.Pmty_alias longident ->
      Doc.concat [Doc.text " = "; printLongidentLocation longident cmtTbl]
    | _ -> Doc.concat [Doc.text ": "; printModType ~state md.pmd_type cmtTbl]
  in
  Doc.concat
    [
      printAttributes ~state ~loc:md.pmd_name.loc md.pmd_attributes cmtTbl;
      Doc.text "module ";
      printComments (Doc.text md.pmd_name.txt) cmtTbl md.pmd_name.loc;
      body;
    ]

and printOpenDescription ~state (openDescription : Parsetree.open_description)
    cmtTbl =
  Doc.concat
    [
      printAttributes ~state openDescription.popen_attributes cmtTbl;
      Doc.text "open";
      (match openDescription.popen_override with
      | Asttypes.Fresh -> Doc.space
      | Asttypes.Override -> Doc.text "! ");
      printLongidentLocation openDescription.popen_lid cmtTbl;
    ]

and printIncludeDescription ~state
    (includeDescription : Parsetree.include_description) cmtTbl =
  Doc.concat
    [
      printAttributes ~state includeDescription.pincl_attributes cmtTbl;
      Doc.text "include ";
      printModType ~state includeDescription.pincl_mod cmtTbl;
    ]

and printIncludeDeclaration ~state
    (includeDeclaration : Parsetree.include_declaration) cmtTbl =
  Doc.concat
    [
      printAttributes ~state includeDeclaration.pincl_attributes cmtTbl;
      Doc.text "include ";
      (let includeDoc =
         printModExpr ~state includeDeclaration.pincl_mod cmtTbl
       in
       if Parens.includeModExpr includeDeclaration.pincl_mod then
         addParens includeDoc
       else includeDoc);
    ]

and printValueBindings ~state ~recFlag (vbs : Parsetree.value_binding list)
    cmtTbl =
  printListi
    ~getLoc:(fun vb -> vb.Parsetree.pvb_loc)
    ~nodes:vbs
    ~print:(printValueBinding ~state ~recFlag)
    cmtTbl

and printValueDescription ~state valueDescription cmtTbl =
  let isExternal =
    match valueDescription.pval_prim with
    | [] -> false
    | _ -> true
  in
  let attrs =
    printAttributes ~state ~loc:valueDescription.pval_name.loc
      valueDescription.pval_attributes cmtTbl
  in
  let header = if isExternal then "external " else "let " in
  Doc.group
    (Doc.concat
       [
         attrs;
         Doc.text header;
         printComments
           (printIdentLike valueDescription.pval_name.txt)
           cmtTbl valueDescription.pval_name.loc;
         Doc.text ": ";
         printTypExpr ~state valueDescription.pval_type cmtTbl;
         (if isExternal then
            Doc.group
              (Doc.concat
                 [
                   Doc.text " =";
                   Doc.indent
                     (Doc.concat
                        [
                          Doc.line;
                          Doc.join ~sep:Doc.line
                            (List.map
                               (fun s ->
                                 Doc.concat
                                   [Doc.text "\""; Doc.text s; Doc.text "\""])
                               valueDescription.pval_prim);
                        ]);
                 ])
          else Doc.nil);
       ])

and printTypeDeclarations ~state ~recFlag typeDeclarations cmtTbl =
  printListi
    ~getLoc:(fun n -> n.Parsetree.ptype_loc)
    ~nodes:typeDeclarations
    ~print:(printTypeDeclaration2 ~state ~recFlag)
    cmtTbl

(*
 * type_declaration = {
 *    ptype_name: string loc;
 *    ptype_params: (core_type * variance) list;
 *          (* ('a1,...'an) t; None represents  _*)
 *    ptype_cstrs: (core_type * core_type * Location.t) list;
 *          (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
 *    ptype_kind: type_kind;
 *    ptype_private: private_flag;   (* = private ... *)
 *    ptype_manifest: core_type option;  (* = T *)
 *    ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
 *    ptype_loc: Location.t;
 * }
 *
 *
 *  type t                     (abstract, no manifest)
 *  type t = T0                (abstract, manifest=T0)
 *  type t = C of T | ...      (variant,  no manifest)
 *  type t = T0 = C of T | ... (variant,  manifest=T0)
 *  type t = {l: T; ...}       (record,   no manifest)
 *  type t = T0 = {l : T; ...} (record,   manifest=T0)
 *  type t = ..                (open,     no manifest)
 *
 *
 * and type_kind =
 *  | Ptype_abstract
 *  | Ptype_variant of constructor_declaration list
 *        (* Invariant: non-empty list *)
 *  | Ptype_record of label_declaration list
 *        (* Invariant: non-empty list *)
 *  | Ptype_open
 *)
and printTypeDeclaration ~state ~name ~equalSign ~recFlag i
    (td : Parsetree.type_declaration) cmtTbl =
  let attrs =
    printAttributes ~state ~loc:td.ptype_loc td.ptype_attributes cmtTbl
  in
  let prefix =
    if i > 0 then Doc.text "and " else Doc.concat [Doc.text "type "; recFlag]
  in
  let typeName = name in
  let typeParams = printTypeParams ~state td.ptype_params cmtTbl in
  let manifestAndKind =
    match td.ptype_kind with
    | Ptype_abstract -> (
      match td.ptype_manifest with
      | None -> Doc.nil
      | Some typ ->
        Doc.concat
          [
            Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
            printPrivateFlag td.ptype_private;
            printTypExpr ~state typ cmtTbl;
          ])
    | Ptype_open ->
      Doc.concat
        [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printPrivateFlag td.ptype_private;
          Doc.text "..";
        ]
    | Ptype_record lds ->
      let manifest =
        match td.ptype_manifest with
        | None -> Doc.nil
        | Some typ ->
          Doc.concat
            [
              Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
              printTypExpr ~state typ cmtTbl;
            ]
      in
      Doc.concat
        [
          manifest;
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printPrivateFlag td.ptype_private;
          printRecordDeclaration ~state lds cmtTbl;
        ]
    | Ptype_variant cds ->
      let manifest =
        match td.ptype_manifest with
        | None -> Doc.nil
        | Some typ ->
          Doc.concat
            [
              Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
              printTypExpr ~state typ cmtTbl;
            ]
      in
      Doc.concat
        [
          manifest;
          Doc.concat [Doc.space; Doc.text equalSign];
          printConstructorDeclarations ~state ~privateFlag:td.ptype_private cds
            cmtTbl;
        ]
  in
  let constraints = printTypeDefinitionConstraints ~state td.ptype_cstrs in
  Doc.group
    (Doc.concat
       [attrs; prefix; typeName; typeParams; manifestAndKind; constraints])

and printTypeDeclaration2 ~state ~recFlag (td : Parsetree.type_declaration)
    cmtTbl i =
  let name =
    let doc = printIdentLike td.Parsetree.ptype_name.txt in
    printComments doc cmtTbl td.ptype_name.loc
  in
  let equalSign = "=" in
  let attrs =
    printAttributes ~state ~loc:td.ptype_loc td.ptype_attributes cmtTbl
  in
  let prefix =
    if i > 0 then Doc.text "and " else Doc.concat [Doc.text "type "; recFlag]
  in
  let typeName = name in
  let typeParams = printTypeParams ~state td.ptype_params cmtTbl in
  let manifestAndKind =
    match td.ptype_kind with
    | Ptype_abstract -> (
      match td.ptype_manifest with
      | None -> Doc.nil
      | Some typ ->
        Doc.concat
          [
            Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
            printPrivateFlag td.ptype_private;
            printTypExpr ~state typ cmtTbl;
          ])
    | Ptype_open ->
      Doc.concat
        [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printPrivateFlag td.ptype_private;
          Doc.text "..";
        ]
    | Ptype_record lds ->
      if lds = [] then
        Doc.concat
          [
            Doc.space;
            Doc.text equalSign;
            Doc.space;
            Doc.lbrace;
            printCommentsInside cmtTbl td.ptype_loc;
            Doc.rbrace;
          ]
      else
        let manifest =
          match td.ptype_manifest with
          | None -> Doc.nil
          | Some typ ->
            Doc.concat
              [
                Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
                printTypExpr ~state typ cmtTbl;
              ]
        in
        Doc.concat
          [
            manifest;
            Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
            printPrivateFlag td.ptype_private;
            printRecordDeclaration ~state lds cmtTbl;
          ]
    | Ptype_variant cds ->
      let manifest =
        match td.ptype_manifest with
        | None -> Doc.nil
        | Some typ ->
          Doc.concat
            [
              Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
              printTypExpr ~state typ cmtTbl;
            ]
      in
      Doc.concat
        [
          manifest;
          Doc.concat [Doc.space; Doc.text equalSign];
          printConstructorDeclarations ~state ~privateFlag:td.ptype_private cds
            cmtTbl;
        ]
  in
  let constraints = printTypeDefinitionConstraints ~state td.ptype_cstrs in
  Doc.group
    (Doc.concat
       [attrs; prefix; typeName; typeParams; manifestAndKind; constraints])

and printTypeDefinitionConstraints ~state cstrs =
  match cstrs with
  | [] -> Doc.nil
  | cstrs ->
    Doc.indent
      (Doc.group
         (Doc.concat
            [
              Doc.line;
              Doc.group
                (Doc.join ~sep:Doc.line
                   (List.map (printTypeDefinitionConstraint ~state) cstrs));
            ]))

and printTypeDefinitionConstraint ~state
    ((typ1, typ2, _loc) :
      Parsetree.core_type * Parsetree.core_type * Location.t) =
  Doc.concat
    [
      Doc.text "constraint ";
      printTypExpr ~state typ1 CommentTable.empty;
      Doc.text " = ";
      printTypExpr ~state typ2 CommentTable.empty;
    ]

and printPrivateFlag (flag : Asttypes.private_flag) =
  match flag with
  | Private -> Doc.text "private "
  | Public -> Doc.nil

and printTypeParams ~state typeParams cmtTbl =
  match typeParams with
  | [] -> Doc.nil
  | typeParams ->
    Doc.group
      (Doc.concat
         [
           Doc.lessThan;
           Doc.indent
             (Doc.concat
                [
                  Doc.softLine;
                  Doc.join
                    ~sep:(Doc.concat [Doc.comma; Doc.line])
                    (List.map
                       (fun typeParam ->
                         let doc = printTypeParam ~state typeParam cmtTbl in
                         printComments doc cmtTbl
                           (fst typeParam).Parsetree.ptyp_loc)
                       typeParams);
                ]);
           Doc.trailingComma;
           Doc.softLine;
           Doc.greaterThan;
         ])

and printTypeParam ~state (param : Parsetree.core_type * Asttypes.variance)
    cmtTbl =
  let typ, variance = param in
  let printedVariance =
    match variance with
    | Covariant -> Doc.text "+"
    | Contravariant -> Doc.text "-"
    | Invariant -> Doc.nil
  in
  Doc.concat [printedVariance; printTypExpr ~state typ cmtTbl]

and printRecordDeclaration ~state (lds : Parsetree.label_declaration list)
    cmtTbl =
  let forceBreak =
    match (lds, List.rev lds) with
    | first :: _, last :: _ ->
      first.pld_loc.loc_start.pos_lnum < last.pld_loc.loc_end.pos_lnum
    | _ -> false
  in
  Doc.breakableGroup ~forceBreak
    (Doc.concat
       [
         Doc.lbrace;
         Doc.indent
           (Doc.concat
              [
                Doc.softLine;
                Doc.join
                  ~sep:(Doc.concat [Doc.comma; Doc.line])
                  (List.map
                     (fun ld ->
                       let doc = printLabelDeclaration ~state ld cmtTbl in
                       printComments doc cmtTbl ld.Parsetree.pld_loc)
                     lds);
              ]);
         Doc.trailingComma;
         Doc.softLine;
         Doc.rbrace;
       ])

and printConstructorDeclarations ~state ~privateFlag
    (cds : Parsetree.constructor_declaration list) cmtTbl =
  let forceBreak =
    match (cds, List.rev cds) with
    | first :: _, last :: _ ->
      first.pcd_loc.loc_start.pos_lnum < last.pcd_loc.loc_end.pos_lnum
    | _ -> false
  in
  let privateFlag =
    match privateFlag with
    | Asttypes.Private -> Doc.concat [Doc.text "private"; Doc.line]
    | Public -> Doc.nil
  in
  let rows =
    printListi
      ~getLoc:(fun cd -> cd.Parsetree.pcd_loc)
      ~nodes:cds
      ~print:(fun cd cmtTbl i ->
        let doc = printConstructorDeclaration2 ~state i cd cmtTbl in
        printComments doc cmtTbl cd.Parsetree.pcd_loc)
      ~forceBreak cmtTbl
  in
  Doc.breakableGroup ~forceBreak
    (Doc.indent (Doc.concat [Doc.line; privateFlag; rows]))

and printConstructorDeclaration2 ~state i
    (cd : Parsetree.constructor_declaration) cmtTbl =
  let attrs = printAttributes ~state cd.pcd_attributes cmtTbl in
  let isDotDotDot = cd.pcd_name.txt = "..." in
  let bar =
    if i > 0 || cd.pcd_attributes <> [] || isDotDotDot then Doc.text "| "
    else Doc.ifBreaks (Doc.text "| ") Doc.nil
  in
  let constrName =
    let doc = Doc.text cd.pcd_name.txt in
    printComments doc cmtTbl cd.pcd_name.loc
  in
  let constrArgs =
    printConstructorArguments ~isDotDotDot ~state ~indent:true cd.pcd_args
      cmtTbl
  in
  let gadt =
    match cd.pcd_res with
    | None -> Doc.nil
    | Some typ ->
      Doc.indent (Doc.concat [Doc.text ": "; printTypExpr ~state typ cmtTbl])
  in
  Doc.concat
    [
      bar;
      Doc.group
        (Doc.concat
           [
             attrs;
             (* TODO: fix parsing of attributes, so when can print them above the bar? *)
             constrName;
             constrArgs;
             gadt;
           ]);
    ]

and printConstructorArguments ?(isDotDotDot = false) ~state ~indent
    (cdArgs : Parsetree.constructor_arguments) cmtTbl =
  match cdArgs with
  | Pcstr_tuple [] -> Doc.nil
  | Pcstr_tuple types ->
    let args =
      Doc.concat
        [
          (if isDotDotDot then Doc.nil else Doc.lparen);
          Doc.indent
            (Doc.concat
               [
                 Doc.softLine;
                 Doc.join
                   ~sep:(Doc.concat [Doc.comma; Doc.line])
                   (List.map
                      (fun typexpr -> printTypExpr ~state typexpr cmtTbl)
                      types);
               ]);
          Doc.trailingComma;
          Doc.softLine;
          (if isDotDotDot then Doc.nil else Doc.rparen);
        ]
    in
    Doc.group (if indent then Doc.indent args else args)
  | Pcstr_record lds ->
    let args =
      Doc.concat
        [
          Doc.lparen;
          (* manually inline the printRecordDeclaration, gives better layout *)
          Doc.lbrace;
          Doc.indent
            (Doc.concat
               [
                 Doc.softLine;
                 Doc.join
                   ~sep:(Doc.concat [Doc.comma; Doc.line])
                   (List.map
                      (fun ld ->
                        let doc = printLabelDeclaration ~state ld cmtTbl in
                        printComments doc cmtTbl ld.Parsetree.pld_loc)
                      lds);
               ]);
          Doc.trailingComma;
          Doc.softLine;
          Doc.rbrace;
          Doc.rparen;
        ]
    in
    if indent then Doc.indent args else args

and printLabelDeclaration ~state (ld : Parsetree.label_declaration) cmtTbl =
  let attrs =
    printAttributes ~state ~loc:ld.pld_name.loc ld.pld_attributes cmtTbl
  in
  let mutableFlag =
    match ld.pld_mutable with
    | Mutable -> Doc.text "mutable "
    | Immutable -> Doc.nil
  in
  let name, isDot =
    let doc, isDot =
      if ld.pld_name.txt = "..." then (Doc.text ld.pld_name.txt, true)
      else (printIdentLike ld.pld_name.txt, false)
    in
    (printComments doc cmtTbl ld.pld_name.loc, isDot)
  in
  let optional = printOptionalLabel ld.pld_attributes in
  Doc.group
    (Doc.concat
       [
         attrs;
         mutableFlag;
         name;
         optional;
         (if isDot then Doc.nil else Doc.text ": ");
         printTypExpr ~state ld.pld_type cmtTbl;
       ])

and printTypExpr ~(state : State.t) (typExpr : Parsetree.core_type) cmtTbl =
  let printArrow ~uncurried ?(arity = max_int) typExpr =
    let attrsBefore, args, returnType =
      ParsetreeViewer.arrowType ~arity typExpr
    in
    let dotted, attrsBefore =
      let dotted =
        state.uncurried_config |> Res_uncurried.getDotted ~uncurried
      in
      (* Converting .ml code to .res requires processing uncurried attributes *)
      let hasBs, attrs = ParsetreeViewer.processBsAttribute attrsBefore in
      (dotted || hasBs, attrs)
    in
    let returnTypeNeedsParens =
      match returnType.ptyp_desc with
      | Ptyp_alias _ -> true
      | _ -> false
    in
    let returnDoc =
      let doc = printTypExpr ~state returnType cmtTbl in
      if returnTypeNeedsParens then Doc.concat [Doc.lparen; doc; Doc.rparen]
      else doc
    in
    match args with
    | [] -> Doc.nil
    | [([], Nolabel, n)] when not dotted ->
      let hasAttrsBefore = not (attrsBefore = []) in
      let attrs =
        if hasAttrsBefore then
          printAttributes ~state ~inline:true attrsBefore cmtTbl
        else Doc.nil
      in
      let typDoc =
        let doc = printTypExpr ~state n cmtTbl in
        match n.ptyp_desc with
        | Ptyp_arrow _ | Ptyp_tuple _ | Ptyp_alias _ -> addParens doc
        | _ when Ast_uncurried.coreTypeIsUncurriedFun n -> addParens doc
        | _ -> doc
      in
      Doc.group
        (Doc.concat
           [
             Doc.group attrs;
             Doc.group
               (if hasAttrsBefore then
                  Doc.concat
                    [
                      Doc.lparen;
                      Doc.indent
                        (Doc.concat
                           [Doc.softLine; typDoc; Doc.text " => "; returnDoc]);
                      Doc.softLine;
                      Doc.rparen;
                    ]
                else Doc.concat [typDoc; Doc.text " => "; returnDoc]);
           ])
    | args ->
      let attrs = printAttributes ~state ~inline:true attrsBefore cmtTbl in
      let renderedArgs =
        Doc.concat
          [
            attrs;
            Doc.text "(";
            Doc.indent
              (Doc.concat
                 [
                   Doc.softLine;
                   (if dotted then Doc.concat [Doc.dot; Doc.space] else Doc.nil);
                   Doc.join
                     ~sep:(Doc.concat [Doc.comma; Doc.line])
                     (List.map
                        (fun tp -> printTypeParameter ~state tp cmtTbl)
                        args);
                 ]);
            Doc.trailingComma;
            Doc.softLine;
            Doc.text ")";
          ]
      in
      Doc.group (Doc.concat [renderedArgs; Doc.text " => "; returnDoc])
  in
  let renderedType =
    match typExpr.ptyp_desc with
    | Ptyp_any -> Doc.text "_"
    | Ptyp_var var ->
      Doc.concat [Doc.text "'"; printIdentLike ~allowUident:true var]
    | Ptyp_extension extension ->
      printExtension ~state ~atModuleLvl:false extension cmtTbl
    | Ptyp_alias (typ, alias) ->
      let typ =
        (* Technically type t = (string, float) => unit as 'x, doesn't require
         * parens around the arrow expression. This is very confusing though.
         * Is the "as" part of "unit" or "(string, float) => unit". By printing
         * parens we guide the user towards its meaning.*)
        let needsParens =
          match typ.ptyp_desc with
          | Ptyp_arrow _ -> true
          | _ when Ast_uncurried.coreTypeIsUncurriedFun typ -> true
          | _ -> false
        in
        let doc = printTypExpr ~state typ cmtTbl in
        if needsParens then Doc.concat [Doc.lparen; doc; Doc.rparen] else doc
      in
      Doc.concat
        [typ; Doc.text " as "; Doc.concat [Doc.text "'"; printIdentLike alias]]
    (* object printings *)
    | Ptyp_object (fields, openFlag) ->
      printObject ~state ~inline:false fields openFlag cmtTbl
    | Ptyp_arrow _ -> printArrow ~uncurried:false typExpr
    | Ptyp_constr _ when Ast_uncurried.coreTypeIsUncurriedFun typExpr ->
      let arity, tArg = Ast_uncurried.coreTypeExtractUncurriedFun typExpr in
      printArrow ~uncurried:true ~arity tArg
    | Ptyp_constr (longidentLoc, [{ptyp_desc = Ptyp_object (fields, openFlag)}])
      ->
      (* for foo<{"a": b}>, when the object is long and needs a line break, we
         want the <{ and }> to stay hugged together *)
      let constrName = printLidentPath longidentLoc cmtTbl in
      Doc.concat
        [
          constrName;
          Doc.lessThan;
          printObject ~state ~inline:true fields openFlag cmtTbl;
          Doc.greaterThan;
        ]
    | Ptyp_constr (longidentLoc, [{ptyp_desc = Parsetree.Ptyp_tuple tuple}]) ->
      let constrName = printLidentPath longidentLoc cmtTbl in
      Doc.group
        (Doc.concat
           [
             constrName;
             Doc.lessThan;
             printTupleType ~state ~inline:true tuple cmtTbl;
             Doc.greaterThan;
           ])
    | Ptyp_constr (longidentLoc, constrArgs) -> (
      let constrName = printLidentPath longidentLoc cmtTbl in
      match constrArgs with
      | [] -> constrName
      | _args ->
        Doc.group
          (Doc.concat
             [
               constrName;
               Doc.lessThan;
               Doc.indent
                 (Doc.concat
                    [
                      Doc.softLine;
                      Doc.join
                        ~sep:(Doc.concat [Doc.comma; Doc.line])
                        (List.map
                           (fun typexpr -> printTypExpr ~state typexpr cmtTbl)
                           constrArgs);
                    ]);
               Doc.trailingComma;
               Doc.softLine;
               Doc.greaterThan;
             ]))
    | Ptyp_tuple types -> printTupleType ~state ~inline:false types cmtTbl
    | Ptyp_poly ([], typ) -> printTypExpr ~state typ cmtTbl
    | Ptyp_poly (stringLocs, typ) ->
      Doc.concat
        [
          Doc.join ~sep:Doc.space
            (List.map
               (fun {Location.txt; loc} ->
                 let doc = Doc.concat [Doc.text "'"; Doc.text txt] in
                 printComments doc cmtTbl loc)
               stringLocs);
          Doc.dot;
          Doc.space;
          printTypExpr ~state typ cmtTbl;
        ]
    | Ptyp_package packageType ->
      printPackageType ~state ~printModuleKeywordAndParens:true packageType
        cmtTbl
    | Ptyp_class _ -> Doc.text "classes are not supported in types"
    | Ptyp_variant (rowFields, closedFlag, labelsOpt) ->
      let forceBreak =
        typExpr.ptyp_loc.Location.loc_start.pos_lnum
        < typExpr.ptyp_loc.loc_end.pos_lnum
      in
      let printRowField = function
        | Parsetree.Rtag ({txt; loc}, attrs, true, []) ->
          let doc =
            Doc.group
              (Doc.concat
                 [
                   printAttributes ~state attrs cmtTbl;
                   Doc.concat [Doc.text "#"; printPolyVarIdent txt];
                 ])
          in
          printComments doc cmtTbl loc
        | Rtag ({txt}, attrs, truth, types) ->
          let doType t =
            match t.Parsetree.ptyp_desc with
            | Ptyp_tuple _ -> printTypExpr ~state t cmtTbl
            | _ ->
              Doc.concat [Doc.lparen; printTypExpr ~state t cmtTbl; Doc.rparen]
          in
          let printedTypes = List.map doType types in
          let cases =
            Doc.join ~sep:(Doc.concat [Doc.line; Doc.text "& "]) printedTypes
          in
          let cases =
            if truth then Doc.concat [Doc.line; Doc.text "& "; cases] else cases
          in
          Doc.group
            (Doc.concat
               [
                 printAttributes ~state attrs cmtTbl;
                 Doc.concat [Doc.text "#"; printPolyVarIdent txt];
                 cases;
               ])
        | Rinherit coreType -> printTypExpr ~state coreType cmtTbl
      in
      let docs = List.map printRowField rowFields in
      let cases = Doc.join ~sep:(Doc.concat [Doc.line; Doc.text "| "]) docs in
      let cases =
        if docs = [] then cases
        else Doc.concat [Doc.ifBreaks (Doc.text "| ") Doc.nil; cases]
      in
      let openingSymbol =
        if closedFlag = Open then Doc.concat [Doc.greaterThan; Doc.line]
        else if labelsOpt = None then Doc.softLine
        else Doc.concat [Doc.lessThan; Doc.line]
      in
      let labels =
        match labelsOpt with
        | None | Some [] -> Doc.nil
        | Some labels ->
          Doc.concat
            (List.map
               (fun label ->
                 Doc.concat [Doc.line; Doc.text "#"; printPolyVarIdent label])
               labels)
      in
      let closingSymbol =
        match labelsOpt with
        | None | Some [] -> Doc.nil
        | _ -> Doc.text " >"
      in
      Doc.breakableGroup ~forceBreak
        (Doc.concat
           [
             Doc.lbracket;
             Doc.indent
               (Doc.concat [openingSymbol; cases; closingSymbol; labels]);
             Doc.softLine;
             Doc.rbracket;
           ])
  in
  let shouldPrintItsOwnAttributes =
    match typExpr.ptyp_desc with
    | Ptyp_arrow _ (* es6 arrow types print their own attributes *) -> true
    | _ -> false
  in
  let doc =
    match typExpr.ptyp_attributes with
    | _ :: _ as attrs when not shouldPrintItsOwnAttributes ->
      Doc.group (Doc.concat [printAttributes ~state attrs cmtTbl; renderedType])
    | _ -> renderedType
  in
  printComments doc cmtTbl typExpr.ptyp_loc

and printObject ~state ~inline fields openFlag cmtTbl =
  let doc =
    match fields with
    | [] ->
      Doc.concat
        [
          Doc.lbrace;
          (match openFlag with
          | Asttypes.Closed -> Doc.dot
          | Open -> Doc.dotdot);
          Doc.rbrace;
        ]
    | fields ->
      Doc.concat
        [
          Doc.lbrace;
          (match openFlag with
          | Asttypes.Closed -> Doc.nil
          | Open -> (
            match fields with
            (* handle `type t = {.. ...objType, "x": int}`
             * .. and ... should have a space in between *)
            | Oinherit _ :: _ -> Doc.text ".. "
            | _ -> Doc.dotdot));
          Doc.indent
            (Doc.concat
               [
                 Doc.softLine;
                 Doc.join
                   ~sep:(Doc.concat [Doc.comma; Doc.line])
                   (List.map
                      (fun field -> printObjectField ~state field cmtTbl)
                      fields);
               ]);
          Doc.trailingComma;
          Doc.softLine;
          Doc.rbrace;
        ]
  in
  if inline then doc else Doc.group doc

and printTupleType ~state ~inline (types : Parsetree.core_type list) cmtTbl =
  let tuple =
    Doc.concat
      [
        Doc.lparen;
        Doc.indent
          (Doc.concat
             [
               Doc.softLine;
               Doc.join
                 ~sep:(Doc.concat [Doc.comma; Doc.line])
                 (List.map
                    (fun typexpr -> printTypExpr ~state typexpr cmtTbl)
                    types);
             ]);
        Doc.trailingComma;
        Doc.softLine;
        Doc.rparen;
      ]
  in
  if inline == false then Doc.group tuple else tuple

and printObjectField ~state (field : Parsetree.object_field) cmtTbl =
  match field with
  | Otag (labelLoc, attrs, typ) ->
    let lbl =
      let doc = Doc.text ("\"" ^ labelLoc.txt ^ "\"") in
      printComments doc cmtTbl labelLoc.loc
    in
    let doc =
      Doc.concat
        [
          printAttributes ~state ~loc:labelLoc.loc attrs cmtTbl;
          lbl;
          Doc.text ": ";
          printTypExpr ~state typ cmtTbl;
        ]
    in
    let cmtLoc = {labelLoc.loc with loc_end = typ.ptyp_loc.loc_end} in
    printComments doc cmtTbl cmtLoc
  | Oinherit typexpr ->
    Doc.concat [Doc.dotdotdot; printTypExpr ~state typexpr cmtTbl]

(* es6 arrow type arg
 * type t = (~foo: string, ~bar: float=?, unit) => unit
 * i.e. ~foo: string, ~bar: float *)
and printTypeParameter ~state (attrs, lbl, typ) cmtTbl =
  (* Converting .ml code to .res requires processing uncurried attributes *)
  let hasBs, attrs = ParsetreeViewer.processBsAttribute attrs in
  let dotted = if hasBs then Doc.concat [Doc.dot; Doc.space] else Doc.nil in
  let attrs = printAttributes ~state attrs cmtTbl in
  let label =
    match lbl with
    | Asttypes.Nolabel -> Doc.nil
    | Labelled lbl ->
      Doc.concat [Doc.text "~"; printIdentLike lbl; Doc.text ": "]
    | Optional lbl ->
      Doc.concat [Doc.text "~"; printIdentLike lbl; Doc.text ": "]
  in
  let optionalIndicator =
    match lbl with
    | Asttypes.Nolabel | Labelled _ -> Doc.nil
    | Optional _lbl -> Doc.text "=?"
  in
  let loc, typ =
    match typ.ptyp_attributes with
    | ({Location.txt = "res.namedArgLoc"; loc}, _) :: attrs ->
      ( {loc with loc_end = typ.ptyp_loc.loc_end},
        {typ with ptyp_attributes = attrs} )
    | _ -> (typ.ptyp_loc, typ)
  in
  let doc =
    Doc.group
      (Doc.concat
         [
           dotted;
           attrs;
           label;
           printTypExpr ~state typ cmtTbl;
           optionalIndicator;
         ])
  in
  printComments doc cmtTbl loc

and printValueBinding ~state ~recFlag (vb : Parsetree.value_binding) cmtTbl i =
  let attrs =
    printAttributes ~state ~loc:vb.pvb_pat.ppat_loc vb.pvb_attributes cmtTbl
  in
  let header =
    if i == 0 then Doc.concat [Doc.text "let "; recFlag] else Doc.text "and "
  in
  match vb with
  | {
   pvb_pat =
     {
       ppat_desc =
         Ppat_constraint (pattern, ({ptyp_desc = Ptyp_poly _} as patTyp));
     };
   pvb_expr = {pexp_desc = Pexp_newtype _} as expr;
  } -> (
    let _uncurried, _attrs, parameters, returnExpr =
      ParsetreeViewer.funExpr expr
    in
    let abstractType =
      match parameters with
      | [NewTypes {locs = vars}] ->
        Doc.concat
          [
            Doc.text "type ";
            Doc.join ~sep:Doc.space
              (List.map (fun var -> Doc.text var.Asttypes.txt) vars);
            Doc.dot;
          ]
      | _ -> Doc.nil
    in
    match returnExpr.pexp_desc with
    | Pexp_constraint (expr, typ) ->
      Doc.group
        (Doc.concat
           [
             attrs;
             header;
             printPattern ~state pattern cmtTbl;
             Doc.text ":";
             Doc.indent
               (Doc.concat
                  [
                    Doc.line;
                    abstractType;
                    Doc.space;
                    printTypExpr ~state typ cmtTbl;
                    Doc.text " =";
                    Doc.concat
                      [Doc.line; printExpressionWithComments ~state expr cmtTbl];
                  ]);
           ])
    | _ ->
      (* Example:
       * let cancel_and_collect_callbacks:
       *   'a 'u 'c. (list<packed_callbacks>, promise<'a, 'u, 'c>) => list<packed_callbacks> =         *  (type x, callbacks_accumulator, p: promise<_, _, c>)
       *)
      Doc.group
        (Doc.concat
           [
             attrs;
             header;
             printPattern ~state pattern cmtTbl;
             Doc.text ":";
             Doc.indent
               (Doc.concat
                  [
                    Doc.line;
                    abstractType;
                    Doc.space;
                    printTypExpr ~state patTyp cmtTbl;
                    Doc.text " =";
                    Doc.concat
                      [Doc.line; printExpressionWithComments ~state expr cmtTbl];
                  ]);
           ]))
  | _ ->
    let optBraces, expr = ParsetreeViewer.processBracesAttr vb.pvb_expr in
    let printedExpr =
      let doc = printExpressionWithComments ~state vb.pvb_expr cmtTbl in
      match Parens.expr vb.pvb_expr with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc expr braces
      | Nothing -> doc
    in
    let patternDoc = printPattern ~state vb.pvb_pat cmtTbl in
    (*
     * we want to optimize the layout of one pipe:
     *   let tbl = data->Js.Array2.reduce((map, curr) => {
     *     ...
     *   })
     * important is that we don't do this for multiple pipes:
     *   let decoratorTags =
     *     items
     *     ->Js.Array2.filter(items => {items.category === Decorators})
     *     ->Belt.Array.map(...)
     * Multiple pipes chained together lend themselves more towards the last layout.
     *)
    if ParsetreeViewer.isSinglePipeExpr vb.pvb_expr then
      Doc.customLayout
        [
          Doc.group
            (Doc.concat
               [
                 attrs; header; patternDoc; Doc.text " ="; Doc.space; printedExpr;
               ]);
          Doc.group
            (Doc.concat
               [
                 attrs;
                 header;
                 patternDoc;
                 Doc.text " =";
                 Doc.indent (Doc.concat [Doc.line; printedExpr]);
               ]);
        ]
    else
      let shouldIndent =
        match optBraces with
        | Some _ -> false
        | _ -> (
          ParsetreeViewer.isBinaryExpression expr
          ||
          match vb.pvb_expr with
          | {
           pexp_attributes = [({Location.txt = "res.ternary"}, _)];
           pexp_desc = Pexp_ifthenelse (ifExpr, _, _);
          } ->
            ParsetreeViewer.isBinaryExpression ifExpr
            || ParsetreeViewer.hasAttributes ifExpr.pexp_attributes
          | {pexp_desc = Pexp_newtype _} -> false
          | {pexp_attributes = [({Location.txt = "res.taggedTemplate"}, _)]} ->
            false
          | e ->
            ParsetreeViewer.hasAttributes e.pexp_attributes
            || ParsetreeViewer.isArrayAccess e)
      in
      Doc.group
        (Doc.concat
           [
             attrs;
             header;
             patternDoc;
             Doc.text " =";
             (if shouldIndent then
                Doc.indent (Doc.concat [Doc.line; printedExpr])
              else Doc.concat [Doc.space; printedExpr]);
           ])

and printPackageType ~state ~printModuleKeywordAndParens
    (packageType : Parsetree.package_type) cmtTbl =
  let doc =
    match packageType with
    | longidentLoc, [] ->
      Doc.group (Doc.concat [printLongidentLocation longidentLoc cmtTbl])
    | longidentLoc, packageConstraints ->
      Doc.group
        (Doc.concat
           [
             printLongidentLocation longidentLoc cmtTbl;
             printPackageConstraints ~state packageConstraints cmtTbl;
             Doc.softLine;
           ])
  in
  if printModuleKeywordAndParens then
    Doc.concat [Doc.text "module("; doc; Doc.rparen]
  else doc

and printPackageConstraints ~state packageConstraints cmtTbl =
  Doc.concat
    [
      Doc.text " with";
      Doc.indent
        (Doc.concat
           [
             Doc.line;
             Doc.join ~sep:Doc.line
               (List.mapi
                  (fun i pc ->
                    let longident, typexpr = pc in
                    let cmtLoc =
                      {
                        longident.Asttypes.loc with
                        loc_end = typexpr.Parsetree.ptyp_loc.loc_end;
                      }
                    in
                    let doc = printPackageConstraint ~state i cmtTbl pc in
                    printComments doc cmtTbl cmtLoc)
                  packageConstraints);
           ]);
    ]

and printPackageConstraint ~state i cmtTbl (longidentLoc, typ) =
  let prefix = if i == 0 then Doc.text "type " else Doc.text "and type " in
  Doc.concat
    [
      prefix;
      printLongidentLocation longidentLoc cmtTbl;
      Doc.text " = ";
      printTypExpr ~state typ cmtTbl;
    ]

and printExtension ~state ~atModuleLvl (stringLoc, payload) cmtTbl =
  let txt = convertBsExtension stringLoc.Location.txt in
  let extName =
    let doc =
      Doc.concat
        [
          Doc.text "%";
          (if atModuleLvl then Doc.text "%" else Doc.nil);
          Doc.text txt;
        ]
    in
    printComments doc cmtTbl stringLoc.Location.loc
  in
  Doc.group (Doc.concat [extName; printPayload ~state payload cmtTbl])

and printPattern ~state (p : Parsetree.pattern) cmtTbl =
  let patternWithoutAttributes =
    match p.ppat_desc with
    | Ppat_any -> Doc.text "_"
    | Ppat_var var -> printIdentLike var.txt
    | Ppat_constant c ->
      let templateLiteral =
        ParsetreeViewer.hasTemplateLiteralAttr p.ppat_attributes
      in
      printConstant ~templateLiteral c
    | Ppat_tuple patterns ->
      Doc.group
        (Doc.concat
           [
             Doc.lparen;
             Doc.indent
               (Doc.concat
                  [
                    Doc.softLine;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun pat -> printPattern ~state pat cmtTbl)
                         patterns);
                  ]);
             Doc.trailingComma;
             Doc.softLine;
             Doc.rparen;
           ])
    | Ppat_array [] ->
      Doc.concat
        [Doc.lbracket; printCommentsInside cmtTbl p.ppat_loc; Doc.rbracket]
    | Ppat_array patterns ->
      Doc.group
        (Doc.concat
           [
             Doc.text "[";
             Doc.indent
               (Doc.concat
                  [
                    Doc.softLine;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun pat -> printPattern ~state pat cmtTbl)
                         patterns);
                  ]);
             Doc.trailingComma;
             Doc.softLine;
             Doc.text "]";
           ])
    | Ppat_construct ({txt = Longident.Lident "()"}, _) ->
      Doc.concat [Doc.lparen; printCommentsInside cmtTbl p.ppat_loc; Doc.rparen]
    | Ppat_construct ({txt = Longident.Lident "[]"}, _) ->
      Doc.concat
        [Doc.text "list{"; printCommentsInside cmtTbl p.ppat_loc; Doc.rbrace]
    | Ppat_construct ({txt = Longident.Lident "::"}, _) ->
      let patterns, tail =
        ParsetreeViewer.collectPatternsFromListConstruct [] p
      in
      let shouldHug =
        match (patterns, tail) with
        | [pat], {ppat_desc = Ppat_construct ({txt = Longident.Lident "[]"}, _)}
          when ParsetreeViewer.isHuggablePattern pat ->
          true
        | _ -> false
      in
      let children =
        Doc.concat
          [
            (if shouldHug then Doc.nil else Doc.softLine);
            Doc.join
              ~sep:(Doc.concat [Doc.text ","; Doc.line])
              (List.map (fun pat -> printPattern ~state pat cmtTbl) patterns);
            (match tail.Parsetree.ppat_desc with
            | Ppat_construct ({txt = Longident.Lident "[]"}, _) -> Doc.nil
            | _ ->
              let doc =
                Doc.concat [Doc.text "..."; printPattern ~state tail cmtTbl]
              in
              let tail = printComments doc cmtTbl tail.ppat_loc in
              Doc.concat [Doc.text ","; Doc.line; tail]);
          ]
      in
      Doc.group
        (Doc.concat
           [
             Doc.text "list{";
             (if shouldHug then children
              else
                Doc.concat
                  [
                    Doc.indent children;
                    Doc.ifBreaks (Doc.text ",") Doc.nil;
                    Doc.softLine;
                  ]);
             Doc.rbrace;
           ])
    | Ppat_construct (constrName, constructorArgs) ->
      let constrName = printLongidentLocation constrName cmtTbl in
      let argsDoc =
        match constructorArgs with
        | None -> Doc.nil
        | Some
            {
              ppat_loc;
              ppat_desc = Ppat_construct ({txt = Longident.Lident "()"}, _);
            } ->
          Doc.concat
            [Doc.lparen; printCommentsInside cmtTbl ppat_loc; Doc.rparen]
        | Some {ppat_desc = Ppat_tuple []; ppat_loc = loc} ->
          Doc.concat [Doc.lparen; printCommentsInside cmtTbl loc; Doc.rparen]
        (* Some((1, 2) *)
        | Some {ppat_desc = Ppat_tuple [({ppat_desc = Ppat_tuple _} as arg)]} ->
          Doc.concat [Doc.lparen; printPattern ~state arg cmtTbl; Doc.rparen]
        | Some {ppat_desc = Ppat_tuple patterns} ->
          Doc.concat
            [
              Doc.lparen;
              Doc.indent
                (Doc.concat
                   [
                     Doc.softLine;
                     Doc.join
                       ~sep:(Doc.concat [Doc.comma; Doc.line])
                       (List.map
                          (fun pat -> printPattern ~state pat cmtTbl)
                          patterns);
                   ]);
              Doc.trailingComma;
              Doc.softLine;
              Doc.rparen;
            ]
        | Some arg ->
          let argDoc = printPattern ~state arg cmtTbl in
          let shouldHug = ParsetreeViewer.isHuggablePattern arg in
          Doc.concat
            [
              Doc.lparen;
              (if shouldHug then argDoc
               else
                 Doc.concat
                   [
                     Doc.indent (Doc.concat [Doc.softLine; argDoc]);
                     Doc.trailingComma;
                     Doc.softLine;
                   ]);
              Doc.rparen;
            ]
      in
      Doc.group (Doc.concat [constrName; argsDoc])
    | Ppat_variant (label, None) ->
      Doc.concat [Doc.text "#"; printPolyVarIdent label]
    | Ppat_variant (label, variantArgs) ->
      let variantName = Doc.concat [Doc.text "#"; printPolyVarIdent label] in
      let argsDoc =
        match variantArgs with
        | None -> Doc.nil
        | Some {ppat_desc = Ppat_construct ({txt = Longident.Lident "()"}, _)}
          ->
          Doc.text "()"
        | Some {ppat_desc = Ppat_tuple []; ppat_loc = loc} ->
          Doc.concat [Doc.lparen; printCommentsInside cmtTbl loc; Doc.rparen]
        (* Some((1, 2) *)
        | Some {ppat_desc = Ppat_tuple [({ppat_desc = Ppat_tuple _} as arg)]} ->
          Doc.concat [Doc.lparen; printPattern ~state arg cmtTbl; Doc.rparen]
        | Some {ppat_desc = Ppat_tuple patterns} ->
          Doc.concat
            [
              Doc.lparen;
              Doc.indent
                (Doc.concat
                   [
                     Doc.softLine;
                     Doc.join
                       ~sep:(Doc.concat [Doc.comma; Doc.line])
                       (List.map
                          (fun pat -> printPattern ~state pat cmtTbl)
                          patterns);
                   ]);
              Doc.trailingComma;
              Doc.softLine;
              Doc.rparen;
            ]
        | Some arg ->
          let argDoc = printPattern ~state arg cmtTbl in
          let shouldHug = ParsetreeViewer.isHuggablePattern arg in
          Doc.concat
            [
              Doc.lparen;
              (if shouldHug then argDoc
               else
                 Doc.concat
                   [
                     Doc.indent (Doc.concat [Doc.softLine; argDoc]);
                     Doc.trailingComma;
                     Doc.softLine;
                   ]);
              Doc.rparen;
            ]
      in
      Doc.group (Doc.concat [variantName; argsDoc])
    | Ppat_type ident ->
      Doc.concat [Doc.text "#..."; printIdentPath ident cmtTbl]
    | Ppat_record ([], Open) ->
      Doc.concat [Doc.lbrace; Doc.text "_"; Doc.rbrace]
    | Ppat_record (rows, openFlag) ->
      Doc.group
        (Doc.concat
           [
             Doc.lbrace;
             Doc.indent
               (Doc.concat
                  [
                    Doc.softLine;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun row -> printPatternRecordRow ~state row cmtTbl)
                         rows);
                    (match openFlag with
                    | Open -> Doc.concat [Doc.text ","; Doc.line; Doc.text "_"]
                    | Closed -> Doc.nil);
                  ]);
             Doc.ifBreaks (Doc.text ",") Doc.nil;
             Doc.softLine;
             Doc.rbrace;
           ])
    | Ppat_exception p ->
      let needsParens =
        match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
      in
      let pat =
        let p = printPattern ~state p cmtTbl in
        if needsParens then Doc.concat [Doc.text "("; p; Doc.text ")"] else p
      in
      Doc.group (Doc.concat [Doc.text "exception"; Doc.line; pat])
    | Ppat_or _ ->
      (* Blue | Red | Green -> [Blue; Red; Green] *)
      let orChain = ParsetreeViewer.collectOrPatternChain p in
      let docs =
        List.mapi
          (fun i pat ->
            let patternDoc = printPattern ~state pat cmtTbl in
            Doc.concat
              [
                (if i == 0 then Doc.nil else Doc.concat [Doc.line; Doc.text "| "]);
                (match pat.ppat_desc with
                (* (Blue | Red) | (Green | Black) | White *)
                | Ppat_or _ -> addParens patternDoc
                | _ -> patternDoc);
              ])
          orChain
      in
      let isSpreadOverMultipleLines =
        match (orChain, List.rev orChain) with
        | first :: _, last :: _ ->
          first.ppat_loc.loc_start.pos_lnum < last.ppat_loc.loc_end.pos_lnum
        | _ -> false
      in
      Doc.breakableGroup ~forceBreak:isSpreadOverMultipleLines (Doc.concat docs)
    | Ppat_extension ext -> printExtension ~state ~atModuleLvl:false ext cmtTbl
    | Ppat_lazy p ->
      let needsParens =
        match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
      in
      let pat =
        let p = printPattern ~state p cmtTbl in
        if needsParens then Doc.concat [Doc.text "("; p; Doc.text ")"] else p
      in
      Doc.concat [Doc.text "lazy "; pat]
    | Ppat_alias (p, aliasLoc) ->
      let needsParens =
        match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
      in
      let renderedPattern =
        let p = printPattern ~state p cmtTbl in
        if needsParens then Doc.concat [Doc.text "("; p; Doc.text ")"] else p
      in
      Doc.concat
        [renderedPattern; Doc.text " as "; printStringLoc aliasLoc cmtTbl]
    (* Note: module(P : S) is represented as *)
    (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_constraint
        ( {ppat_desc = Ppat_unpack stringLoc},
          {ptyp_desc = Ptyp_package packageType; ptyp_loc} ) ->
      Doc.concat
        [
          Doc.text "module(";
          printComments (Doc.text stringLoc.txt) cmtTbl stringLoc.loc;
          Doc.text ": ";
          printComments
            (printPackageType ~state ~printModuleKeywordAndParens:false
               packageType cmtTbl)
            cmtTbl ptyp_loc;
          Doc.rparen;
        ]
    | Ppat_constraint (pattern, typ) ->
      Doc.concat
        [
          printPattern ~state pattern cmtTbl;
          Doc.text ": ";
          printTypExpr ~state typ cmtTbl;
        ]
    (* Note: module(P : S) is represented as *)
    (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_unpack stringLoc ->
      Doc.concat
        [
          Doc.text "module(";
          printComments (Doc.text stringLoc.txt) cmtTbl stringLoc.loc;
          Doc.rparen;
        ]
    | Ppat_interval (a, b) ->
      Doc.concat [printConstant a; Doc.text " .. "; printConstant b]
    | Ppat_open _ -> Doc.nil
  in
  let doc =
    match p.ppat_attributes with
    | [] -> patternWithoutAttributes
    | attrs ->
      Doc.group
        (Doc.concat
           [printAttributes ~state attrs cmtTbl; patternWithoutAttributes])
  in
  printComments doc cmtTbl p.ppat_loc

and printPatternRecordRow ~state row cmtTbl =
  match row with
  (* punned {x}*)
  | ( ({Location.txt = Longident.Lident ident} as longident),
      {Parsetree.ppat_desc = Ppat_var {txt; _}; ppat_attributes} )
    when ident = txt ->
    Doc.concat
      [
        printOptionalLabel ppat_attributes;
        printAttributes ~state ppat_attributes cmtTbl;
        printLidentPath longident cmtTbl;
      ]
  | longident, pattern ->
    let locForComments =
      {longident.loc with loc_end = pattern.Parsetree.ppat_loc.loc_end}
    in
    let rhsDoc =
      let doc = printPattern ~state pattern cmtTbl in
      let doc =
        if Parens.patternRecordRowRhs pattern then addParens doc else doc
      in
      Doc.concat [printOptionalLabel pattern.ppat_attributes; doc]
    in
    let doc =
      Doc.group
        (Doc.concat
           [
             printLidentPath longident cmtTbl;
             Doc.text ":";
             (if ParsetreeViewer.isHuggablePattern pattern then
                Doc.concat [Doc.space; rhsDoc]
              else Doc.indent (Doc.concat [Doc.line; rhsDoc]));
           ])
    in
    printComments doc cmtTbl locForComments

and printExpressionWithComments ~state expr cmtTbl : Doc.t =
  let doc = printExpression ~state expr cmtTbl in
  printComments doc cmtTbl expr.Parsetree.pexp_loc

and printIfChain ~state pexp_attributes ifs elseExpr cmtTbl =
  let ifDocs =
    Doc.join ~sep:Doc.space
      (List.mapi
         (fun i (outerLoc, ifExpr, thenExpr) ->
           let ifTxt = if i > 0 then Doc.text "else if " else Doc.text "if " in
           let doc =
             match ifExpr with
             | ParsetreeViewer.If ifExpr ->
               let condition =
                 if ParsetreeViewer.isBlockExpr ifExpr then
                   printExpressionBlock ~state ~braces:true ifExpr cmtTbl
                 else
                   let doc = printExpressionWithComments ~state ifExpr cmtTbl in
                   match Parens.expr ifExpr with
                   | Parens.Parenthesized -> addParens doc
                   | Braced braces -> printBraces doc ifExpr braces
                   | Nothing -> Doc.ifBreaks (addParens doc) doc
               in
               Doc.concat
                 [
                   ifTxt;
                   Doc.group condition;
                   Doc.space;
                   (let thenExpr =
                      match ParsetreeViewer.processBracesAttr thenExpr with
                      (* This case only happens when coming from Reason, we strip braces *)
                      | Some _, expr -> expr
                      | _ -> thenExpr
                    in
                    printExpressionBlock ~state ~braces:true thenExpr cmtTbl);
                 ]
             | IfLet (pattern, conditionExpr) ->
               let conditionDoc =
                 let doc =
                   printExpressionWithComments ~state conditionExpr cmtTbl
                 in
                 match Parens.expr conditionExpr with
                 | Parens.Parenthesized -> addParens doc
                 | Braced braces -> printBraces doc conditionExpr braces
                 | Nothing -> doc
               in
               Doc.concat
                 [
                   ifTxt;
                   Doc.text "let ";
                   printPattern ~state pattern cmtTbl;
                   Doc.text " = ";
                   conditionDoc;
                   Doc.space;
                   printExpressionBlock ~state ~braces:true thenExpr cmtTbl;
                 ]
           in
           printLeadingComments doc cmtTbl.leading outerLoc)
         ifs)
  in
  let elseDoc =
    match elseExpr with
    | None -> Doc.nil
    | Some expr ->
      Doc.concat
        [
          Doc.text " else "; printExpressionBlock ~state ~braces:true expr cmtTbl;
        ]
  in
  let attrs = ParsetreeViewer.filterFragileMatchAttributes pexp_attributes in
  Doc.concat [printAttributes ~state attrs cmtTbl; ifDocs; elseDoc]

and printExpression ~state (e : Parsetree.expression) cmtTbl =
  let printArrow e =
    let uncurried, attrsOnArrow, parameters, returnExpr =
      ParsetreeViewer.funExpr e
    in
    let ParsetreeViewer.{async; bs; attributes = attrs} =
      ParsetreeViewer.processFunctionAttributes attrsOnArrow
    in
    let uncurried = uncurried || bs in
    let returnExpr, typConstraint =
      match returnExpr.pexp_desc with
      | Pexp_constraint (expr, typ) ->
        ( {
            expr with
            pexp_attributes =
              List.concat [expr.pexp_attributes; returnExpr.pexp_attributes];
          },
          Some typ )
      | _ -> (returnExpr, None)
    in
    let hasConstraint =
      match typConstraint with
      | Some _ -> true
      | None -> false
    in
    let parametersDoc =
      printExprFunParameters ~state ~inCallback:NoCallback ~uncurried ~async
        ~hasConstraint parameters cmtTbl
    in
    let returnExprDoc =
      let optBraces, _ = ParsetreeViewer.processBracesAttr returnExpr in
      let shouldInline =
        match (returnExpr.pexp_desc, optBraces) with
        | _, Some _ -> true
        | ( ( Pexp_array _ | Pexp_tuple _
            | Pexp_construct (_, Some _)
            | Pexp_record _ ),
            _ ) ->
          true
        | _ -> false
      in
      let shouldIndent =
        match returnExpr.pexp_desc with
        | Pexp_sequence _ | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _
        | Pexp_open _ ->
          false
        | _ -> true
      in
      let returnDoc =
        let doc = printExpressionWithComments ~state returnExpr cmtTbl in
        match Parens.expr returnExpr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc returnExpr braces
        | Nothing -> doc
      in
      if shouldInline then Doc.concat [Doc.space; returnDoc]
      else
        Doc.group
          (if shouldIndent then Doc.indent (Doc.concat [Doc.line; returnDoc])
           else Doc.concat [Doc.space; returnDoc])
    in
    let typConstraintDoc =
      match typConstraint with
      | Some typ ->
        let typDoc =
          let doc = printTypExpr ~state typ cmtTbl in
          if Parens.arrowReturnTypExpr typ then addParens doc else doc
        in
        Doc.concat [Doc.text ": "; typDoc]
      | _ -> Doc.nil
    in
    let attrs = printAttributes ~state attrs cmtTbl in
    Doc.group
      (Doc.concat
         [attrs; parametersDoc; typConstraintDoc; Doc.text " =>"; returnExprDoc])
  in
  let uncurried = Ast_uncurried.exprIsUncurriedFun e in
  let e_fun =
    if uncurried then Ast_uncurried.exprExtractUncurriedFun e else e
  in
  let printedExpression =
    match e_fun.pexp_desc with
    | Pexp_fun
        ( Nolabel,
          None,
          {ppat_desc = Ppat_var {txt = "__x"}},
          {pexp_desc = Pexp_apply _} )
    | Pexp_construct
        ( {txt = Lident "Function$"},
          Some
            {
              pexp_desc =
                Pexp_fun
                  ( Nolabel,
                    None,
                    {ppat_desc = Ppat_var {txt = "__x"}},
                    {pexp_desc = Pexp_apply _} );
            } ) ->
      (* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
      printExpressionWithComments ~state
        (ParsetreeViewer.rewriteUnderscoreApply e_fun)
        cmtTbl
    | Pexp_fun _ | Pexp_newtype _ -> printArrow e
    | Parsetree.Pexp_constant c ->
      printConstant ~templateLiteral:(ParsetreeViewer.isTemplateLiteral e) c
    | Pexp_construct _ when ParsetreeViewer.hasJsxAttribute e.pexp_attributes ->
      printJsxFragment ~state e cmtTbl
    | Pexp_construct ({txt = Longident.Lident "()"}, _) -> Doc.text "()"
    | Pexp_construct ({txt = Longident.Lident "[]"}, _) ->
      Doc.concat
        [Doc.text "list{"; printCommentsInside cmtTbl e.pexp_loc; Doc.rbrace]
    | Pexp_construct ({txt = Longident.Lident "::"}, _) ->
      let expressions, spread = ParsetreeViewer.collectListExpressions e in
      let spreadDoc =
        match spread with
        | Some expr ->
          Doc.concat
            [
              Doc.text ",";
              Doc.line;
              Doc.dotdotdot;
              (let doc = printExpressionWithComments ~state expr cmtTbl in
               match Parens.expr expr with
               | Parens.Parenthesized -> addParens doc
               | Braced braces -> printBraces doc expr braces
               | Nothing -> doc);
            ]
        | None -> Doc.nil
      in
      Doc.group
        (Doc.concat
           [
             Doc.text "list{";
             Doc.indent
               (Doc.concat
                  [
                    Doc.softLine;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun expr ->
                           let doc =
                             printExpressionWithComments ~state expr cmtTbl
                           in
                           match Parens.expr expr with
                           | Parens.Parenthesized -> addParens doc
                           | Braced braces -> printBraces doc expr braces
                           | Nothing -> doc)
                         expressions);
                    spreadDoc;
                  ]);
             Doc.trailingComma;
             Doc.softLine;
             Doc.rbrace;
           ])
    | Pexp_construct (longidentLoc, args) ->
      let constr = printLongidentLocation longidentLoc cmtTbl in
      let args =
        match args with
        | None -> Doc.nil
        | Some {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}
          ->
          Doc.text "()"
        (* Some((1, 2)) *)
        | Some {pexp_desc = Pexp_tuple [({pexp_desc = Pexp_tuple _} as arg)]} ->
          Doc.concat
            [
              Doc.lparen;
              (let doc = printExpressionWithComments ~state arg cmtTbl in
               match Parens.expr arg with
               | Parens.Parenthesized -> addParens doc
               | Braced braces -> printBraces doc arg braces
               | Nothing -> doc);
              Doc.rparen;
            ]
        | Some {pexp_desc = Pexp_tuple args} ->
          Doc.concat
            [
              Doc.lparen;
              Doc.indent
                (Doc.concat
                   [
                     Doc.softLine;
                     Doc.join
                       ~sep:(Doc.concat [Doc.comma; Doc.line])
                       (List.map
                          (fun expr ->
                            let doc =
                              printExpressionWithComments ~state expr cmtTbl
                            in
                            match Parens.expr expr with
                            | Parens.Parenthesized -> addParens doc
                            | Braced braces -> printBraces doc expr braces
                            | Nothing -> doc)
                          args);
                   ]);
              Doc.trailingComma;
              Doc.softLine;
              Doc.rparen;
            ]
        | Some arg ->
          let argDoc =
            let doc = printExpressionWithComments ~state arg cmtTbl in
            match Parens.expr arg with
            | Parens.Parenthesized -> addParens doc
            | Braced braces -> printBraces doc arg braces
            | Nothing -> doc
          in
          let shouldHug = ParsetreeViewer.isHuggableExpression arg in
          Doc.concat
            [
              Doc.lparen;
              (if shouldHug then argDoc
               else
                 Doc.concat
                   [
                     Doc.indent (Doc.concat [Doc.softLine; argDoc]);
                     Doc.trailingComma;
                     Doc.softLine;
                   ]);
              Doc.rparen;
            ]
      in
      Doc.group (Doc.concat [constr; args])
    | Pexp_ident path -> printLidentPath path cmtTbl
    | Pexp_tuple exprs ->
      Doc.group
        (Doc.concat
           [
             Doc.lparen;
             Doc.indent
               (Doc.concat
                  [
                    Doc.softLine;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun expr ->
                           let doc =
                             printExpressionWithComments ~state expr cmtTbl
                           in
                           match Parens.expr expr with
                           | Parens.Parenthesized -> addParens doc
                           | Braced braces -> printBraces doc expr braces
                           | Nothing -> doc)
                         exprs);
                  ]);
             Doc.ifBreaks (Doc.text ",") Doc.nil;
             Doc.softLine;
             Doc.rparen;
           ])
    | Pexp_array [] ->
      Doc.concat
        [Doc.lbracket; printCommentsInside cmtTbl e.pexp_loc; Doc.rbracket]
    | Pexp_array exprs ->
      Doc.group
        (Doc.concat
           [
             Doc.lbracket;
             Doc.indent
               (Doc.concat
                  [
                    Doc.softLine;
                    Doc.join
                      ~sep:(Doc.concat [Doc.text ","; Doc.line])
                      (List.map
                         (fun expr ->
                           let doc =
                             printExpressionWithComments ~state expr cmtTbl
                           in
                           match Parens.expr expr with
                           | Parens.Parenthesized -> addParens doc
                           | Braced braces -> printBraces doc expr braces
                           | Nothing -> doc)
                         exprs);
                  ]);
             Doc.trailingComma;
             Doc.softLine;
             Doc.rbracket;
           ])
    | Pexp_variant (label, args) ->
      let variantName = Doc.concat [Doc.text "#"; printPolyVarIdent label] in
      let args =
        match args with
        | None -> Doc.nil
        | Some {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}
          ->
          Doc.text "()"
        (* #poly((1, 2) *)
        | Some {pexp_desc = Pexp_tuple [({pexp_desc = Pexp_tuple _} as arg)]} ->
          Doc.concat
            [
              Doc.lparen;
              (let doc = printExpressionWithComments ~state arg cmtTbl in
               match Parens.expr arg with
               | Parens.Parenthesized -> addParens doc
               | Braced braces -> printBraces doc arg braces
               | Nothing -> doc);
              Doc.rparen;
            ]
        | Some {pexp_desc = Pexp_tuple args} ->
          Doc.concat
            [
              Doc.lparen;
              Doc.indent
                (Doc.concat
                   [
                     Doc.softLine;
                     Doc.join
                       ~sep:(Doc.concat [Doc.comma; Doc.line])
                       (List.map
                          (fun expr ->
                            let doc =
                              printExpressionWithComments ~state expr cmtTbl
                            in
                            match Parens.expr expr with
                            | Parens.Parenthesized -> addParens doc
                            | Braced braces -> printBraces doc expr braces
                            | Nothing -> doc)
                          args);
                   ]);
              Doc.trailingComma;
              Doc.softLine;
              Doc.rparen;
            ]
        | Some arg ->
          let argDoc =
            let doc = printExpressionWithComments ~state arg cmtTbl in
            match Parens.expr arg with
            | Parens.Parenthesized -> addParens doc
            | Braced braces -> printBraces doc arg braces
            | Nothing -> doc
          in
          let shouldHug = ParsetreeViewer.isHuggableExpression arg in
          Doc.concat
            [
              Doc.lparen;
              (if shouldHug then argDoc
               else
                 Doc.concat
                   [
                     Doc.indent (Doc.concat [Doc.softLine; argDoc]);
                     Doc.trailingComma;
                     Doc.softLine;
                   ]);
              Doc.rparen;
            ]
      in
      Doc.group (Doc.concat [variantName; args])
    | Pexp_record (rows, spreadExpr) ->
      if rows = [] then
        Doc.concat
          [Doc.lbrace; printCommentsInside cmtTbl e.pexp_loc; Doc.rbrace]
      else
        let spread =
          match spreadExpr with
          | None -> Doc.nil
          | Some ({pexp_desc} as expr) ->
            let doc =
              match pexp_desc with
              | Pexp_ident {txt = expr} -> printLident expr
              | _ -> printExpression ~state expr cmtTbl
            in
            let docWithSpread =
              Doc.concat
                [
                  Doc.dotdotdot;
                  (match Parens.expr expr with
                  | Parens.Parenthesized -> addParens doc
                  | Braced braces -> printBraces doc expr braces
                  | Nothing -> doc);
                ]
            in
            Doc.concat
              [
                printComments docWithSpread cmtTbl expr.Parsetree.pexp_loc;
                Doc.comma;
                Doc.line;
              ]
        in
        (* If the record is written over multiple lines, break automatically
         * `let x = {a: 1, b: 3}` -> same line, break when line-width exceeded
         * `let x = {
         *   a: 1,
         *   b: 2,
         *  }` -> record is written on multiple lines, break the group *)
        let forceBreak =
          e.pexp_loc.loc_start.pos_lnum < e.pexp_loc.loc_end.pos_lnum
        in
        let punningAllowed =
          match (spreadExpr, rows) with
          | None, [_] -> false (* disallow punning for single-element records *)
          | _ -> true
        in
        Doc.breakableGroup ~forceBreak
          (Doc.concat
             [
               Doc.lbrace;
               Doc.indent
                 (Doc.concat
                    [
                      Doc.softLine;
                      spread;
                      Doc.join
                        ~sep:(Doc.concat [Doc.text ","; Doc.line])
                        (List.map
                           (fun row ->
                             printExpressionRecordRow ~state row cmtTbl
                               punningAllowed)
                           rows);
                    ]);
               Doc.trailingComma;
               Doc.softLine;
               Doc.rbrace;
             ])
    | Pexp_extension extension -> (
      match extension with
      | ( {txt = "bs.obj" | "obj"},
          PStr
            [
              {
                pstr_loc = loc;
                pstr_desc = Pstr_eval ({pexp_desc = Pexp_record (rows, _)}, []);
              };
            ] ) ->
        (* If the object is written over multiple lines, break automatically
         * `let x = {"a": 1, "b": 3}` -> same line, break when line-width exceeded
         * `let x = {
         *   "a": 1,
         *   "b": 2,
         *  }` -> object is written on multiple lines, break the group *)
        let forceBreak = loc.loc_start.pos_lnum < loc.loc_end.pos_lnum in
        Doc.breakableGroup ~forceBreak
          (Doc.concat
             [
               Doc.lbrace;
               Doc.indent
                 (Doc.concat
                    [
                      Doc.softLine;
                      Doc.join
                        ~sep:(Doc.concat [Doc.text ","; Doc.line])
                        (List.map
                           (fun row -> printBsObjectRow ~state row cmtTbl)
                           rows);
                    ]);
               Doc.trailingComma;
               Doc.softLine;
               Doc.rbrace;
             ])
      | extension -> printExtension ~state ~atModuleLvl:false extension cmtTbl)
    | Pexp_apply (e, [(Nolabel, {pexp_desc = Pexp_array subLists})])
      when ParsetreeViewer.isSpreadBeltArrayConcat e ->
      printBeltArrayConcatApply ~state subLists cmtTbl
    | Pexp_apply (e, [(Nolabel, {pexp_desc = Pexp_array subLists})])
      when ParsetreeViewer.isSpreadBeltListConcat e ->
      printBeltListConcatApply ~state subLists cmtTbl
    | Pexp_apply (callExpr, args) ->
      if ParsetreeViewer.isUnaryExpression e then
        printUnaryExpression ~state e cmtTbl
      else if ParsetreeViewer.isTemplateLiteral e then
        printTemplateLiteral ~state e cmtTbl
      else if ParsetreeViewer.isTaggedTemplateLiteral e then
        printTaggedTemplateLiteral ~state callExpr args cmtTbl
      else if ParsetreeViewer.isBinaryExpression e then
        printBinaryExpression ~state e cmtTbl
      else printPexpApply ~state e cmtTbl
    | Pexp_unreachable -> Doc.dot
    | Pexp_field (expr, longidentLoc) ->
      let lhs =
        let doc = printExpressionWithComments ~state expr cmtTbl in
        match Parens.fieldExpr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [lhs; Doc.dot; printLidentPath longidentLoc cmtTbl]
    | Pexp_setfield (expr1, longidentLoc, expr2) ->
      printSetFieldExpr ~state e.pexp_attributes expr1 longidentLoc expr2
        e.pexp_loc cmtTbl
    | Pexp_ifthenelse (_ifExpr, _thenExpr, _elseExpr)
      when ParsetreeViewer.isTernaryExpr e ->
      let parts, alternate = ParsetreeViewer.collectTernaryParts e in
      let ternaryDoc =
        match parts with
        | (condition1, consequent1) :: rest ->
          Doc.group
            (Doc.concat
               [
                 printTernaryOperand ~state condition1 cmtTbl;
                 Doc.indent
                   (Doc.concat
                      [
                        Doc.line;
                        Doc.indent
                          (Doc.concat
                             [
                               Doc.text "? ";
                               printTernaryOperand ~state consequent1 cmtTbl;
                             ]);
                        Doc.concat
                          (List.map
                             (fun (condition, consequent) ->
                               Doc.concat
                                 [
                                   Doc.line;
                                   Doc.text ": ";
                                   printTernaryOperand ~state condition cmtTbl;
                                   Doc.line;
                                   Doc.text "? ";
                                   printTernaryOperand ~state consequent cmtTbl;
                                 ])
                             rest);
                        Doc.line;
                        Doc.text ": ";
                        Doc.indent (printTernaryOperand ~state alternate cmtTbl);
                      ]);
               ])
        | _ -> Doc.nil
      in
      let attrs = ParsetreeViewer.filterTernaryAttributes e.pexp_attributes in
      let needsParens =
        match ParsetreeViewer.filterParsingAttrs attrs with
        | [] -> false
        | _ -> true
      in
      Doc.concat
        [
          printAttributes ~state attrs cmtTbl;
          (if needsParens then addParens ternaryDoc else ternaryDoc);
        ]
    | Pexp_ifthenelse (_ifExpr, _thenExpr, _elseExpr) ->
      let ifs, elseExpr = ParsetreeViewer.collectIfExpressions e in
      printIfChain ~state e.pexp_attributes ifs elseExpr cmtTbl
    | Pexp_while (expr1, expr2) ->
      let condition =
        let doc = printExpressionWithComments ~state expr1 cmtTbl in
        match Parens.expr expr1 with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc expr1 braces
        | Nothing -> doc
      in
      Doc.breakableGroup ~forceBreak:true
        (Doc.concat
           [
             Doc.text "while ";
             (if ParsetreeViewer.isBlockExpr expr1 then condition
              else Doc.group (Doc.ifBreaks (addParens condition) condition));
             Doc.space;
             printExpressionBlock ~state ~braces:true expr2 cmtTbl;
           ])
    | Pexp_for (pattern, fromExpr, toExpr, directionFlag, body) ->
      Doc.breakableGroup ~forceBreak:true
        (Doc.concat
           [
             Doc.text "for ";
             printPattern ~state pattern cmtTbl;
             Doc.text " in ";
             (let doc = printExpressionWithComments ~state fromExpr cmtTbl in
              match Parens.expr fromExpr with
              | Parens.Parenthesized -> addParens doc
              | Braced braces -> printBraces doc fromExpr braces
              | Nothing -> doc);
             printDirectionFlag directionFlag;
             (let doc = printExpressionWithComments ~state toExpr cmtTbl in
              match Parens.expr toExpr with
              | Parens.Parenthesized -> addParens doc
              | Braced braces -> printBraces doc toExpr braces
              | Nothing -> doc);
             Doc.space;
             printExpressionBlock ~state ~braces:true body cmtTbl;
           ])
    | Pexp_constraint
        ( {pexp_desc = Pexp_pack modExpr},
          {ptyp_desc = Ptyp_package packageType; ptyp_loc} ) ->
      Doc.group
        (Doc.concat
           [
             Doc.text "module(";
             Doc.indent
               (Doc.concat
                  [
                    Doc.softLine;
                    printModExpr ~state modExpr cmtTbl;
                    Doc.text ": ";
                    printComments
                      (printPackageType ~state
                         ~printModuleKeywordAndParens:false packageType cmtTbl)
                      cmtTbl ptyp_loc;
                  ]);
             Doc.softLine;
             Doc.rparen;
           ])
    | Pexp_constraint (expr, typ) ->
      let exprDoc =
        let doc = printExpressionWithComments ~state expr cmtTbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.concat [exprDoc; Doc.text ": "; printTypExpr ~state typ cmtTbl]
    | Pexp_letmodule ({txt = _modName}, _modExpr, _expr) ->
      printExpressionBlock ~state ~braces:true e cmtTbl
    | Pexp_letexception (_extensionConstructor, _expr) ->
      printExpressionBlock ~state ~braces:true e cmtTbl
    | Pexp_assert expr ->
      let expr = printExpressionWithComments ~state expr cmtTbl in
      Doc.concat [Doc.text "assert("; expr; Doc.text ")"]
    | Pexp_lazy expr ->
      let rhs =
        let doc = printExpressionWithComments ~state expr cmtTbl in
        match Parens.lazyOrAssertOrAwaitExprRhs expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.group (Doc.concat [Doc.text "lazy "; rhs])
    | Pexp_open (_overrideFlag, _longidentLoc, _expr) ->
      printExpressionBlock ~state ~braces:true e cmtTbl
    | Pexp_pack modExpr ->
      Doc.group
        (Doc.concat
           [
             Doc.text "module(";
             Doc.indent
               (Doc.concat [Doc.softLine; printModExpr ~state modExpr cmtTbl]);
             Doc.softLine;
             Doc.rparen;
           ])
    | Pexp_sequence _ -> printExpressionBlock ~state ~braces:true e cmtTbl
    | Pexp_let _ -> printExpressionBlock ~state ~braces:true e cmtTbl
    | Pexp_try (expr, cases) ->
      let exprDoc =
        let doc = printExpressionWithComments ~state expr cmtTbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.concat
        [
          Doc.text "try ";
          exprDoc;
          Doc.text " catch ";
          printCases ~state cases cmtTbl;
        ]
    | Pexp_match (_, [_; _]) when ParsetreeViewer.isIfLetExpr e ->
      let ifs, elseExpr = ParsetreeViewer.collectIfExpressions e in
      printIfChain ~state e.pexp_attributes ifs elseExpr cmtTbl
    | Pexp_match (expr, cases) ->
      let exprDoc =
        let doc = printExpressionWithComments ~state expr cmtTbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.concat
        [Doc.text "switch "; exprDoc; Doc.space; printCases ~state cases cmtTbl]
    | Pexp_function cases ->
      Doc.concat [Doc.text "x => switch x "; printCases ~state cases cmtTbl]
    | Pexp_coerce (expr, typOpt, typ) ->
      let docExpr = printExpressionWithComments ~state expr cmtTbl in
      let docTyp = printTypExpr ~state typ cmtTbl in
      let ofType =
        match typOpt with
        | None -> Doc.nil
        | Some typ1 ->
          Doc.concat [Doc.text ": "; printTypExpr ~state typ1 cmtTbl]
      in
      Doc.concat
        [Doc.lparen; docExpr; ofType; Doc.text " :> "; docTyp; Doc.rparen]
    | Pexp_send (parentExpr, label) ->
      let parentDoc =
        let doc = printExpressionWithComments ~state parentExpr cmtTbl in
        match Parens.unaryExprOperand parentExpr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc parentExpr braces
        | Nothing -> doc
      in
      let member =
        let memberDoc = printComments (Doc.text label.txt) cmtTbl label.loc in
        Doc.concat [Doc.text "\""; memberDoc; Doc.text "\""]
      in
      Doc.group (Doc.concat [parentDoc; Doc.lbracket; member; Doc.rbracket])
    | Pexp_new _ -> Doc.text "Pexp_new not implemented in printer"
    | Pexp_setinstvar _ -> Doc.text "Pexp_setinstvar not implemented in printer"
    | Pexp_override _ -> Doc.text "Pexp_override not implemented in printer"
    | Pexp_poly _ -> Doc.text "Pexp_poly not implemented in printer"
    | Pexp_object _ -> Doc.text "Pexp_object not implemented in printer"
  in
  let exprWithAwait =
    if ParsetreeViewer.hasAwaitAttribute e.pexp_attributes then
      let rhs =
        match
          Parens.lazyOrAssertOrAwaitExprRhs ~inAwait:true
            {
              e with
              pexp_attributes =
                List.filter
                  (function
                    | {Location.txt = "res.braces" | "ns.braces"}, _ -> false
                    | _ -> true)
                  e.pexp_attributes;
            }
        with
        | Parens.Parenthesized -> addParens printedExpression
        | Braced braces -> printBraces printedExpression e braces
        | Nothing -> printedExpression
      in
      Doc.concat [Doc.text "await "; rhs]
    else printedExpression
  in
  let shouldPrintItsOwnAttributes =
    match e.pexp_desc with
    | Pexp_apply _ | Pexp_fun _ | Pexp_newtype _ | Pexp_setfield _
    | Pexp_ifthenelse _ ->
      true
    | Pexp_match _ when ParsetreeViewer.isIfLetExpr e -> true
    | Pexp_construct _ when ParsetreeViewer.hasJsxAttribute e.pexp_attributes ->
      true
    | _ -> false
  in
  match e.pexp_attributes with
  | [] -> exprWithAwait
  | attrs when not shouldPrintItsOwnAttributes ->
    Doc.group (Doc.concat [printAttributes ~state attrs cmtTbl; exprWithAwait])
  | _ -> exprWithAwait

and printPexpFun ~state ~inCallback e cmtTbl =
  let uncurried, attrsOnArrow, parameters, returnExpr =
    ParsetreeViewer.funExpr e
  in
  let ParsetreeViewer.{async; bs; attributes = attrs} =
    ParsetreeViewer.processFunctionAttributes attrsOnArrow
  in
  let uncurried = bs || uncurried in
  let returnExpr, typConstraint =
    match returnExpr.pexp_desc with
    | Pexp_constraint (expr, typ) ->
      ( {
          expr with
          pexp_attributes =
            List.concat [expr.pexp_attributes; returnExpr.pexp_attributes];
        },
        Some typ )
    | _ -> (returnExpr, None)
  in
  let parametersDoc =
    printExprFunParameters ~state ~inCallback ~async ~uncurried
      ~hasConstraint:
        (match typConstraint with
        | Some _ -> true
        | None -> false)
      parameters cmtTbl
  in
  let returnShouldIndent =
    match returnExpr.pexp_desc with
    | Pexp_sequence _ | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _
    | Pexp_open _ ->
      false
    | _ -> true
  in
  let returnExprDoc =
    let optBraces, _ = ParsetreeViewer.processBracesAttr returnExpr in
    let shouldInline =
      match (returnExpr.pexp_desc, optBraces) with
      | _, Some _ -> true
      | ( ( Pexp_array _ | Pexp_tuple _
          | Pexp_construct (_, Some _)
          | Pexp_record _ ),
          _ ) ->
        true
      | _ -> false
    in
    let returnDoc =
      let doc = printExpressionWithComments ~state returnExpr cmtTbl in
      match Parens.expr returnExpr with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc returnExpr braces
      | Nothing -> doc
    in
    if shouldInline then Doc.concat [Doc.space; returnDoc]
    else
      Doc.group
        (if returnShouldIndent then
           Doc.concat
             [
               Doc.indent (Doc.concat [Doc.line; returnDoc]);
               (match inCallback with
               | FitsOnOneLine | ArgumentsFitOnOneLine -> Doc.softLine
               | _ -> Doc.nil);
             ]
         else Doc.concat [Doc.space; returnDoc])
  in
  let typConstraintDoc =
    match typConstraint with
    | Some typ -> Doc.concat [Doc.text ": "; printTypExpr ~state typ cmtTbl]
    | _ -> Doc.nil
  in
  Doc.concat
    [
      printAttributes ~state attrs cmtTbl;
      parametersDoc;
      typConstraintDoc;
      Doc.text " =>";
      returnExprDoc;
    ]

and printTernaryOperand ~state expr cmtTbl =
  let doc = printExpressionWithComments ~state expr cmtTbl in
  match Parens.ternaryOperand expr with
  | Parens.Parenthesized -> addParens doc
  | Braced braces -> printBraces doc expr braces
  | Nothing -> doc

and printSetFieldExpr ~state attrs lhs longidentLoc rhs loc cmtTbl =
  let rhsDoc =
    let doc = printExpressionWithComments ~state rhs cmtTbl in
    match Parens.setFieldExprRhs rhs with
    | Parens.Parenthesized -> addParens doc
    | Braced braces -> printBraces doc rhs braces
    | Nothing -> doc
  in
  let lhsDoc =
    let doc = printExpressionWithComments ~state lhs cmtTbl in
    match Parens.fieldExpr lhs with
    | Parens.Parenthesized -> addParens doc
    | Braced braces -> printBraces doc lhs braces
    | Nothing -> doc
  in
  let shouldIndent = ParsetreeViewer.isBinaryExpression rhs in
  let doc =
    Doc.group
      (Doc.concat
         [
           lhsDoc;
           Doc.dot;
           printLidentPath longidentLoc cmtTbl;
           Doc.text " =";
           (if shouldIndent then
              Doc.group (Doc.indent (Doc.concat [Doc.line; rhsDoc]))
            else Doc.concat [Doc.space; rhsDoc]);
         ])
  in
  let doc =
    match attrs with
    | [] -> doc
    | attrs -> Doc.group (Doc.concat [printAttributes ~state attrs cmtTbl; doc])
  in
  printComments doc cmtTbl loc

and printTemplateLiteral ~state expr cmtTbl =
  let tag = ref "js" in
  let rec walkExpr expr =
    let open Parsetree in
    match expr.pexp_desc with
    | Pexp_apply
        ( {pexp_desc = Pexp_ident {txt = Longident.Lident "^"}},
          [(Nolabel, arg1); (Nolabel, arg2)] ) ->
      let lhs = walkExpr arg1 in
      let rhs = walkExpr arg2 in
      Doc.concat [lhs; rhs]
    | Pexp_constant (Pconst_string (txt, Some prefix)) ->
      tag := prefix;
      printStringContents txt
    | _ ->
      let doc = printExpressionWithComments ~state expr cmtTbl in
      let doc =
        match Parens.expr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc expr braces
        | Nothing -> doc
      in
      Doc.group (Doc.concat [Doc.text "${"; Doc.indent doc; Doc.rbrace])
  in
  let content = walkExpr expr in
  Doc.concat
    [
      (if !tag = "js" then Doc.nil else Doc.text !tag);
      Doc.text "`";
      content;
      Doc.text "`";
    ]

and printTaggedTemplateLiteral ~state callExpr args cmtTbl =
  let stringsList, valuesList =
    match args with
    | [
     (_, {Parsetree.pexp_desc = Pexp_array strings});
     (_, {Parsetree.pexp_desc = Pexp_array values});
    ] ->
      (strings, values)
    | _ -> assert false
  in

  let strings =
    List.map
      (fun x ->
        match x with
        | {Parsetree.pexp_desc = Pexp_constant (Pconst_string (txt, _))} ->
          printStringContents txt
        | _ -> assert false)
      stringsList
  in

  let values =
    List.map
      (fun x ->
        Doc.concat
          [
            Doc.text "${";
            printExpressionWithComments ~state x cmtTbl;
            Doc.text "}";
          ])
      valuesList
  in

  let process strings values =
    let rec aux acc = function
      | [], [] -> acc
      | a_head :: a_rest, b -> aux (Doc.concat [acc; a_head]) (b, a_rest)
      | _ -> assert false
    in
    aux Doc.nil (strings, values)
  in

  let content : Doc.t = process strings values in

  let tag = printExpressionWithComments ~state callExpr cmtTbl in
  Doc.concat [tag; Doc.text "`"; content; Doc.text "`"]

and printUnaryExpression ~state expr cmtTbl =
  let printUnaryOperator op =
    Doc.text
      (match op with
      | "~+" -> "+"
      | "~+." -> "+."
      | "~-" -> "-"
      | "~-." -> "-."
      | "not" -> "!"
      | _ -> assert false)
  in
  match expr.pexp_desc with
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [(Nolabel, operand)] ) ->
    let printedOperand =
      let doc = printExpressionWithComments ~state operand cmtTbl in
      match Parens.unaryExprOperand operand with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc operand braces
      | Nothing -> doc
    in
    let doc = Doc.concat [printUnaryOperator operator; printedOperand] in
    printComments doc cmtTbl expr.pexp_loc
  | _ -> assert false

and printBinaryExpression ~state (expr : Parsetree.expression) cmtTbl =
  let printBinaryOperator ~inlineRhs operator =
    let operatorTxt =
      match operator with
      | "|." | "|.u" -> "->"
      | "^" -> "++"
      | "=" -> "=="
      | "==" -> "==="
      | "<>" -> "!="
      | "!=" -> "!=="
      | txt -> txt
    in
    let spacingBeforeOperator =
      if operator = "|." || operator = "|.u" then Doc.softLine
      else if operator = "|>" then Doc.line
      else Doc.space
    in
    let spacingAfterOperator =
      if operator = "|." || operator = "|.u" then Doc.nil
      else if operator = "|>" then Doc.space
      else if inlineRhs then Doc.space
      else Doc.line
    in
    Doc.concat
      [spacingBeforeOperator; Doc.text operatorTxt; spacingAfterOperator]
  in
  let printOperand ~isLhs ~isMultiline expr parentOperator =
    let rec flatten ~isLhs ~isMultiline expr parentOperator =
      if ParsetreeViewer.isBinaryExpression expr then
        match expr with
        | {
         pexp_desc =
           Pexp_apply
             ( {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
               [(_, left); (_, right)] );
        } ->
          if
            ParsetreeViewer.flattenableOperators parentOperator operator
            && not (ParsetreeViewer.hasAttributes expr.pexp_attributes)
          then
            let leftPrinted = flatten ~isLhs:true ~isMultiline left operator in
            let rightPrinted =
              let rightPrinteableAttrs, rightInternalAttrs =
                ParsetreeViewer.partitionPrintableAttributes
                  right.pexp_attributes
              in
              let doc =
                printExpressionWithComments ~state
                  {right with pexp_attributes = rightInternalAttrs}
                  cmtTbl
              in
              let doc =
                if Parens.flattenOperandRhs parentOperator right then
                  Doc.concat [Doc.lparen; doc; Doc.rparen]
                else doc
              in
              let doc =
                Doc.concat
                  [printAttributes ~state rightPrinteableAttrs cmtTbl; doc]
              in
              match rightPrinteableAttrs with
              | [] -> doc
              | _ -> addParens doc
            in
            let isAwait =
              ParsetreeViewer.hasAwaitAttribute expr.pexp_attributes
            in
            let doc =
              if isAwait then
                let parens =
                  Res_parens.binaryOperatorInsideAwaitNeedsParens operator
                in
                Doc.concat
                  [
                    Doc.lparen;
                    Doc.text "await ";
                    (if parens then Doc.lparen else Doc.nil);
                    leftPrinted;
                    printBinaryOperator ~inlineRhs:false operator;
                    rightPrinted;
                    (if parens then Doc.rparen else Doc.nil);
                    Doc.rparen;
                  ]
              else
                match operator with
                | ("|." | "|.u") when isMultiline ->
                  (* If the pipe-chain is written over multiple lines, break automatically
                   * `let x = a->b->c -> same line, break when line-width exceeded
                   * `let x = a->
                   *   b->c` -> pipe-chain is written on multiple lines, break the group *)
                  Doc.breakableGroup ~forceBreak:true
                    (Doc.concat
                       [
                         leftPrinted;
                         printBinaryOperator ~inlineRhs:false operator;
                         rightPrinted;
                       ])
                | _ ->
                  Doc.concat
                    [
                      leftPrinted;
                      printBinaryOperator ~inlineRhs:false operator;
                      rightPrinted;
                    ]
            in

            let doc =
              if (not isLhs) && Parens.rhsBinaryExprOperand operator expr then
                Doc.concat [Doc.lparen; doc; Doc.rparen]
              else doc
            in
            printComments doc cmtTbl expr.pexp_loc
          else
            let printeableAttrs, internalAttrs =
              ParsetreeViewer.partitionPrintableAttributes expr.pexp_attributes
            in
            let doc =
              printExpressionWithComments ~state
                {expr with pexp_attributes = internalAttrs}
                cmtTbl
            in
            let doc =
              if
                Parens.subBinaryExprOperand parentOperator operator
                || printeableAttrs <> []
                   && (ParsetreeViewer.isBinaryExpression expr
                      || ParsetreeViewer.isTernaryExpr expr)
              then Doc.concat [Doc.lparen; doc; Doc.rparen]
              else doc
            in
            Doc.concat [printAttributes ~state printeableAttrs cmtTbl; doc]
        | _ -> assert false
      else
        match expr.pexp_desc with
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Longident.Lident "^"; loc}},
              [(Nolabel, _); (Nolabel, _)] )
          when loc.loc_ghost ->
          let doc = printTemplateLiteral ~state expr cmtTbl in
          printComments doc cmtTbl expr.Parsetree.pexp_loc
        | Pexp_setfield (lhs, field, rhs) ->
          let doc =
            printSetFieldExpr ~state expr.pexp_attributes lhs field rhs
              expr.pexp_loc cmtTbl
          in
          if isLhs then addParens doc else doc
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
              [(Nolabel, lhs); (Nolabel, rhs)] ) ->
          let rhsDoc = printExpressionWithComments ~state rhs cmtTbl in
          let lhsDoc = printExpressionWithComments ~state lhs cmtTbl in
          (* TODO: unify indentation of "=" *)
          let shouldIndent = ParsetreeViewer.isBinaryExpression rhs in
          let doc =
            Doc.group
              (Doc.concat
                 [
                   lhsDoc;
                   Doc.text " =";
                   (if shouldIndent then
                      Doc.group (Doc.indent (Doc.concat [Doc.line; rhsDoc]))
                    else Doc.concat [Doc.space; rhsDoc]);
                 ])
          in
          let doc =
            match expr.pexp_attributes with
            | [] -> doc
            | attrs ->
              Doc.group (Doc.concat [printAttributes ~state attrs cmtTbl; doc])
          in
          if isLhs then addParens doc else doc
        | _ -> (
          let doc = printExpressionWithComments ~state expr cmtTbl in
          match Parens.binaryExprOperand ~isLhs expr with
          | Parens.Parenthesized -> addParens doc
          | Braced braces -> printBraces doc expr braces
          | Nothing -> doc)
    in
    flatten ~isLhs ~isMultiline expr parentOperator
  in
  match expr.pexp_desc with
  | Pexp_apply
      ( {
          pexp_desc =
            Pexp_ident {txt = Longident.Lident (("|." | "|.u" | "|>") as op)};
        },
        [(Nolabel, lhs); (Nolabel, rhs)] )
    when not
           (ParsetreeViewer.isBinaryExpression lhs
           || ParsetreeViewer.isBinaryExpression rhs
           || printAttributes ~state expr.pexp_attributes cmtTbl <> Doc.nil) ->
    let lhsHasCommentBelow = hasCommentBelow cmtTbl lhs.pexp_loc in
    let lhsDoc = printOperand ~isLhs:true ~isMultiline:false lhs op in
    let rhsDoc = printOperand ~isLhs:false ~isMultiline:false rhs op in
    Doc.group
      (Doc.concat
         [
           printAttributes ~state expr.pexp_attributes cmtTbl;
           lhsDoc;
           (match (lhsHasCommentBelow, op) with
           | true, ("|." | "|.u") -> Doc.concat [Doc.softLine; Doc.text "->"]
           | false, ("|." | "|.u") -> Doc.text "->"
           | true, "|>" -> Doc.concat [Doc.line; Doc.text "|> "]
           | false, "|>" -> Doc.text " |> "
           | _ -> Doc.nil);
           rhsDoc;
         ])
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [(Nolabel, lhs); (Nolabel, rhs)] ) ->
    let isMultiline =
      lhs.pexp_loc.loc_start.pos_lnum < rhs.pexp_loc.loc_start.pos_lnum
    in

    let right =
      let operatorWithRhs =
        let rhsDoc =
          printOperand
            ~isLhs:(ParsetreeViewer.isRhsBinaryOperator operator)
            ~isMultiline rhs operator
        in
        Doc.concat
          [
            printBinaryOperator
              ~inlineRhs:(ParsetreeViewer.shouldInlineRhsBinaryExpr rhs)
              operator;
            rhsDoc;
          ]
      in
      if ParsetreeViewer.shouldIndentBinaryExpr expr then
        Doc.group (Doc.indent operatorWithRhs)
      else operatorWithRhs
    in
    let doc =
      Doc.group
        (Doc.concat
           [
             printOperand
               ~isLhs:(not @@ ParsetreeViewer.isRhsBinaryOperator operator)
               ~isMultiline lhs operator;
             right;
           ])
    in
    Doc.group
      (Doc.concat
         [
           printAttributes ~state expr.pexp_attributes cmtTbl;
           (match
              Parens.binaryExpr
                {
                  expr with
                  pexp_attributes =
                    ParsetreeViewer.filterPrintableAttributes
                      expr.pexp_attributes;
                }
            with
           | Braced bracesLoc -> printBraces doc expr bracesLoc
           | Parenthesized -> addParens doc
           | Nothing -> doc);
         ])
  | _ -> Doc.nil

and printBeltArrayConcatApply ~state subLists cmtTbl =
  let makeSpreadDoc commaBeforeSpread = function
    | Some expr ->
      Doc.concat
        [
          commaBeforeSpread;
          Doc.dotdotdot;
          (let doc = printExpressionWithComments ~state expr cmtTbl in
           match Parens.expr expr with
           | Parens.Parenthesized -> addParens doc
           | Braced braces -> printBraces doc expr braces
           | Nothing -> doc);
        ]
    | None -> Doc.nil
  in
  let makeSubListDoc (expressions, spread) =
    let commaBeforeSpread =
      match expressions with
      | [] -> Doc.nil
      | _ -> Doc.concat [Doc.text ","; Doc.line]
    in
    let spreadDoc = makeSpreadDoc commaBeforeSpread spread in
    Doc.concat
      [
        Doc.join
          ~sep:(Doc.concat [Doc.text ","; Doc.line])
          (List.map
             (fun expr ->
               let doc = printExpressionWithComments ~state expr cmtTbl in
               match Parens.expr expr with
               | Parens.Parenthesized -> addParens doc
               | Braced braces -> printBraces doc expr braces
               | Nothing -> doc)
             expressions);
        spreadDoc;
      ]
  in
  Doc.group
    (Doc.concat
       [
         Doc.lbracket;
         Doc.indent
           (Doc.concat
              [
                Doc.softLine;
                Doc.join
                  ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map makeSubListDoc
                     (List.map ParsetreeViewer.collectArrayExpressions subLists));
              ]);
         Doc.trailingComma;
         Doc.softLine;
         Doc.rbracket;
       ])

and printBeltListConcatApply ~state subLists cmtTbl =
  let makeSpreadDoc commaBeforeSpread = function
    | Some expr ->
      Doc.concat
        [
          commaBeforeSpread;
          Doc.dotdotdot;
          (let doc = printExpressionWithComments ~state expr cmtTbl in
           match Parens.expr expr with
           | Parens.Parenthesized -> addParens doc
           | Braced braces -> printBraces doc expr braces
           | Nothing -> doc);
        ]
    | None -> Doc.nil
  in
  let makeSubListDoc (expressions, spread) =
    let commaBeforeSpread =
      match expressions with
      | [] -> Doc.nil
      | _ -> Doc.concat [Doc.text ","; Doc.line]
    in
    let spreadDoc = makeSpreadDoc commaBeforeSpread spread in
    Doc.concat
      [
        Doc.join
          ~sep:(Doc.concat [Doc.text ","; Doc.line])
          (List.map
             (fun expr ->
               let doc = printExpressionWithComments ~state expr cmtTbl in
               match Parens.expr expr with
               | Parens.Parenthesized -> addParens doc
               | Braced braces -> printBraces doc expr braces
               | Nothing -> doc)
             expressions);
        spreadDoc;
      ]
  in
  Doc.group
    (Doc.concat
       [
         Doc.text "list{";
         Doc.indent
           (Doc.concat
              [
                Doc.softLine;
                Doc.join
                  ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map makeSubListDoc
                     (List.map ParsetreeViewer.collectListExpressions subLists));
              ]);
         Doc.trailingComma;
         Doc.softLine;
         Doc.rbrace;
       ])

(* callExpr(arg1, arg2) *)
and printPexpApply ~state expr cmtTbl =
  match expr.pexp_desc with
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Longident.Lident "##"}},
        [(Nolabel, parentExpr); (Nolabel, memberExpr)] ) ->
    let parentDoc =
      let doc = printExpressionWithComments ~state parentExpr cmtTbl in
      match Parens.unaryExprOperand parentExpr with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc parentExpr braces
      | Nothing -> doc
    in
    let member =
      let memberDoc =
        match memberExpr.pexp_desc with
        | Pexp_ident lident ->
          printComments (printLongident lident.txt) cmtTbl memberExpr.pexp_loc
        | _ -> printExpressionWithComments ~state memberExpr cmtTbl
      in
      Doc.concat [Doc.text "\""; memberDoc; Doc.text "\""]
    in
    Doc.group
      (Doc.concat
         [
           printAttributes ~state expr.pexp_attributes cmtTbl;
           parentDoc;
           Doc.lbracket;
           member;
           Doc.rbracket;
         ])
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
        [(Nolabel, lhs); (Nolabel, rhs)] ) -> (
    let rhsDoc =
      let doc = printExpressionWithComments ~state rhs cmtTbl in
      match Parens.expr rhs with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc rhs braces
      | Nothing -> doc
    in
    (* TODO: unify indentation of "=" *)
    let shouldIndent =
      (not (ParsetreeViewer.isBracedExpr rhs))
      && ParsetreeViewer.isBinaryExpression rhs
    in
    let doc =
      Doc.group
        (Doc.concat
           [
             printExpressionWithComments ~state lhs cmtTbl;
             Doc.text " =";
             (if shouldIndent then
                Doc.group (Doc.indent (Doc.concat [Doc.line; rhsDoc]))
              else Doc.concat [Doc.space; rhsDoc]);
           ])
    in
    match expr.pexp_attributes with
    | [] -> doc
    | attrs -> Doc.group (Doc.concat [printAttributes ~state attrs cmtTbl; doc])
    )
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")}},
        [(Nolabel, parentExpr); (Nolabel, memberExpr)] )
    when not (ParsetreeViewer.isRewrittenUnderscoreApplySugar parentExpr) ->
    (* Don't print the Array.get(_, 0) sugar a.k.a. (__x) => Array.get(__x, 0) as _[0] *)
    let member =
      let memberDoc =
        let doc = printExpressionWithComments ~state memberExpr cmtTbl in
        match Parens.expr memberExpr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc memberExpr braces
        | Nothing -> doc
      in
      let shouldInline =
        match memberExpr.pexp_desc with
        | Pexp_constant _ | Pexp_ident _ -> true
        | _ -> false
      in
      if shouldInline then memberDoc
      else
        Doc.concat
          [Doc.indent (Doc.concat [Doc.softLine; memberDoc]); Doc.softLine]
    in
    let parentDoc =
      let doc = printExpressionWithComments ~state parentExpr cmtTbl in
      match Parens.unaryExprOperand parentExpr with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc parentExpr braces
      | Nothing -> doc
    in
    Doc.group
      (Doc.concat
         [
           printAttributes ~state expr.pexp_attributes cmtTbl;
           parentDoc;
           Doc.lbracket;
           member;
           Doc.rbracket;
         ])
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "set")}},
        [(Nolabel, parentExpr); (Nolabel, memberExpr); (Nolabel, targetExpr)] )
    ->
    let member =
      let memberDoc =
        let doc = printExpressionWithComments ~state memberExpr cmtTbl in
        match Parens.expr memberExpr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc memberExpr braces
        | Nothing -> doc
      in
      let shouldInline =
        match memberExpr.pexp_desc with
        | Pexp_constant _ | Pexp_ident _ -> true
        | _ -> false
      in
      if shouldInline then memberDoc
      else
        Doc.concat
          [Doc.indent (Doc.concat [Doc.softLine; memberDoc]); Doc.softLine]
    in
    let shouldIndentTargetExpr =
      if ParsetreeViewer.isBracedExpr targetExpr then false
      else
        ParsetreeViewer.isBinaryExpression targetExpr
        ||
        match targetExpr with
        | {
         pexp_attributes = [({Location.txt = "res.ternary"}, _)];
         pexp_desc = Pexp_ifthenelse (ifExpr, _, _);
        } ->
          ParsetreeViewer.isBinaryExpression ifExpr
          || ParsetreeViewer.hasAttributes ifExpr.pexp_attributes
        | {pexp_desc = Pexp_newtype _} -> false
        | e ->
          ParsetreeViewer.hasAttributes e.pexp_attributes
          || ParsetreeViewer.isArrayAccess e
    in
    let targetExpr =
      let doc = printExpressionWithComments ~state targetExpr cmtTbl in
      match Parens.expr targetExpr with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc targetExpr braces
      | Nothing -> doc
    in
    let parentDoc =
      let doc = printExpressionWithComments ~state parentExpr cmtTbl in
      match Parens.unaryExprOperand parentExpr with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc parentExpr braces
      | Nothing -> doc
    in
    Doc.group
      (Doc.concat
         [
           printAttributes ~state expr.pexp_attributes cmtTbl;
           parentDoc;
           Doc.lbracket;
           member;
           Doc.rbracket;
           Doc.text " =";
           (if shouldIndentTargetExpr then
              Doc.indent (Doc.concat [Doc.line; targetExpr])
            else Doc.concat [Doc.space; targetExpr]);
         ])
  (* TODO: cleanup, are those branches even remotely performant? *)
  | Pexp_apply ({pexp_desc = Pexp_ident lident}, args)
    when ParsetreeViewer.isJsxExpression expr ->
    printJsxExpression ~state lident args cmtTbl
  | Pexp_apply (callExpr, args) ->
    let args =
      List.map
        (fun (lbl, arg) -> (lbl, ParsetreeViewer.rewriteUnderscoreApply arg))
        args
    in
    let uncurried, attrs =
      ParsetreeViewer.processUncurriedAppAttribute expr.pexp_attributes
    in
    let partial, attrs = ParsetreeViewer.processPartialAppAttribute attrs in
    let args =
      if partial then
        let loc =
          {Asttypes.txt = "res.partial"; Asttypes.loc = expr.pexp_loc}
        in
        let attr = (loc, Parsetree.PTyp (Ast_helper.Typ.any ())) in
        let dummy =
          Ast_helper.Exp.constant ~attrs:[attr] (Ast_helper.Const.int 0)
        in
        args @ [(Asttypes.Labelled "...", dummy)]
      else args
    in
    let dotted = state.uncurried_config |> Res_uncurried.getDotted ~uncurried in
    let callExprDoc =
      let doc = printExpressionWithComments ~state callExpr cmtTbl in
      match Parens.callExpr callExpr with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc callExpr braces
      | Nothing -> doc
    in
    if ParsetreeViewer.requiresSpecialCallbackPrintingFirstArg args then
      let argsDoc =
        printArgumentsWithCallbackInFirstPosition ~dotted ~state args cmtTbl
      in
      Doc.concat [printAttributes ~state attrs cmtTbl; callExprDoc; argsDoc]
    else if ParsetreeViewer.requiresSpecialCallbackPrintingLastArg args then
      let argsDoc =
        printArgumentsWithCallbackInLastPosition ~state ~dotted args cmtTbl
      in
      (*
       * Fixes the following layout (the `[` and `]` should break):
       *   [fn(x => {
       *     let _ = x
       *   }), fn(y => {
       *     let _ = y
       *   }), fn(z => {
       *     let _ = z
       *   })]
       * See `Doc.willBreak documentation in interface file for more context.
       * Context:
       *  https://github.com/rescript-lang/syntax/issues/111
       *  https://github.com/rescript-lang/syntax/issues/166
       *)
      let maybeBreakParent =
        if Doc.willBreak argsDoc then Doc.breakParent else Doc.nil
      in
      Doc.concat
        [
          maybeBreakParent;
          printAttributes ~state attrs cmtTbl;
          callExprDoc;
          argsDoc;
        ]
    else
      let argsDoc = printArguments ~state ~dotted ~partial args cmtTbl in
      Doc.concat [printAttributes ~state attrs cmtTbl; callExprDoc; argsDoc]
  | _ -> assert false

and printJsxExpression ~state lident args cmtTbl =
  let name = printJsxName lident in
  let formattedProps, children = printJsxProps ~state args cmtTbl in
  (* <div className="test" /> *)
  let hasChildren =
    match children with
    | Some
        {
          Parsetree.pexp_desc =
            Pexp_construct ({txt = Longident.Lident "[]"}, None);
        } ->
      false
    | None -> false
    | _ -> true
  in
  let isSelfClosing =
    match children with
    | Some
        {
          Parsetree.pexp_desc =
            Pexp_construct ({txt = Longident.Lident "[]"}, None);
          pexp_loc = loc;
        } ->
      not (hasCommentsInside cmtTbl loc)
    | _ -> false
  in
  let printChildren children =
    let lineSep =
      match children with
      | Some expr ->
        if hasNestedJsxOrMoreThanOneChild expr then Doc.hardLine else Doc.line
      | None -> Doc.line
    in
    Doc.concat
      [
        Doc.indent
          (Doc.concat
             [
               Doc.line;
               (match children with
               | Some childrenExpression ->
                 printJsxChildren ~state childrenExpression ~sep:lineSep cmtTbl
               | None -> Doc.nil);
             ]);
        lineSep;
      ]
  in
  Doc.group
    (Doc.concat
       [
         Doc.group
           (Doc.concat
              [
                printComments
                  (Doc.concat [Doc.lessThan; name])
                  cmtTbl lident.Asttypes.loc;
                formattedProps;
                (match children with
                | Some
                    {
                      Parsetree.pexp_desc =
                        Pexp_construct ({txt = Longident.Lident "[]"}, None);
                    }
                  when isSelfClosing ->
                  Doc.text "/>"
                | _ ->
                  (* if tag A has trailing comments then put > on the next line
                     <A
                     // comments
                     >
                     </A>
                  *)
                  if hasTrailingComments cmtTbl lident.Asttypes.loc then
                    Doc.concat [Doc.softLine; Doc.greaterThan]
                  else Doc.greaterThan);
              ]);
         (if isSelfClosing then Doc.nil
          else
            Doc.concat
              [
                (if hasChildren then printChildren children
                 else
                   match children with
                   | Some
                       {
                         Parsetree.pexp_desc =
                           Pexp_construct ({txt = Longident.Lident "[]"}, None);
                         pexp_loc = loc;
                       } ->
                     printCommentsInside cmtTbl loc
                   | _ -> Doc.nil);
                Doc.text "</";
                name;
                Doc.greaterThan;
              ]);
       ])

and printJsxFragment ~state expr cmtTbl =
  let opening = Doc.text "<>" in
  let closing = Doc.text "</>" in
  let lineSep =
    if hasNestedJsxOrMoreThanOneChild expr then Doc.hardLine else Doc.line
  in
  Doc.group
    (Doc.concat
       [
         opening;
         (match expr.pexp_desc with
         | Pexp_construct ({txt = Longident.Lident "[]"}, None) -> Doc.nil
         | _ ->
           Doc.indent
             (Doc.concat
                [Doc.line; printJsxChildren ~state expr ~sep:lineSep cmtTbl]));
         lineSep;
         closing;
       ])

and printJsxChildren ~state (childrenExpr : Parsetree.expression) ~sep cmtTbl =
  match childrenExpr.pexp_desc with
  | Pexp_construct ({txt = Longident.Lident "::"}, _) ->
    let children, _ = ParsetreeViewer.collectListExpressions childrenExpr in
    Doc.group
      (Doc.join ~sep
         (List.map
            (fun (expr : Parsetree.expression) ->
              let leadingLineCommentPresent =
                hasLeadingLineComment cmtTbl expr.pexp_loc
              in
              let exprDoc = printExpressionWithComments ~state expr cmtTbl in
              let addParensOrBraces exprDoc =
                (* {(20: int)} make sure that we also protect the expression inside *)
                let innerDoc =
                  if Parens.bracedExpr expr then addParens exprDoc else exprDoc
                in
                if leadingLineCommentPresent then addBraces innerDoc
                else Doc.concat [Doc.lbrace; innerDoc; Doc.rbrace]
              in
              match Parens.jsxChildExpr expr with
              | Nothing -> exprDoc
              | Parenthesized -> addParensOrBraces exprDoc
              | Braced bracesLoc ->
                printComments (addParensOrBraces exprDoc) cmtTbl bracesLoc)
            children))
  | _ ->
    let leadingLineCommentPresent =
      hasLeadingLineComment cmtTbl childrenExpr.pexp_loc
    in
    let exprDoc = printExpressionWithComments ~state childrenExpr cmtTbl in
    Doc.concat
      [
        Doc.dotdotdot;
        (match Parens.jsxChildExpr childrenExpr with
        | Parenthesized | Braced _ ->
          let innerDoc =
            if Parens.bracedExpr childrenExpr then addParens exprDoc
            else exprDoc
          in
          if leadingLineCommentPresent then addBraces innerDoc
          else Doc.concat [Doc.lbrace; innerDoc; Doc.rbrace]
        | Nothing -> exprDoc);
      ]

and printJsxProps ~state args cmtTbl : Doc.t * Parsetree.expression option =
  (* This function was introduced because we have different formatting behavior for self-closing tags and other tags
     we always put /> on a new line for self-closing tag when it breaks
     <A
      a=""
     />

     <A
     a="">
      <B />
     </A>
     we should remove this function once the format is unified
  *)
  let isSelfClosing children =
    match children with
    | {
     Parsetree.pexp_desc = Pexp_construct ({txt = Longident.Lident "[]"}, None);
     pexp_loc = loc;
    } ->
      not (hasCommentsInside cmtTbl loc)
    | _ -> false
  in
  let rec loop props args =
    match args with
    | [] -> (Doc.nil, None)
    | [
     (Asttypes.Labelled "children", children);
     ( Asttypes.Nolabel,
       {
         Parsetree.pexp_desc =
           Pexp_construct ({txt = Longident.Lident "()"}, None);
       } );
    ] ->
      let doc = if isSelfClosing children then Doc.line else Doc.nil in
      (doc, Some children)
    | ((_, expr) as lastProp)
      :: [
           (Asttypes.Labelled "children", children);
           ( Asttypes.Nolabel,
             {
               Parsetree.pexp_desc =
                 Pexp_construct ({txt = Longident.Lident "()"}, None);
             } );
         ] ->
      let loc =
        match expr.Parsetree.pexp_attributes with
        | ({Location.txt = "res.namedArgLoc"; loc}, _) :: _attrs ->
          {loc with loc_end = expr.pexp_loc.loc_end}
        | _ -> expr.pexp_loc
      in
      let trailingCommentsPresent = hasTrailingComments cmtTbl loc in
      let propDoc = printJsxProp ~state lastProp cmtTbl in
      let formattedProps =
        Doc.concat
          [
            Doc.indent
              (Doc.concat
                 [
                   Doc.line;
                   Doc.group
                     (Doc.join ~sep:Doc.line (propDoc :: props |> List.rev));
                 ]);
            (* print > on new line if the last prop has trailing comments *)
            (match (isSelfClosing children, trailingCommentsPresent) with
            (* we always put /> on a new line when a self-closing tag breaks *)
            | true, _ -> Doc.line
            | false, true -> Doc.softLine
            | false, false -> Doc.nil);
          ]
      in
      (formattedProps, Some children)
    | arg :: args ->
      let propDoc = printJsxProp ~state arg cmtTbl in
      loop (propDoc :: props) args
  in
  loop [] args

and printJsxProp ~state arg cmtTbl =
  match arg with
  | ( ((Asttypes.Labelled lblTxt | Optional lblTxt) as lbl),
      {
        Parsetree.pexp_attributes =
          [({Location.txt = "res.namedArgLoc"; loc = argLoc}, _)];
        pexp_desc = Pexp_ident {txt = Longident.Lident ident};
      } )
    when lblTxt = ident (* jsx punning *) -> (
    match lbl with
    | Nolabel -> Doc.nil
    | Labelled _lbl -> printComments (printIdentLike ident) cmtTbl argLoc
    | Optional _lbl ->
      let doc = Doc.concat [Doc.question; printIdentLike ident] in
      printComments doc cmtTbl argLoc)
  | ( ((Asttypes.Labelled lblTxt | Optional lblTxt) as lbl),
      {
        Parsetree.pexp_attributes = [];
        pexp_desc = Pexp_ident {txt = Longident.Lident ident};
      } )
    when lblTxt = ident (* jsx punning when printing from Reason *) -> (
    match lbl with
    | Nolabel -> Doc.nil
    | Labelled _lbl -> printIdentLike ident
    | Optional _lbl -> Doc.concat [Doc.question; printIdentLike ident])
  | Asttypes.Labelled "_spreadProps", expr ->
    let doc = printExpressionWithComments ~state expr cmtTbl in
    Doc.concat [Doc.lbrace; Doc.dotdotdot; doc; Doc.rbrace]
  | lbl, expr ->
    let argLoc, expr =
      match expr.pexp_attributes with
      | ({Location.txt = "res.namedArgLoc"; loc}, _) :: attrs ->
        (loc, {expr with pexp_attributes = attrs})
      | _ -> (Location.none, expr)
    in
    let lblDoc =
      match lbl with
      | Asttypes.Labelled lbl ->
        let lbl = printComments (printIdentLike lbl) cmtTbl argLoc in
        Doc.concat [lbl; Doc.equal]
      | Asttypes.Optional lbl ->
        let lbl = printComments (printIdentLike lbl) cmtTbl argLoc in
        Doc.concat [lbl; Doc.equal; Doc.question]
      | Nolabel -> Doc.nil
    in
    let exprDoc =
      let leadingLineCommentPresent =
        hasLeadingLineComment cmtTbl expr.pexp_loc
      in
      let doc = printExpressionWithComments ~state expr cmtTbl in
      match Parens.jsxPropExpr expr with
      | Parenthesized | Braced _ ->
        (* {(20: int)} make sure that we also protect the expression inside *)
        let innerDoc = if Parens.bracedExpr expr then addParens doc else doc in
        if leadingLineCommentPresent then addBraces innerDoc
        else Doc.concat [Doc.lbrace; innerDoc; Doc.rbrace]
      | _ -> doc
    in
    let fullLoc = {argLoc with loc_end = expr.pexp_loc.loc_end} in
    printComments (Doc.concat [lblDoc; exprDoc]) cmtTbl fullLoc

(* div -> div.
 * Navabar.createElement -> Navbar
 * Staff.Users.createElement -> Staff.Users *)
and printJsxName {txt = lident} =
  let printIdent = printIdentLike ~allowUident:true ~allowHyphen:true in
  let rec flatten acc lident =
    match lident with
    | Longident.Lident txt -> printIdent txt :: acc
    | Ldot (lident, "createElement") -> flatten acc lident
    | Ldot (lident, txt) -> flatten (printIdent txt :: acc) lident
    | _ -> acc
  in
  match lident with
  | Longident.Lident txt -> printIdent txt
  | _ as lident ->
    let segments = flatten [] lident in
    Doc.join ~sep:Doc.dot segments

and printArgumentsWithCallbackInFirstPosition ~dotted ~state args cmtTbl =
  (* Because the same subtree gets printed twice, we need to copy the cmtTbl.
   * consumed comments need to be marked not-consumed and reprinted…
   * Cheng's different comment algorithm will solve this. *)
  let state = State.nextCustomLayout state in
  let cmtTblCopy = CommentTable.copy cmtTbl in
  let callback, printedArgs =
    match args with
    | (lbl, expr) :: args ->
      let lblDoc =
        match lbl with
        | Asttypes.Nolabel -> Doc.nil
        | Asttypes.Labelled txt ->
          Doc.concat [Doc.tilde; printIdentLike txt; Doc.equal]
        | Asttypes.Optional txt ->
          Doc.concat [Doc.tilde; printIdentLike txt; Doc.equal; Doc.question]
      in
      let callback =
        Doc.concat
          [lblDoc; printPexpFun ~state ~inCallback:FitsOnOneLine expr cmtTbl]
      in
      let callback = lazy (printComments callback cmtTbl expr.pexp_loc) in
      let printedArgs =
        lazy
          (Doc.join
             ~sep:(Doc.concat [Doc.comma; Doc.line])
             (List.map (fun arg -> printArgument ~state arg cmtTbl) args))
      in
      (callback, printedArgs)
    | _ -> assert false
  in

  (* Thing.map((arg1, arg2) => MyModuleBlah.toList(argument), foo) *)
  (* Thing.map((arg1, arg2) => {
   *   MyModuleBlah.toList(argument)
   * }, longArgumet, veryLooooongArgument)
   *)
  let fitsOnOneLine =
    lazy
      (Doc.concat
         [
           (if dotted then Doc.text "(. " else Doc.lparen);
           Lazy.force callback;
           Doc.comma;
           Doc.line;
           Lazy.force printedArgs;
           Doc.rparen;
         ])
  in

  (* Thing.map(
   *   (param1, parm2) => doStuff(param1, parm2),
   *   arg1,
   *   arg2,
   *   arg3,
   * )
   *)
  let breakAllArgs = lazy (printArguments ~state ~dotted args cmtTblCopy) in

  (* Sometimes one of the non-callback arguments will break.
   * There might be a single line comment in there, or a multiline string etc.
   * showDialog(
   *   ~onConfirm={() => ()},
   *   `
   *   Do you really want to leave this workspace?
   *   Some more text with detailed explanations...
   *   `,
   *   ~danger=true,
   *   // comment   --> here a single line comment
   *   ~confirmText="Yes, I am sure!",
   *  )
   * In this case, we always want the arguments broken over multiple lines,
   * like a normal function call.
   *)
  if state |> State.shouldBreakCallback then Lazy.force breakAllArgs
  else if Doc.willBreak (Lazy.force printedArgs) then Lazy.force breakAllArgs
  else Doc.customLayout [Lazy.force fitsOnOneLine; Lazy.force breakAllArgs]

and printArgumentsWithCallbackInLastPosition ~state ~dotted args cmtTbl =
  (* Because the same subtree gets printed twice, we need to copy the cmtTbl.
   * consumed comments need to be marked not-consumed and reprinted…
   * Cheng's different comment algorithm will solve this. *)
  let state = state |> State.nextCustomLayout in
  let cmtTblCopy = CommentTable.copy cmtTbl in
  let cmtTblCopy2 = CommentTable.copy cmtTbl in
  let rec loop acc args =
    match args with
    | [] -> (lazy Doc.nil, lazy Doc.nil, lazy Doc.nil)
    | [(lbl, expr)] ->
      let lblDoc =
        match lbl with
        | Asttypes.Nolabel -> Doc.nil
        | Asttypes.Labelled txt ->
          Doc.concat [Doc.tilde; printIdentLike txt; Doc.equal]
        | Asttypes.Optional txt ->
          Doc.concat [Doc.tilde; printIdentLike txt; Doc.equal; Doc.question]
      in
      let callbackFitsOnOneLine =
        lazy
          (let pexpFunDoc =
             printPexpFun ~state ~inCallback:FitsOnOneLine expr cmtTbl
           in
           let doc = Doc.concat [lblDoc; pexpFunDoc] in
           printComments doc cmtTbl expr.pexp_loc)
      in
      let callbackArgumentsFitsOnOneLine =
        lazy
          (let pexpFunDoc =
             printPexpFun ~state ~inCallback:ArgumentsFitOnOneLine expr
               cmtTblCopy
           in
           let doc = Doc.concat [lblDoc; pexpFunDoc] in
           printComments doc cmtTblCopy expr.pexp_loc)
      in
      ( lazy (Doc.concat (List.rev acc)),
        callbackFitsOnOneLine,
        callbackArgumentsFitsOnOneLine )
    | arg :: args ->
      let argDoc = printArgument ~state arg cmtTbl in
      loop (Doc.line :: Doc.comma :: argDoc :: acc) args
  in
  let printedArgs, callback, callback2 = loop [] args in

  (* Thing.map(foo, (arg1, arg2) => MyModuleBlah.toList(argument)) *)
  let fitsOnOneLine =
    lazy
      (Doc.concat
         [
           (if dotted then Doc.text "(." else Doc.lparen);
           Lazy.force printedArgs;
           Lazy.force callback;
           Doc.rparen;
         ])
  in

  (* Thing.map(longArgumet, veryLooooongArgument, (arg1, arg2) =>
   *   MyModuleBlah.toList(argument)
   * )
   *)
  let arugmentsFitOnOneLine =
    lazy
      (Doc.concat
         [
           (if dotted then Doc.text "(." else Doc.lparen);
           Lazy.force printedArgs;
           Doc.breakableGroup ~forceBreak:true (Lazy.force callback2);
           Doc.rparen;
         ])
  in

  (* Thing.map(
   *   arg1,
   *   arg2,
   *   arg3,
   *   (param1, parm2) => doStuff(param1, parm2)
   * )
   *)
  let breakAllArgs = lazy (printArguments ~state ~dotted args cmtTblCopy2) in

  (* Sometimes one of the non-callback arguments will break.
   * There might be a single line comment in there, or a multiline string etc.
   * showDialog(
   *   `
   *   Do you really want to leave this workspace?
   *   Some more text with detailed explanations...
   *   `,
   *   ~danger=true,
   *   // comment   --> here a single line comment
   *   ~confirmText="Yes, I am sure!",
   *   ~onConfirm={() => ()},
   *  )
   * In this case, we always want the arguments broken over multiple lines,
   * like a normal function call.
   *)
  if state |> State.shouldBreakCallback then Lazy.force breakAllArgs
  else if Doc.willBreak (Lazy.force printedArgs) then Lazy.force breakAllArgs
  else
    Doc.customLayout
      [
        Lazy.force fitsOnOneLine;
        Lazy.force arugmentsFitOnOneLine;
        Lazy.force breakAllArgs;
      ]

and printArguments ~state ~dotted ?(partial = false)
    (args : (Asttypes.arg_label * Parsetree.expression) list) cmtTbl =
  match args with
  | [
   ( Nolabel,
     {
       pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _);
       pexp_loc = loc;
     } );
  ] -> (
    (* See "parseCallExpr", ghost unit expression is used the implement
     * arity zero vs arity one syntax.
     * Related: https://github.com/rescript-lang/syntax/issues/138 *)
    match (dotted, loc.loc_ghost) with
    | true, true -> Doc.text "(.)" (* arity zero *)
    | true, false -> Doc.text "(. ())" (* arity one *)
    | _ -> Doc.text "()")
  | [(Nolabel, arg)] when ParsetreeViewer.isHuggableExpression arg ->
    let argDoc =
      let doc = printExpressionWithComments ~state arg cmtTbl in
      match Parens.expr arg with
      | Parens.Parenthesized -> addParens doc
      | Braced braces -> printBraces doc arg braces
      | Nothing -> doc
    in
    Doc.concat
      [(if dotted then Doc.text "(. " else Doc.lparen); argDoc; Doc.rparen]
  | args ->
    (* Avoid printing trailing comma when there is ... in function application *)
    let hasPartialAttr, printedArgs =
      List.fold_right
        (fun arg (flag, acc) ->
          let _, expr = arg in
          let hasPartialAttr =
            ParsetreeViewer.hasPartialAttribute expr.Parsetree.pexp_attributes
          in
          let doc = printArgument ~state arg cmtTbl in
          (flag || hasPartialAttr, doc :: acc))
        args (false, [])
    in
    Doc.group
      (Doc.concat
         [
           (if dotted then Doc.text "(." else Doc.lparen);
           Doc.indent
             (Doc.concat
                [
                  (if dotted then Doc.line else Doc.softLine);
                  Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) printedArgs;
                ]);
           (if partial || hasPartialAttr then Doc.nil else Doc.trailingComma);
           Doc.softLine;
           Doc.rparen;
         ])

(*
 * argument ::=
 *   | _                            (* syntax sugar *)
 *   | expr
 *   | expr : type
 *   | ~ label-name
 *   | ~ label-name
 *   | ~ label-name ?
 *   | ~ label-name =   expr
 *   | ~ label-name =   _           (* syntax sugar *)
 *   | ~ label-name =   expr : type
 *   | ~ label-name = ? expr
 *   | ~ label-name = ? _           (* syntax sugar *)
 *   | ~ label-name = ? expr : type *)
and printArgument ~state (argLbl, arg) cmtTbl =
  match (argLbl, arg) with
  (* ~a (punned)*)
  | ( Labelled lbl,
      ({
         pexp_desc = Pexp_ident {txt = Longident.Lident name};
         pexp_attributes = [] | [({Location.txt = "res.namedArgLoc"}, _)];
       } as argExpr) )
    when lbl = name && not (ParsetreeViewer.isBracedExpr argExpr) ->
    let loc =
      match arg.pexp_attributes with
      | ({Location.txt = "res.namedArgLoc"; loc}, _) :: _ -> loc
      | _ -> arg.pexp_loc
    in
    let doc = Doc.concat [Doc.tilde; printIdentLike lbl] in
    printComments doc cmtTbl loc
  (* ~a: int (punned)*)
  | ( Labelled lbl,
      {
        pexp_desc =
          Pexp_constraint
            ( ({pexp_desc = Pexp_ident {txt = Longident.Lident name}} as argExpr),
              typ );
        pexp_loc;
        pexp_attributes =
          ([] | [({Location.txt = "res.namedArgLoc"}, _)]) as attrs;
      } )
    when lbl = name && not (ParsetreeViewer.isBracedExpr argExpr) ->
    let loc =
      match attrs with
      | ({Location.txt = "res.namedArgLoc"; loc}, _) :: _ ->
        {loc with loc_end = pexp_loc.loc_end}
      | _ -> arg.pexp_loc
    in
    let doc =
      Doc.concat
        [
          Doc.tilde;
          printIdentLike lbl;
          Doc.text ": ";
          printTypExpr ~state typ cmtTbl;
        ]
    in
    printComments doc cmtTbl loc
  (* ~a? (optional lbl punned)*)
  | ( Optional lbl,
      {
        pexp_desc = Pexp_ident {txt = Longident.Lident name};
        pexp_attributes = [] | [({Location.txt = "res.namedArgLoc"}, _)];
      } )
    when lbl = name ->
    let loc =
      match arg.pexp_attributes with
      | ({Location.txt = "res.namedArgLoc"; loc}, _) :: _ -> loc
      | _ -> arg.pexp_loc
    in
    let doc = Doc.concat [Doc.tilde; printIdentLike lbl; Doc.question] in
    printComments doc cmtTbl loc
  | _lbl, expr ->
    let argLoc, expr =
      match expr.pexp_attributes with
      | ({Location.txt = "res.namedArgLoc"; loc}, _) :: attrs ->
        (loc, {expr with pexp_attributes = attrs})
      | _ -> (expr.pexp_loc, expr)
    in
    let printedLbl, dotdotdot =
      match argLbl with
      | Nolabel -> (Doc.nil, false)
      | Labelled "..." ->
        let doc = Doc.text "..." in
        (printComments doc cmtTbl argLoc, true)
      | Labelled lbl ->
        let doc = Doc.concat [Doc.tilde; printIdentLike lbl; Doc.equal] in
        (printComments doc cmtTbl argLoc, false)
      | Optional lbl ->
        let doc =
          Doc.concat [Doc.tilde; printIdentLike lbl; Doc.equal; Doc.question]
        in
        (printComments doc cmtTbl argLoc, false)
    in
    let printedExpr =
      let doc = printExpressionWithComments ~state expr cmtTbl in
      match Parens.expr expr with
      | Parenthesized -> addParens doc
      | Braced braces -> printBraces doc expr braces
      | Nothing -> doc
    in
    let loc = {argLoc with loc_end = expr.pexp_loc.loc_end} in
    let doc =
      if dotdotdot then printedLbl else Doc.concat [printedLbl; printedExpr]
    in
    printComments doc cmtTbl loc

and printCases ~state (cases : Parsetree.case list) cmtTbl =
  Doc.breakableGroup ~forceBreak:true
    (Doc.concat
       [
         Doc.lbrace;
         Doc.concat
           [
             Doc.line;
             printList
               ~getLoc:(fun n ->
                 {
                   n.Parsetree.pc_lhs.ppat_loc with
                   loc_end =
                     (match ParsetreeViewer.processBracesAttr n.pc_rhs with
                     | None, _ -> n.pc_rhs.pexp_loc.loc_end
                     | Some ({loc}, _), _ -> loc.Location.loc_end);
                 })
               ~print:(printCase ~state) ~nodes:cases cmtTbl;
           ];
         Doc.line;
         Doc.rbrace;
       ])

and printCase ~state (case : Parsetree.case) cmtTbl =
  let rhs =
    match case.pc_rhs.pexp_desc with
    | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _ | Pexp_open _
    | Pexp_sequence _ ->
      printExpressionBlock ~state
        ~braces:(ParsetreeViewer.isBracedExpr case.pc_rhs)
        case.pc_rhs cmtTbl
    | _ -> (
      let doc = printExpressionWithComments ~state case.pc_rhs cmtTbl in
      match Parens.expr case.pc_rhs with
      | Parenthesized -> addParens doc
      | _ -> doc)
  in

  let guard =
    match case.pc_guard with
    | None -> Doc.nil
    | Some expr ->
      Doc.group
        (Doc.concat
           [
             Doc.line;
             Doc.text "if ";
             printExpressionWithComments ~state expr cmtTbl;
           ])
  in
  let shouldInlineRhs =
    match case.pc_rhs.pexp_desc with
    | Pexp_construct ({txt = Longident.Lident ("()" | "true" | "false")}, _)
    | Pexp_constant _ | Pexp_ident _ ->
      true
    | _ when ParsetreeViewer.isHuggableRhs case.pc_rhs -> true
    | _ -> false
  in
  let shouldIndentPattern =
    match case.pc_lhs.ppat_desc with
    | Ppat_or _ -> false
    | _ -> true
  in
  let patternDoc =
    let doc = printPattern ~state case.pc_lhs cmtTbl in
    match case.pc_lhs.ppat_desc with
    | Ppat_constraint _ -> addParens doc
    | _ -> doc
  in
  let content =
    Doc.concat
      [
        (if shouldIndentPattern then Doc.indent patternDoc else patternDoc);
        Doc.indent guard;
        Doc.text " =>";
        Doc.indent
          (Doc.concat [(if shouldInlineRhs then Doc.space else Doc.line); rhs]);
      ]
  in
  Doc.group (Doc.concat [Doc.text "| "; content])

and printExprFunParameters ~state ~inCallback ~async ~uncurried ~hasConstraint
    parameters cmtTbl =
  let dotted = state.uncurried_config |> Res_uncurried.getDotted ~uncurried in
  match parameters with
  (* let f = _ => () *)
  | [
   ParsetreeViewer.Parameter
     {
       attrs = [];
       lbl = Asttypes.Nolabel;
       defaultExpr = None;
       pat = {Parsetree.ppat_desc = Ppat_any; ppat_loc};
     };
  ]
    when not dotted ->
    let any =
      let doc = if hasConstraint then Doc.text "(_)" else Doc.text "_" in
      printComments doc cmtTbl ppat_loc
    in
    if async then addAsync any else any
  (* let f = a => () *)
  | [
   ParsetreeViewer.Parameter
     {
       attrs = [];
       lbl = Asttypes.Nolabel;
       defaultExpr = None;
       pat =
         {
           Parsetree.ppat_desc = Ppat_var stringLoc;
           Parsetree.ppat_attributes = attrs;
         };
     };
  ]
    when not dotted ->
    let txtDoc =
      let var = printIdentLike stringLoc.txt in
      let var =
        match attrs with
        | [] -> if hasConstraint then addParens var else var
        | attrs ->
          let attrs = printAttributes ~state attrs cmtTbl in
          addParens (Doc.concat [attrs; var])
      in
      if async then addAsync var else var
    in
    printComments txtDoc cmtTbl stringLoc.loc
  (* let f = () => () *)
  | [
   ParsetreeViewer.Parameter
     {
       attrs = [];
       lbl = Asttypes.Nolabel;
       defaultExpr = None;
       pat =
         {ppat_desc = Ppat_construct ({txt = Longident.Lident "()"; loc}, None)};
     };
  ]
    when not dotted ->
    let doc =
      let lparenRparen = Doc.text "()" in
      if async then addAsync lparenRparen else lparenRparen
    in
    printComments doc cmtTbl loc
  (* let f = (~greeting, ~from as hometown, ~x=?) => () *)
  | parameters ->
    let inCallback =
      match inCallback with
      | FitsOnOneLine -> true
      | _ -> false
    in
    let maybeAsyncLparen =
      let lparen = if dotted then Doc.text "(. " else Doc.lparen in
      if async then addAsync lparen else lparen
    in
    let shouldHug = ParsetreeViewer.parametersShouldHug parameters in
    let printedParamaters =
      Doc.concat
        [
          (if shouldHug || inCallback then Doc.nil else Doc.softLine);
          Doc.join
            ~sep:(Doc.concat [Doc.comma; Doc.line])
            (List.map
               (fun p -> printExpFunParameter ~state p cmtTbl)
               parameters);
        ]
    in
    Doc.group
      (Doc.concat
         [
           maybeAsyncLparen;
           (if shouldHug || inCallback then printedParamaters
            else
              Doc.concat
                [Doc.indent printedParamaters; Doc.trailingComma; Doc.softLine]);
           Doc.rparen;
         ])

and printExpFunParameter ~state parameter cmtTbl =
  match parameter with
  | ParsetreeViewer.NewTypes {attrs; locs = lbls} ->
    Doc.group
      (Doc.concat
         [
           printAttributes ~state attrs cmtTbl;
           Doc.text "type ";
           (* XX *)
           Doc.join ~sep:Doc.space
             (List.map
                (fun lbl ->
                  printComments
                    (printIdentLike lbl.Asttypes.txt)
                    cmtTbl lbl.Asttypes.loc)
                lbls);
         ])
  | Parameter {attrs; lbl; defaultExpr; pat = pattern} ->
    let hasBs, attrs = ParsetreeViewer.processBsAttribute attrs in
    let dotted = if hasBs then Doc.concat [Doc.dot; Doc.space] else Doc.nil in
    let attrs = printAttributes ~state attrs cmtTbl in
    (* =defaultValue *)
    let defaultExprDoc =
      match defaultExpr with
      | Some expr ->
        Doc.concat
          [Doc.text "="; printExpressionWithComments ~state expr cmtTbl]
      | None -> Doc.nil
    in
    (* ~from as hometown
     * ~from                   ->  punning *)
    let labelWithPattern =
      match (lbl, pattern) with
      | Asttypes.Nolabel, pattern -> printPattern ~state pattern cmtTbl
      | ( (Asttypes.Labelled lbl | Optional lbl),
          {ppat_desc = Ppat_var stringLoc; ppat_attributes} )
        when lbl = stringLoc.txt ->
        (* ~d *)
        Doc.concat
          [
            printAttributes ~state ppat_attributes cmtTbl;
            Doc.text "~";
            printIdentLike lbl;
          ]
      | ( (Asttypes.Labelled lbl | Optional lbl),
          {
            ppat_desc = Ppat_constraint ({ppat_desc = Ppat_var {txt}}, typ);
            ppat_attributes;
          } )
        when lbl = txt ->
        (* ~d: e *)
        Doc.concat
          [
            printAttributes ~state ppat_attributes cmtTbl;
            Doc.text "~";
            printIdentLike lbl;
            Doc.text ": ";
            printTypExpr ~state typ cmtTbl;
          ]
      | (Asttypes.Labelled lbl | Optional lbl), pattern ->
        (* ~b as c *)
        Doc.concat
          [
            Doc.text "~";
            printIdentLike lbl;
            Doc.text " as ";
            printPattern ~state pattern cmtTbl;
          ]
    in
    let optionalLabelSuffix =
      match (lbl, defaultExpr) with
      | Asttypes.Optional _, None -> Doc.text "=?"
      | _ -> Doc.nil
    in
    let doc =
      Doc.group
        (Doc.concat
           [
             dotted; attrs; labelWithPattern; defaultExprDoc; optionalLabelSuffix;
           ])
    in
    let cmtLoc =
      match defaultExpr with
      | None -> (
        match pattern.ppat_attributes with
        | ({Location.txt = "res.namedArgLoc"; loc}, _) :: _ ->
          {loc with loc_end = pattern.ppat_loc.loc_end}
        | _ -> pattern.ppat_loc)
      | Some expr ->
        let startPos =
          match pattern.ppat_attributes with
          | ({Location.txt = "res.namedArgLoc"; loc}, _) :: _ -> loc.loc_start
          | _ -> pattern.ppat_loc.loc_start
        in
        {
          pattern.ppat_loc with
          loc_start = startPos;
          loc_end = expr.pexp_loc.loc_end;
        }
    in
    printComments doc cmtTbl cmtLoc

and printExpressionBlock ~state ~braces expr cmtTbl =
  let rec collectRows acc expr =
    match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_letmodule (modName, modExpr, expr2) ->
      let name =
        let doc = Doc.text modName.txt in
        printComments doc cmtTbl modName.loc
      in
      let name, modExpr =
        match modExpr.pmod_desc with
        | Pmod_constraint (modExpr2, modType)
          when not (ParsetreeViewer.hasAwaitAttribute modExpr.pmod_attributes)
          ->
          let name =
            Doc.concat [name; Doc.text ": "; printModType ~state modType cmtTbl]
          in
          (name, modExpr2)
        | _ -> (name, modExpr)
      in
      let letModuleDoc =
        Doc.concat
          [
            Doc.text "module ";
            name;
            Doc.text " = ";
            printModExpr ~state modExpr cmtTbl;
          ]
      in
      let loc = {expr.pexp_loc with loc_end = modExpr.pmod_loc.loc_end} in
      collectRows ((loc, letModuleDoc) :: acc) expr2
    | Pexp_letexception (extensionConstructor, expr2) ->
      let loc =
        let loc =
          {expr.pexp_loc with loc_end = extensionConstructor.pext_loc.loc_end}
        in
        match getFirstLeadingComment cmtTbl loc with
        | None -> loc
        | Some comment ->
          let cmtLoc = Comment.loc comment in
          {cmtLoc with loc_end = loc.loc_end}
      in
      let letExceptionDoc =
        printExceptionDef ~state extensionConstructor cmtTbl
      in
      collectRows ((loc, letExceptionDoc) :: acc) expr2
    | Pexp_open (overrideFlag, longidentLoc, expr2) ->
      let openDoc =
        Doc.concat
          [
            Doc.text "open";
            printOverrideFlag overrideFlag;
            Doc.space;
            printLongidentLocation longidentLoc cmtTbl;
          ]
      in
      let loc = {expr.pexp_loc with loc_end = longidentLoc.loc.loc_end} in
      collectRows ((loc, openDoc) :: acc) expr2
    | Pexp_sequence (expr1, expr2) ->
      let exprDoc =
        let doc = printExpression ~state expr1 cmtTbl in
        match Parens.expr expr1 with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc expr1 braces
        | Nothing -> doc
      in
      let loc = expr1.pexp_loc in
      collectRows ((loc, exprDoc) :: acc) expr2
    | Pexp_let (recFlag, valueBindings, expr2) -> (
      let loc =
        let loc =
          match (valueBindings, List.rev valueBindings) with
          | vb :: _, lastVb :: _ ->
            {vb.pvb_loc with loc_end = lastVb.pvb_loc.loc_end}
          | _ -> Location.none
        in
        match getFirstLeadingComment cmtTbl loc with
        | None -> loc
        | Some comment ->
          let cmtLoc = Comment.loc comment in
          {cmtLoc with loc_end = loc.loc_end}
      in
      let recFlag =
        match recFlag with
        | Asttypes.Nonrecursive -> Doc.nil
        | Asttypes.Recursive -> Doc.text "rec "
      in
      let letDoc = printValueBindings ~state ~recFlag valueBindings cmtTbl in
      (* let () = {
       *   let () = foo()
       *   ()
       * }
       * We don't need to print the () on the last line of the block
       *)
      match expr2.pexp_desc with
      | Pexp_construct ({txt = Longident.Lident "()"}, _) ->
        List.rev ((loc, letDoc) :: acc)
      | _ -> collectRows ((loc, letDoc) :: acc) expr2)
    | _ ->
      let exprDoc =
        let doc = printExpression ~state expr cmtTbl in
        match Parens.expr expr with
        | Parens.Parenthesized -> addParens doc
        | Braced braces -> printBraces doc expr braces
        | Nothing -> doc
      in
      List.rev ((expr.pexp_loc, exprDoc) :: acc)
  in
  let rows = collectRows [] expr in
  let block =
    printList ~getLoc:fst ~nodes:rows
      ~print:(fun (_, doc) _ -> doc)
      ~forceBreak:true cmtTbl
  in
  Doc.breakableGroup ~forceBreak:true
    (if braces then
       Doc.concat
         [
           Doc.lbrace;
           Doc.indent (Doc.concat [Doc.line; block]);
           Doc.line;
           Doc.rbrace;
         ]
     else block)

(*
 * // user types:
 * let f = (a, b) => { a + b }
 *
 * // printer: everything is on one line
 * let f = (a, b) => { a + b }
 *
 * // user types: over multiple lines
 * let f = (a, b) => {
 *   a + b
 * }
 *
 * // printer: over multiple lines
 * let f = (a, b) => {
 *   a + b
 * }
 *)
and printBraces doc expr bracesLoc =
  let overMultipleLines =
    let open Location in
    bracesLoc.loc_end.pos_lnum > bracesLoc.loc_start.pos_lnum
  in
  match expr.Parsetree.pexp_desc with
  | Pexp_letmodule _ | Pexp_letexception _ | Pexp_let _ | Pexp_open _
  | Pexp_sequence _ ->
    (* already has braces *)
    doc
  | _ ->
    Doc.breakableGroup ~forceBreak:overMultipleLines
      (Doc.concat
         [
           Doc.lbrace;
           Doc.indent
             (Doc.concat
                [
                  Doc.softLine;
                  (if Parens.bracedExpr expr then addParens doc else doc);
                ]);
           Doc.softLine;
           Doc.rbrace;
         ])

and printOverrideFlag overrideFlag =
  match overrideFlag with
  | Asttypes.Override -> Doc.text "!"
  | Fresh -> Doc.nil

and printDirectionFlag flag =
  match flag with
  | Asttypes.Downto -> Doc.text " downto "
  | Asttypes.Upto -> Doc.text " to "

and printExpressionRecordRow ~state (lbl, expr) cmtTbl punningAllowed =
  let cmtLoc = {lbl.loc with loc_end = expr.pexp_loc.loc_end} in
  let doc =
    Doc.group
      (match expr.pexp_desc with
      | Pexp_ident {txt = Lident key; loc = _keyLoc}
        when punningAllowed && Longident.last lbl.txt = key ->
        (* print punned field *)
        Doc.concat
          [
            printAttributes ~state expr.pexp_attributes cmtTbl;
            printOptionalLabel expr.pexp_attributes;
            printLidentPath lbl cmtTbl;
          ]
      | _ ->
        Doc.concat
          [
            printLidentPath lbl cmtTbl;
            Doc.text ": ";
            printOptionalLabel expr.pexp_attributes;
            (let doc = printExpressionWithComments ~state expr cmtTbl in
             match Parens.exprRecordRowRhs expr with
             | Parens.Parenthesized -> addParens doc
             | Braced braces -> printBraces doc expr braces
             | Nothing -> doc);
          ])
  in
  printComments doc cmtTbl cmtLoc

and printBsObjectRow ~state (lbl, expr) cmtTbl =
  let cmtLoc = {lbl.loc with loc_end = expr.pexp_loc.loc_end} in
  let lblDoc =
    let doc =
      Doc.concat [Doc.text "\""; printLongident lbl.txt; Doc.text "\""]
    in
    printComments doc cmtTbl lbl.loc
  in
  let doc =
    Doc.concat
      [
        lblDoc;
        Doc.text ": ";
        (let doc = printExpressionWithComments ~state expr cmtTbl in
         match Parens.expr expr with
         | Parens.Parenthesized -> addParens doc
         | Braced braces -> printBraces doc expr braces
         | Nothing -> doc);
      ]
  in
  printComments doc cmtTbl cmtLoc

(* The optional loc indicates whether we need to print the attributes in
 * relation to some location. In practise this means the following:
 *  `@attr type t = string` -> on the same line, print on the same line
 *  `@attr
 *   type t = string` -> attr is on prev line, print the attributes
 *   with a line break between, we respect the users' original layout *)
and printAttributes ?loc ?(inline = false) ~state (attrs : Parsetree.attributes)
    cmtTbl =
  match ParsetreeViewer.filterParsingAttrs attrs with
  | [] -> Doc.nil
  | attrs ->
    let lineBreak =
      match loc with
      | None -> Doc.line
      | Some loc -> (
        match List.rev attrs with
        | ({loc = firstLoc}, _) :: _
          when loc.loc_start.pos_lnum > firstLoc.loc_end.pos_lnum ->
          Doc.hardLine
        | _ -> Doc.line)
    in
    Doc.concat
      [
        Doc.group
          (Doc.joinWithSep
             (List.map (fun attr -> printAttribute ~state attr cmtTbl) attrs));
        (if inline then Doc.space else lineBreak);
      ]

and printPayload ~state (payload : Parsetree.payload) cmtTbl =
  match payload with
  | PStr [] -> Doc.nil
  | PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
    let exprDoc = printExpressionWithComments ~state expr cmtTbl in
    let needsParens =
      match attrs with
      | [] -> false
      | _ -> true
    in
    let shouldHug = ParsetreeViewer.isHuggableExpression expr in
    if shouldHug then
      Doc.concat
        [
          Doc.lparen;
          printAttributes ~state attrs cmtTbl;
          (if needsParens then addParens exprDoc else exprDoc);
          Doc.rparen;
        ]
    else
      Doc.concat
        [
          Doc.lparen;
          Doc.indent
            (Doc.concat
               [
                 Doc.softLine;
                 printAttributes ~state attrs cmtTbl;
                 (if needsParens then addParens exprDoc else exprDoc);
               ]);
          Doc.softLine;
          Doc.rparen;
        ]
  | PStr [({pstr_desc = Pstr_value (_recFlag, _bindings)} as si)] ->
    addParens (printStructureItem ~state si cmtTbl)
  | PStr structure -> addParens (printStructure ~state structure cmtTbl)
  | PTyp typ ->
    Doc.concat
      [
        Doc.lparen;
        Doc.text ":";
        Doc.indent (Doc.concat [Doc.line; printTypExpr ~state typ cmtTbl]);
        Doc.softLine;
        Doc.rparen;
      ]
  | PPat (pat, optExpr) ->
    let whenDoc =
      match optExpr with
      | Some expr ->
        Doc.concat
          [
            Doc.line;
            Doc.text "if ";
            printExpressionWithComments ~state expr cmtTbl;
          ]
      | None -> Doc.nil
    in
    Doc.concat
      [
        Doc.lparen;
        Doc.indent
          (Doc.concat
             [
               Doc.softLine;
               Doc.text "? ";
               printPattern ~state pat cmtTbl;
               whenDoc;
             ]);
        Doc.softLine;
        Doc.rparen;
      ]
  | PSig signature ->
    Doc.concat
      [
        Doc.lparen;
        Doc.text ":";
        Doc.indent
          (Doc.concat [Doc.line; printSignature ~state signature cmtTbl]);
        Doc.softLine;
        Doc.rparen;
      ]

and printAttribute ?(standalone = false) ~state
    ((id, payload) : Parsetree.attribute) cmtTbl =
  match (id, payload) with
  | ( {txt = "res.doc"},
      PStr
        [
          {
            pstr_desc =
              Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (txt, _))}, _);
          };
        ] ) ->
    ( Doc.concat
        [
          Doc.text (if standalone then "/***" else "/**");
          Doc.text txt;
          Doc.text "*/";
        ],
      Doc.hardLine )
  | _ ->
    let id =
      match id.txt with
      | "uncurried.swap" ->
        state.uncurried_config <- Config.Swap;
        id
      | "uncurried" ->
        state.uncurried_config <- Config.Uncurried;
        id
      | _ -> id
    in
    ( Doc.group
        (Doc.concat
           [
             Doc.text (if standalone then "@@" else "@");
             Doc.text (convertBsExternalAttribute id.txt);
             printPayload ~state payload cmtTbl;
           ]),
      Doc.line )

and printModExpr ~state modExpr cmtTbl =
  let doc =
    match modExpr.pmod_desc with
    | Pmod_ident longidentLoc -> printLongidentLocation longidentLoc cmtTbl
    | Pmod_structure [] ->
      let shouldBreak =
        modExpr.pmod_loc.loc_start.pos_lnum < modExpr.pmod_loc.loc_end.pos_lnum
      in
      Doc.breakableGroup ~forceBreak:shouldBreak
        (Doc.concat
           [Doc.lbrace; printCommentsInside cmtTbl modExpr.pmod_loc; Doc.rbrace])
    | Pmod_structure structure ->
      Doc.breakableGroup ~forceBreak:true
        (Doc.concat
           [
             Doc.lbrace;
             Doc.indent
               (Doc.concat
                  [Doc.softLine; printStructure ~state structure cmtTbl]);
             Doc.softLine;
             Doc.rbrace;
           ])
    | Pmod_unpack expr ->
      let shouldHug =
        match expr.pexp_desc with
        | Pexp_let _ -> true
        | Pexp_constraint
            ({pexp_desc = Pexp_let _}, {ptyp_desc = Ptyp_package _packageType})
          ->
          true
        | _ -> false
      in
      let expr, moduleConstraint =
        match expr.pexp_desc with
        | Pexp_constraint
            (expr, {ptyp_desc = Ptyp_package packageType; ptyp_loc}) ->
          let packageDoc =
            let doc =
              printPackageType ~state ~printModuleKeywordAndParens:false
                packageType cmtTbl
            in
            printComments doc cmtTbl ptyp_loc
          in
          let typeDoc =
            Doc.group
              (Doc.concat
                 [Doc.text ":"; Doc.indent (Doc.concat [Doc.line; packageDoc])])
          in
          (expr, typeDoc)
        | _ -> (expr, Doc.nil)
      in
      let unpackDoc =
        Doc.group
          (Doc.concat
             [printExpressionWithComments ~state expr cmtTbl; moduleConstraint])
      in
      Doc.group
        (Doc.concat
           [
             Doc.text "unpack(";
             (if shouldHug then unpackDoc
              else
                Doc.concat
                  [
                    Doc.indent (Doc.concat [Doc.softLine; unpackDoc]);
                    Doc.softLine;
                  ]);
             Doc.rparen;
           ])
    | Pmod_extension extension ->
      printExtension ~state ~atModuleLvl:false extension cmtTbl
    | Pmod_apply _ ->
      let args, callExpr = ParsetreeViewer.modExprApply modExpr in
      let isUnitSugar =
        match args with
        | [{pmod_desc = Pmod_structure []}] -> true
        | _ -> false
      in
      let shouldHug =
        match args with
        | [{pmod_desc = Pmod_structure _}] -> true
        | _ -> false
      in
      Doc.group
        (Doc.concat
           [
             printModExpr ~state callExpr cmtTbl;
             (if isUnitSugar then
                printModApplyArg ~state (List.hd args [@doesNotRaise]) cmtTbl
              else
                Doc.concat
                  [
                    Doc.lparen;
                    (if shouldHug then
                       printModApplyArg ~state
                         (List.hd args [@doesNotRaise])
                         cmtTbl
                     else
                       Doc.indent
                         (Doc.concat
                            [
                              Doc.softLine;
                              Doc.join
                                ~sep:(Doc.concat [Doc.comma; Doc.line])
                                (List.map
                                   (fun modArg ->
                                     printModApplyArg ~state modArg cmtTbl)
                                   args);
                            ]));
                    (if not shouldHug then
                       Doc.concat [Doc.trailingComma; Doc.softLine]
                     else Doc.nil);
                    Doc.rparen;
                  ]);
           ])
    | Pmod_constraint (modExpr, modType) ->
      Doc.concat
        [
          printModExpr ~state modExpr cmtTbl;
          Doc.text ": ";
          printModType ~state modType cmtTbl;
        ]
    | Pmod_functor _ -> printModFunctor ~state modExpr cmtTbl
  in
  let doc =
    if ParsetreeViewer.hasAwaitAttribute modExpr.pmod_attributes then
      match modExpr.pmod_desc with
      | Pmod_constraint _ ->
        Doc.concat [Doc.text "await "; Doc.lparen; doc; Doc.rparen]
      | _ -> Doc.concat [Doc.text "await "; doc]
    else doc
  in
  printComments doc cmtTbl modExpr.pmod_loc

and printModFunctor ~state modExpr cmtTbl =
  let parameters, returnModExpr = ParsetreeViewer.modExprFunctor modExpr in
  (* let shouldInline = match returnModExpr.pmod_desc with *)
  (* | Pmod_structure _ | Pmod_ident _ -> true *)
  (* | Pmod_constraint ({pmod_desc = Pmod_structure _}, _) -> true *)
  (* | _ -> false *)
  (* in *)
  let returnConstraint, returnModExpr =
    match returnModExpr.pmod_desc with
    | Pmod_constraint (modExpr, modType) ->
      let constraintDoc =
        let doc = printModType ~state modType cmtTbl in
        if Parens.modExprFunctorConstraint modType then addParens doc else doc
      in
      let modConstraint = Doc.concat [Doc.text ": "; constraintDoc] in
      (modConstraint, printModExpr ~state modExpr cmtTbl)
    | _ -> (Doc.nil, printModExpr ~state returnModExpr cmtTbl)
  in
  let parametersDoc =
    match parameters with
    | [(attrs, {txt = "*"}, None)] ->
      Doc.group
        (Doc.concat [printAttributes ~state attrs cmtTbl; Doc.text "()"])
    | [([], {txt = lbl}, None)] -> Doc.text lbl
    | parameters ->
      Doc.group
        (Doc.concat
           [
             Doc.lparen;
             Doc.indent
               (Doc.concat
                  [
                    Doc.softLine;
                    Doc.join
                      ~sep:(Doc.concat [Doc.comma; Doc.line])
                      (List.map
                         (fun param -> printModFunctorParam ~state param cmtTbl)
                         parameters);
                  ]);
             Doc.trailingComma;
             Doc.softLine;
             Doc.rparen;
           ])
  in
  Doc.group
    (Doc.concat
       [parametersDoc; returnConstraint; Doc.text " => "; returnModExpr])

and printModFunctorParam ~state (attrs, lbl, optModType) cmtTbl =
  let cmtLoc =
    match optModType with
    | None -> lbl.Asttypes.loc
    | Some modType ->
      {lbl.loc with loc_end = modType.Parsetree.pmty_loc.loc_end}
  in
  let attrs = printAttributes ~state attrs cmtTbl in
  let lblDoc =
    let doc = if lbl.txt = "*" then Doc.text "()" else Doc.text lbl.txt in
    printComments doc cmtTbl lbl.loc
  in
  let doc =
    Doc.group
      (Doc.concat
         [
           attrs;
           lblDoc;
           (match optModType with
           | None -> Doc.nil
           | Some modType ->
             Doc.concat [Doc.text ": "; printModType ~state modType cmtTbl]);
         ])
  in
  printComments doc cmtTbl cmtLoc

and printModApplyArg ~state modExpr cmtTbl =
  match modExpr.pmod_desc with
  | Pmod_structure [] -> Doc.text "()"
  | _ -> printModExpr ~state modExpr cmtTbl

and printExceptionDef ~state (constr : Parsetree.extension_constructor) cmtTbl =
  let kind =
    match constr.pext_kind with
    | Pext_rebind longident ->
      Doc.indent
        (Doc.concat
           [Doc.text " ="; Doc.line; printLongidentLocation longident cmtTbl])
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadtDoc =
        match gadt with
        | Some typ -> Doc.concat [Doc.text ": "; printTypExpr ~state typ cmtTbl]
        | None -> Doc.nil
      in
      Doc.concat
        [printConstructorArguments ~state ~indent:false args cmtTbl; gadtDoc]
  in
  let name =
    printComments (Doc.text constr.pext_name.txt) cmtTbl constr.pext_name.loc
  in
  let doc =
    Doc.group
      (Doc.concat
         [
           printAttributes ~state constr.pext_attributes cmtTbl;
           Doc.text "exception ";
           name;
           kind;
         ])
  in
  printComments doc cmtTbl constr.pext_loc

and printExtensionConstructor ~state (constr : Parsetree.extension_constructor)
    cmtTbl i =
  let attrs = printAttributes ~state constr.pext_attributes cmtTbl in
  let bar =
    if i > 0 then Doc.text "| " else Doc.ifBreaks (Doc.text "| ") Doc.nil
  in
  let kind =
    match constr.pext_kind with
    | Pext_rebind longident ->
      Doc.indent
        (Doc.concat
           [Doc.text " ="; Doc.line; printLongidentLocation longident cmtTbl])
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadtDoc =
        match gadt with
        | Some typ -> Doc.concat [Doc.text ": "; printTypExpr ~state typ cmtTbl]
        | None -> Doc.nil
      in
      Doc.concat
        [printConstructorArguments ~state ~indent:false args cmtTbl; gadtDoc]
  in
  let name =
    printComments (Doc.text constr.pext_name.txt) cmtTbl constr.pext_name.loc
  in
  Doc.concat [bar; Doc.group (Doc.concat [attrs; name; kind])]

let printTypeParams params = printTypeParams ~state:(State.init ()) params
let printTypExpr t = printTypExpr ~state:(State.init ()) t
let printExpression e = printExpression ~state:(State.init ()) e
let printPattern p = printPattern ~state:(State.init ()) p

let printImplementation ~width (s : Parsetree.structure) ~comments =
  let cmtTbl = CommentTable.make () in
  CommentTable.walkStructure s cmtTbl comments;
  (* CommentTable.log cmtTbl; *)
  let doc = printStructure ~state:(State.init ()) s cmtTbl in
  (* Doc.debug doc; *)
  Doc.toString ~width doc ^ "\n"

let printInterface ~width (s : Parsetree.signature) ~comments =
  let cmtTbl = CommentTable.make () in
  CommentTable.walkSignature s cmtTbl comments;
  Doc.toString ~width (printSignature ~state:(State.init ()) s cmtTbl) ^ "\n"

let printStructure = printStructure ~state:(State.init ())
