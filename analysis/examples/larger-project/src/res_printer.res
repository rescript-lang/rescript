module Doc = Res_doc
module CommentTable = Res_comments_table
module Comment = Res_comment
module Token = Res_token
module Parens = Res_parens
module ParsetreeViewer = Res_parsetree_viewer

type callbackStyle =
  /* regular arrow function, example: `let f = x => x + 1` */
  | NoCallback
  /* `Thing.map(foo, (arg1, arg2) => MyModuleBlah.toList(argument))` */
  | FitsOnOneLine
  /* Thing.map(longArgumet, veryLooooongArgument, (arg1, arg2) =>
   *   MyModuleBlah.toList(argument)
   * )
   */
  | ArgumentsFitOnOneLine

/* Since compiler version 8.3, the bs. prefix is no longer needed */
/* Synced from
 https://github.com/rescript-lang/rescript-compiler/blob/29174de1a5fde3b16cf05d10f5ac109cfac5c4ca/jscomp/frontend/ast_external_process.ml#L291-L367 */
let convertBsExternalAttribute = x =>
  switch x {
  | "bs.as" => "as"
  | "bs.deriving" => "deriving"
  | "bs.get" => "get"
  | "bs.get_index" => "get_index"
  | "bs.ignore" => "ignore"
  | "bs.inline" => "inline"
  | "bs.int" => "int"
  | "bs.meth" => "meth"
  | "bs.module" => "module"
  | "bs.new" => "new"
  | "bs.obj" => "obj"
  | "bs.optional" => "optional"
  | "bs.return" => "return"
  | "bs.send" => "send"
  | "bs.scope" => "scope"
  | "bs.set" => "set"
  | "bs.set_index" => "set_index"
  | "bs.splice" | "bs.variadic" => "variadic"
  | "bs.string" => "string"
  | "bs.this" => "this"
  | "bs.uncurry" => "uncurry"
  | "bs.unwrap" => "unwrap"
  | "bs.val" => "val"
  /* bs.send.pipe shouldn't be transformed */
  | txt => txt
  }

/* These haven't been needed for a long time now */
/* Synced from
 https://github.com/rescript-lang/rescript-compiler/blob/29174de1a5fde3b16cf05d10f5ac109cfac5c4ca/jscomp/frontend/ast_exp_extension.ml */
let convertBsExtension = x =>
  switch x {
  | "bs.debugger" => "debugger"
  | "bs.external" => "raw"
  /* We should never see this one since we use the sugared object form, but still */
  | "bs.obj" => "obj"
  | "bs.raw" => "raw"
  | "bs.re" => "re"
  /* TODO: what about bs.time and bs.node? */
  | txt => txt
  }

let addParens = doc =>
  Doc.group(
    Doc.concat(list{
      Doc.lparen,
      Doc.indent(Doc.concat(list{Doc.softLine, doc})),
      Doc.softLine,
      Doc.rparen,
    }),
  )

let addBraces = doc =>
  Doc.group(
    Doc.concat(list{
      Doc.lbrace,
      Doc.indent(Doc.concat(list{Doc.softLine, doc})),
      Doc.softLine,
      Doc.rbrace,
    }),
  )

let getFirstLeadingComment = (tbl, loc) =>
  switch Hashtbl.find(tbl.CommentTable.leading, loc) {
  | list{comment, ..._} => Some(comment)
  | list{} => None
  | exception Not_found => None
  }

/* Checks if `loc` has a leading line comment, i.e. `// comment above` */
let hasLeadingLineComment = (tbl, loc) =>
  switch getFirstLeadingComment(tbl, loc) {
  | Some(comment) => Comment.isSingleLineComment(comment)
  | None => false
  }

let hasCommentBelow = (tbl, loc) =>
  switch Hashtbl.find(tbl.CommentTable.trailing, loc) {
  | list{comment, ..._} =>
    let commentLoc = Comment.loc(comment)
    commentLoc.Location.loc_start.pos_lnum > loc.Location.loc_end.pos_lnum
  | list{} => false
  | exception Not_found => false
  }

let printMultilineCommentContent = txt => {
  /* Turns
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
   */
  let rec indentStars = (lines, acc) =>
    switch lines {
    | list{} => Doc.nil
    | list{lastLine} =>
      let line = String.trim(lastLine)
      let doc = Doc.text(" " ++ line)
      let trailingSpace = if line == "" {
        Doc.nil
      } else {
        Doc.space
      }
      List.rev(list{trailingSpace, doc, ...acc}) |> Doc.concat
    | list{line, ...lines} =>
      let line = String.trim(line)
      if line !== "" && String.unsafe_get(line, 0) === '*' {
        let doc = Doc.text(" " ++ line)
        indentStars(lines, list{Doc.hardLine, doc, ...acc})
      } else {
        let trailingSpace = {
          let len = String.length(txt)
          if len > 0 && String.unsafe_get(txt, len - 1) == ' ' {
            Doc.space
          } else {
            Doc.nil
          }
        }

        let content = Comment.trimSpaces(txt)
        Doc.concat(list{Doc.text(content), trailingSpace})
      }
    }

  let lines = String.split_on_char('\n', txt)
  switch lines {
  | list{} => Doc.text("/* */")
  | list{line} =>
    Doc.concat(list{Doc.text("/* "), Doc.text(Comment.trimSpaces(line)), Doc.text(" */")})
  | list{first, ...rest} =>
    let firstLine = Comment.trimSpaces(first)
    Doc.concat(list{
      Doc.text("/*"),
      switch firstLine {
      | "" | "*" => Doc.nil
      | _ => Doc.space
      },
      indentStars(rest, list{Doc.hardLine, Doc.text(firstLine)}),
      Doc.text("*/"),
    })
  }
}

let printTrailingComment = (prevLoc: Location.t, nodeLoc: Location.t, comment) => {
  let singleLine = Comment.isSingleLineComment(comment)
  let content = {
    let txt = Comment.txt(comment)
    if singleLine {
      Doc.text("//" ++ txt)
    } else {
      printMultilineCommentContent(txt)
    }
  }

  let diff = {
    let cmtStart = Comment.loc(comment).loc_start
    cmtStart.pos_lnum - prevLoc.loc_end.pos_lnum
  }

  let isBelow = Comment.loc(comment).loc_start.pos_lnum > nodeLoc.loc_end.pos_lnum
  if diff > 0 || isBelow {
    Doc.concat(list{
      Doc.breakParent,
      Doc.lineSuffix(
        Doc.concat(list{
          Doc.hardLine,
          if diff > 1 {
            Doc.hardLine
          } else {
            Doc.nil
          },
          content,
        }),
      ),
    })
  } else if !singleLine {
    Doc.concat(list{Doc.space, content})
  } else {
    Doc.lineSuffix(Doc.concat(list{Doc.space, content}))
  }
}

let printLeadingComment = (~nextComment=?, comment) => {
  let singleLine = Comment.isSingleLineComment(comment)
  let content = {
    let txt = Comment.txt(comment)
    if singleLine {
      Doc.text("//" ++ txt)
    } else {
      printMultilineCommentContent(txt)
    }
  }

  let separator = Doc.concat(list{
    if singleLine {
      Doc.concat(list{Doc.hardLine, Doc.breakParent})
    } else {
      Doc.nil
    },
    switch nextComment {
    | Some(next) =>
      let nextLoc = Comment.loc(next)
      let currLoc = Comment.loc(comment)
      let diff = nextLoc.Location.loc_start.pos_lnum - currLoc.Location.loc_end.pos_lnum

      let nextSingleLine = Comment.isSingleLineComment(next)
      if singleLine && nextSingleLine {
        if diff > 1 {
          Doc.hardLine
        } else {
          Doc.nil
        }
      } else if singleLine && !nextSingleLine {
        if diff > 1 {
          Doc.hardLine
        } else {
          Doc.nil
        }
      } else if diff > 1 {
        Doc.concat(list{Doc.hardLine, Doc.hardLine})
      } else if diff === 1 {
        Doc.hardLine
      } else {
        Doc.space
      }
    | None => Doc.nil
    },
  })

  Doc.concat(list{content, separator})
}

let printCommentsInside = (cmtTbl, loc) => {
  let rec loop = (acc, comments) =>
    switch comments {
    | list{} => Doc.nil
    | list{comment} =>
      let cmtDoc = printLeadingComment(comment)
      let doc = Doc.group(Doc.concat(list{Doc.concat(List.rev(list{cmtDoc, ...acc}))}))

      doc
    | list{comment, ...list{nextComment, ..._comments} as rest} =>
      let cmtDoc = printLeadingComment(~nextComment, comment)
      loop(list{cmtDoc, ...acc}, rest)
    }

  switch Hashtbl.find(cmtTbl.CommentTable.inside, loc) {
  | exception Not_found => Doc.nil
  | comments =>
    Hashtbl.remove(cmtTbl.inside, loc)
    Doc.group(loop(list{}, comments))
  }
}

let printLeadingComments = (node, tbl, loc) => {
  let rec loop = (acc, comments) =>
    switch comments {
    | list{} => node
    | list{comment} =>
      let cmtDoc = printLeadingComment(comment)
      let diff = loc.Location.loc_start.pos_lnum - Comment.loc(comment).Location.loc_end.pos_lnum

      let separator = if Comment.isSingleLineComment(comment) {
        if diff > 1 {
          Doc.hardLine
        } else {
          Doc.nil
        }
      } else if diff === 0 {
        Doc.space
      } else if diff > 1 {
        Doc.concat(list{Doc.hardLine, Doc.hardLine})
      } else {
        Doc.hardLine
      }

      let doc = Doc.group(
        Doc.concat(list{Doc.concat(List.rev(list{cmtDoc, ...acc})), separator, node}),
      )

      doc
    | list{comment, ...list{nextComment, ..._comments} as rest} =>
      let cmtDoc = printLeadingComment(~nextComment, comment)
      loop(list{cmtDoc, ...acc}, rest)
    }

  switch Hashtbl.find(tbl, loc) {
  | exception Not_found => node
  | comments =>
    /* Remove comments from tbl: Some ast nodes have the same location.
     * We only want to print comments once */
    Hashtbl.remove(tbl, loc)
    loop(list{}, comments)
  }
}

let printTrailingComments = (node, tbl, loc) => {
  let rec loop = (prev, acc, comments) =>
    switch comments {
    | list{} => Doc.concat(List.rev(acc))
    | list{comment, ...comments} =>
      let cmtDoc = printTrailingComment(prev, loc, comment)
      loop(Comment.loc(comment), list{cmtDoc, ...acc}, comments)
    }

  switch Hashtbl.find(tbl, loc) {
  | exception Not_found => node
  | list{} => node
  | list{_first, ..._} as comments =>
    /* Remove comments from tbl: Some ast nodes have the same location.
     * We only want to print comments once */
    Hashtbl.remove(tbl, loc)
    let cmtsDoc = loop(loc, list{}, comments)
    Doc.concat(list{node, cmtsDoc})
  }
}

let printComments = (doc, tbl: CommentTable.t, loc) => {
  let docWithLeadingComments = printLeadingComments(doc, tbl.leading, loc)
  printTrailingComments(docWithLeadingComments, tbl.trailing, loc)
}

let printList = (~getLoc, ~nodes, ~print, ~forceBreak=false, t) => {
  let rec loop = (prevLoc: Location.t, acc, nodes) =>
    switch nodes {
    | list{} => (prevLoc, Doc.concat(List.rev(acc)))
    | list{node, ...nodes} =>
      let loc = getLoc(node)
      let startPos = switch getFirstLeadingComment(t, loc) {
      | None => loc.loc_start
      | Some(comment) => Comment.loc(comment).loc_start
      }

      let sep = if startPos.pos_lnum - prevLoc.loc_end.pos_lnum > 1 {
        Doc.concat(list{Doc.hardLine, Doc.hardLine})
      } else {
        Doc.hardLine
      }

      let doc = printComments(print(node, t), t, loc)
      loop(loc, list{doc, sep, ...acc}, nodes)
    }

  switch nodes {
  | list{} => Doc.nil
  | list{node, ...nodes} =>
    let firstLoc = getLoc(node)
    let doc = printComments(print(node, t), t, firstLoc)
    let (lastLoc, docs) = loop(firstLoc, list{doc}, nodes)
    let forceBreak = forceBreak || firstLoc.loc_start.pos_lnum !== lastLoc.loc_end.pos_lnum

    Doc.breakableGroup(~forceBreak, docs)
  }
}

let printListi = (~getLoc, ~nodes, ~print, ~forceBreak=false, t) => {
  let rec loop = (i, prevLoc: Location.t, acc, nodes) =>
    switch nodes {
    | list{} => (prevLoc, Doc.concat(List.rev(acc)))
    | list{node, ...nodes} =>
      let loc = getLoc(node)
      let startPos = switch getFirstLeadingComment(t, loc) {
      | None => loc.loc_start
      | Some(comment) => Comment.loc(comment).loc_start
      }

      let sep = if startPos.pos_lnum - prevLoc.loc_end.pos_lnum > 1 {
        Doc.concat(list{Doc.hardLine, Doc.hardLine})
      } else {
        Doc.line
      }

      let doc = printComments(print(node, t, i), t, loc)
      loop(i + 1, loc, list{doc, sep, ...acc}, nodes)
    }

  switch nodes {
  | list{} => Doc.nil
  | list{node, ...nodes} =>
    let firstLoc = getLoc(node)
    let doc = printComments(print(node, t, 0), t, firstLoc)
    let (lastLoc, docs) = loop(1, firstLoc, list{doc}, nodes)
    let forceBreak = forceBreak || firstLoc.loc_start.pos_lnum !== lastLoc.loc_end.pos_lnum

    Doc.breakableGroup(~forceBreak, docs)
  }
}

let rec printLongidentAux = (accu, x) =>
  switch x {
  | Longident.Lident(s) => list{Doc.text(s), ...accu}
  | Ldot(lid, s) => printLongidentAux(list{Doc.text(s), ...accu}, lid)
  | Lapply(lid1, lid2) =>
    let d1 = Doc.join(~sep=Doc.dot, printLongidentAux(list{}, lid1))
    let d2 = Doc.join(~sep=Doc.dot, printLongidentAux(list{}, lid2))
    list{Doc.concat(list{d1, Doc.lparen, d2, Doc.rparen}), ...accu}
  }

let printLongident = x =>
  switch x {
  | Longident.Lident(txt) => Doc.text(txt)
  | lid => Doc.join(~sep=Doc.dot, printLongidentAux(list{}, lid))
  }

type identifierStyle =
  | ExoticIdent
  | NormalIdent

let classifyIdentContent = (~allowUident=false, txt) =>
  if Token.isKeywordTxt(txt) {
    ExoticIdent
  } else {
    let len = String.length(txt)
    let rec loop = i =>
      if i === len {
        NormalIdent
      } else if i === 0 {
        switch String.unsafe_get(txt, i) {
        | 'A' .. 'Z' if allowUident => loop(i + 1)
        | 'a' .. 'z' | '_' => loop(i + 1)
        | _ => ExoticIdent
        }
      } else {
        switch String.unsafe_get(txt, i) {
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '\'' | '_' => loop(i + 1)
        | _ => ExoticIdent
        }
      }

    loop(0)
  }

let printIdentLike = (~allowUident=?, txt) =>
  switch classifyIdentContent(~allowUident?, txt) {
  | ExoticIdent => Doc.concat(list{Doc.text("\\\""), Doc.text(txt), Doc.text("\"")})
  | NormalIdent => Doc.text(txt)
  }

let rec unsafe_for_all_range = (s, ~start, ~finish, p) =>
  start > finish ||
    (p(String.unsafe_get(s, start)) && unsafe_for_all_range(s, ~start=start + 1, ~finish, p))

let for_all_from = (s, start, p) => {
  let len = String.length(s)
  unsafe_for_all_range(s, ~start, ~finish=len - 1, p)
}

/* See https://github.com/rescript-lang/rescript-compiler/blob/726cfa534314b586e5b5734471bc2023ad99ebd9/jscomp/ext/ext_string.ml#L510 */
let isValidNumericPolyvarNumber = (x: string) => {
  let len = String.length(x)
  len > 0 && {
      let a = Char.code(String.unsafe_get(x, 0))
      a <= 57 && if len > 1 {
          a > 48 &&
            for_all_from(x, 1, x =>
              switch x {
              | '0' .. '9' => true
              | _ => false
              }
            )
        } else {
          a >= 48
        }
    }
}

/* Exotic identifiers in poly-vars have a "lighter" syntax: #"ease-in" */
let printPolyVarIdent = txt =>
  /* numeric poly-vars don't need quotes: #644 */
  if isValidNumericPolyvarNumber(txt) {
    Doc.text(txt)
  } else {
    switch classifyIdentContent(~allowUident=true, txt) {
    | ExoticIdent => Doc.concat(list{Doc.text("\""), Doc.text(txt), Doc.text("\"")})
    | NormalIdent =>
      switch txt {
      | "" => Doc.concat(list{Doc.text("\""), Doc.text(txt), Doc.text("\"")})
      | _ => Doc.text(txt)
      }
    }
  }

let printLident = l => {
  let flatLidOpt = lid => {
    let rec flat = (accu, x) =>
      switch x {
      | Longident.Lident(s) => Some(list{s, ...accu})
      | Ldot(lid, s) => flat(list{s, ...accu}, lid)
      | Lapply(_, _) => None
      }

    flat(list{}, lid)
  }

  switch l {
  | Longident.Lident(txt) => printIdentLike(txt)
  | Longident.Ldot(path, txt) =>
    let doc = switch flatLidOpt(path) {
    | Some(txts) =>
      Doc.concat(list{
        Doc.join(~sep=Doc.dot, List.map(Doc.text, txts)),
        Doc.dot,
        printIdentLike(txt),
      })
    | None => Doc.text("printLident: Longident.Lapply is not supported")
    }

    doc
  | Lapply(_, _) => Doc.text("printLident: Longident.Lapply is not supported")
  }
}

let printLongidentLocation = (l, cmtTbl) => {
  let doc = printLongident(l.Location.txt)
  printComments(doc, cmtTbl, l.loc)
}

/* Module.SubModule.x */
let printLidentPath = (path, cmtTbl) => {
  let doc = printLident(path.Location.txt)
  printComments(doc, cmtTbl, path.loc)
}

/* Module.SubModule.x or Module.SubModule.X */
let printIdentPath = (path, cmtTbl) => {
  let doc = printLident(path.Location.txt)
  printComments(doc, cmtTbl, path.loc)
}

let printStringLoc = (sloc, cmtTbl) => {
  let doc = printIdentLike(sloc.Location.txt)
  printComments(doc, cmtTbl, sloc.loc)
}

let printStringContents = txt => {
  let lines = String.split_on_char('\n', txt)
  Doc.join(~sep=Doc.literalLine, List.map(Doc.text, lines))
}

let printConstant = (~templateLiteral=false, c) =>
  switch c {
  | Parsetree.Pconst_integer(s, suffix) =>
    switch suffix {
    | Some(c) => Doc.text(s ++ Char.escaped(c))
    | None => Doc.text(s)
    }
  | Pconst_string(txt, None) =>
    Doc.concat(list{Doc.text("\""), printStringContents(txt), Doc.text("\"")})
  | Pconst_string(txt, Some(prefix)) =>
    if prefix == "INTERNAL_RES_CHAR_CONTENTS" {
      Doc.concat(list{Doc.text("'"), Doc.text(txt), Doc.text("'")})
    } else {
      let (lquote, rquote) = if templateLiteral {
        ("`", "`")
      } else {
        ("\"", "\"")
      }

      Doc.concat(list{
        if prefix == "js" {
          Doc.nil
        } else {
          Doc.text(prefix)
        },
        Doc.text(lquote),
        printStringContents(txt),
        Doc.text(rquote),
      })
    }
  | Pconst_float(s, _) => Doc.text(s)
  | Pconst_char(c) =>
    let str = switch c {
    | '\'' => "\\'"
    | '\\' => "\\\\"
    | '\n' => "\\n"
    | '\t' => "\\t"
    | '\r' => "\\r"
    | '\b' => "\\b"
    | ' ' .. '~' as c =>
      let s = (@doesNotRaise Bytes.create)(1)
      Bytes.unsafe_set(s, 0, c)
      Bytes.unsafe_to_string(s)
    | c => Res_utf8.encodeCodePoint(Obj.magic(c))
    }

    Doc.text("'" ++ (str ++ "'"))
  }

let rec printStructure = (s: Parsetree.structure, t) =>
  switch s {
  | list{} => printCommentsInside(t, Location.none)
  | structure =>
    printList(~getLoc=s => s.Parsetree.pstr_loc, ~nodes=structure, ~print=printStructureItem, t)
  }

and printStructureItem = (si: Parsetree.structure_item, cmtTbl) =>
  switch si.pstr_desc {
  | Pstr_value(rec_flag, valueBindings) =>
    let recFlag = switch rec_flag {
    | Asttypes.Nonrecursive => Doc.nil
    | Asttypes.Recursive => Doc.text("rec ")
    }

    printValueBindings(~recFlag, valueBindings, cmtTbl)
  | Pstr_type(recFlag, typeDeclarations) =>
    let recFlag = switch recFlag {
    | Asttypes.Nonrecursive => Doc.nil
    | Asttypes.Recursive => Doc.text("rec ")
    }

    printTypeDeclarations(~recFlag, typeDeclarations, cmtTbl)
  | Pstr_primitive(valueDescription) => printValueDescription(valueDescription, cmtTbl)
  | Pstr_eval(expr, attrs) =>
    let exprDoc = {
      let doc = printExpressionWithComments(expr, cmtTbl)
      switch Parens.structureExpr(expr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr, braces)
      | Nothing => doc
      }
    }

    Doc.concat(list{printAttributes(attrs, cmtTbl), exprDoc})
  | Pstr_attribute(attr) => Doc.concat(list{Doc.text("@"), printAttribute(attr, cmtTbl)})
  | Pstr_extension(extension, attrs) =>
    Doc.concat(list{
      printAttributes(attrs, cmtTbl),
      Doc.concat(list{printExtension(~atModuleLvl=true, extension, cmtTbl)}),
    })
  | Pstr_include(includeDeclaration) => printIncludeDeclaration(includeDeclaration, cmtTbl)
  | Pstr_open(openDescription) => printOpenDescription(openDescription, cmtTbl)
  | Pstr_modtype(modTypeDecl) => printModuleTypeDeclaration(modTypeDecl, cmtTbl)
  | Pstr_module(moduleBinding) => printModuleBinding(~isRec=false, moduleBinding, cmtTbl, 0)
  | Pstr_recmodule(moduleBindings) =>
    printListi(
      ~getLoc=mb => mb.Parsetree.pmb_loc,
      ~nodes=moduleBindings,
      ~print=printModuleBinding(~isRec=true),
      cmtTbl,
    )
  | Pstr_exception(extensionConstructor) => printExceptionDef(extensionConstructor, cmtTbl)
  | Pstr_typext(typeExtension) => printTypeExtension(typeExtension, cmtTbl)
  | Pstr_class(_) | Pstr_class_type(_) => Doc.nil
  }

and printTypeExtension = (te: Parsetree.type_extension, cmtTbl) => {
  let prefix = Doc.text("type ")
  let name = printLidentPath(te.ptyext_path, cmtTbl)
  let typeParams = printTypeParams(te.ptyext_params, cmtTbl)
  let extensionConstructors = {
    let ecs = te.ptyext_constructors
    let forceBreak = switch (ecs, List.rev(ecs)) {
    | (list{first, ..._}, list{last, ..._}) =>
      first.pext_loc.loc_start.pos_lnum > te.ptyext_path.loc.loc_end.pos_lnum ||
        first.pext_loc.loc_start.pos_lnum < last.pext_loc.loc_end.pos_lnum
    | _ => false
    }

    let privateFlag = switch te.ptyext_private {
    | Asttypes.Private => Doc.concat(list{Doc.text("private"), Doc.line})
    | Public => Doc.nil
    }

    let rows = printListi(
      ~getLoc=n => n.Parsetree.pext_loc,
      ~print=printExtensionConstructor,
      ~nodes=ecs,
      ~forceBreak,
      cmtTbl,
    )

    Doc.breakableGroup(
      ~forceBreak,
      Doc.indent(
        Doc.concat(list{
          Doc.line,
          privateFlag,
          rows,
          /* Doc.join ~sep:Doc.line ( */
          /* List.mapi printExtensionConstructor ecs */
          /* ) */
        }),
      ),
    )
  }

  Doc.group(
    Doc.concat(list{
      printAttributes(~loc=te.ptyext_path.loc, te.ptyext_attributes, cmtTbl),
      prefix,
      name,
      typeParams,
      Doc.text(" +="),
      extensionConstructors,
    }),
  )
}

and printModuleBinding = (~isRec, moduleBinding, cmtTbl, i) => {
  let prefix = if i == 0 {
    Doc.concat(list{
      Doc.text("module "),
      if isRec {
        Doc.text("rec ")
      } else {
        Doc.nil
      },
    })
  } else {
    Doc.text("and ")
  }

  let (modExprDoc, modConstraintDoc) = switch moduleBinding.pmb_expr {
  | {pmod_desc: Pmod_constraint(modExpr, modType)} => (
      printModExpr(modExpr, cmtTbl),
      Doc.concat(list{Doc.text(": "), printModType(modType, cmtTbl)}),
    )
  | modExpr => (printModExpr(modExpr, cmtTbl), Doc.nil)
  }

  let modName = {
    let doc = Doc.text(moduleBinding.pmb_name.Location.txt)
    printComments(doc, cmtTbl, moduleBinding.pmb_name.loc)
  }

  let doc = Doc.concat(list{
    printAttributes(~loc=moduleBinding.pmb_name.loc, moduleBinding.pmb_attributes, cmtTbl),
    prefix,
    modName,
    modConstraintDoc,
    Doc.text(" = "),
    modExprDoc,
  })
  printComments(doc, cmtTbl, moduleBinding.pmb_loc)
}

and printModuleTypeDeclaration = (modTypeDecl: Parsetree.module_type_declaration, cmtTbl) => {
  let modName = {
    let doc = Doc.text(modTypeDecl.pmtd_name.txt)
    printComments(doc, cmtTbl, modTypeDecl.pmtd_name.loc)
  }

  Doc.concat(list{
    printAttributes(modTypeDecl.pmtd_attributes, cmtTbl),
    Doc.text("module type "),
    modName,
    switch modTypeDecl.pmtd_type {
    | None => Doc.nil
    | Some(modType) => Doc.concat(list{Doc.text(" = "), printModType(modType, cmtTbl)})
    },
  })
}

and printModType = (modType, cmtTbl) => {
  let modTypeDoc = switch modType.pmty_desc {
  | Parsetree.Pmty_ident(longident) =>
    Doc.concat(list{
      printAttributes(~loc=longident.loc, modType.pmty_attributes, cmtTbl),
      printLongidentLocation(longident, cmtTbl),
    })
  | Pmty_signature(list{}) =>
    let shouldBreak = modType.pmty_loc.loc_start.pos_lnum < modType.pmty_loc.loc_end.pos_lnum

    Doc.breakableGroup(
      ~forceBreak=shouldBreak,
      Doc.concat(list{
        Doc.lbrace,
        Doc.indent(Doc.concat(list{Doc.softLine, printCommentsInside(cmtTbl, modType.pmty_loc)})),
        Doc.softLine,
        Doc.rbrace,
      }),
    )
  | Pmty_signature(signature) =>
    let signatureDoc = Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat(list{
        Doc.lbrace,
        Doc.indent(Doc.concat(list{Doc.line, printSignature(signature, cmtTbl)})),
        Doc.line,
        Doc.rbrace,
      }),
    )
    Doc.concat(list{printAttributes(modType.pmty_attributes, cmtTbl), signatureDoc})
  | Pmty_functor(_) =>
    let (parameters, returnType) = ParsetreeViewer.functorType(modType)
    let parametersDoc = switch parameters {
    | list{} => Doc.nil
    | list{(attrs, {Location.txt: "_", loc}, Some(modType))} =>
      let cmtLoc = {...loc, loc_end: modType.Parsetree.pmty_loc.loc_end}

      let attrs = printAttributes(attrs, cmtTbl)
      let doc = Doc.concat(list{attrs, printModType(modType, cmtTbl)})
      printComments(doc, cmtTbl, cmtLoc)
    | params =>
      Doc.group(
        Doc.concat(list{
          Doc.lparen,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                List.map(((attrs, lbl, modType)) => {
                  let cmtLoc = switch modType {
                  | None => lbl.Asttypes.loc
                  | Some(modType) => {
                      ...lbl.Asttypes.loc,
                      loc_end: modType.Parsetree.pmty_loc.loc_end,
                    }
                  }

                  let attrs = printAttributes(attrs, cmtTbl)
                  let lblDoc = if lbl.Location.txt == "_" || lbl.txt == "*" {
                    Doc.nil
                  } else {
                    let doc = Doc.text(lbl.txt)
                    printComments(doc, cmtTbl, lbl.loc)
                  }

                  let doc = Doc.concat(list{
                    attrs,
                    lblDoc,
                    switch modType {
                    | None => Doc.nil
                    | Some(modType) =>
                      Doc.concat(list{
                        if lbl.txt == "_" {
                          Doc.nil
                        } else {
                          Doc.text(": ")
                        },
                        printModType(modType, cmtTbl),
                      })
                    },
                  })
                  printComments(doc, cmtTbl, cmtLoc)
                }, params),
              ),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        }),
      )
    }

    let returnDoc = {
      let doc = printModType(returnType, cmtTbl)
      if Parens.modTypeFunctorReturn(returnType) {
        addParens(doc)
      } else {
        doc
      }
    }

    Doc.group(
      Doc.concat(list{
        parametersDoc,
        Doc.group(Doc.concat(list{Doc.text(" =>"), Doc.line, returnDoc})),
      }),
    )
  | Pmty_typeof(modExpr) =>
    Doc.concat(list{Doc.text("module type of "), printModExpr(modExpr, cmtTbl)})
  | Pmty_extension(extension) => printExtension(~atModuleLvl=false, extension, cmtTbl)
  | Pmty_alias(longident) =>
    Doc.concat(list{Doc.text("module "), printLongidentLocation(longident, cmtTbl)})
  | Pmty_with(modType, withConstraints) =>
    let operand = {
      let doc = printModType(modType, cmtTbl)
      if Parens.modTypeWithOperand(modType) {
        addParens(doc)
      } else {
        doc
      }
    }

    Doc.group(
      Doc.concat(list{
        operand,
        Doc.indent(Doc.concat(list{Doc.line, printWithConstraints(withConstraints, cmtTbl)})),
      }),
    )
  }

  let attrsAlreadyPrinted = switch modType.pmty_desc {
  | Pmty_functor(_) | Pmty_signature(_) | Pmty_ident(_) => true
  | _ => false
  }

  let doc = Doc.concat(list{
    if attrsAlreadyPrinted {
      Doc.nil
    } else {
      printAttributes(modType.pmty_attributes, cmtTbl)
    },
    modTypeDoc,
  })
  printComments(doc, cmtTbl, modType.pmty_loc)
}

and printWithConstraints = (withConstraints, cmtTbl) => {
  let rows = List.mapi((i, withConstraint) =>
    Doc.group(
      Doc.concat(list{
        if i === 0 {
          Doc.text("with ")
        } else {
          Doc.text("and ")
        },
        printWithConstraint(withConstraint, cmtTbl),
      }),
    )
  , withConstraints)

  Doc.join(~sep=Doc.line, rows)
}

and printWithConstraint = (withConstraint: Parsetree.with_constraint, cmtTbl) =>
  switch withConstraint {
  /* with type X.t = ... */
  | Pwith_type(longident, typeDeclaration) =>
    Doc.group(
      printTypeDeclaration(
        ~name=printLidentPath(longident, cmtTbl),
        ~equalSign="=",
        ~recFlag=Doc.nil,
        0,
        typeDeclaration,
        CommentTable.empty,
      ),
    )
  /* with module X.Y = Z */
  | Pwith_module({txt: longident1}, {txt: longident2}) =>
    Doc.concat(list{
      Doc.text("module "),
      printLongident(longident1),
      Doc.text(" ="),
      Doc.indent(Doc.concat(list{Doc.line, printLongident(longident2)})),
    })
  /* with type X.t := ..., same format as [Pwith_type] */
  | Pwith_typesubst(longident, typeDeclaration) =>
    Doc.group(
      printTypeDeclaration(
        ~name=printLidentPath(longident, cmtTbl),
        ~equalSign=":=",
        ~recFlag=Doc.nil,
        0,
        typeDeclaration,
        CommentTable.empty,
      ),
    )
  | Pwith_modsubst({txt: longident1}, {txt: longident2}) =>
    Doc.concat(list{
      Doc.text("module "),
      printLongident(longident1),
      Doc.text(" :="),
      Doc.indent(Doc.concat(list{Doc.line, printLongident(longident2)})),
    })
  }

and printSignature = (signature, cmtTbl) =>
  switch signature {
  | list{} => printCommentsInside(cmtTbl, Location.none)
  | signature =>
    printList(
      ~getLoc=s => s.Parsetree.psig_loc,
      ~nodes=signature,
      ~print=printSignatureItem,
      cmtTbl,
    )
  }

and printSignatureItem = (si: Parsetree.signature_item, cmtTbl) =>
  switch si.psig_desc {
  | Parsetree.Psig_value(valueDescription) => printValueDescription(valueDescription, cmtTbl)
  | Psig_type(recFlag, typeDeclarations) =>
    let recFlag = switch recFlag {
    | Asttypes.Nonrecursive => Doc.nil
    | Asttypes.Recursive => Doc.text("rec ")
    }

    printTypeDeclarations(~recFlag, typeDeclarations, cmtTbl)
  | Psig_typext(typeExtension) => printTypeExtension(typeExtension, cmtTbl)
  | Psig_exception(extensionConstructor) => printExceptionDef(extensionConstructor, cmtTbl)
  | Psig_module(moduleDeclaration) => printModuleDeclaration(moduleDeclaration, cmtTbl)
  | Psig_recmodule(moduleDeclarations) => printRecModuleDeclarations(moduleDeclarations, cmtTbl)
  | Psig_modtype(modTypeDecl) => printModuleTypeDeclaration(modTypeDecl, cmtTbl)
  | Psig_open(openDescription) => printOpenDescription(openDescription, cmtTbl)
  | Psig_include(includeDescription) => printIncludeDescription(includeDescription, cmtTbl)
  | Psig_attribute(attr) => Doc.concat(list{Doc.text("@"), printAttribute(attr, cmtTbl)})
  | Psig_extension(extension, attrs) =>
    Doc.concat(list{
      printAttributes(attrs, cmtTbl),
      Doc.concat(list{printExtension(~atModuleLvl=true, extension, cmtTbl)}),
    })
  | Psig_class(_) | Psig_class_type(_) => Doc.nil
  }

and printRecModuleDeclarations = (moduleDeclarations, cmtTbl) =>
  printListi(
    ~getLoc=n => n.Parsetree.pmd_loc,
    ~nodes=moduleDeclarations,
    ~print=printRecModuleDeclaration,
    cmtTbl,
  )

and printRecModuleDeclaration = (md, cmtTbl, i) => {
  let body = switch md.pmd_type.pmty_desc {
  | Parsetree.Pmty_alias(longident) =>
    Doc.concat(list{Doc.text(" = "), printLongidentLocation(longident, cmtTbl)})
  | _ =>
    let needsParens = switch md.pmd_type.pmty_desc {
    | Pmty_with(_) => true
    | _ => false
    }

    let modTypeDoc = {
      let doc = printModType(md.pmd_type, cmtTbl)
      if needsParens {
        addParens(doc)
      } else {
        doc
      }
    }

    Doc.concat(list{Doc.text(": "), modTypeDoc})
  }

  let prefix = if i < 1 {
    "module rec "
  } else {
    "and "
  }
  Doc.concat(list{
    printAttributes(~loc=md.pmd_name.loc, md.pmd_attributes, cmtTbl),
    Doc.text(prefix),
    printComments(Doc.text(md.pmd_name.txt), cmtTbl, md.pmd_name.loc),
    body,
  })
}

and printModuleDeclaration = (md: Parsetree.module_declaration, cmtTbl) => {
  let body = switch md.pmd_type.pmty_desc {
  | Parsetree.Pmty_alias(longident) =>
    Doc.concat(list{Doc.text(" = "), printLongidentLocation(longident, cmtTbl)})
  | _ => Doc.concat(list{Doc.text(": "), printModType(md.pmd_type, cmtTbl)})
  }

  Doc.concat(list{
    printAttributes(~loc=md.pmd_name.loc, md.pmd_attributes, cmtTbl),
    Doc.text("module "),
    printComments(Doc.text(md.pmd_name.txt), cmtTbl, md.pmd_name.loc),
    body,
  })
}

and printOpenDescription = (openDescription: Parsetree.open_description, cmtTbl) =>
  Doc.concat(list{
    printAttributes(openDescription.popen_attributes, cmtTbl),
    Doc.text("open"),
    switch openDescription.popen_override {
    | Asttypes.Fresh => Doc.space
    | Asttypes.Override => Doc.text("! ")
    },
    printLongidentLocation(openDescription.popen_lid, cmtTbl),
  })

and printIncludeDescription = (includeDescription: Parsetree.include_description, cmtTbl) =>
  Doc.concat(list{
    printAttributes(includeDescription.pincl_attributes, cmtTbl),
    Doc.text("include "),
    printModType(includeDescription.pincl_mod, cmtTbl),
  })

and printIncludeDeclaration = (includeDeclaration: Parsetree.include_declaration, cmtTbl) =>
  Doc.concat(list{
    printAttributes(includeDeclaration.pincl_attributes, cmtTbl),
    Doc.text("include "),
    {
      let includeDoc = printModExpr(includeDeclaration.pincl_mod, cmtTbl)

      if Parens.includeModExpr(includeDeclaration.pincl_mod) {
        addParens(includeDoc)
      } else {
        includeDoc
      }
    },
  })

and printValueBindings = (~recFlag, vbs: list<Parsetree.value_binding>, cmtTbl) =>
  printListi(
    ~getLoc=vb => vb.Parsetree.pvb_loc,
    ~nodes=vbs,
    ~print=printValueBinding(~recFlag),
    cmtTbl,
  )

and printValueDescription = (valueDescription, cmtTbl) => {
  let isExternal = switch valueDescription.pval_prim {
  | list{} => false
  | _ => true
  }

  let attrs = printAttributes(
    ~loc=valueDescription.pval_name.loc,
    valueDescription.pval_attributes,
    cmtTbl,
  )

  let header = if isExternal {
    "external "
  } else {
    "let "
  }
  Doc.group(
    Doc.concat(list{
      attrs,
      Doc.text(header),
      printComments(
        printIdentLike(valueDescription.pval_name.txt),
        cmtTbl,
        valueDescription.pval_name.loc,
      ),
      Doc.text(": "),
      printTypExpr(valueDescription.pval_type, cmtTbl),
      if isExternal {
        Doc.group(
          Doc.concat(list{
            Doc.text(" ="),
            Doc.indent(
              Doc.concat(list{
                Doc.line,
                Doc.join(
                  ~sep=Doc.line,
                  List.map(
                    s => Doc.concat(list{Doc.text("\""), Doc.text(s), Doc.text("\"")}),
                    valueDescription.pval_prim,
                  ),
                ),
              }),
            ),
          }),
        )
      } else {
        Doc.nil
      },
    }),
  )
}

and printTypeDeclarations = (~recFlag, typeDeclarations, cmtTbl) =>
  printListi(
    ~getLoc=n => n.Parsetree.ptype_loc,
    ~nodes=typeDeclarations,
    ~print=printTypeDeclaration2(~recFlag),
    cmtTbl,
  )

/*
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
 */
and printTypeDeclaration = (
  ~name,
  ~equalSign,
  ~recFlag,
  i,
  td: Parsetree.type_declaration,
  cmtTbl,
) => {
  let attrs = printAttributes(~loc=td.ptype_loc, td.ptype_attributes, cmtTbl)
  let prefix = if i > 0 {
    Doc.text("and ")
  } else {
    Doc.concat(list{Doc.text("type "), recFlag})
  }

  let typeName = name
  let typeParams = printTypeParams(td.ptype_params, cmtTbl)
  let manifestAndKind = switch td.ptype_kind {
  | Ptype_abstract =>
    switch td.ptype_manifest {
    | None => Doc.nil
    | Some(typ) =>
      Doc.concat(list{
        Doc.concat(list{Doc.space, Doc.text(equalSign), Doc.space}),
        printPrivateFlag(td.ptype_private),
        printTypExpr(typ, cmtTbl),
      })
    }
  | Ptype_open =>
    Doc.concat(list{
      Doc.concat(list{Doc.space, Doc.text(equalSign), Doc.space}),
      printPrivateFlag(td.ptype_private),
      Doc.text(".."),
    })
  | Ptype_record(lds) =>
    let manifest = switch td.ptype_manifest {
    | None => Doc.nil
    | Some(typ) =>
      Doc.concat(list{
        Doc.concat(list{Doc.space, Doc.text(equalSign), Doc.space}),
        printTypExpr(typ, cmtTbl),
      })
    }

    Doc.concat(list{
      manifest,
      Doc.concat(list{Doc.space, Doc.text(equalSign), Doc.space}),
      printPrivateFlag(td.ptype_private),
      printRecordDeclaration(lds, cmtTbl),
    })
  | Ptype_variant(cds) =>
    let manifest = switch td.ptype_manifest {
    | None => Doc.nil
    | Some(typ) =>
      Doc.concat(list{
        Doc.concat(list{Doc.space, Doc.text(equalSign), Doc.space}),
        printTypExpr(typ, cmtTbl),
      })
    }

    Doc.concat(list{
      manifest,
      Doc.concat(list{Doc.space, Doc.text(equalSign)}),
      printConstructorDeclarations(~privateFlag=td.ptype_private, cds, cmtTbl),
    })
  }

  let constraints = printTypeDefinitionConstraints(td.ptype_cstrs)
  Doc.group(Doc.concat(list{attrs, prefix, typeName, typeParams, manifestAndKind, constraints}))
}

and printTypeDeclaration2 = (~recFlag, td: Parsetree.type_declaration, cmtTbl, i) => {
  let name = {
    let doc = printIdentLike(td.Parsetree.ptype_name.txt)
    printComments(doc, cmtTbl, td.ptype_name.loc)
  }

  let equalSign = "="
  let attrs = printAttributes(~loc=td.ptype_loc, td.ptype_attributes, cmtTbl)
  let prefix = if i > 0 {
    Doc.text("and ")
  } else {
    Doc.concat(list{Doc.text("type "), recFlag})
  }

  let typeName = name
  let typeParams = printTypeParams(td.ptype_params, cmtTbl)
  let manifestAndKind = switch td.ptype_kind {
  | Ptype_abstract =>
    switch td.ptype_manifest {
    | None => Doc.nil
    | Some(typ) =>
      Doc.concat(list{
        Doc.concat(list{Doc.space, Doc.text(equalSign), Doc.space}),
        printPrivateFlag(td.ptype_private),
        printTypExpr(typ, cmtTbl),
      })
    }
  | Ptype_open =>
    Doc.concat(list{
      Doc.concat(list{Doc.space, Doc.text(equalSign), Doc.space}),
      printPrivateFlag(td.ptype_private),
      Doc.text(".."),
    })
  | Ptype_record(lds) =>
    let manifest = switch td.ptype_manifest {
    | None => Doc.nil
    | Some(typ) =>
      Doc.concat(list{
        Doc.concat(list{Doc.space, Doc.text(equalSign), Doc.space}),
        printTypExpr(typ, cmtTbl),
      })
    }

    Doc.concat(list{
      manifest,
      Doc.concat(list{Doc.space, Doc.text(equalSign), Doc.space}),
      printPrivateFlag(td.ptype_private),
      printRecordDeclaration(lds, cmtTbl),
    })
  | Ptype_variant(cds) =>
    let manifest = switch td.ptype_manifest {
    | None => Doc.nil
    | Some(typ) =>
      Doc.concat(list{
        Doc.concat(list{Doc.space, Doc.text(equalSign), Doc.space}),
        printTypExpr(typ, cmtTbl),
      })
    }

    Doc.concat(list{
      manifest,
      Doc.concat(list{Doc.space, Doc.text(equalSign)}),
      printConstructorDeclarations(~privateFlag=td.ptype_private, cds, cmtTbl),
    })
  }

  let constraints = printTypeDefinitionConstraints(td.ptype_cstrs)
  Doc.group(Doc.concat(list{attrs, prefix, typeName, typeParams, manifestAndKind, constraints}))
}

and printTypeDefinitionConstraints = cstrs =>
  switch cstrs {
  | list{} => Doc.nil
  | cstrs =>
    Doc.indent(
      Doc.group(
        Doc.concat(list{
          Doc.line,
          Doc.group(Doc.join(~sep=Doc.line, List.map(printTypeDefinitionConstraint, cstrs))),
        }),
      ),
    )
  }

and printTypeDefinitionConstraint = (
  (typ1, typ2, _loc): (Parsetree.core_type, Parsetree.core_type, Location.t),
) =>
  Doc.concat(list{
    Doc.text("constraint "),
    printTypExpr(typ1, CommentTable.empty),
    Doc.text(" = "),
    printTypExpr(typ2, CommentTable.empty),
  })

and printPrivateFlag = (flag: Asttypes.private_flag) =>
  switch flag {
  | Private => Doc.text("private ")
  | Public => Doc.nil
  }

and printTypeParams = (typeParams, cmtTbl) =>
  switch typeParams {
  | list{} => Doc.nil
  | typeParams =>
    Doc.group(
      Doc.concat(list{
        Doc.lessThan,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(typeParam => {
                let doc = printTypeParam(typeParam, cmtTbl)
                printComments(doc, cmtTbl, fst(typeParam).Parsetree.ptyp_loc)
              }, typeParams)),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.greaterThan,
      }),
    )
  }

and printTypeParam = (param: (Parsetree.core_type, Asttypes.variance), cmtTbl) => {
  let (typ, variance) = param
  let printedVariance = switch variance {
  | Covariant => Doc.text("+")
  | Contravariant => Doc.text("-")
  | Invariant => Doc.nil
  }

  Doc.concat(list{printedVariance, printTypExpr(typ, cmtTbl)})
}

and printRecordDeclaration = (lds: list<Parsetree.label_declaration>, cmtTbl) => {
  let forceBreak = switch (lds, List.rev(lds)) {
  | (list{first, ..._}, list{last, ..._}) =>
    first.pld_loc.loc_start.pos_lnum < last.pld_loc.loc_end.pos_lnum
  | _ => false
  }

  Doc.breakableGroup(
    ~forceBreak,
    Doc.concat(list{
      Doc.lbrace,
      Doc.indent(
        Doc.concat(list{
          Doc.softLine,
          Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(ld => {
              let doc = printLabelDeclaration(ld, cmtTbl)
              printComments(doc, cmtTbl, ld.Parsetree.pld_loc)
            }, lds)),
        }),
      ),
      Doc.trailingComma,
      Doc.softLine,
      Doc.rbrace,
    }),
  )
}

and printConstructorDeclarations = (
  ~privateFlag,
  cds: list<Parsetree.constructor_declaration>,
  cmtTbl,
) => {
  let forceBreak = switch (cds, List.rev(cds)) {
  | (list{first, ..._}, list{last, ..._}) =>
    first.pcd_loc.loc_start.pos_lnum < last.pcd_loc.loc_end.pos_lnum
  | _ => false
  }

  let privateFlag = switch privateFlag {
  | Asttypes.Private => Doc.concat(list{Doc.text("private"), Doc.line})
  | Public => Doc.nil
  }

  let rows = printListi(
    ~getLoc=cd => cd.Parsetree.pcd_loc,
    ~nodes=cds,
    ~print=(cd, cmtTbl, i) => {
      let doc = printConstructorDeclaration2(i, cd, cmtTbl)
      printComments(doc, cmtTbl, cd.Parsetree.pcd_loc)
    },
    ~forceBreak,
    cmtTbl,
  )

  Doc.breakableGroup(~forceBreak, Doc.indent(Doc.concat(list{Doc.line, privateFlag, rows})))
}

and printConstructorDeclaration2 = (i, cd: Parsetree.constructor_declaration, cmtTbl) => {
  let attrs = printAttributes(cd.pcd_attributes, cmtTbl)
  let bar = if i > 0 || cd.pcd_attributes != list{} {
    Doc.text("| ")
  } else {
    Doc.ifBreaks(Doc.text("| "), Doc.nil)
  }

  let constrName = {
    let doc = Doc.text(cd.pcd_name.txt)
    printComments(doc, cmtTbl, cd.pcd_name.loc)
  }

  let constrArgs = printConstructorArguments(~indent=true, cd.pcd_args, cmtTbl)
  let gadt = switch cd.pcd_res {
  | None => Doc.nil
  | Some(typ) => Doc.indent(Doc.concat(list{Doc.text(": "), printTypExpr(typ, cmtTbl)}))
  }

  Doc.concat(list{
    bar,
    Doc.group(
      Doc.concat(list{
        attrs /* TODO: fix parsing of attributes, so when can print them above the bar? */,
        constrName,
        constrArgs,
        gadt,
      }),
    ),
  })
}

and printConstructorArguments = (~indent, cdArgs: Parsetree.constructor_arguments, cmtTbl) =>
  switch cdArgs {
  | Pcstr_tuple(list{}) => Doc.nil
  | Pcstr_tuple(types) =>
    let args = Doc.concat(list{
      Doc.lparen,
      Doc.indent(
        Doc.concat(list{
          Doc.softLine,
          Doc.join(
            ~sep=Doc.concat(list{Doc.comma, Doc.line}),
            List.map(typexpr => printTypExpr(typexpr, cmtTbl), types),
          ),
        }),
      ),
      Doc.trailingComma,
      Doc.softLine,
      Doc.rparen,
    })
    Doc.group(
      if indent {
        Doc.indent(args)
      } else {
        args
      },
    )
  | Pcstr_record(lds) =>
    let args = Doc.concat(list{
      Doc.lparen,
      /* manually inline the printRecordDeclaration, gives better layout */
      Doc.lbrace,
      Doc.indent(
        Doc.concat(list{
          Doc.softLine,
          Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(ld => {
              let doc = printLabelDeclaration(ld, cmtTbl)
              printComments(doc, cmtTbl, ld.Parsetree.pld_loc)
            }, lds)),
        }),
      ),
      Doc.trailingComma,
      Doc.softLine,
      Doc.rbrace,
      Doc.rparen,
    })
    if indent {
      Doc.indent(args)
    } else {
      args
    }
  }

and printLabelDeclaration = (ld: Parsetree.label_declaration, cmtTbl) => {
  let attrs = printAttributes(~loc=ld.pld_name.loc, ld.pld_attributes, cmtTbl)
  let mutableFlag = switch ld.pld_mutable {
  | Mutable => Doc.text("mutable ")
  | Immutable => Doc.nil
  }

  let name = {
    let doc = printIdentLike(ld.pld_name.txt)
    printComments(doc, cmtTbl, ld.pld_name.loc)
  }

  Doc.group(
    Doc.concat(list{attrs, mutableFlag, name, Doc.text(": "), printTypExpr(ld.pld_type, cmtTbl)}),
  )
}

and printTypExpr = (typExpr: Parsetree.core_type, cmtTbl) => {
  let renderedType = switch typExpr.ptyp_desc {
  | Ptyp_any => Doc.text("_")
  | Ptyp_var(var) => Doc.concat(list{Doc.text("'"), printIdentLike(~allowUident=true, var)})
  | Ptyp_extension(extension) => printExtension(~atModuleLvl=false, extension, cmtTbl)
  | Ptyp_alias(typ, alias) =>
    let typ = {
      /* Technically type t = (string, float) => unit as 'x, doesn't require
       * parens around the arrow expression. This is very confusing though.
       * Is the "as" part of "unit" or "(string, float) => unit". By printing
       * parens we guide the user towards its meaning. */
      let needsParens = switch typ.ptyp_desc {
      | Ptyp_arrow(_) => true
      | _ => false
      }

      let doc = printTypExpr(typ, cmtTbl)
      if needsParens {
        Doc.concat(list{Doc.lparen, doc, Doc.rparen})
      } else {
        doc
      }
    }

    Doc.concat(list{typ, Doc.text(" as "), Doc.concat(list{Doc.text("'"), printIdentLike(alias)})})

  /* object printings */
  | Ptyp_object(fields, openFlag) => printObject(~inline=false, fields, openFlag, cmtTbl)
  | Ptyp_constr(longidentLoc, list{{ptyp_desc: Ptyp_object(fields, openFlag)}}) =>
    /* for foo<{"a": b}>, when the object is long and needs a line break, we
     want the <{ and }> to stay hugged together */
    let constrName = printLidentPath(longidentLoc, cmtTbl)
    Doc.concat(list{
      constrName,
      Doc.lessThan,
      printObject(~inline=true, fields, openFlag, cmtTbl),
      Doc.greaterThan,
    })

  | Ptyp_constr(longidentLoc, list{{ptyp_desc: Parsetree.Ptyp_tuple(tuple)}}) =>
    let constrName = printLidentPath(longidentLoc, cmtTbl)
    Doc.group(
      Doc.concat(list{
        constrName,
        Doc.lessThan,
        printTupleType(~inline=true, tuple, cmtTbl),
        Doc.greaterThan,
      }),
    )
  | Ptyp_constr(longidentLoc, constrArgs) =>
    let constrName = printLidentPath(longidentLoc, cmtTbl)
    switch constrArgs {
    | list{} => constrName
    | _args =>
      Doc.group(
        Doc.concat(list{
          constrName,
          Doc.lessThan,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                List.map(typexpr => printTypExpr(typexpr, cmtTbl), constrArgs),
              ),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.greaterThan,
        }),
      )
    }
  | Ptyp_arrow(_) =>
    let (attrsBefore, args, returnType) = ParsetreeViewer.arrowType(typExpr)
    let returnTypeNeedsParens = switch returnType.ptyp_desc {
    | Ptyp_alias(_) => true
    | _ => false
    }

    let returnDoc = {
      let doc = printTypExpr(returnType, cmtTbl)
      if returnTypeNeedsParens {
        Doc.concat(list{Doc.lparen, doc, Doc.rparen})
      } else {
        doc
      }
    }

    let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(attrsBefore)

    switch args {
    | list{} => Doc.nil
    | list{(list{}, Nolabel, n)} if !isUncurried =>
      let hasAttrsBefore = !(attrs == list{})
      let attrs = if hasAttrsBefore {
        printAttributes(~inline=true, attrsBefore, cmtTbl)
      } else {
        Doc.nil
      }

      let typDoc = {
        let doc = printTypExpr(n, cmtTbl)
        switch n.ptyp_desc {
        | Ptyp_arrow(_) | Ptyp_tuple(_) | Ptyp_alias(_) => addParens(doc)
        | _ => doc
        }
      }

      Doc.group(
        Doc.concat(list{
          Doc.group(attrs),
          Doc.group(
            if hasAttrsBefore {
              Doc.concat(list{
                Doc.lparen,
                Doc.indent(Doc.concat(list{Doc.softLine, typDoc, Doc.text(" => "), returnDoc})),
                Doc.softLine,
                Doc.rparen,
              })
            } else {
              Doc.concat(list{typDoc, Doc.text(" => "), returnDoc})
            },
          ),
        }),
      )
    | args =>
      let attrs = printAttributes(~inline=true, attrs, cmtTbl)
      let renderedArgs = Doc.concat(list{
        attrs,
        Doc.text("("),
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            if isUncurried {
              Doc.concat(list{Doc.dot, Doc.space})
            } else {
              Doc.nil
            },
            Doc.join(
              ~sep=Doc.concat(list{Doc.comma, Doc.line}),
              List.map(tp => printTypeParameter(tp, cmtTbl), args),
            ),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.text(")"),
      })
      Doc.group(Doc.concat(list{renderedArgs, Doc.text(" => "), returnDoc}))
    }
  | Ptyp_tuple(types) => printTupleType(~inline=false, types, cmtTbl)
  | Ptyp_poly(list{}, typ) => printTypExpr(typ, cmtTbl)
  | Ptyp_poly(stringLocs, typ) =>
    Doc.concat(list{Doc.join(~sep=Doc.space, List.map(({Location.txt: txt, loc}) => {
          let doc = Doc.concat(list{Doc.text("'"), Doc.text(txt)})
          printComments(doc, cmtTbl, loc)
        }, stringLocs)), Doc.dot, Doc.space, printTypExpr(typ, cmtTbl)})
  | Ptyp_package(packageType) =>
    printPackageType(~printModuleKeywordAndParens=true, packageType, cmtTbl)
  | Ptyp_class(_) => Doc.text("classes are not supported in types")
  | Ptyp_variant(rowFields, closedFlag, labelsOpt) =>
    let forceBreak =
      typExpr.ptyp_loc.Location.loc_start.pos_lnum < typExpr.ptyp_loc.loc_end.pos_lnum
    let printRowField = x =>
      switch x {
      | Parsetree.Rtag({txt}, attrs, true, list{}) =>
        Doc.group(
          Doc.concat(list{
            printAttributes(attrs, cmtTbl),
            Doc.concat(list{Doc.text("#"), printPolyVarIdent(txt)}),
          }),
        )
      | Rtag({txt}, attrs, truth, types) =>
        let doType = t =>
          switch t.Parsetree.ptyp_desc {
          | Ptyp_tuple(_) => printTypExpr(t, cmtTbl)
          | _ => Doc.concat(list{Doc.lparen, printTypExpr(t, cmtTbl), Doc.rparen})
          }

        let printedTypes = List.map(doType, types)
        let cases = Doc.join(~sep=Doc.concat(list{Doc.line, Doc.text("& ")}), printedTypes)
        let cases = if truth {
          Doc.concat(list{Doc.line, Doc.text("& "), cases})
        } else {
          cases
        }
        Doc.group(
          Doc.concat(list{
            printAttributes(attrs, cmtTbl),
            Doc.concat(list{Doc.text("#"), printPolyVarIdent(txt)}),
            cases,
          }),
        )
      | Rinherit(coreType) => printTypExpr(coreType, cmtTbl)
      }

    let docs = List.map(printRowField, rowFields)
    let cases = Doc.join(~sep=Doc.concat(list{Doc.line, Doc.text("| ")}), docs)
    let cases = if docs == list{} {
      cases
    } else {
      Doc.concat(list{Doc.ifBreaks(Doc.text("| "), Doc.nil), cases})
    }

    let openingSymbol = if closedFlag == Open {
      Doc.concat(list{Doc.greaterThan, Doc.line})
    } else if labelsOpt == None {
      Doc.softLine
    } else {
      Doc.concat(list{Doc.lessThan, Doc.line})
    }
    let labels = switch labelsOpt {
    | None
    | Some(list{}) => Doc.nil
    | Some(labels) =>
      Doc.concat(
        List.map(
          label => Doc.concat(list{Doc.line, Doc.text("#"), printPolyVarIdent(label)}),
          labels,
        ),
      )
    }

    let closingSymbol = switch labelsOpt {
    | None | Some(list{}) => Doc.nil
    | _ => Doc.text(" >")
    }

    Doc.breakableGroup(
      ~forceBreak,
      Doc.concat(list{
        Doc.lbracket,
        Doc.indent(Doc.concat(list{openingSymbol, cases, closingSymbol, labels})),
        Doc.softLine,
        Doc.rbracket,
      }),
    )
  }

  let shouldPrintItsOwnAttributes = switch typExpr.ptyp_desc {
  | Ptyp_arrow(_) /* es6 arrow types print their own attributes */ => true
  | _ => false
  }

  let doc = switch typExpr.ptyp_attributes {
  | list{_, ..._} as attrs if !shouldPrintItsOwnAttributes =>
    Doc.group(Doc.concat(list{printAttributes(attrs, cmtTbl), renderedType}))
  | _ => renderedType
  }

  printComments(doc, cmtTbl, typExpr.ptyp_loc)
}

and printObject = (~inline, fields, openFlag, cmtTbl) => {
  let doc = switch fields {
  | list{} =>
    Doc.concat(list{
      Doc.lbrace,
      switch openFlag {
      | Asttypes.Closed => Doc.dot
      | Open => Doc.dotdot
      },
      Doc.rbrace,
    })
  | fields =>
    Doc.concat(list{
      Doc.lbrace,
      switch openFlag {
      | Asttypes.Closed => Doc.nil
      | Open =>
        switch fields {
        /* handle `type t = {.. ...objType, "x": int}`
         * .. and ... should have a space in between */
        | list{Oinherit(_), ..._} => Doc.text(".. ")
        | _ => Doc.dotdot
        }
      },
      Doc.indent(
        Doc.concat(list{
          Doc.softLine,
          Doc.join(
            ~sep=Doc.concat(list{Doc.comma, Doc.line}),
            List.map(field => printObjectField(field, cmtTbl), fields),
          ),
        }),
      ),
      Doc.trailingComma,
      Doc.softLine,
      Doc.rbrace,
    })
  }

  if inline {
    doc
  } else {
    Doc.group(doc)
  }
}

and printTupleType = (~inline, types: list<Parsetree.core_type>, cmtTbl) => {
  let tuple = Doc.concat(list{
    Doc.lparen,
    Doc.indent(
      Doc.concat(list{
        Doc.softLine,
        Doc.join(
          ~sep=Doc.concat(list{Doc.comma, Doc.line}),
          List.map(typexpr => printTypExpr(typexpr, cmtTbl), types),
        ),
      }),
    ),
    Doc.trailingComma,
    Doc.softLine,
    Doc.rparen,
  })

  if inline === false {
    Doc.group(tuple)
  } else {
    tuple
  }
}

and printObjectField = (field: Parsetree.object_field, cmtTbl) =>
  switch field {
  | Otag(labelLoc, attrs, typ) =>
    let lbl = {
      let doc = Doc.text("\"" ++ (labelLoc.txt ++ "\""))
      printComments(doc, cmtTbl, labelLoc.loc)
    }

    let doc = Doc.concat(list{
      printAttributes(~loc=labelLoc.loc, attrs, cmtTbl),
      lbl,
      Doc.text(": "),
      printTypExpr(typ, cmtTbl),
    })
    let cmtLoc = {...labelLoc.loc, loc_end: typ.ptyp_loc.loc_end}
    printComments(doc, cmtTbl, cmtLoc)
  | Oinherit(typexpr) => Doc.concat(list{Doc.dotdotdot, printTypExpr(typexpr, cmtTbl)})
  }

/* es6 arrow type arg
 * type t = (~foo: string, ~bar: float=?, unit) => unit
 * i.e. ~foo: string, ~bar: float */
and printTypeParameter = ((attrs, lbl, typ), cmtTbl) => {
  let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(attrs)
  let uncurried = if isUncurried {
    Doc.concat(list{Doc.dot, Doc.space})
  } else {
    Doc.nil
  }
  let attrs = printAttributes(attrs, cmtTbl)
  let label = switch lbl {
  | Asttypes.Nolabel => Doc.nil
  | Labelled(lbl) => Doc.concat(list{Doc.text("~"), printIdentLike(lbl), Doc.text(": ")})
  | Optional(lbl) => Doc.concat(list{Doc.text("~"), printIdentLike(lbl), Doc.text(": ")})
  }

  let optionalIndicator = switch lbl {
  | Asttypes.Nolabel
  | Labelled(_) => Doc.nil
  | Optional(_lbl) => Doc.text("=?")
  }

  let (loc, typ) = switch typ.ptyp_attributes {
  | list{({Location.txt: "ns.namedArgLoc", loc}, _), ...attrs} => (
      {...loc, loc_end: typ.ptyp_loc.loc_end},
      {...typ, ptyp_attributes: attrs},
    )
  | _ => (typ.ptyp_loc, typ)
  }

  let doc = Doc.group(
    Doc.concat(list{uncurried, attrs, label, printTypExpr(typ, cmtTbl), optionalIndicator}),
  )
  printComments(doc, cmtTbl, loc)
}

and printValueBinding = (~recFlag, vb, cmtTbl, i) => {
  let attrs = printAttributes(~loc=vb.pvb_pat.ppat_loc, vb.pvb_attributes, cmtTbl)
  let header = if i === 0 {
    Doc.concat(list{Doc.text("let "), recFlag})
  } else {
    Doc.text("and ")
  }

  switch vb {
  | {
      pvb_pat: {ppat_desc: Ppat_constraint(pattern, {ptyp_desc: Ptyp_poly(_)} as patTyp)},
      pvb_expr: {pexp_desc: Pexp_newtype(_)} as expr,
    } =>
    let (_attrs, parameters, returnExpr) = ParsetreeViewer.funExpr(expr)
    let abstractType = switch parameters {
    | list{NewTypes({locs: vars})} =>
      Doc.concat(list{
        Doc.text("type "),
        Doc.join(~sep=Doc.space, List.map(var => Doc.text(var.Asttypes.txt), vars)),
        Doc.dot,
      })
    | _ => Doc.nil
    }

    switch returnExpr.pexp_desc {
    | Pexp_constraint(expr, typ) =>
      Doc.group(
        Doc.concat(list{
          attrs,
          header,
          printPattern(pattern, cmtTbl),
          Doc.text(":"),
          Doc.indent(
            Doc.concat(list{
              Doc.line,
              abstractType,
              Doc.space,
              printTypExpr(typ, cmtTbl),
              Doc.text(" ="),
              Doc.concat(list{Doc.line, printExpressionWithComments(expr, cmtTbl)}),
            }),
          ),
        }),
      )
    | _ =>
      /* Example:
       * let cancel_and_collect_callbacks:
       *   'a 'u 'c. (list<packed_callbacks>, promise<'a, 'u, 'c>) => list<packed_callbacks> =         *  (type x, callbacks_accumulator, p: promise<_, _, c>)
       */
      Doc.group(
        Doc.concat(list{
          attrs,
          header,
          printPattern(pattern, cmtTbl),
          Doc.text(":"),
          Doc.indent(
            Doc.concat(list{
              Doc.line,
              abstractType,
              Doc.space,
              printTypExpr(patTyp, cmtTbl),
              Doc.text(" ="),
              Doc.concat(list{Doc.line, printExpressionWithComments(expr, cmtTbl)}),
            }),
          ),
        }),
      )
    }
  | _ =>
    let (optBraces, expr) = ParsetreeViewer.processBracesAttr(vb.pvb_expr)
    let printedExpr = {
      let doc = printExpressionWithComments(vb.pvb_expr, cmtTbl)
      switch Parens.expr(vb.pvb_expr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr, braces)
      | Nothing => doc
      }
    }

    let patternDoc = printPattern(vb.pvb_pat, cmtTbl)
    /*
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
     */
    if ParsetreeViewer.isSinglePipeExpr(vb.pvb_expr) {
      Doc.customLayout(list{
        Doc.group(
          Doc.concat(list{attrs, header, patternDoc, Doc.text(" ="), Doc.space, printedExpr}),
        ),
        Doc.group(
          Doc.concat(list{
            attrs,
            header,
            patternDoc,
            Doc.text(" ="),
            Doc.indent(Doc.concat(list{Doc.line, printedExpr})),
          }),
        ),
      })
    } else {
      let shouldIndent = switch optBraces {
      | Some(_) => false
      | _ =>
        ParsetreeViewer.isBinaryExpression(expr) ||
        switch vb.pvb_expr {
        | {
            pexp_attributes: list{({Location.txt: "ns.ternary"}, _)},
            pexp_desc: Pexp_ifthenelse(ifExpr, _, _),
          } =>
          ParsetreeViewer.isBinaryExpression(ifExpr) ||
          ParsetreeViewer.hasAttributes(ifExpr.pexp_attributes)
        | {pexp_desc: Pexp_newtype(_)} => false
        | e => ParsetreeViewer.hasAttributes(e.pexp_attributes) || ParsetreeViewer.isArrayAccess(e)
        }
      }

      Doc.group(
        Doc.concat(list{
          attrs,
          header,
          patternDoc,
          Doc.text(" ="),
          if shouldIndent {
            Doc.indent(Doc.concat(list{Doc.line, printedExpr}))
          } else {
            Doc.concat(list{Doc.space, printedExpr})
          },
        }),
      )
    }
  }
}

and printPackageType = (
  ~printModuleKeywordAndParens,
  packageType: Parsetree.package_type,
  cmtTbl,
) => {
  let doc = switch packageType {
  | (longidentLoc, list{}) =>
    Doc.group(Doc.concat(list{printLongidentLocation(longidentLoc, cmtTbl)}))
  | (longidentLoc, packageConstraints) =>
    Doc.group(
      Doc.concat(list{
        printLongidentLocation(longidentLoc, cmtTbl),
        printPackageConstraints(packageConstraints, cmtTbl),
        Doc.softLine,
      }),
    )
  }

  if printModuleKeywordAndParens {
    Doc.concat(list{Doc.text("module("), doc, Doc.rparen})
  } else {
    doc
  }
}

and printPackageConstraints = (packageConstraints, cmtTbl) =>
  Doc.concat(list{
    Doc.text(" with"),
    Doc.indent(Doc.concat(list{Doc.line, Doc.join(~sep=Doc.line, List.mapi((i, pc) => {
            let (longident, typexpr) = pc
            let cmtLoc = {
              ...longident.Asttypes.loc,
              loc_end: typexpr.Parsetree.ptyp_loc.loc_end,
            }
            let doc = printPackageConstraint(i, cmtTbl, pc)
            printComments(doc, cmtTbl, cmtLoc)
          }, packageConstraints))})),
  })

and printPackageConstraint = (i, cmtTbl, (longidentLoc, typ)) => {
  let prefix = if i === 0 {
    Doc.text("type ")
  } else {
    Doc.text("and type ")
  }
  Doc.concat(list{
    prefix,
    printLongidentLocation(longidentLoc, cmtTbl),
    Doc.text(" = "),
    printTypExpr(typ, cmtTbl),
  })
}

and printExtension = (~atModuleLvl, (stringLoc, payload), cmtTbl) => {
  let txt = convertBsExtension(stringLoc.Location.txt)
  let extName = {
    let doc = Doc.concat(list{
      Doc.text("%"),
      if atModuleLvl {
        Doc.text("%")
      } else {
        Doc.nil
      },
      Doc.text(txt),
    })
    printComments(doc, cmtTbl, stringLoc.Location.loc)
  }

  Doc.group(Doc.concat(list{extName, printPayload(payload, cmtTbl)}))
}

and printPattern = (p: Parsetree.pattern, cmtTbl) => {
  let patternWithoutAttributes = switch p.ppat_desc {
  | Ppat_any => Doc.text("_")
  | Ppat_var(var) => printIdentLike(var.txt)
  | Ppat_constant(c) =>
    let templateLiteral = ParsetreeViewer.hasTemplateLiteralAttr(p.ppat_attributes)
    printConstant(~templateLiteral, c)
  | Ppat_tuple(patterns) =>
    Doc.group(
      Doc.concat(list{
        Doc.lparen,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list{Doc.text(","), Doc.line}),
              List.map(pat => printPattern(pat, cmtTbl), patterns),
            ),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rparen,
      }),
    )
  | Ppat_array(list{}) =>
    Doc.concat(list{Doc.lbracket, printCommentsInside(cmtTbl, p.ppat_loc), Doc.rbracket})
  | Ppat_array(patterns) =>
    Doc.group(
      Doc.concat(list{
        Doc.text("["),
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list{Doc.text(","), Doc.line}),
              List.map(pat => printPattern(pat, cmtTbl), patterns),
            ),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.text("]"),
      }),
    )
  | Ppat_construct({txt: Longident.Lident("()")}, _) =>
    Doc.concat(list{Doc.lparen, printCommentsInside(cmtTbl, p.ppat_loc), Doc.rparen})
  | Ppat_construct({txt: Longident.Lident("[]")}, _) =>
    Doc.concat(list{Doc.text("list{"), printCommentsInside(cmtTbl, p.ppat_loc), Doc.rbrace})
  | Ppat_construct({txt: Longident.Lident("::")}, _) =>
    let (patterns, tail) = ParsetreeViewer.collectPatternsFromListConstruct(list{}, p)
    let shouldHug = switch (patterns, tail) {
    | (list{pat}, {ppat_desc: Ppat_construct({txt: Longident.Lident("[]")}, _)})
      if ParsetreeViewer.isHuggablePattern(pat) => true
    | _ => false
    }

    let children = Doc.concat(list{
      if shouldHug {
        Doc.nil
      } else {
        Doc.softLine
      },
      Doc.join(
        ~sep=Doc.concat(list{Doc.text(","), Doc.line}),
        List.map(pat => printPattern(pat, cmtTbl), patterns),
      ),
      switch tail.Parsetree.ppat_desc {
      | Ppat_construct({txt: Longident.Lident("[]")}, _) => Doc.nil
      | _ =>
        let doc = Doc.concat(list{Doc.text("..."), printPattern(tail, cmtTbl)})
        let tail = printComments(doc, cmtTbl, tail.ppat_loc)
        Doc.concat(list{Doc.text(","), Doc.line, tail})
      },
    })
    Doc.group(
      Doc.concat(list{
        Doc.text("list{"),
        if shouldHug {
          children
        } else {
          Doc.concat(list{Doc.indent(children), Doc.ifBreaks(Doc.text(","), Doc.nil), Doc.softLine})
        },
        Doc.rbrace,
      }),
    )
  | Ppat_construct(constrName, constructorArgs) =>
    let constrName = printLongidentLocation(constrName, cmtTbl)
    let argsDoc = switch constructorArgs {
    | None => Doc.nil
    | Some({ppat_loc, ppat_desc: Ppat_construct({txt: Longident.Lident("()")}, _)}) =>
      Doc.concat(list{Doc.lparen, printCommentsInside(cmtTbl, ppat_loc), Doc.rparen})
    | Some({ppat_desc: Ppat_tuple(list{}), ppat_loc: loc}) =>
      Doc.concat(list{Doc.lparen, Doc.softLine, printCommentsInside(cmtTbl, loc), Doc.rparen})
    /* Some((1, 2) */
    | Some({ppat_desc: Ppat_tuple(list{{ppat_desc: Ppat_tuple(_)} as arg})}) =>
      Doc.concat(list{Doc.lparen, printPattern(arg, cmtTbl), Doc.rparen})
    | Some({ppat_desc: Ppat_tuple(patterns)}) =>
      Doc.concat(list{
        Doc.lparen,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list{Doc.comma, Doc.line}),
              List.map(pat => printPattern(pat, cmtTbl), patterns),
            ),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rparen,
      })
    | Some(arg) =>
      let argDoc = printPattern(arg, cmtTbl)
      let shouldHug = ParsetreeViewer.isHuggablePattern(arg)
      Doc.concat(list{
        Doc.lparen,
        if shouldHug {
          argDoc
        } else {
          Doc.concat(list{
            Doc.indent(Doc.concat(list{Doc.softLine, argDoc})),
            Doc.trailingComma,
            Doc.softLine,
          })
        },
        Doc.rparen,
      })
    }

    Doc.group(Doc.concat(list{constrName, argsDoc}))
  | Ppat_variant(label, None) => Doc.concat(list{Doc.text("#"), printPolyVarIdent(label)})
  | Ppat_variant(label, variantArgs) =>
    let variantName = Doc.concat(list{Doc.text("#"), printPolyVarIdent(label)})
    let argsDoc = switch variantArgs {
    | None => Doc.nil
    | Some({ppat_desc: Ppat_construct({txt: Longident.Lident("()")}, _)}) => Doc.text("()")
    | Some({ppat_desc: Ppat_tuple(list{}), ppat_loc: loc}) =>
      Doc.concat(list{Doc.lparen, Doc.softLine, printCommentsInside(cmtTbl, loc), Doc.rparen})
    /* Some((1, 2) */
    | Some({ppat_desc: Ppat_tuple(list{{ppat_desc: Ppat_tuple(_)} as arg})}) =>
      Doc.concat(list{Doc.lparen, printPattern(arg, cmtTbl), Doc.rparen})
    | Some({ppat_desc: Ppat_tuple(patterns)}) =>
      Doc.concat(list{
        Doc.lparen,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list{Doc.comma, Doc.line}),
              List.map(pat => printPattern(pat, cmtTbl), patterns),
            ),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rparen,
      })
    | Some(arg) =>
      let argDoc = printPattern(arg, cmtTbl)
      let shouldHug = ParsetreeViewer.isHuggablePattern(arg)
      Doc.concat(list{
        Doc.lparen,
        if shouldHug {
          argDoc
        } else {
          Doc.concat(list{
            Doc.indent(Doc.concat(list{Doc.softLine, argDoc})),
            Doc.trailingComma,
            Doc.softLine,
          })
        },
        Doc.rparen,
      })
    }

    Doc.group(Doc.concat(list{variantName, argsDoc}))
  | Ppat_type(ident) => Doc.concat(list{Doc.text("#..."), printIdentPath(ident, cmtTbl)})
  | Ppat_record(rows, openFlag) =>
    Doc.group(
      Doc.concat(list{
        Doc.lbrace,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list{Doc.text(","), Doc.line}),
              List.map(row => printPatternRecordRow(row, cmtTbl), rows),
            ),
            switch openFlag {
            | Open => Doc.concat(list{Doc.text(","), Doc.line, Doc.text("_")})
            | Closed => Doc.nil
            },
          }),
        ),
        Doc.ifBreaks(Doc.text(","), Doc.nil),
        Doc.softLine,
        Doc.rbrace,
      }),
    )

  | Ppat_exception(p) =>
    let needsParens = switch p.ppat_desc {
    | Ppat_or(_, _) | Ppat_alias(_, _) => true
    | _ => false
    }

    let pat = {
      let p = printPattern(p, cmtTbl)
      if needsParens {
        Doc.concat(list{Doc.text("("), p, Doc.text(")")})
      } else {
        p
      }
    }

    Doc.group(Doc.concat(list{Doc.text("exception"), Doc.line, pat}))
  | Ppat_or(_) =>
    /* Blue | Red | Green -> [Blue; Red; Green] */
    let orChain = ParsetreeViewer.collectOrPatternChain(p)
    let docs = List.mapi((i, pat) => {
      let patternDoc = printPattern(pat, cmtTbl)
      Doc.concat(list{
        if i === 0 {
          Doc.nil
        } else {
          Doc.concat(list{Doc.line, Doc.text("| ")})
        },
        switch pat.ppat_desc {
        /* (Blue | Red) | (Green | Black) | White */
        | Ppat_or(_) => addParens(patternDoc)
        | _ => patternDoc
        },
      })
    }, orChain)
    let isSpreadOverMultipleLines = switch (orChain, List.rev(orChain)) {
    | (list{first, ..._}, list{last, ..._}) =>
      first.ppat_loc.loc_start.pos_lnum < last.ppat_loc.loc_end.pos_lnum
    | _ => false
    }

    Doc.breakableGroup(~forceBreak=isSpreadOverMultipleLines, Doc.concat(docs))
  | Ppat_extension(ext) => printExtension(~atModuleLvl=false, ext, cmtTbl)
  | Ppat_lazy(p) =>
    let needsParens = switch p.ppat_desc {
    | Ppat_or(_, _) | Ppat_alias(_, _) => true
    | _ => false
    }

    let pat = {
      let p = printPattern(p, cmtTbl)
      if needsParens {
        Doc.concat(list{Doc.text("("), p, Doc.text(")")})
      } else {
        p
      }
    }

    Doc.concat(list{Doc.text("lazy "), pat})
  | Ppat_alias(p, aliasLoc) =>
    let needsParens = switch p.ppat_desc {
    | Ppat_or(_, _) | Ppat_alias(_, _) => true
    | _ => false
    }

    let renderedPattern = {
      let p = printPattern(p, cmtTbl)
      if needsParens {
        Doc.concat(list{Doc.text("("), p, Doc.text(")")})
      } else {
        p
      }
    }

    Doc.concat(list{renderedPattern, Doc.text(" as "), printStringLoc(aliasLoc, cmtTbl)})

  /* Note: module(P : S) is represented as */
  /* Ppat_constraint(Ppat_unpack, Ptyp_package) */
  | Ppat_constraint(
      {ppat_desc: Ppat_unpack(stringLoc)},
      {ptyp_desc: Ptyp_package(packageType), ptyp_loc},
    ) =>
    Doc.concat(list{
      Doc.text("module("),
      printComments(Doc.text(stringLoc.txt), cmtTbl, stringLoc.loc),
      Doc.text(": "),
      printComments(
        printPackageType(~printModuleKeywordAndParens=false, packageType, cmtTbl),
        cmtTbl,
        ptyp_loc,
      ),
      Doc.rparen,
    })
  | Ppat_constraint(pattern, typ) =>
    Doc.concat(list{printPattern(pattern, cmtTbl), Doc.text(": "), printTypExpr(typ, cmtTbl)})

  /* Note: module(P : S) is represented as */
  /* Ppat_constraint(Ppat_unpack, Ptyp_package) */
  | Ppat_unpack(stringLoc) =>
    Doc.concat(list{
      Doc.text("module("),
      printComments(Doc.text(stringLoc.txt), cmtTbl, stringLoc.loc),
      Doc.rparen,
    })
  | Ppat_interval(a, b) => Doc.concat(list{printConstant(a), Doc.text(" .. "), printConstant(b)})
  | Ppat_open(_) => Doc.nil
  }

  let doc = switch p.ppat_attributes {
  | list{} => patternWithoutAttributes
  | attrs => Doc.group(Doc.concat(list{printAttributes(attrs, cmtTbl), patternWithoutAttributes}))
  }

  printComments(doc, cmtTbl, p.ppat_loc)
}

and printPatternRecordRow = (row, cmtTbl) =>
  switch row {
  /* punned {x} */
  | (
      {Location.txt: Longident.Lident(ident)} as longident,
      {Parsetree.ppat_desc: Ppat_var({txt, _})},
    ) if ident == txt =>
    printLidentPath(longident, cmtTbl)
  | (longident, pattern) =>
    let locForComments = {
      ...longident.loc,
      loc_end: pattern.Parsetree.ppat_loc.loc_end,
    }
    let rhsDoc = {
      let doc = printPattern(pattern, cmtTbl)
      if Parens.patternRecordRowRhs(pattern) {
        addParens(doc)
      } else {
        doc
      }
    }

    let doc = Doc.group(
      Doc.concat(list{
        printLidentPath(longident, cmtTbl),
        Doc.text(":"),
        if ParsetreeViewer.isHuggablePattern(pattern) {
          Doc.concat(list{Doc.space, rhsDoc})
        } else {
          Doc.indent(Doc.concat(list{Doc.line, rhsDoc}))
        },
      }),
    )
    printComments(doc, cmtTbl, locForComments)
  }

and printExpressionWithComments = (expr, cmtTbl) => {
  let doc = printExpression(expr, cmtTbl)
  printComments(doc, cmtTbl, expr.Parsetree.pexp_loc)
}

and printIfChain = (pexp_attributes, ifs, elseExpr, cmtTbl) => {
  let ifDocs = Doc.join(~sep=Doc.space, List.mapi((i, (ifExpr, thenExpr)) => {
      let ifTxt = if i > 0 {
        Doc.text("else if ")
      } else {
        Doc.text("if ")
      }
      switch ifExpr {
      | ParsetreeViewer.If(ifExpr) =>
        let condition = if ParsetreeViewer.isBlockExpr(ifExpr) {
          printExpressionBlock(~braces=true, ifExpr, cmtTbl)
        } else {
          let doc = printExpressionWithComments(ifExpr, cmtTbl)
          switch Parens.expr(ifExpr) {
          | Parens.Parenthesized => addParens(doc)
          | Braced(braces) => printBraces(doc, ifExpr, braces)
          | Nothing => Doc.ifBreaks(addParens(doc), doc)
          }
        }

        Doc.concat(list{
          ifTxt,
          Doc.group(condition),
          Doc.space,
          {
            let thenExpr = switch ParsetreeViewer.processBracesAttr(thenExpr) {
            /* This case only happens when coming from Reason, we strip braces */
            | (Some(_), expr) => expr
            | _ => thenExpr
            }

            printExpressionBlock(~braces=true, thenExpr, cmtTbl)
          },
        })
      | IfLet(pattern, conditionExpr) =>
        let conditionDoc = {
          let doc = printExpressionWithComments(conditionExpr, cmtTbl)
          switch Parens.expr(conditionExpr) {
          | Parens.Parenthesized => addParens(doc)
          | Braced(braces) => printBraces(doc, conditionExpr, braces)
          | Nothing => doc
          }
        }

        Doc.concat(list{
          ifTxt,
          Doc.text("let "),
          printPattern(pattern, cmtTbl),
          Doc.text(" = "),
          conditionDoc,
          Doc.space,
          printExpressionBlock(~braces=true, thenExpr, cmtTbl),
        })
      }
    }, ifs))
  let elseDoc = switch elseExpr {
  | None => Doc.nil
  | Some(expr) =>
    Doc.concat(list{Doc.text(" else "), printExpressionBlock(~braces=true, expr, cmtTbl)})
  }

  let attrs = ParsetreeViewer.filterFragileMatchAttributes(pexp_attributes)
  Doc.concat(list{printAttributes(attrs, cmtTbl), ifDocs, elseDoc})
}

and printExpression = (e: Parsetree.expression, cmtTbl) => {
  let printedExpression = switch e.pexp_desc {
  | Parsetree.Pexp_constant(c) =>
    printConstant(~templateLiteral=ParsetreeViewer.isTemplateLiteral(e), c)
  | Pexp_construct(_) if ParsetreeViewer.hasJsxAttribute(e.pexp_attributes) =>
    printJsxFragment(e, cmtTbl)
  | Pexp_construct({txt: Longident.Lident("()")}, _) => Doc.text("()")
  | Pexp_construct({txt: Longident.Lident("[]")}, _) =>
    Doc.concat(list{Doc.text("list{"), printCommentsInside(cmtTbl, e.pexp_loc), Doc.rbrace})
  | Pexp_construct({txt: Longident.Lident("::")}, _) =>
    let (expressions, spread) = ParsetreeViewer.collectListExpressions(e)
    let spreadDoc = switch spread {
    | Some(expr) =>
      Doc.concat(list{
        Doc.text(","),
        Doc.line,
        Doc.dotdotdot,
        {
          let doc = printExpressionWithComments(expr, cmtTbl)
          switch Parens.expr(expr) {
          | Parens.Parenthesized => addParens(doc)
          | Braced(braces) => printBraces(doc, expr, braces)
          | Nothing => doc
          }
        },
      })
    | None => Doc.nil
    }

    Doc.group(
      Doc.concat(list{
        Doc.text("list{"),
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(~sep=Doc.concat(list{Doc.text(","), Doc.line}), List.map(expr => {
                let doc = printExpressionWithComments(expr, cmtTbl)
                switch Parens.expr(expr) {
                | Parens.Parenthesized => addParens(doc)
                | Braced(braces) => printBraces(doc, expr, braces)
                | Nothing => doc
                }
              }, expressions)),
            spreadDoc,
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rbrace,
      }),
    )
  | Pexp_construct(longidentLoc, args) =>
    let constr = printLongidentLocation(longidentLoc, cmtTbl)
    let args = switch args {
    | None => Doc.nil
    | Some({pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, _)}) => Doc.text("()")
    /* Some((1, 2)) */
    | Some({pexp_desc: Pexp_tuple(list{{pexp_desc: Pexp_tuple(_)} as arg})}) =>
      Doc.concat(list{
        Doc.lparen,
        {
          let doc = printExpressionWithComments(arg, cmtTbl)
          switch Parens.expr(arg) {
          | Parens.Parenthesized => addParens(doc)
          | Braced(braces) => printBraces(doc, arg, braces)
          | Nothing => doc
          }
        },
        Doc.rparen,
      })
    | Some({pexp_desc: Pexp_tuple(args)}) =>
      Doc.concat(list{
        Doc.lparen,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(expr => {
                let doc = printExpressionWithComments(expr, cmtTbl)
                switch Parens.expr(expr) {
                | Parens.Parenthesized => addParens(doc)
                | Braced(braces) => printBraces(doc, expr, braces)
                | Nothing => doc
                }
              }, args)),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rparen,
      })
    | Some(arg) =>
      let argDoc = {
        let doc = printExpressionWithComments(arg, cmtTbl)
        switch Parens.expr(arg) {
        | Parens.Parenthesized => addParens(doc)
        | Braced(braces) => printBraces(doc, arg, braces)
        | Nothing => doc
        }
      }

      let shouldHug = ParsetreeViewer.isHuggableExpression(arg)
      Doc.concat(list{
        Doc.lparen,
        if shouldHug {
          argDoc
        } else {
          Doc.concat(list{
            Doc.indent(Doc.concat(list{Doc.softLine, argDoc})),
            Doc.trailingComma,
            Doc.softLine,
          })
        },
        Doc.rparen,
      })
    }

    Doc.group(Doc.concat(list{constr, args}))
  | Pexp_ident(path) => printLidentPath(path, cmtTbl)
  | Pexp_tuple(exprs) =>
    Doc.group(
      Doc.concat(list{
        Doc.lparen,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(~sep=Doc.concat(list{Doc.text(","), Doc.line}), List.map(expr => {
                let doc = printExpressionWithComments(expr, cmtTbl)
                switch Parens.expr(expr) {
                | Parens.Parenthesized => addParens(doc)
                | Braced(braces) => printBraces(doc, expr, braces)
                | Nothing => doc
                }
              }, exprs)),
          }),
        ),
        Doc.ifBreaks(Doc.text(","), Doc.nil),
        Doc.softLine,
        Doc.rparen,
      }),
    )
  | Pexp_array(list{}) =>
    Doc.concat(list{Doc.lbracket, printCommentsInside(cmtTbl, e.pexp_loc), Doc.rbracket})
  | Pexp_array(exprs) =>
    Doc.group(
      Doc.concat(list{
        Doc.lbracket,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(~sep=Doc.concat(list{Doc.text(","), Doc.line}), List.map(expr => {
                let doc = printExpressionWithComments(expr, cmtTbl)
                switch Parens.expr(expr) {
                | Parens.Parenthesized => addParens(doc)
                | Braced(braces) => printBraces(doc, expr, braces)
                | Nothing => doc
                }
              }, exprs)),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rbracket,
      }),
    )
  | Pexp_variant(label, args) =>
    let variantName = Doc.concat(list{Doc.text("#"), printPolyVarIdent(label)})
    let args = switch args {
    | None => Doc.nil
    | Some({pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, _)}) => Doc.text("()")
    /* #poly((1, 2) */
    | Some({pexp_desc: Pexp_tuple(list{{pexp_desc: Pexp_tuple(_)} as arg})}) =>
      Doc.concat(list{
        Doc.lparen,
        {
          let doc = printExpressionWithComments(arg, cmtTbl)
          switch Parens.expr(arg) {
          | Parens.Parenthesized => addParens(doc)
          | Braced(braces) => printBraces(doc, arg, braces)
          | Nothing => doc
          }
        },
        Doc.rparen,
      })
    | Some({pexp_desc: Pexp_tuple(args)}) =>
      Doc.concat(list{
        Doc.lparen,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(expr => {
                let doc = printExpressionWithComments(expr, cmtTbl)
                switch Parens.expr(expr) {
                | Parens.Parenthesized => addParens(doc)
                | Braced(braces) => printBraces(doc, expr, braces)
                | Nothing => doc
                }
              }, args)),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rparen,
      })
    | Some(arg) =>
      let argDoc = {
        let doc = printExpressionWithComments(arg, cmtTbl)
        switch Parens.expr(arg) {
        | Parens.Parenthesized => addParens(doc)
        | Braced(braces) => printBraces(doc, arg, braces)
        | Nothing => doc
        }
      }

      let shouldHug = ParsetreeViewer.isHuggableExpression(arg)
      Doc.concat(list{
        Doc.lparen,
        if shouldHug {
          argDoc
        } else {
          Doc.concat(list{
            Doc.indent(Doc.concat(list{Doc.softLine, argDoc})),
            Doc.trailingComma,
            Doc.softLine,
          })
        },
        Doc.rparen,
      })
    }

    Doc.group(Doc.concat(list{variantName, args}))
  | Pexp_record(rows, spreadExpr) =>
    let spread = switch spreadExpr {
    | None => Doc.nil
    | Some(expr) =>
      Doc.concat(list{
        Doc.dotdotdot,
        {
          let doc = printExpressionWithComments(expr, cmtTbl)
          switch Parens.expr(expr) {
          | Parens.Parenthesized => addParens(doc)
          | Braced(braces) => printBraces(doc, expr, braces)
          | Nothing => doc
          }
        },
        Doc.comma,
        Doc.line,
      })
    }

    /* If the record is written over multiple lines, break automatically
     * `let x = {a: 1, b: 3}` -> same line, break when line-width exceeded
     * `let x = {
     *   a: 1,
     *   b: 2,
     *  }` -> record is written on multiple lines, break the group */
    let forceBreak = e.pexp_loc.loc_start.pos_lnum < e.pexp_loc.loc_end.pos_lnum

    let punningAllowed = switch (spreadExpr, rows) {
    | (None, list{_}) => false /* disallow punning for single-element records */
    | _ => true
    }

    Doc.breakableGroup(
      ~forceBreak,
      Doc.concat(list{
        Doc.lbrace,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            spread,
            Doc.join(
              ~sep=Doc.concat(list{Doc.text(","), Doc.line}),
              List.map(row => printRecordRow(row, cmtTbl, punningAllowed), rows),
            ),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rbrace,
      }),
    )
  | Pexp_extension(extension) =>
    switch extension {
    | (
        {txt: "bs.obj" | "obj"},
        PStr(list{{
          pstr_loc: loc,
          pstr_desc: Pstr_eval({pexp_desc: Pexp_record(rows, _)}, list{}),
        }}),
      ) =>
      /* If the object is written over multiple lines, break automatically
       * `let x = {"a": 1, "b": 3}` -> same line, break when line-width exceeded
       * `let x = {
       *   "a": 1,
       *   "b": 2,
       *  }` -> object is written on multiple lines, break the group */
      let forceBreak = loc.loc_start.pos_lnum < loc.loc_end.pos_lnum

      Doc.breakableGroup(
        ~forceBreak,
        Doc.concat(list{
          Doc.lbrace,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list{Doc.text(","), Doc.line}),
                List.map(row => printBsObjectRow(row, cmtTbl), rows),
              ),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rbrace,
        }),
      )
    | extension => printExtension(~atModuleLvl=false, extension, cmtTbl)
    }
  | Pexp_apply(_) =>
    if ParsetreeViewer.isUnaryExpression(e) {
      printUnaryExpression(e, cmtTbl)
    } else if ParsetreeViewer.isTemplateLiteral(e) {
      printTemplateLiteral(e, cmtTbl)
    } else if ParsetreeViewer.isBinaryExpression(e) {
      printBinaryExpression(e, cmtTbl)
    } else {
      printPexpApply(e, cmtTbl)
    }
  | Pexp_unreachable => Doc.dot
  | Pexp_field(expr, longidentLoc) =>
    let lhs = {
      let doc = printExpressionWithComments(expr, cmtTbl)
      switch Parens.fieldExpr(expr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr, braces)
      | Nothing => doc
      }
    }

    Doc.concat(list{lhs, Doc.dot, printLidentPath(longidentLoc, cmtTbl)})
  | Pexp_setfield(expr1, longidentLoc, expr2) =>
    printSetFieldExpr(e.pexp_attributes, expr1, longidentLoc, expr2, e.pexp_loc, cmtTbl)
  | Pexp_ifthenelse(_ifExpr, _thenExpr, _elseExpr) if ParsetreeViewer.isTernaryExpr(e) =>
    let (parts, alternate) = ParsetreeViewer.collectTernaryParts(e)
    let ternaryDoc = switch parts {
    | list{(condition1, consequent1), ...rest} =>
      Doc.group(
        Doc.concat(list{
          printTernaryOperand(condition1, cmtTbl),
          Doc.indent(
            Doc.concat(list{
              Doc.line,
              Doc.indent(
                Doc.concat(list{Doc.text("? "), printTernaryOperand(consequent1, cmtTbl)}),
              ),
              Doc.concat(
                List.map(
                  ((condition, consequent)) =>
                    Doc.concat(list{
                      Doc.line,
                      Doc.text(": "),
                      printTernaryOperand(condition, cmtTbl),
                      Doc.line,
                      Doc.text("? "),
                      printTernaryOperand(consequent, cmtTbl),
                    }),
                  rest,
                ),
              ),
              Doc.line,
              Doc.text(": "),
              Doc.indent(printTernaryOperand(alternate, cmtTbl)),
            }),
          ),
        }),
      )
    | _ => Doc.nil
    }

    let attrs = ParsetreeViewer.filterTernaryAttributes(e.pexp_attributes)
    let needsParens = switch ParsetreeViewer.filterParsingAttrs(attrs) {
    | list{} => false
    | _ => true
    }

    Doc.concat(list{
      printAttributes(attrs, cmtTbl),
      if needsParens {
        addParens(ternaryDoc)
      } else {
        ternaryDoc
      },
    })
  | Pexp_ifthenelse(_ifExpr, _thenExpr, _elseExpr) =>
    let (ifs, elseExpr) = ParsetreeViewer.collectIfExpressions(e)
    printIfChain(e.pexp_attributes, ifs, elseExpr, cmtTbl)
  | Pexp_while(expr1, expr2) =>
    let condition = {
      let doc = printExpressionWithComments(expr1, cmtTbl)
      switch Parens.expr(expr1) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr1, braces)
      | Nothing => doc
      }
    }

    Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat(list{
        Doc.text("while "),
        if ParsetreeViewer.isBlockExpr(expr1) {
          condition
        } else {
          Doc.group(Doc.ifBreaks(addParens(condition), condition))
        },
        Doc.space,
        printExpressionBlock(~braces=true, expr2, cmtTbl),
      }),
    )
  | Pexp_for(pattern, fromExpr, toExpr, directionFlag, body) =>
    Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat(list{
        Doc.text("for "),
        printPattern(pattern, cmtTbl),
        Doc.text(" in "),
        {
          let doc = printExpressionWithComments(fromExpr, cmtTbl)
          switch Parens.expr(fromExpr) {
          | Parens.Parenthesized => addParens(doc)
          | Braced(braces) => printBraces(doc, fromExpr, braces)
          | Nothing => doc
          }
        },
        printDirectionFlag(directionFlag),
        {
          let doc = printExpressionWithComments(toExpr, cmtTbl)
          switch Parens.expr(toExpr) {
          | Parens.Parenthesized => addParens(doc)
          | Braced(braces) => printBraces(doc, toExpr, braces)
          | Nothing => doc
          }
        },
        Doc.space,
        printExpressionBlock(~braces=true, body, cmtTbl),
      }),
    )
  | Pexp_constraint(
      {pexp_desc: Pexp_pack(modExpr)},
      {ptyp_desc: Ptyp_package(packageType), ptyp_loc},
    ) =>
    Doc.group(
      Doc.concat(list{
        Doc.text("module("),
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            printModExpr(modExpr, cmtTbl),
            Doc.text(": "),
            printComments(
              printPackageType(~printModuleKeywordAndParens=false, packageType, cmtTbl),
              cmtTbl,
              ptyp_loc,
            ),
          }),
        ),
        Doc.softLine,
        Doc.rparen,
      }),
    )

  | Pexp_constraint(expr, typ) =>
    let exprDoc = {
      let doc = printExpressionWithComments(expr, cmtTbl)
      switch Parens.expr(expr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr, braces)
      | Nothing => doc
      }
    }

    Doc.concat(list{exprDoc, Doc.text(": "), printTypExpr(typ, cmtTbl)})
  | Pexp_letmodule({txt: _modName}, _modExpr, _expr) =>
    printExpressionBlock(~braces=true, e, cmtTbl)
  | Pexp_letexception(_extensionConstructor, _expr) => printExpressionBlock(~braces=true, e, cmtTbl)
  | Pexp_assert(expr) =>
    let rhs = {
      let doc = printExpressionWithComments(expr, cmtTbl)
      switch Parens.lazyOrAssertExprRhs(expr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr, braces)
      | Nothing => doc
      }
    }

    Doc.concat(list{Doc.text("assert "), rhs})
  | Pexp_lazy(expr) =>
    let rhs = {
      let doc = printExpressionWithComments(expr, cmtTbl)
      switch Parens.lazyOrAssertExprRhs(expr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr, braces)
      | Nothing => doc
      }
    }

    Doc.group(Doc.concat(list{Doc.text("lazy "), rhs}))
  | Pexp_open(_overrideFlag, _longidentLoc, _expr) => printExpressionBlock(~braces=true, e, cmtTbl)
  | Pexp_pack(modExpr) =>
    Doc.group(
      Doc.concat(list{
        Doc.text("module("),
        Doc.indent(Doc.concat(list{Doc.softLine, printModExpr(modExpr, cmtTbl)})),
        Doc.softLine,
        Doc.rparen,
      }),
    )
  | Pexp_sequence(_) => printExpressionBlock(~braces=true, e, cmtTbl)
  | Pexp_let(_) => printExpressionBlock(~braces=true, e, cmtTbl)
  | Pexp_fun(Nolabel, None, {ppat_desc: Ppat_var({txt: "__x"})}, {pexp_desc: Pexp_apply(_)}) =>
    /* (__x) => f(a, __x, c) -----> f(a, _, c) */
    printExpressionWithComments(ParsetreeViewer.rewriteUnderscoreApply(e), cmtTbl)
  | Pexp_fun(_) | Pexp_newtype(_) =>
    let (attrsOnArrow, parameters, returnExpr) = ParsetreeViewer.funExpr(e)
    let (uncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(attrsOnArrow)

    let (returnExpr, typConstraint) = switch returnExpr.pexp_desc {
    | Pexp_constraint(expr, typ) => (
        {
          ...expr,
          pexp_attributes: List.concat(list{expr.pexp_attributes, returnExpr.pexp_attributes}),
        },
        Some(typ),
      )
    | _ => (returnExpr, None)
    }

    let hasConstraint = switch typConstraint {
    | Some(_) => true
    | None => false
    }
    let parametersDoc = printExprFunParameters(
      ~inCallback=NoCallback,
      ~uncurried,
      ~hasConstraint,
      parameters,
      cmtTbl,
    )

    let returnExprDoc = {
      let (optBraces, _) = ParsetreeViewer.processBracesAttr(returnExpr)
      let shouldInline = switch (returnExpr.pexp_desc, optBraces) {
      | (_, Some(_)) => true
      | (
          Pexp_array(_)
          | Pexp_tuple(_)
          | Pexp_construct(_, Some(_))
          | Pexp_record(_),
          _,
        ) => true
      | _ => false
      }

      let shouldIndent = switch returnExpr.pexp_desc {
      | Pexp_sequence(_)
      | Pexp_let(_)
      | Pexp_letmodule(_)
      | Pexp_letexception(_)
      | Pexp_open(_) => false
      | _ => true
      }

      let returnDoc = {
        let doc = printExpressionWithComments(returnExpr, cmtTbl)
        switch Parens.expr(returnExpr) {
        | Parens.Parenthesized => addParens(doc)
        | Braced(braces) => printBraces(doc, returnExpr, braces)
        | Nothing => doc
        }
      }

      if shouldInline {
        Doc.concat(list{Doc.space, returnDoc})
      } else {
        Doc.group(
          if shouldIndent {
            Doc.indent(Doc.concat(list{Doc.line, returnDoc}))
          } else {
            Doc.concat(list{Doc.space, returnDoc})
          },
        )
      }
    }

    let typConstraintDoc = switch typConstraint {
    | Some(typ) =>
      let typDoc = {
        let doc = printTypExpr(typ, cmtTbl)
        if Parens.arrowReturnTypExpr(typ) {
          addParens(doc)
        } else {
          doc
        }
      }

      Doc.concat(list{Doc.text(": "), typDoc})
    | _ => Doc.nil
    }

    let attrs = printAttributes(attrs, cmtTbl)
    Doc.group(
      Doc.concat(list{attrs, parametersDoc, typConstraintDoc, Doc.text(" =>"), returnExprDoc}),
    )
  | Pexp_try(expr, cases) =>
    let exprDoc = {
      let doc = printExpressionWithComments(expr, cmtTbl)
      switch Parens.expr(expr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr, braces)
      | Nothing => doc
      }
    }

    Doc.concat(list{Doc.text("try "), exprDoc, Doc.text(" catch "), printCases(cases, cmtTbl)})
  | Pexp_match(_, list{_, _}) if ParsetreeViewer.isIfLetExpr(e) =>
    let (ifs, elseExpr) = ParsetreeViewer.collectIfExpressions(e)
    printIfChain(e.pexp_attributes, ifs, elseExpr, cmtTbl)
  | Pexp_match(expr, cases) =>
    let exprDoc = {
      let doc = printExpressionWithComments(expr, cmtTbl)
      switch Parens.expr(expr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr, braces)
      | Nothing => doc
      }
    }

    Doc.concat(list{Doc.text("switch "), exprDoc, Doc.space, printCases(cases, cmtTbl)})
  | Pexp_function(cases) => Doc.concat(list{Doc.text("x => switch x "), printCases(cases, cmtTbl)})
  | Pexp_coerce(expr, typOpt, typ) =>
    let docExpr = printExpressionWithComments(expr, cmtTbl)
    let docTyp = printTypExpr(typ, cmtTbl)
    let ofType = switch typOpt {
    | None => Doc.nil
    | Some(typ1) => Doc.concat(list{Doc.text(": "), printTypExpr(typ1, cmtTbl)})
    }

    Doc.concat(list{Doc.lparen, docExpr, ofType, Doc.text(" :> "), docTyp, Doc.rparen})
  | Pexp_send(parentExpr, label) =>
    let parentDoc = {
      let doc = printExpressionWithComments(parentExpr, cmtTbl)
      switch Parens.unaryExprOperand(parentExpr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, parentExpr, braces)
      | Nothing => doc
      }
    }

    let member = {
      let memberDoc = printComments(Doc.text(label.txt), cmtTbl, label.loc)
      Doc.concat(list{Doc.text("\""), memberDoc, Doc.text("\"")})
    }

    Doc.group(Doc.concat(list{parentDoc, Doc.lbracket, member, Doc.rbracket}))
  | Pexp_new(_) => Doc.text("Pexp_new not impemented in printer")
  | Pexp_setinstvar(_) => Doc.text("Pexp_setinstvar not impemented in printer")
  | Pexp_override(_) => Doc.text("Pexp_override not impemented in printer")
  | Pexp_poly(_) => Doc.text("Pexp_poly not impemented in printer")
  | Pexp_object(_) => Doc.text("Pexp_object not impemented in printer")
  }

  let shouldPrintItsOwnAttributes = switch e.pexp_desc {
  | Pexp_apply(_)
  | Pexp_fun(_)
  | Pexp_newtype(_)
  | Pexp_setfield(_)
  | Pexp_ifthenelse(_) => true
  | Pexp_match(_) if ParsetreeViewer.isIfLetExpr(e) => true
  | Pexp_construct(_) if ParsetreeViewer.hasJsxAttribute(e.pexp_attributes) => true
  | _ => false
  }

  switch e.pexp_attributes {
  | list{} => printedExpression
  | attrs if !shouldPrintItsOwnAttributes =>
    Doc.group(Doc.concat(list{printAttributes(attrs, cmtTbl), printedExpression}))
  | _ => printedExpression
  }
}

and printPexpFun = (~inCallback, e, cmtTbl) => {
  let (attrsOnArrow, parameters, returnExpr) = ParsetreeViewer.funExpr(e)
  let (uncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(attrsOnArrow)

  let (returnExpr, typConstraint) = switch returnExpr.pexp_desc {
  | Pexp_constraint(expr, typ) => (
      {
        ...expr,
        pexp_attributes: List.concat(list{expr.pexp_attributes, returnExpr.pexp_attributes}),
      },
      Some(typ),
    )
  | _ => (returnExpr, None)
  }

  let parametersDoc = printExprFunParameters(
    ~inCallback,
    ~uncurried,
    ~hasConstraint=switch typConstraint {
    | Some(_) => true
    | None => false
    },
    parameters,
    cmtTbl,
  )
  let returnShouldIndent = switch returnExpr.pexp_desc {
  | Pexp_sequence(_)
  | Pexp_let(_)
  | Pexp_letmodule(_)
  | Pexp_letexception(_)
  | Pexp_open(_) => false
  | _ => true
  }

  let returnExprDoc = {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(returnExpr)
    let shouldInline = switch (returnExpr.pexp_desc, optBraces) {
    | (_, Some(_)) => true
    | (
        Pexp_array(_)
        | Pexp_tuple(_)
        | Pexp_construct(_, Some(_))
        | Pexp_record(_),
        _,
      ) => true
    | _ => false
    }

    let returnDoc = {
      let doc = printExpressionWithComments(returnExpr, cmtTbl)
      switch Parens.expr(returnExpr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, returnExpr, braces)
      | Nothing => doc
      }
    }

    if shouldInline {
      Doc.concat(list{Doc.space, returnDoc})
    } else {
      Doc.group(
        if returnShouldIndent {
          Doc.concat(list{
            Doc.indent(Doc.concat(list{Doc.line, returnDoc})),
            switch inCallback {
            | FitsOnOneLine | ArgumentsFitOnOneLine => Doc.softLine
            | _ => Doc.nil
            },
          })
        } else {
          Doc.concat(list{Doc.space, returnDoc})
        },
      )
    }
  }

  let typConstraintDoc = switch typConstraint {
  | Some(typ) => Doc.concat(list{Doc.text(": "), printTypExpr(typ, cmtTbl)})
  | _ => Doc.nil
  }

  Doc.concat(list{
    printAttributes(attrs, cmtTbl),
    parametersDoc,
    typConstraintDoc,
    Doc.text(" =>"),
    returnExprDoc,
  })
}

and printTernaryOperand = (expr, cmtTbl) => {
  let doc = printExpressionWithComments(expr, cmtTbl)
  switch Parens.ternaryOperand(expr) {
  | Parens.Parenthesized => addParens(doc)
  | Braced(braces) => printBraces(doc, expr, braces)
  | Nothing => doc
  }
}

and printSetFieldExpr = (attrs, lhs, longidentLoc, rhs, loc, cmtTbl) => {
  let rhsDoc = {
    let doc = printExpressionWithComments(rhs, cmtTbl)
    switch Parens.setFieldExprRhs(rhs) {
    | Parens.Parenthesized => addParens(doc)
    | Braced(braces) => printBraces(doc, rhs, braces)
    | Nothing => doc
    }
  }

  let lhsDoc = {
    let doc = printExpressionWithComments(lhs, cmtTbl)
    switch Parens.fieldExpr(lhs) {
    | Parens.Parenthesized => addParens(doc)
    | Braced(braces) => printBraces(doc, lhs, braces)
    | Nothing => doc
    }
  }

  let shouldIndent = ParsetreeViewer.isBinaryExpression(rhs)
  let doc = Doc.group(
    Doc.concat(list{
      lhsDoc,
      Doc.dot,
      printLidentPath(longidentLoc, cmtTbl),
      Doc.text(" ="),
      if shouldIndent {
        Doc.group(Doc.indent(Doc.concat(list{Doc.line, rhsDoc})))
      } else {
        Doc.concat(list{Doc.space, rhsDoc})
      },
    }),
  )
  let doc = switch attrs {
  | list{} => doc
  | attrs => Doc.group(Doc.concat(list{printAttributes(attrs, cmtTbl), doc}))
  }

  printComments(doc, cmtTbl, loc)
}

and printTemplateLiteral = (expr, cmtTbl) => {
  let tag = ref("js")
  let rec walkExpr = expr => {
    open Parsetree
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident("^")})},
        list{(Nolabel, arg1), (Nolabel, arg2)},
      ) =>
      let lhs = walkExpr(arg1)
      let rhs = walkExpr(arg2)
      Doc.concat(list{lhs, rhs})
    | Pexp_constant(Pconst_string(txt, Some(prefix))) =>
      tag := prefix
      printStringContents(txt)
    | _ =>
      let doc = printExpressionWithComments(expr, cmtTbl)
      Doc.group(Doc.concat(list{Doc.text("${"), Doc.indent(doc), Doc.rbrace}))
    }
  }

  let content = walkExpr(expr)
  Doc.concat(list{
    if tag.contents == "js" {
      Doc.nil
    } else {
      Doc.text(tag.contents)
    },
    Doc.text("`"),
    content,
    Doc.text("`"),
  })
}

and printUnaryExpression = (expr, cmtTbl) => {
  let printUnaryOperator = op =>
    Doc.text(
      switch op {
      | "~+" => "+"
      | "~+." => "+."
      | "~-" => "-"
      | "~-." => "-."
      | "not" => "!"
      | _ => assert false
      },
    )
  switch expr.pexp_desc {
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
      list{(Nolabel, operand)},
    ) =>
    let printedOperand = {
      let doc = printExpressionWithComments(operand, cmtTbl)
      switch Parens.unaryExprOperand(operand) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, operand, braces)
      | Nothing => doc
      }
    }

    let doc = Doc.concat(list{printUnaryOperator(operator), printedOperand})
    printComments(doc, cmtTbl, expr.pexp_loc)
  | _ => assert false
  }
}

and printBinaryExpression = (expr: Parsetree.expression, cmtTbl) => {
  let printBinaryOperator = (~inlineRhs, operator) => {
    let operatorTxt = switch operator {
    | "^" => "++"
    | "=" => "=="
    | "==" => "==="
    | "<>" => "!="
    | "!=" => "!=="
    | txt => txt
    }

    let spacingBeforeOperator = if operator == "|." {
      Doc.softLine
    } else if operator == "|>" {
      Doc.line
    } else {
      Doc.space
    }

    let spacingAfterOperator = if operator == "|." {
      Doc.nil
    } else if operator == "|>" {
      Doc.space
    } else if inlineRhs {
      Doc.space
    } else {
      Doc.line
    }

    Doc.concat(list{spacingBeforeOperator, Doc.text(operatorTxt), spacingAfterOperator})
  }

  let printOperand = (~isLhs, expr, parentOperator) => {
    let rec flatten = (~isLhs, expr, parentOperator) =>
      if ParsetreeViewer.isBinaryExpression(expr) {
        switch expr {
        | {
            pexp_desc: Pexp_apply(
              {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
              list{(_, left), (_, right)},
            ),
          } =>
          if (
            ParsetreeViewer.flattenableOperators(parentOperator, operator) &&
            !ParsetreeViewer.hasAttributes(expr.pexp_attributes)
          ) {
            let leftPrinted = flatten(~isLhs=true, left, operator)
            let rightPrinted = {
              let (_, rightAttrs) = ParsetreeViewer.partitionPrintableAttributes(
                right.pexp_attributes,
              )

              let doc = printExpressionWithComments({...right, pexp_attributes: rightAttrs}, cmtTbl)

              let doc = if Parens.flattenOperandRhs(parentOperator, right) {
                Doc.concat(list{Doc.lparen, doc, Doc.rparen})
              } else {
                doc
              }

              let printableAttrs = ParsetreeViewer.filterPrintableAttributes(right.pexp_attributes)

              let doc = Doc.concat(list{printAttributes(printableAttrs, cmtTbl), doc})
              switch printableAttrs {
              | list{} => doc
              | _ => addParens(doc)
              }
            }

            let doc = Doc.concat(list{
              leftPrinted,
              printBinaryOperator(~inlineRhs=false, operator),
              rightPrinted,
            })
            let doc = if !isLhs && Parens.rhsBinaryExprOperand(operator, expr) {
              Doc.concat(list{Doc.lparen, doc, Doc.rparen})
            } else {
              doc
            }

            printComments(doc, cmtTbl, expr.pexp_loc)
          } else {
            let doc = printExpressionWithComments({...expr, pexp_attributes: list{}}, cmtTbl)
            let doc = if (
              Parens.subBinaryExprOperand(parentOperator, operator) ||
              (expr.pexp_attributes != list{} &&
                (ParsetreeViewer.isBinaryExpression(expr) || ParsetreeViewer.isTernaryExpr(expr)))
            ) {
              Doc.concat(list{Doc.lparen, doc, Doc.rparen})
            } else {
              doc
            }
            Doc.concat(list{printAttributes(expr.pexp_attributes, cmtTbl), doc})
          }
        | _ => assert false
        }
      } else {
        switch expr.pexp_desc {
        | Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Longident.Lident("^"), loc})},
            list{(Nolabel, _), (Nolabel, _)},
          ) if loc.loc_ghost =>
          let doc = printTemplateLiteral(expr, cmtTbl)
          printComments(doc, cmtTbl, expr.Parsetree.pexp_loc)
        | Pexp_setfield(lhs, field, rhs) =>
          let doc = printSetFieldExpr(expr.pexp_attributes, lhs, field, rhs, expr.pexp_loc, cmtTbl)
          if isLhs {
            addParens(doc)
          } else {
            doc
          }
        | Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Longident.Lident("#=")})},
            list{(Nolabel, lhs), (Nolabel, rhs)},
          ) =>
          let rhsDoc = printExpressionWithComments(rhs, cmtTbl)
          let lhsDoc = printExpressionWithComments(lhs, cmtTbl)
          /* TODO: unify indentation of "=" */
          let shouldIndent = ParsetreeViewer.isBinaryExpression(rhs)
          let doc = Doc.group(
            Doc.concat(list{
              lhsDoc,
              Doc.text(" ="),
              if shouldIndent {
                Doc.group(Doc.indent(Doc.concat(list{Doc.line, rhsDoc})))
              } else {
                Doc.concat(list{Doc.space, rhsDoc})
              },
            }),
          )
          let doc = switch expr.pexp_attributes {
          | list{} => doc
          | attrs => Doc.group(Doc.concat(list{printAttributes(attrs, cmtTbl), doc}))
          }

          if isLhs {
            addParens(doc)
          } else {
            doc
          }
        | _ =>
          let doc = printExpressionWithComments(expr, cmtTbl)
          switch Parens.binaryExprOperand(~isLhs, expr) {
          | Parens.Parenthesized => addParens(doc)
          | Braced(braces) => printBraces(doc, expr, braces)
          | Nothing => doc
          }
        }
      }

    flatten(~isLhs, expr, parentOperator)
  }

  switch expr.pexp_desc {
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Lident(("|." | "|>") as op)})},
      list{(Nolabel, lhs), (Nolabel, rhs)},
    ) if !(ParsetreeViewer.isBinaryExpression(lhs) || ParsetreeViewer.isBinaryExpression(rhs)) =>
    let lhsHasCommentBelow = hasCommentBelow(cmtTbl, lhs.pexp_loc)
    let lhsDoc = printOperand(~isLhs=true, lhs, op)
    let rhsDoc = printOperand(~isLhs=false, rhs, op)
    Doc.group(
      Doc.concat(list{
        lhsDoc,
        switch (lhsHasCommentBelow, op) {
        | (true, "|.") => Doc.concat(list{Doc.softLine, Doc.text("->")})
        | (false, "|.") => Doc.text("->")
        | (true, "|>") => Doc.concat(list{Doc.line, Doc.text("|> ")})
        | (false, "|>") => Doc.text(" |> ")
        | _ => Doc.nil
        },
        rhsDoc,
      }),
    )
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
      list{(Nolabel, lhs), (Nolabel, rhs)},
    ) =>
    let right = {
      let operatorWithRhs = {
        let rhsDoc = printOperand(~isLhs=false, rhs, operator)
        Doc.concat(list{
          printBinaryOperator(~inlineRhs=ParsetreeViewer.shouldInlineRhsBinaryExpr(rhs), operator),
          rhsDoc,
        })
      }
      if ParsetreeViewer.shouldIndentBinaryExpr(expr) {
        Doc.group(Doc.indent(operatorWithRhs))
      } else {
        operatorWithRhs
      }
    }

    let doc = Doc.group(Doc.concat(list{printOperand(~isLhs=true, lhs, operator), right}))
    Doc.group(
      Doc.concat(list{
        printAttributes(expr.pexp_attributes, cmtTbl),
        switch Parens.binaryExpr({
          ...expr,
          pexp_attributes: List.filter(attr =>
            switch attr {
            | ({Location.txt: "ns.braces"}, _) => false
            | _ => true
            }
          , expr.pexp_attributes),
        }) {
        | Braced(bracesLoc) => printBraces(doc, expr, bracesLoc)
        | Parenthesized => addParens(doc)
        | Nothing => doc
        },
      }),
    )
  | _ => Doc.nil
  }
}

/* callExpr(arg1, arg2) */
and printPexpApply = (expr, cmtTbl) =>
  switch expr.pexp_desc {
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Lident("##")})},
      list{(Nolabel, parentExpr), (Nolabel, memberExpr)},
    ) =>
    let parentDoc = {
      let doc = printExpressionWithComments(parentExpr, cmtTbl)
      switch Parens.unaryExprOperand(parentExpr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, parentExpr, braces)
      | Nothing => doc
      }
    }

    let member = {
      let memberDoc = switch memberExpr.pexp_desc {
      | Pexp_ident(lident) => printComments(printLongident(lident.txt), cmtTbl, memberExpr.pexp_loc)
      | _ => printExpressionWithComments(memberExpr, cmtTbl)
      }

      Doc.concat(list{Doc.text("\""), memberDoc, Doc.text("\"")})
    }

    Doc.group(
      Doc.concat(list{
        printAttributes(expr.pexp_attributes, cmtTbl),
        parentDoc,
        Doc.lbracket,
        member,
        Doc.rbracket,
      }),
    )
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Lident("#=")})},
      list{(Nolabel, lhs), (Nolabel, rhs)},
    ) =>
    let rhsDoc = {
      let doc = printExpressionWithComments(rhs, cmtTbl)
      switch Parens.expr(rhs) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, rhs, braces)
      | Nothing => doc
      }
    }

    /* TODO: unify indentation of "=" */
    let shouldIndent = !ParsetreeViewer.isBracedExpr(rhs) && ParsetreeViewer.isBinaryExpression(rhs)
    let doc = Doc.group(
      Doc.concat(list{
        printExpressionWithComments(lhs, cmtTbl),
        Doc.text(" ="),
        if shouldIndent {
          Doc.group(Doc.indent(Doc.concat(list{Doc.line, rhsDoc})))
        } else {
          Doc.concat(list{Doc.space, rhsDoc})
        },
      }),
    )
    switch expr.pexp_attributes {
    | list{} => doc
    | attrs => Doc.group(Doc.concat(list{printAttributes(attrs, cmtTbl), doc}))
    }
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Ldot(Lident("Array"), "get")})},
      list{(Nolabel, parentExpr), (Nolabel, memberExpr)},
    ) if !ParsetreeViewer.isRewrittenUnderscoreApplySugar(parentExpr) =>
    /* Don't print the Array.get(_, 0) sugar a.k.a. (__x) => Array.get(__x, 0) as _[0] */
    let member = {
      let memberDoc = {
        let doc = printExpressionWithComments(memberExpr, cmtTbl)
        switch Parens.expr(memberExpr) {
        | Parens.Parenthesized => addParens(doc)
        | Braced(braces) => printBraces(doc, memberExpr, braces)
        | Nothing => doc
        }
      }

      let shouldInline = switch memberExpr.pexp_desc {
      | Pexp_constant(_) | Pexp_ident(_) => true
      | _ => false
      }

      if shouldInline {
        memberDoc
      } else {
        Doc.concat(list{Doc.indent(Doc.concat(list{Doc.softLine, memberDoc})), Doc.softLine})
      }
    }

    let parentDoc = {
      let doc = printExpressionWithComments(parentExpr, cmtTbl)
      switch Parens.unaryExprOperand(parentExpr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, parentExpr, braces)
      | Nothing => doc
      }
    }

    Doc.group(
      Doc.concat(list{
        printAttributes(expr.pexp_attributes, cmtTbl),
        parentDoc,
        Doc.lbracket,
        member,
        Doc.rbracket,
      }),
    )
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt: Longident.Ldot(Lident("Array"), "set")})},
      list{(Nolabel, parentExpr), (Nolabel, memberExpr), (Nolabel, targetExpr)},
    ) =>
    let member = {
      let memberDoc = {
        let doc = printExpressionWithComments(memberExpr, cmtTbl)
        switch Parens.expr(memberExpr) {
        | Parens.Parenthesized => addParens(doc)
        | Braced(braces) => printBraces(doc, memberExpr, braces)
        | Nothing => doc
        }
      }

      let shouldInline = switch memberExpr.pexp_desc {
      | Pexp_constant(_) | Pexp_ident(_) => true
      | _ => false
      }

      if shouldInline {
        memberDoc
      } else {
        Doc.concat(list{Doc.indent(Doc.concat(list{Doc.softLine, memberDoc})), Doc.softLine})
      }
    }

    let shouldIndentTargetExpr = if ParsetreeViewer.isBracedExpr(targetExpr) {
      false
    } else {
      ParsetreeViewer.isBinaryExpression(targetExpr) ||
      switch targetExpr {
      | {
          pexp_attributes: list{({Location.txt: "ns.ternary"}, _)},
          pexp_desc: Pexp_ifthenelse(ifExpr, _, _),
        } =>
        ParsetreeViewer.isBinaryExpression(ifExpr) ||
        ParsetreeViewer.hasAttributes(ifExpr.pexp_attributes)
      | {pexp_desc: Pexp_newtype(_)} => false
      | e => ParsetreeViewer.hasAttributes(e.pexp_attributes) || ParsetreeViewer.isArrayAccess(e)
      }
    }

    let targetExpr = {
      let doc = printExpressionWithComments(targetExpr, cmtTbl)
      switch Parens.expr(targetExpr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, targetExpr, braces)
      | Nothing => doc
      }
    }

    let parentDoc = {
      let doc = printExpressionWithComments(parentExpr, cmtTbl)
      switch Parens.unaryExprOperand(parentExpr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, parentExpr, braces)
      | Nothing => doc
      }
    }

    Doc.group(
      Doc.concat(list{
        printAttributes(expr.pexp_attributes, cmtTbl),
        parentDoc,
        Doc.lbracket,
        member,
        Doc.rbracket,
        Doc.text(" ="),
        if shouldIndentTargetExpr {
          Doc.indent(Doc.concat(list{Doc.line, targetExpr}))
        } else {
          Doc.concat(list{Doc.space, targetExpr})
        },
      }),
    )
  /* TODO: cleanup, are those branches even remotely performant? */
  | Pexp_apply({pexp_desc: Pexp_ident(lident)}, args) if ParsetreeViewer.isJsxExpression(expr) =>
    printJsxExpression(lident, args, cmtTbl)
  | Pexp_apply(callExpr, args) =>
    let args = List.map(((lbl, arg)) => (lbl, ParsetreeViewer.rewriteUnderscoreApply(arg)), args)

    let (uncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(expr.pexp_attributes)

    let callExprDoc = {
      let doc = printExpressionWithComments(callExpr, cmtTbl)
      switch Parens.callExpr(callExpr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, callExpr, braces)
      | Nothing => doc
      }
    }

    if ParsetreeViewer.requiresSpecialCallbackPrintingFirstArg(args) {
      let argsDoc = printArgumentsWithCallbackInFirstPosition(~uncurried, args, cmtTbl)

      Doc.concat(list{printAttributes(attrs, cmtTbl), callExprDoc, argsDoc})
    } else if ParsetreeViewer.requiresSpecialCallbackPrintingLastArg(args) {
      let argsDoc = printArgumentsWithCallbackInLastPosition(~uncurried, args, cmtTbl)

      /*
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
       */
      let maybeBreakParent = if Doc.willBreak(argsDoc) {
        Doc.breakParent
      } else {
        Doc.nil
      }

      Doc.concat(list{maybeBreakParent, printAttributes(attrs, cmtTbl), callExprDoc, argsDoc})
    } else {
      let argsDoc = printArguments(~uncurried, args, cmtTbl)
      Doc.concat(list{printAttributes(attrs, cmtTbl), callExprDoc, argsDoc})
    }
  | _ => assert false
  }

and printJsxExpression = (lident, args, cmtTbl) => {
  let name = printJsxName(lident)
  let (formattedProps, children) = printJsxProps(args, cmtTbl)
  /* <div className="test" /> */
  let isSelfClosing = switch children {
  | Some({Parsetree.pexp_desc: Pexp_construct({txt: Longident.Lident("[]")}, None)}) => true
  | _ => false
  }

  Doc.group(
    Doc.concat(list{
      Doc.group(
        Doc.concat(list{
          printComments(Doc.concat(list{Doc.lessThan, name}), cmtTbl, lident.Asttypes.loc),
          formattedProps,
          if isSelfClosing {
            Doc.concat(list{Doc.line, Doc.text("/>")})
          } else {
            Doc.nil
          },
        }),
      ),
      if isSelfClosing {
        Doc.nil
      } else {
        Doc.concat(list{
          Doc.greaterThan,
          Doc.indent(
            Doc.concat(list{
              Doc.line,
              switch children {
              | Some(childrenExpression) => printJsxChildren(childrenExpression, cmtTbl)
              | None => Doc.nil
              },
            }),
          ),
          Doc.line,
          Doc.text("</"),
          name,
          Doc.greaterThan,
        })
      },
    }),
  )
}

and printJsxFragment = (expr, cmtTbl) => {
  let opening = Doc.text("<>")
  let closing = Doc.text("</>")
  /* let (children, _) = ParsetreeViewer.collectListExpressions expr in */
  Doc.group(
    Doc.concat(list{
      opening,
      switch expr.pexp_desc {
      | Pexp_construct({txt: Longident.Lident("[]")}, None) => Doc.nil
      | _ => Doc.indent(Doc.concat(list{Doc.line, printJsxChildren(expr, cmtTbl)}))
      },
      Doc.line,
      closing,
    }),
  )
}

and printJsxChildren = (childrenExpr: Parsetree.expression, cmtTbl) =>
  switch childrenExpr.pexp_desc {
  | Pexp_construct({txt: Longident.Lident("::")}, _) =>
    let (children, _) = ParsetreeViewer.collectListExpressions(childrenExpr)
    Doc.group(Doc.join(~sep=Doc.line, List.map((expr: Parsetree.expression) => {
          let leadingLineCommentPresent = hasLeadingLineComment(cmtTbl, expr.pexp_loc)
          let exprDoc = printExpressionWithComments(expr, cmtTbl)
          switch Parens.jsxChildExpr(expr) {
          | Parenthesized | Braced(_) =>
            /* {(20: int)} make sure that we also protect the expression inside */
            let innerDoc = if Parens.bracedExpr(expr) {
              addParens(exprDoc)
            } else {
              exprDoc
            }
            if leadingLineCommentPresent {
              addBraces(innerDoc)
            } else {
              Doc.concat(list{Doc.lbrace, innerDoc, Doc.rbrace})
            }
          | Nothing => exprDoc
          }
        }, children)))
  | _ =>
    let leadingLineCommentPresent = hasLeadingLineComment(cmtTbl, childrenExpr.pexp_loc)
    let exprDoc = printExpressionWithComments(childrenExpr, cmtTbl)
    Doc.concat(list{
      Doc.dotdotdot,
      switch Parens.jsxChildExpr(childrenExpr) {
      | Parenthesized | Braced(_) =>
        let innerDoc = if Parens.bracedExpr(childrenExpr) {
          addParens(exprDoc)
        } else {
          exprDoc
        }
        if leadingLineCommentPresent {
          addBraces(innerDoc)
        } else {
          Doc.concat(list{Doc.lbrace, innerDoc, Doc.rbrace})
        }
      | Nothing => exprDoc
      },
    })
  }

and printJsxProps = (args, cmtTbl): (Doc.t, option<Parsetree.expression>) => {
  let rec loop = (props, args) =>
    switch args {
    | list{} => (Doc.nil, None)
    | list{
        (Asttypes.Labelled("children"), children),
        (
          Asttypes.Nolabel,
          {Parsetree.pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, None)},
        ),
      } =>
      let formattedProps = Doc.indent(
        switch props {
        | list{} => Doc.nil
        | props => Doc.concat(list{Doc.line, Doc.group(Doc.join(~sep=Doc.line, props |> List.rev))})
        },
      )
      (formattedProps, Some(children))
    | list{arg, ...args} =>
      let propDoc = printJsxProp(arg, cmtTbl)
      loop(list{propDoc, ...props}, args)
    }

  loop(list{}, args)
}

and printJsxProp = (arg, cmtTbl) =>
  switch arg {
  | (
      (Asttypes.Labelled(lblTxt) | Optional(lblTxt)) as lbl,
      {
        Parsetree.pexp_attributes: list{({Location.txt: "ns.namedArgLoc", loc: argLoc}, _)},
        pexp_desc: Pexp_ident({txt: Longident.Lident(ident)}),
      },
    ) if lblTxt == ident /* jsx punning */ =>
    switch lbl {
    | Nolabel => Doc.nil
    | Labelled(_lbl) => printComments(printIdentLike(ident), cmtTbl, argLoc)
    | Optional(_lbl) =>
      let doc = Doc.concat(list{Doc.question, printIdentLike(ident)})
      printComments(doc, cmtTbl, argLoc)
    }
  | (
      (Asttypes.Labelled(lblTxt) | Optional(lblTxt)) as lbl,
      {Parsetree.pexp_attributes: list{}, pexp_desc: Pexp_ident({txt: Longident.Lident(ident)})},
    ) if lblTxt == ident /* jsx punning when printing from Reason */ =>
    switch lbl {
    | Nolabel => Doc.nil
    | Labelled(_lbl) => printIdentLike(ident)
    | Optional(_lbl) => Doc.concat(list{Doc.question, printIdentLike(ident)})
    }
  | (lbl, expr) =>
    let (argLoc, expr) = switch expr.pexp_attributes {
    | list{({Location.txt: "ns.namedArgLoc", loc}, _), ...attrs} => (
        loc,
        {...expr, pexp_attributes: attrs},
      )
    | _ => (Location.none, expr)
    }

    let lblDoc = switch lbl {
    | Asttypes.Labelled(lbl) =>
      let lbl = printComments(printIdentLike(lbl), cmtTbl, argLoc)
      Doc.concat(list{lbl, Doc.equal})
    | Asttypes.Optional(lbl) =>
      let lbl = printComments(printIdentLike(lbl), cmtTbl, argLoc)
      Doc.concat(list{lbl, Doc.equal, Doc.question})
    | Nolabel => Doc.nil
    }

    let exprDoc = {
      let leadingLineCommentPresent = hasLeadingLineComment(cmtTbl, expr.pexp_loc)
      let doc = printExpressionWithComments(expr, cmtTbl)
      switch Parens.jsxPropExpr(expr) {
      | Parenthesized | Braced(_) =>
        /* {(20: int)} make sure that we also protect the expression inside */
        let innerDoc = if Parens.bracedExpr(expr) {
          addParens(doc)
        } else {
          doc
        }
        if leadingLineCommentPresent {
          addBraces(innerDoc)
        } else {
          Doc.concat(list{Doc.lbrace, innerDoc, Doc.rbrace})
        }
      | _ => doc
      }
    }

    let fullLoc = {...argLoc, loc_end: expr.pexp_loc.loc_end}
    printComments(Doc.concat(list{lblDoc, exprDoc}), cmtTbl, fullLoc)
  }

/* div -> div.
 * Navabar.createElement -> Navbar
 * Staff.Users.createElement -> Staff.Users */
and printJsxName = ({txt: lident}) => {
  let rec flatten = (acc, lident) =>
    switch lident {
    | Longident.Lident(txt) => list{txt, ...acc}
    | Ldot(lident, txt) =>
      let acc = if txt == "createElement" {
        acc
      } else {
        list{txt, ...acc}
      }
      flatten(acc, lident)
    | _ => acc
    }

  switch lident {
  | Longident.Lident(txt) => Doc.text(txt)
  | _ as lident =>
    let segments = flatten(list{}, lident)
    Doc.join(~sep=Doc.dot, List.map(Doc.text, segments))
  }
}

and printArgumentsWithCallbackInFirstPosition = (~uncurried, args, cmtTbl) => {
  /* Because the same subtree gets printed twice, we need to copy the cmtTbl.
   * consumed comments need to be marked not-consumed and reprinted…
   * Cheng's different comment algorithm will solve this. */
  let cmtTblCopy = CommentTable.copy(cmtTbl)
  let (callback, printedArgs) = switch args {
  | list{(lbl, expr), ...args} =>
    let lblDoc = switch lbl {
    | Asttypes.Nolabel => Doc.nil
    | Asttypes.Labelled(txt) => Doc.concat(list{Doc.tilde, printIdentLike(txt), Doc.equal})
    | Asttypes.Optional(txt) =>
      Doc.concat(list{Doc.tilde, printIdentLike(txt), Doc.equal, Doc.question})
    }

    let callback = Doc.concat(list{lblDoc, printPexpFun(~inCallback=FitsOnOneLine, expr, cmtTbl)})
    let callback = printComments(callback, cmtTbl, expr.pexp_loc)
    let printedArgs = Doc.join(
      ~sep=Doc.concat(list{Doc.comma, Doc.line}),
      List.map(arg => printArgument(arg, cmtTbl), args),
    )

    (callback, printedArgs)
  | _ => assert false
  }

  /* Thing.map((arg1, arg2) => MyModuleBlah.toList(argument), foo) */
  /* Thing.map((arg1, arg2) => {
   *   MyModuleBlah.toList(argument)
   * }, longArgumet, veryLooooongArgument)
   */
  let fitsOnOneLine = Doc.concat(list{
    if uncurried {
      Doc.text("(. ")
    } else {
      Doc.lparen
    },
    callback,
    Doc.comma,
    Doc.line,
    printedArgs,
    Doc.rparen,
  })

  /* Thing.map(
   *   (param1, parm2) => doStuff(param1, parm2),
   *   arg1,
   *   arg2,
   *   arg3,
   * )
   */
  let breakAllArgs = printArguments(~uncurried, args, cmtTblCopy)

  /* Sometimes one of the non-callback arguments will break.
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
   */
  if Doc.willBreak(printedArgs) {
    breakAllArgs
  } else {
    Doc.customLayout(list{fitsOnOneLine, breakAllArgs})
  }
}

and printArgumentsWithCallbackInLastPosition = (~uncurried, args, cmtTbl) => {
  /* Because the same subtree gets printed twice, we need to copy the cmtTbl.
   * consumed comments need to be marked not-consumed and reprinted…
   * Cheng's different comment algorithm will solve this. */
  let cmtTblCopy = CommentTable.copy(cmtTbl)
  let cmtTblCopy2 = CommentTable.copy(cmtTbl)
  let rec loop = (acc, args) =>
    switch args {
    | list{} => (Doc.nil, Doc.nil, Doc.nil)
    | list{(lbl, expr)} =>
      let lblDoc = switch lbl {
      | Asttypes.Nolabel => Doc.nil
      | Asttypes.Labelled(txt) => Doc.concat(list{Doc.tilde, printIdentLike(txt), Doc.equal})
      | Asttypes.Optional(txt) =>
        Doc.concat(list{Doc.tilde, printIdentLike(txt), Doc.equal, Doc.question})
      }

      let callbackFitsOnOneLine = {
        let pexpFunDoc = printPexpFun(~inCallback=FitsOnOneLine, expr, cmtTbl)
        let doc = Doc.concat(list{lblDoc, pexpFunDoc})
        printComments(doc, cmtTbl, expr.pexp_loc)
      }

      let callbackArgumentsFitsOnOneLine = {
        let pexpFunDoc = printPexpFun(~inCallback=ArgumentsFitOnOneLine, expr, cmtTblCopy)
        let doc = Doc.concat(list{lblDoc, pexpFunDoc})
        printComments(doc, cmtTblCopy, expr.pexp_loc)
      }

      (Doc.concat(List.rev(acc)), callbackFitsOnOneLine, callbackArgumentsFitsOnOneLine)
    | list{arg, ...args} =>
      let argDoc = printArgument(arg, cmtTbl)
      loop(list{Doc.line, Doc.comma, argDoc, ...acc}, args)
    }

  let (printedArgs, callback, callback2) = loop(list{}, args)

  /* Thing.map(foo, (arg1, arg2) => MyModuleBlah.toList(argument)) */
  let fitsOnOneLine = Doc.concat(list{
    if uncurried {
      Doc.text("(.")
    } else {
      Doc.lparen
    },
    printedArgs,
    callback,
    Doc.rparen,
  })

  /* Thing.map(longArgumet, veryLooooongArgument, (arg1, arg2) =>
   *   MyModuleBlah.toList(argument)
   * )
   */
  let arugmentsFitOnOneLine = Doc.concat(list{
    if uncurried {
      Doc.text("(.")
    } else {
      Doc.lparen
    },
    printedArgs,
    Doc.breakableGroup(~forceBreak=true, callback2),
    Doc.rparen,
  })

  /* Thing.map(
   *   arg1,
   *   arg2,
   *   arg3,
   *   (param1, parm2) => doStuff(param1, parm2)
   * )
   */
  let breakAllArgs = printArguments(~uncurried, args, cmtTblCopy2)

  /* Sometimes one of the non-callback arguments will break.
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
   */
  if Doc.willBreak(printedArgs) {
    breakAllArgs
  } else {
    Doc.customLayout(list{fitsOnOneLine, arugmentsFitOnOneLine, breakAllArgs})
  }
}

and printArguments = (~uncurried, args: list<(Asttypes.arg_label, Parsetree.expression)>, cmtTbl) =>
  switch args {
  | list{(Nolabel, {pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, _), pexp_loc: loc})} =>
    /* See "parseCallExpr", ghost unit expression is used the implement
     * arity zero vs arity one syntax.
     * Related: https://github.com/rescript-lang/syntax/issues/138 */
    switch (uncurried, loc.loc_ghost) {
    | (true, true) => Doc.text("(.)") /* arity zero */
    | (true, false) => Doc.text("(. ())") /* arity one */
    | _ => Doc.text("()")
    }
  | list{(Nolabel, arg)} if ParsetreeViewer.isHuggableExpression(arg) =>
    let argDoc = {
      let doc = printExpressionWithComments(arg, cmtTbl)
      switch Parens.expr(arg) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, arg, braces)
      | Nothing => doc
      }
    }

    Doc.concat(list{
      if uncurried {
        Doc.text("(. ")
      } else {
        Doc.lparen
      },
      argDoc,
      Doc.rparen,
    })
  | args =>
    Doc.group(
      Doc.concat(list{
        if uncurried {
          Doc.text("(.")
        } else {
          Doc.lparen
        },
        Doc.indent(
          Doc.concat(list{
            if uncurried {
              Doc.line
            } else {
              Doc.softLine
            },
            Doc.join(
              ~sep=Doc.concat(list{Doc.comma, Doc.line}),
              List.map(arg => printArgument(arg, cmtTbl), args),
            ),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rparen,
      }),
    )
  }

/*
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
 *   | ~ label-name = ? expr : type */
and printArgument = ((argLbl, arg), cmtTbl) =>
  switch (argLbl, arg) {
  /* ~a (punned) */
  | (
      Asttypes.Labelled(lbl),
      {
        pexp_desc: Pexp_ident({txt: Longident.Lident(name)}),
        pexp_attributes: list{} | list{({Location.txt: "ns.namedArgLoc"}, _)},
      } as argExpr,
    ) if lbl == name && !ParsetreeViewer.isBracedExpr(argExpr) =>
    let loc = switch arg.pexp_attributes {
    | list{({Location.txt: "ns.namedArgLoc", loc}, _), ..._} => loc
    | _ => arg.pexp_loc
    }

    let doc = Doc.concat(list{Doc.tilde, printIdentLike(lbl)})
    printComments(doc, cmtTbl, loc)

  /* ~a: int (punned) */
  | (
      Asttypes.Labelled(lbl),
      {
        pexp_desc: Pexp_constraint(
          {pexp_desc: Pexp_ident({txt: Longident.Lident(name)})} as argExpr,
          typ,
        ),
        pexp_loc,
        pexp_attributes: (list{} | list{({Location.txt: "ns.namedArgLoc"}, _)}) as attrs,
      },
    ) if lbl == name && !ParsetreeViewer.isBracedExpr(argExpr) =>
    let loc = switch attrs {
    | list{({Location.txt: "ns.namedArgLoc", loc}, _), ..._} => {...loc, loc_end: pexp_loc.loc_end}
    | _ => arg.pexp_loc
    }

    let doc = Doc.concat(list{
      Doc.tilde,
      printIdentLike(lbl),
      Doc.text(": "),
      printTypExpr(typ, cmtTbl),
    })
    printComments(doc, cmtTbl, loc)
  /* ~a? (optional lbl punned) */
  | (
      Asttypes.Optional(lbl),
      {
        pexp_desc: Pexp_ident({txt: Longident.Lident(name)}),
        pexp_attributes: list{} | list{({Location.txt: "ns.namedArgLoc"}, _)},
      },
    ) if lbl == name =>
    let loc = switch arg.pexp_attributes {
    | list{({Location.txt: "ns.namedArgLoc", loc}, _), ..._} => loc
    | _ => arg.pexp_loc
    }

    let doc = Doc.concat(list{Doc.tilde, printIdentLike(lbl), Doc.question})
    printComments(doc, cmtTbl, loc)
  | (_lbl, expr) =>
    let (argLoc, expr) = switch expr.pexp_attributes {
    | list{({Location.txt: "ns.namedArgLoc", loc}, _), ...attrs} => (
        loc,
        {...expr, pexp_attributes: attrs},
      )
    | _ => (expr.pexp_loc, expr)
    }

    let printedLbl = switch argLbl {
    | Asttypes.Nolabel => Doc.nil
    | Asttypes.Labelled(lbl) =>
      let doc = Doc.concat(list{Doc.tilde, printIdentLike(lbl), Doc.equal})
      printComments(doc, cmtTbl, argLoc)
    | Asttypes.Optional(lbl) =>
      let doc = Doc.concat(list{Doc.tilde, printIdentLike(lbl), Doc.equal, Doc.question})
      printComments(doc, cmtTbl, argLoc)
    }

    let printedExpr = {
      let doc = printExpressionWithComments(expr, cmtTbl)
      switch Parens.expr(expr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr, braces)
      | Nothing => doc
      }
    }

    let loc = {...argLoc, loc_end: expr.pexp_loc.loc_end}
    let doc = Doc.concat(list{printedLbl, printedExpr})
    printComments(doc, cmtTbl, loc)
  }

and printCases = (cases: list<Parsetree.case>, cmtTbl) =>
  Doc.breakableGroup(
    ~forceBreak=true,
    Doc.concat(list{
      Doc.lbrace,
      Doc.concat(list{
        Doc.line,
        printList(
          ~getLoc=n => {...n.Parsetree.pc_lhs.ppat_loc, loc_end: n.pc_rhs.pexp_loc.loc_end},
          ~print=printCase,
          ~nodes=cases,
          cmtTbl,
        ),
      }),
      Doc.line,
      Doc.rbrace,
    }),
  )

and printCase = (case: Parsetree.case, cmtTbl) => {
  let rhs = switch case.pc_rhs.pexp_desc {
  | Pexp_let(_)
  | Pexp_letmodule(_)
  | Pexp_letexception(_)
  | Pexp_open(_)
  | Pexp_sequence(_) =>
    printExpressionBlock(~braces=ParsetreeViewer.isBracedExpr(case.pc_rhs), case.pc_rhs, cmtTbl)
  | _ =>
    let doc = printExpressionWithComments(case.pc_rhs, cmtTbl)
    switch Parens.expr(case.pc_rhs) {
    | Parenthesized => addParens(doc)
    | _ => doc
    }
  }

  let guard = switch case.pc_guard {
  | None => Doc.nil
  | Some(expr) =>
    Doc.group(
      Doc.concat(list{Doc.line, Doc.text("if "), printExpressionWithComments(expr, cmtTbl)}),
    )
  }

  let shouldInlineRhs = switch case.pc_rhs.pexp_desc {
  | Pexp_construct({txt: Longident.Lident("()" | "true" | "false")}, _)
  | Pexp_constant(_)
  | Pexp_ident(_) => true
  | _ if ParsetreeViewer.isHuggableRhs(case.pc_rhs) => true
  | _ => false
  }

  let shouldIndentPattern = switch case.pc_lhs.ppat_desc {
  | Ppat_or(_) => false
  | _ => true
  }

  let patternDoc = {
    let doc = printPattern(case.pc_lhs, cmtTbl)
    switch case.pc_lhs.ppat_desc {
    | Ppat_constraint(_) => addParens(doc)
    | _ => doc
    }
  }

  let content = Doc.concat(list{
    if shouldIndentPattern {
      Doc.indent(patternDoc)
    } else {
      patternDoc
    },
    Doc.indent(guard),
    Doc.text(" =>"),
    Doc.indent(
      Doc.concat(list{
        if shouldInlineRhs {
          Doc.space
        } else {
          Doc.line
        },
        rhs,
      }),
    ),
  })
  Doc.group(Doc.concat(list{Doc.text("| "), content}))
}

and printExprFunParameters = (~inCallback, ~uncurried, ~hasConstraint, parameters, cmtTbl) =>
  switch parameters {
  /* let f = _ => () */
  | list{ParsetreeViewer.Parameter({
      attrs: list{},
      lbl: Asttypes.Nolabel,
      defaultExpr: None,
      pat: {Parsetree.ppat_desc: Ppat_any},
    })} if !uncurried =>
    if hasConstraint {
      Doc.text("(_)")
    } else {
      Doc.text("_")
    }
  /* let f = a => () */
  | list{ParsetreeViewer.Parameter({
      attrs: list{},
      lbl: Asttypes.Nolabel,
      defaultExpr: None,
      pat: {Parsetree.ppat_desc: Ppat_var(stringLoc)},
    })} if !uncurried =>
    let txtDoc = {
      let var = printIdentLike(stringLoc.txt)
      if hasConstraint {
        addParens(var)
      } else {
        var
      }
    }

    printComments(txtDoc, cmtTbl, stringLoc.loc)
  /* let f = () => () */
  | list{ParsetreeViewer.Parameter({
      attrs: list{},
      lbl: Asttypes.Nolabel,
      defaultExpr: None,
      pat: {ppat_desc: Ppat_construct({txt: Longident.Lident("()")}, None)},
    })} if !uncurried =>
    Doc.text("()")
  /* let f = (~greeting, ~from as hometown, ~x=?) => () */
  | parameters =>
    let inCallback = switch inCallback {
    | FitsOnOneLine => true
    | _ => false
    }

    let lparen = if uncurried {
      Doc.text("(. ")
    } else {
      Doc.lparen
    }
    let shouldHug = ParsetreeViewer.parametersShouldHug(parameters)
    let printedParamaters = Doc.concat(list{
      if shouldHug || inCallback {
        Doc.nil
      } else {
        Doc.softLine
      },
      Doc.join(
        ~sep=Doc.concat(list{Doc.comma, Doc.line}),
        List.map(p => printExpFunParameter(p, cmtTbl), parameters),
      ),
    })
    Doc.group(
      Doc.concat(list{
        lparen,
        if shouldHug || inCallback {
          printedParamaters
        } else {
          Doc.concat(list{Doc.indent(printedParamaters), Doc.trailingComma, Doc.softLine})
        },
        Doc.rparen,
      }),
    )
  }

and printExpFunParameter = (parameter, cmtTbl) =>
  switch parameter {
  | ParsetreeViewer.NewTypes({attrs, locs: lbls}) =>
    Doc.group(
      Doc.concat(list{
        printAttributes(attrs, cmtTbl),
        Doc.text("type "),
        Doc.join(
          ~sep=Doc.space,
          List.map(
            lbl => printComments(printIdentLike(lbl.Asttypes.txt), cmtTbl, lbl.Asttypes.loc),
            lbls,
          ),
        ),
      }),
    )
  | Parameter({attrs, lbl, defaultExpr, pat: pattern}) =>
    let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute(attrs)
    let uncurried = if isUncurried {
      Doc.concat(list{Doc.dot, Doc.space})
    } else {
      Doc.nil
    }
    let attrs = printAttributes(attrs, cmtTbl)
    /* =defaultValue */
    let defaultExprDoc = switch defaultExpr {
    | Some(expr) => Doc.concat(list{Doc.text("="), printExpressionWithComments(expr, cmtTbl)})
    | None => Doc.nil
    }

    /* ~from as hometown
     * ~from                   ->  punning */
    let labelWithPattern = switch (lbl, pattern) {
    | (Asttypes.Nolabel, pattern) => printPattern(pattern, cmtTbl)
    | (
        Asttypes.Labelled(lbl) | Optional(lbl),
        {
          ppat_desc: Ppat_var(stringLoc),
          ppat_attributes: list{} | list{({Location.txt: "ns.namedArgLoc"}, _)},
        },
      ) if lbl == stringLoc.txt =>
      /* ~d */
      Doc.concat(list{Doc.text("~"), printIdentLike(lbl)})
    | (
        Asttypes.Labelled(lbl) | Optional(lbl),
        {
          ppat_desc: Ppat_constraint({ppat_desc: Ppat_var({txt})}, typ),
          ppat_attributes: list{} | list{({Location.txt: "ns.namedArgLoc"}, _)},
        },
      ) if lbl == txt =>
      /* ~d: e */
      Doc.concat(list{
        Doc.text("~"),
        printIdentLike(lbl),
        Doc.text(": "),
        printTypExpr(typ, cmtTbl),
      })
    | (Asttypes.Labelled(lbl) | Optional(lbl), pattern) =>
      /* ~b as c */
      Doc.concat(list{
        Doc.text("~"),
        printIdentLike(lbl),
        Doc.text(" as "),
        printPattern(pattern, cmtTbl),
      })
    }

    let optionalLabelSuffix = switch (lbl, defaultExpr) {
    | (Asttypes.Optional(_), None) => Doc.text("=?")
    | _ => Doc.nil
    }

    let doc = Doc.group(
      Doc.concat(list{uncurried, attrs, labelWithPattern, defaultExprDoc, optionalLabelSuffix}),
    )
    let cmtLoc = switch defaultExpr {
    | None =>
      switch pattern.ppat_attributes {
      | list{({Location.txt: "ns.namedArgLoc", loc}, _), ..._} => {
          ...loc,
          loc_end: pattern.ppat_loc.loc_end,
        }
      | _ => pattern.ppat_loc
      }
    | Some(expr) =>
      let startPos = switch pattern.ppat_attributes {
      | list{({Location.txt: "ns.namedArgLoc", loc}, _), ..._} => loc.loc_start
      | _ => pattern.ppat_loc.loc_start
      }
      {
        ...pattern.ppat_loc,
        loc_start: startPos,
        loc_end: expr.pexp_loc.loc_end,
      }
    }

    printComments(doc, cmtTbl, cmtLoc)
  }

and printExpressionBlock = (~braces, expr, cmtTbl) => {
  let rec collectRows = (acc, expr) =>
    switch expr.Parsetree.pexp_desc {
    | Parsetree.Pexp_letmodule(modName, modExpr, expr2) =>
      let name = {
        let doc = Doc.text(modName.txt)
        printComments(doc, cmtTbl, modName.loc)
      }

      let letModuleDoc = Doc.concat(list{
        Doc.text("module "),
        name,
        Doc.text(" = "),
        printModExpr(modExpr, cmtTbl),
      })
      let loc = {...expr.pexp_loc, loc_end: modExpr.pmod_loc.loc_end}
      collectRows(list{(loc, letModuleDoc), ...acc}, expr2)
    | Pexp_letexception(extensionConstructor, expr2) =>
      let loc = {
        let loc = {...expr.pexp_loc, loc_end: extensionConstructor.pext_loc.loc_end}
        switch getFirstLeadingComment(cmtTbl, loc) {
        | None => loc
        | Some(comment) =>
          let cmtLoc = Comment.loc(comment)
          {...cmtLoc, loc_end: loc.loc_end}
        }
      }

      let letExceptionDoc = printExceptionDef(extensionConstructor, cmtTbl)
      collectRows(list{(loc, letExceptionDoc), ...acc}, expr2)
    | Pexp_open(overrideFlag, longidentLoc, expr2) =>
      let openDoc = Doc.concat(list{
        Doc.text("open"),
        printOverrideFlag(overrideFlag),
        Doc.space,
        printLongidentLocation(longidentLoc, cmtTbl),
      })
      let loc = {...expr.pexp_loc, loc_end: longidentLoc.loc.loc_end}
      collectRows(list{(loc, openDoc), ...acc}, expr2)
    | Pexp_sequence(expr1, expr2) =>
      let exprDoc = {
        let doc = printExpression(expr1, cmtTbl)
        switch Parens.expr(expr1) {
        | Parens.Parenthesized => addParens(doc)
        | Braced(braces) => printBraces(doc, expr1, braces)
        | Nothing => doc
        }
      }

      let loc = expr1.pexp_loc
      collectRows(list{(loc, exprDoc), ...acc}, expr2)
    | Pexp_let(recFlag, valueBindings, expr2) =>
      let loc = {
        let loc = switch (valueBindings, List.rev(valueBindings)) {
        | (list{vb, ..._}, list{lastVb, ..._}) => {...vb.pvb_loc, loc_end: lastVb.pvb_loc.loc_end}
        | _ => Location.none
        }

        switch getFirstLeadingComment(cmtTbl, loc) {
        | None => loc
        | Some(comment) =>
          let cmtLoc = Comment.loc(comment)
          {...cmtLoc, loc_end: loc.loc_end}
        }
      }

      let recFlag = switch recFlag {
      | Asttypes.Nonrecursive => Doc.nil
      | Asttypes.Recursive => Doc.text("rec ")
      }

      let letDoc = printValueBindings(~recFlag, valueBindings, cmtTbl)
      /* let () = {
       *   let () = foo()
       *   ()
       * }
       * We don't need to print the () on the last line of the block
       */
      switch expr2.pexp_desc {
      | Pexp_construct({txt: Longident.Lident("()")}, _) => List.rev(list{(loc, letDoc), ...acc})
      | _ => collectRows(list{(loc, letDoc), ...acc}, expr2)
      }
    | _ =>
      let exprDoc = {
        let doc = printExpression(expr, cmtTbl)
        switch Parens.expr(expr) {
        | Parens.Parenthesized => addParens(doc)
        | Braced(braces) => printBraces(doc, expr, braces)
        | Nothing => doc
        }
      }

      List.rev(list{(expr.pexp_loc, exprDoc), ...acc})
    }

  let rows = collectRows(list{}, expr)
  let block = printList(
    ~getLoc=fst,
    ~nodes=rows,
    ~print=((_, doc), _) => doc,
    ~forceBreak=true,
    cmtTbl,
  )

  Doc.breakableGroup(
    ~forceBreak=true,
    if braces {
      Doc.concat(list{
        Doc.lbrace,
        Doc.indent(Doc.concat(list{Doc.line, block})),
        Doc.line,
        Doc.rbrace,
      })
    } else {
      block
    },
  )
}

/*
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
 */
and printBraces = (doc, expr, bracesLoc) => {
  let overMultipleLines = {
    open Location
    bracesLoc.loc_end.pos_lnum > bracesLoc.loc_start.pos_lnum
  }

  switch expr.Parsetree.pexp_desc {
  | Pexp_letmodule(_)
  | Pexp_letexception(_)
  | Pexp_let(_)
  | Pexp_open(_)
  | Pexp_sequence(_) => /* already has braces */
    doc
  | _ =>
    Doc.breakableGroup(
      ~forceBreak=overMultipleLines,
      Doc.concat(list{
        Doc.lbrace,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            if Parens.bracedExpr(expr) {
              addParens(doc)
            } else {
              doc
            },
          }),
        ),
        Doc.softLine,
        Doc.rbrace,
      }),
    )
  }
}

and printOverrideFlag = overrideFlag =>
  switch overrideFlag {
  | Asttypes.Override => Doc.text("!")
  | Fresh => Doc.nil
  }

and printDirectionFlag = flag =>
  switch flag {
  | Asttypes.Downto => Doc.text(" downto ")
  | Asttypes.Upto => Doc.text(" to ")
  }

and printRecordRow = ((lbl, expr), cmtTbl, punningAllowed) => {
  let cmtLoc = {...lbl.loc, loc_end: expr.pexp_loc.loc_end}
  let doc = Doc.group(
    switch expr.pexp_desc {
    | Pexp_ident({txt: Lident(key), loc: keyLoc})
      if punningAllowed &&
      (Longident.last(lbl.txt) == key &&
      lbl.loc.loc_start.pos_cnum === keyLoc.loc_start.pos_cnum) =>
      /* print punned field */
      printLidentPath(lbl, cmtTbl)
    | _ =>
      Doc.concat(list{
        printLidentPath(lbl, cmtTbl),
        Doc.text(": "),
        {
          let doc = printExpressionWithComments(expr, cmtTbl)
          switch Parens.expr(expr) {
          | Parens.Parenthesized => addParens(doc)
          | Braced(braces) => printBraces(doc, expr, braces)
          | Nothing => doc
          }
        },
      })
    },
  )
  printComments(doc, cmtTbl, cmtLoc)
}

and printBsObjectRow = ((lbl, expr), cmtTbl) => {
  let cmtLoc = {...lbl.loc, loc_end: expr.pexp_loc.loc_end}
  let lblDoc = {
    let doc = Doc.concat(list{Doc.text("\""), printLongident(lbl.txt), Doc.text("\"")})
    printComments(doc, cmtTbl, lbl.loc)
  }

  let doc = Doc.concat(list{
    lblDoc,
    Doc.text(": "),
    {
      let doc = printExpressionWithComments(expr, cmtTbl)
      switch Parens.expr(expr) {
      | Parens.Parenthesized => addParens(doc)
      | Braced(braces) => printBraces(doc, expr, braces)
      | Nothing => doc
      }
    },
  })
  printComments(doc, cmtTbl, cmtLoc)
}

/* The optional loc indicates whether we need to print the attributes in
 * relation to some location. In practise this means the following:
 *  `@attr type t = string` -> on the same line, print on the same line
 *  `@attr
 *   type t = string` -> attr is on prev line, print the attributes
 *   with a line break between, we respect the users' original layout */
and printAttributes = (~loc=?, ~inline=false, attrs: Parsetree.attributes, cmtTbl) =>
  switch ParsetreeViewer.filterParsingAttrs(attrs) {
  | list{} => Doc.nil
  | attrs =>
    let lineBreak = switch loc {
    | None => Doc.line
    | Some(loc) =>
      switch List.rev(attrs) {
      | list{({loc: firstLoc}, _), ..._}
        if loc.loc_start.pos_lnum > firstLoc.loc_end.pos_lnum => Doc.hardLine
      | _ => Doc.line
      }
    }

    Doc.concat(list{
      Doc.group(Doc.join(~sep=Doc.line, List.map(attr => printAttribute(attr, cmtTbl), attrs))),
      if inline {
        Doc.space
      } else {
        lineBreak
      },
    })
  }

and printPayload = (payload: Parsetree.payload, cmtTbl) =>
  switch payload {
  | PStr(list{}) => Doc.nil
  | PStr(list{{pstr_desc: Pstr_eval(expr, attrs)}}) =>
    let exprDoc = printExpressionWithComments(expr, cmtTbl)
    let needsParens = switch attrs {
    | list{} => false
    | _ => true
    }
    let shouldHug = ParsetreeViewer.isHuggableExpression(expr)
    if shouldHug {
      Doc.concat(list{
        Doc.lparen,
        printAttributes(attrs, cmtTbl),
        if needsParens {
          addParens(exprDoc)
        } else {
          exprDoc
        },
        Doc.rparen,
      })
    } else {
      Doc.concat(list{
        Doc.lparen,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            printAttributes(attrs, cmtTbl),
            if needsParens {
              addParens(exprDoc)
            } else {
              exprDoc
            },
          }),
        ),
        Doc.softLine,
        Doc.rparen,
      })
    }
  | PStr(list{{pstr_desc: Pstr_value(_recFlag, _bindings)} as si}) =>
    addParens(printStructureItem(si, cmtTbl))
  | PStr(structure) => addParens(printStructure(structure, cmtTbl))
  | PTyp(typ) =>
    Doc.concat(list{
      Doc.lparen,
      Doc.text(":"),
      Doc.indent(Doc.concat(list{Doc.line, printTypExpr(typ, cmtTbl)})),
      Doc.softLine,
      Doc.rparen,
    })
  | PPat(pat, optExpr) =>
    let whenDoc = switch optExpr {
    | Some(expr) =>
      Doc.concat(list{Doc.line, Doc.text("if "), printExpressionWithComments(expr, cmtTbl)})
    | None => Doc.nil
    }

    Doc.concat(list{
      Doc.lparen,
      Doc.indent(
        Doc.concat(list{Doc.softLine, Doc.text("? "), printPattern(pat, cmtTbl), whenDoc}),
      ),
      Doc.softLine,
      Doc.rparen,
    })
  | PSig(signature) =>
    Doc.concat(list{
      Doc.lparen,
      Doc.text(":"),
      Doc.indent(Doc.concat(list{Doc.line, printSignature(signature, cmtTbl)})),
      Doc.softLine,
      Doc.rparen,
    })
  }

and printAttribute = ((id, payload): Parsetree.attribute, cmtTbl) =>
  Doc.group(
    Doc.concat(list{
      Doc.text("@"),
      Doc.text(convertBsExternalAttribute(id.txt)),
      printPayload(payload, cmtTbl),
    }),
  )

and printModExpr = (modExpr, cmtTbl) => {
  let doc = switch modExpr.pmod_desc {
  | Pmod_ident(longidentLoc) => printLongidentLocation(longidentLoc, cmtTbl)
  | Pmod_structure(list{}) =>
    let shouldBreak = modExpr.pmod_loc.loc_start.pos_lnum < modExpr.pmod_loc.loc_end.pos_lnum

    Doc.breakableGroup(
      ~forceBreak=shouldBreak,
      Doc.concat(list{
        Doc.lbrace,
        Doc.indent(Doc.concat(list{Doc.softLine, printCommentsInside(cmtTbl, modExpr.pmod_loc)})),
        Doc.softLine,
        Doc.rbrace,
      }),
    )
  | Pmod_structure(structure) =>
    Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat(list{
        Doc.lbrace,
        Doc.indent(Doc.concat(list{Doc.softLine, printStructure(structure, cmtTbl)})),
        Doc.softLine,
        Doc.rbrace,
      }),
    )
  | Pmod_unpack(expr) =>
    let shouldHug = switch expr.pexp_desc {
    | Pexp_let(_) => true
    | Pexp_constraint({pexp_desc: Pexp_let(_)}, {ptyp_desc: Ptyp_package(_packageType)}) => true
    | _ => false
    }

    let (expr, moduleConstraint) = switch expr.pexp_desc {
    | Pexp_constraint(expr, {ptyp_desc: Ptyp_package(packageType), ptyp_loc}) =>
      let packageDoc = {
        let doc = printPackageType(~printModuleKeywordAndParens=false, packageType, cmtTbl)
        printComments(doc, cmtTbl, ptyp_loc)
      }

      let typeDoc = Doc.group(
        Doc.concat(list{Doc.text(":"), Doc.indent(Doc.concat(list{Doc.line, packageDoc}))}),
      )
      (expr, typeDoc)
    | _ => (expr, Doc.nil)
    }

    let unpackDoc = Doc.group(
      Doc.concat(list{printExpressionWithComments(expr, cmtTbl), moduleConstraint}),
    )
    Doc.group(
      Doc.concat(list{
        Doc.text("unpack("),
        if shouldHug {
          unpackDoc
        } else {
          Doc.concat(list{Doc.indent(Doc.concat(list{Doc.softLine, unpackDoc})), Doc.softLine})
        },
        Doc.rparen,
      }),
    )
  | Pmod_extension(extension) => printExtension(~atModuleLvl=false, extension, cmtTbl)
  | Pmod_apply(_) =>
    let (args, callExpr) = ParsetreeViewer.modExprApply(modExpr)
    let isUnitSugar = switch args {
    | list{{pmod_desc: Pmod_structure(list{})}} => true
    | _ => false
    }

    let shouldHug = switch args {
    | list{{pmod_desc: Pmod_structure(_)}} => true
    | _ => false
    }

    Doc.group(
      Doc.concat(list{
        printModExpr(callExpr, cmtTbl),
        if isUnitSugar {
          printModApplyArg(@doesNotRaise List.hd(args), cmtTbl)
        } else {
          Doc.concat(list{
            Doc.lparen,
            if shouldHug {
              printModApplyArg(@doesNotRaise List.hd(args), cmtTbl)
            } else {
              Doc.indent(
                Doc.concat(list{
                  Doc.softLine,
                  Doc.join(
                    ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                    List.map(modArg => printModApplyArg(modArg, cmtTbl), args),
                  ),
                }),
              )
            },
            if !shouldHug {
              Doc.concat(list{Doc.trailingComma, Doc.softLine})
            } else {
              Doc.nil
            },
            Doc.rparen,
          })
        },
      }),
    )
  | Pmod_constraint(modExpr, modType) =>
    Doc.concat(list{printModExpr(modExpr, cmtTbl), Doc.text(": "), printModType(modType, cmtTbl)})
  | Pmod_functor(_) => printModFunctor(modExpr, cmtTbl)
  }

  printComments(doc, cmtTbl, modExpr.pmod_loc)
}

and printModFunctor = (modExpr, cmtTbl) => {
  let (parameters, returnModExpr) = ParsetreeViewer.modExprFunctor(modExpr)
  /* let shouldInline = match returnModExpr.pmod_desc with */
  /* | Pmod_structure _ | Pmod_ident _ -> true */
  /* | Pmod_constraint ({pmod_desc = Pmod_structure _}, _) -> true */
  /* | _ -> false */
  /* in */
  let (returnConstraint, returnModExpr) = switch returnModExpr.pmod_desc {
  | Pmod_constraint(modExpr, modType) =>
    let constraintDoc = {
      let doc = printModType(modType, cmtTbl)
      if Parens.modExprFunctorConstraint(modType) {
        addParens(doc)
      } else {
        doc
      }
    }

    let modConstraint = Doc.concat(list{Doc.text(": "), constraintDoc})
    (modConstraint, printModExpr(modExpr, cmtTbl))
  | _ => (Doc.nil, printModExpr(returnModExpr, cmtTbl))
  }

  let parametersDoc = switch parameters {
  | list{(attrs, {txt: "*"}, None)} =>
    Doc.group(Doc.concat(list{printAttributes(attrs, cmtTbl), Doc.text("()")}))
  | list{(list{}, {txt: lbl}, None)} => Doc.text(lbl)
  | parameters =>
    Doc.group(
      Doc.concat(list{
        Doc.lparen,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list{Doc.comma, Doc.line}),
              List.map(param => printModFunctorParam(param, cmtTbl), parameters),
            ),
          }),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rparen,
      }),
    )
  }

  Doc.group(Doc.concat(list{parametersDoc, returnConstraint, Doc.text(" => "), returnModExpr}))
}

and printModFunctorParam = ((attrs, lbl, optModType), cmtTbl) => {
  let cmtLoc = switch optModType {
  | None => lbl.Asttypes.loc
  | Some(modType) => {
      ...lbl.loc,
      loc_end: modType.Parsetree.pmty_loc.loc_end,
    }
  }

  let attrs = printAttributes(attrs, cmtTbl)
  let lblDoc = {
    let doc = if lbl.txt == "*" {
      Doc.text("()")
    } else {
      Doc.text(lbl.txt)
    }
    printComments(doc, cmtTbl, lbl.loc)
  }

  let doc = Doc.group(
    Doc.concat(list{
      attrs,
      lblDoc,
      switch optModType {
      | None => Doc.nil
      | Some(modType) => Doc.concat(list{Doc.text(": "), printModType(modType, cmtTbl)})
      },
    }),
  )
  printComments(doc, cmtTbl, cmtLoc)
}

and printModApplyArg = (modExpr, cmtTbl) =>
  switch modExpr.pmod_desc {
  | Pmod_structure(list{}) => Doc.text("()")
  | _ => printModExpr(modExpr, cmtTbl)
  }

and printExceptionDef = (constr: Parsetree.extension_constructor, cmtTbl) => {
  let kind = switch constr.pext_kind {
  | Pext_rebind(longident) =>
    Doc.indent(
      Doc.concat(list{Doc.text(" ="), Doc.line, printLongidentLocation(longident, cmtTbl)}),
    )
  | Pext_decl(Pcstr_tuple(list{}), None) => Doc.nil
  | Pext_decl(args, gadt) =>
    let gadtDoc = switch gadt {
    | Some(typ) => Doc.concat(list{Doc.text(": "), printTypExpr(typ, cmtTbl)})
    | None => Doc.nil
    }

    Doc.concat(list{printConstructorArguments(~indent=false, args, cmtTbl), gadtDoc})
  }

  let name = printComments(Doc.text(constr.pext_name.txt), cmtTbl, constr.pext_name.loc)

  let doc = Doc.group(
    Doc.concat(list{
      printAttributes(constr.pext_attributes, cmtTbl),
      Doc.text("exception "),
      name,
      kind,
    }),
  )
  printComments(doc, cmtTbl, constr.pext_loc)
}

and printExtensionConstructor = (constr: Parsetree.extension_constructor, cmtTbl, i) => {
  let attrs = printAttributes(constr.pext_attributes, cmtTbl)
  let bar = if i > 0 {
    Doc.text("| ")
  } else {
    Doc.ifBreaks(Doc.text("| "), Doc.nil)
  }

  let kind = switch constr.pext_kind {
  | Pext_rebind(longident) =>
    Doc.indent(
      Doc.concat(list{Doc.text(" ="), Doc.line, printLongidentLocation(longident, cmtTbl)}),
    )
  | Pext_decl(Pcstr_tuple(list{}), None) => Doc.nil
  | Pext_decl(args, gadt) =>
    let gadtDoc = switch gadt {
    | Some(typ) => Doc.concat(list{Doc.text(": "), printTypExpr(typ, cmtTbl)})
    | None => Doc.nil
    }

    Doc.concat(list{printConstructorArguments(~indent=false, args, cmtTbl), gadtDoc})
  }

  let name = printComments(Doc.text(constr.pext_name.txt), cmtTbl, constr.pext_name.loc)

  Doc.concat(list{bar, Doc.group(Doc.concat(list{attrs, name, kind}))})
}

let printImplementation = (~width, s: Parsetree.structure, ~comments) => {
  let cmtTbl = CommentTable.make()
  CommentTable.walkStructure(s, cmtTbl, comments)
  /* CommentTable.log cmtTbl; */
  let doc = printStructure(s, cmtTbl)
  /* Doc.debug doc; */
  Doc.toString(~width, doc) ++ "\n"
}

let printInterface = (~width, s: Parsetree.signature, ~comments) => {
  let cmtTbl = CommentTable.make()
  CommentTable.walkSignature(s, cmtTbl, comments)
  Doc.toString(~width, printSignature(s, cmtTbl)) ++ "\n"
}

