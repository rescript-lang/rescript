module MiniBuffer: {
  type t
  let add_char: (t, char) => unit
  let add_string: (t, string) => unit
  let contents: t => string
  let create: int => t
  let flush_newline: t => unit
  let length: t => int
  let unsafe_get: (t, int) => char
} = {
  type t = {
    mutable buffer: bytes,
    mutable position: int,
    mutable length: int,
  }

  let create = n => {
    let n = if n < 1 {
      1
    } else {
      n
    }
    let s = (@doesNotRaise Bytes.create)(n)
    {buffer: s, position: 0, length: n}
  }

  let contents = b => Bytes.sub_string(b.buffer, 0, b.position)

  let unsafe_get = (b, ofs) => Bytes.unsafe_get(b.buffer, ofs)

  let length = b => b.position

  /* Can't be called directly, don't add to the interface */
  let resize_internal = (b, more) => {
    let len = b.length
    let new_len = ref(len)
    while b.position + more > new_len.contents {
      new_len := 2 * new_len.contents
    }
    if new_len.contents > Sys.max_string_length {
      if b.position + more <= Sys.max_string_length {
        new_len := Sys.max_string_length
      }
    }
    let new_buffer = (@doesNotRaise Bytes.create)(new_len.contents)
    /* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. */
    @doesNotRaise
    Bytes.blit(b.buffer, 0, new_buffer, 0, b.position)
    b.buffer = new_buffer
    b.length = new_len.contents
  }

  let add_char = (b, c) => {
    let pos = b.position
    if pos >= b.length {
      resize_internal(b, 1)
    }
    Bytes.unsafe_set(b.buffer, pos, c)
    b.position = pos + 1
  }

  let add_string = (b, s) => {
    let len = String.length(s)
    let new_position = b.position + len
    if new_position > b.length {
      resize_internal(b, len)
    }
    @doesNotRaise
    Bytes.blit_string(s, 0, b.buffer, b.position, len)
    b.position = new_position
  }

  /* adds newline and trims all preceding whitespace */
  let flush_newline = b => {
    let position = ref(b.position)
    while Bytes.unsafe_get(b.buffer, position.contents - 1) == ' ' && position.contents >= 0 {
      position := position.contents - 1
    }
    b.position = position.contents
    add_char(b, '\n')
  }
}

module Doc = {
  type mode = Break | Flat

  type lineStyle =
    | Classic /* fits? -> replace with space */
    | Soft /* fits? -> replaced with nothing */
    | Hard /* always included, forces breaks in parents */

  type rec t =
    | Nil
    | Text(string)
    | Concat(list<t>)
    | Indent(t)
    | IfBreaks({yes: t, no: t})
    | LineSuffix(t)
    | LineBreak(lineStyle)
    | Group({shouldBreak: bool, doc: t})
    | CustomLayout(list<t>)
    | BreakParent
  /* | Cursor */

  let nil = Nil
  let line = LineBreak(Classic)
  let hardLine = LineBreak(Hard)
  let softLine = LineBreak(Soft)
  let text = s => Text(s)
  let concat = l => Concat(l)
  let indent = d => Indent(d)
  let ifBreaks = (t, f) => IfBreaks({yes: t, no: f})
  let lineSuffix = d => LineSuffix(d)
  let group = d => Group({shouldBreak: false, doc: d})
  let breakableGroup = (~forceBreak, d) => Group({shouldBreak: forceBreak, doc: d})
  let customLayout = gs => CustomLayout(gs)
  let breakParent = BreakParent
  /* let cursor = Cursor */

  let space = Text(" ")
  let comma = Text(",")
  let dot = Text(".")
  let dotdot = Text("..")
  let dotdotdot = Text("...")
  let lessThan = Text("<")
  let greaterThan = Text(">")
  let lbrace = Text("{")
  let rbrace = Text("}")
  let lparen = Text("(")
  let rparen = Text(")")
  let lbracket = Text("[")
  let rbracket = Text("]")
  let question = Text("?")
  let tilde = Text("~")
  let equal = Text("=")
  let trailingComma = IfBreaks({yes: comma, no: nil})
  let doubleQuote = Text("\"")

  let propagateForcedBreaks = doc => {
    let rec walk = doc =>
      switch doc {
      | Text(_) | Nil | LineSuffix(_) => (false, doc)
      | BreakParent => (true, Nil)
      | LineBreak(Hard) => (true, doc)
      | LineBreak(Classic | Soft) => (false, doc)
      | Indent(children) =>
        let (childForcesBreak, newChildren) = walk(children)
        (childForcesBreak, Indent(newChildren))
      | IfBreaks({yes: trueDoc, no: falseDoc}) =>
        let (falseForceBreak, falseDoc) = walk(falseDoc)
        if falseForceBreak {
          let (_, trueDoc) = walk(trueDoc)
          (true, trueDoc)
        } else {
          let (forceBreak, trueDoc) = walk(trueDoc)
          (forceBreak, IfBreaks({yes: trueDoc, no: falseDoc}))
        }
      | Group({shouldBreak: forceBreak, doc: children}) =>
        let (childForcesBreak, newChildren) = walk(children)
        let shouldBreak = forceBreak || childForcesBreak
        (shouldBreak, Group({shouldBreak: shouldBreak, doc: newChildren}))
      | Concat(children) =>
        let (forceBreak, newChildren) = List.fold_left(((forceBreak, newChildren), child) => {
          let (childForcesBreak, newChild) = walk(child)
          (forceBreak || childForcesBreak, list{newChild, ...newChildren})
        }, (false, list{}), children)

        (forceBreak, Concat(List.rev(newChildren)))
      | CustomLayout(children) =>
        /* When using CustomLayout, we don't want to propagate forced breaks
         * from the children up. By definition it picks the first layout that fits
         * otherwise it takes the last of the list.
         * However we do want to propagate forced breaks in the sublayouts. They
         * might need to be broken. We just don't propagate them any higher here */
        let children = switch walk(Concat(children)) {
        | (_, Concat(children)) => children
        | _ => assert false
        }

        (false, CustomLayout(children))
      }

    let (_, processedDoc) = walk(doc)
    processedDoc
  }

  let join = (~sep, docs) => {
    let rec loop = (acc, sep, docs) =>
      switch docs {
      | list{} => List.rev(acc)
      | list{x} => List.rev(list{x, ...acc})
      | list{x, ...xs} => loop(list{sep, x, ...acc}, sep, xs)
      }

    Concat(loop(list{}, sep, docs))
  }

  let rec fits = (w, doc) =>
    switch doc {
    | _ when w < 0 => false
    | list{} => true
    | list{(_ind, _mode, Text(txt)), ...rest} => fits(w - String.length(txt), rest)
    | list{(ind, mode, Indent(doc)), ...rest} => fits(w, list{(ind + 2, mode, doc), ...rest})
    | list{(_ind, Flat, LineBreak(break)), ...rest} =>
      if break == Hard {
        true
      } else {
        let w = if break == Classic {
          w - 1
        } else {
          w
        }
        fits(w, rest)
      }
    | list{(_ind, _mode, Nil), ...rest} => fits(w, rest)
    | list{(_ind, Break, LineBreak(_break)), ..._rest} => true
    | list{(ind, mode, Group({shouldBreak: forceBreak, doc})), ...rest} =>
      let mode = if forceBreak {
        Break
      } else {
        mode
      }
      fits(w, list{(ind, mode, doc), ...rest})
    | list{(ind, mode, IfBreaks({yes: breakDoc, no: flatDoc})), ...rest} =>
      if mode == Break {
        fits(w, list{(ind, mode, breakDoc), ...rest})
      } else {
        fits(w, list{(ind, mode, flatDoc), ...rest})
      }
    | list{(ind, mode, Concat(docs)), ...rest} =>
      let ops = List.map(doc => (ind, mode, doc), docs)
      fits(w, List.append(ops, rest))
    /* | (_ind, _mode, Cursor)::rest -> fits w rest */
    | list{(_ind, _mode, LineSuffix(_)), ...rest} => fits(w, rest)
    | list{(_ind, _mode, BreakParent), ...rest} => fits(w, rest)
    | list{(ind, mode, CustomLayout(list{hd, ..._})), ...rest} =>
      /* TODO: if we have nested custom layouts, what we should do here? */
      fits(w, list{(ind, mode, hd), ...rest})
    | list{(_ind, _mode, CustomLayout(_)), ...rest} => fits(w, rest)
    }

  let toString = (~width, doc) => {
    let doc = propagateForcedBreaks(doc)
    let buffer = MiniBuffer.create(1000)

    let rec process = (~pos, lineSuffices, stack) =>
      switch stack {
      | list{(ind, mode, doc) as cmd, ...rest} =>
        switch doc {
        | Nil | BreakParent => process(~pos, lineSuffices, rest)
        | Text(txt) =>
          MiniBuffer.add_string(buffer, txt)
          process(~pos=String.length(txt) + pos, lineSuffices, rest)
        | LineSuffix(doc) => process(~pos, list{(ind, mode, doc), ...lineSuffices}, rest)
        | Concat(docs) =>
          let ops = List.map(doc => (ind, mode, doc), docs)
          process(~pos, lineSuffices, List.append(ops, rest))
        | Indent(doc) => process(~pos, lineSuffices, list{(ind + 2, mode, doc), ...rest})
        | IfBreaks({yes: breakDoc, no: flatDoc}) =>
          if mode == Break {
            process(~pos, lineSuffices, list{(ind, mode, breakDoc), ...rest})
          } else {
            process(~pos, lineSuffices, list{(ind, mode, flatDoc), ...rest})
          }
        | LineBreak(lineStyle) =>
          if mode == Break {
            switch lineSuffices {
            | list{} =>
              MiniBuffer.flush_newline(buffer)
              MiniBuffer.add_string(buffer, @doesNotRaise String.make(ind, ' '))
              process(~pos=ind, list{}, rest)
            | _docs =>
              process(
                ~pos=ind,
                list{},
                List.concat(list{List.rev(lineSuffices), list{cmd, ...rest}}),
              )
            }
          } else {
            /* mode = Flat */
            let pos = switch lineStyle {
            | Classic =>
              MiniBuffer.add_string(buffer, " ")
              pos + 1
            | Hard =>
              MiniBuffer.flush_newline(buffer)
              0
            | Soft => pos
            }

            process(~pos, lineSuffices, rest)
          }
        | Group({shouldBreak, doc}) =>
          if shouldBreak || !fits(width - pos, list{(ind, Flat, doc), ...rest}) {
            process(~pos, lineSuffices, list{(ind, Break, doc), ...rest})
          } else {
            process(~pos, lineSuffices, list{(ind, Flat, doc), ...rest})
          }
        | CustomLayout(docs) =>
          let rec findGroupThatFits = groups =>
            switch groups {
            | list{} => Nil
            | list{lastGroup} => lastGroup
            | list{doc, ...docs} =>
              if fits(width - pos, list{(ind, Flat, doc), ...rest}) {
                doc
              } else {
                findGroupThatFits(docs)
              }
            }

          let doc = findGroupThatFits(docs)
          process(~pos, lineSuffices, list{(ind, Flat, doc), ...rest})
        }
      | list{} =>
        switch lineSuffices {
        | list{} => ()
        | suffices => process(~pos=0, list{}, List.rev(suffices))
        }
      }

    process(~pos=0, list{}, list{(0, Flat, doc)})

    let len = MiniBuffer.length(buffer)
    if len > 0 && MiniBuffer.unsafe_get(buffer, len - 1) !== '\n' {
      MiniBuffer.add_char(buffer, '\n')
    }
    MiniBuffer.contents(buffer)
  }

  @live
  let debug = t => {
    let rec toDoc = x =>
      switch x {
      | Nil => text("nil")
      | BreakParent => text("breakparent")
      | Text(txt) => text("text(" ++ (txt ++ ")"))
      | LineSuffix(doc) =>
        group(
          concat(list{
            text("linesuffix("),
            indent(concat(list{line, toDoc(doc)})),
            line,
            text(")"),
          }),
        )
      | Concat(docs) =>
        group(
          concat(list{
            text("concat("),
            indent(
              concat(list{line, join(~sep=concat(list{text(","), line}), List.map(toDoc, docs))}),
            ),
            line,
            text(")"),
          }),
        )
      | CustomLayout(docs) =>
        group(
          concat(list{
            text("customLayout("),
            indent(
              concat(list{line, join(~sep=concat(list{text(","), line}), List.map(toDoc, docs))}),
            ),
            line,
            text(")"),
          }),
        )
      | Indent(doc) => concat(list{text("indent("), softLine, toDoc(doc), softLine, text(")")})
      | IfBreaks({yes: trueDoc, no: falseDoc}) =>
        group(
          concat(list{
            text("ifBreaks("),
            indent(
              concat(list{line, toDoc(trueDoc), concat(list{text(","), line}), toDoc(falseDoc)}),
            ),
            line,
            text(")"),
          }),
        )
      | LineBreak(break) =>
        let breakTxt = switch break {
        | Classic => "Classic"
        | Soft => "Soft"
        | Hard => "Hard"
        }

        text("LineBreak(" ++ (breakTxt ++ ")"))
      | Group({shouldBreak, doc}) =>
        group(
          concat(list{
            text("Group("),
            indent(
              concat(list{
                line,
                text("shouldBreak: " ++ string_of_bool(shouldBreak)),
                concat(list{text(","), line}),
                toDoc(doc),
              }),
            ),
            line,
            text(")"),
          }),
        )
      }

    let doc = toDoc(t)
    toString(~width=10, doc) |> print_endline
  }
}

module Sexp: {
  type t

  let atom: string => t
  let list: list<t> => t
  let toString: t => string
} = {
  type rec t =
    | Atom(string)
    | List(list<t>)

  let atom = s => Atom(s)
  let list = l => List(l)

  let rec toDoc = t =>
    switch t {
    | Atom(s) => Doc.text(s)
    | List(list{}) => Doc.text("()")
    | List(list{sexpr}) => Doc.concat(list{Doc.lparen, toDoc(sexpr), Doc.rparen})
    | List(list{hd, ...tail}) =>
      Doc.group(
        Doc.concat(list{
          Doc.lparen,
          toDoc(hd),
          Doc.indent(Doc.concat(list{Doc.line, Doc.join(~sep=Doc.line, List.map(toDoc, tail))})),
          Doc.rparen,
        }),
      )
    }

  let toString = sexpr => {
    let doc = toDoc(sexpr)
    Doc.toString(~width=80, doc)
  }
}

module SexpAst: {
  let implementation: Parsetree.structure => Sexp.t
  let interface: Parsetree.signature => Sexp.t
} = {
  open Parsetree

  let mapEmpty = (~f, items) =>
    switch items {
    | list{} => list{Sexp.list(list{})}
    | items => List.map(f, items)
    }

  let string = txt => Sexp.atom("\"" ++ (txt ++ "\""))

  let char = c => Sexp.atom("'" ++ (Char.escaped(c) ++ "'"))

  let optChar = oc =>
    switch oc {
    | None => Sexp.atom("None")
    | Some(c) => Sexp.list(list{Sexp.atom("Some"), char(c)})
    }

  let longident = l => {
    let rec loop = l =>
      switch l {
      | Longident.Lident(ident) => Sexp.list(list{Sexp.atom("Lident"), string(ident)})
      | Longident.Ldot(lident, txt) => Sexp.list(list{Sexp.atom("Ldot"), loop(lident), string(txt)})
      | Longident.Lapply(l1, l2) => Sexp.list(list{Sexp.atom("Lapply"), loop(l1), loop(l2)})
      }

    Sexp.list(list{Sexp.atom("longident"), loop(l)})
  }

  let closedFlag = flag =>
    switch flag {
    | Asttypes.Closed => Sexp.atom("Closed")
    | Open => Sexp.atom("Open")
    }

  let directionFlag = flag =>
    switch flag {
    | Asttypes.Upto => Sexp.atom("Upto")
    | Downto => Sexp.atom("Downto")
    }

  let recFlag = flag =>
    switch flag {
    | Asttypes.Recursive => Sexp.atom("Recursive")
    | Nonrecursive => Sexp.atom("Nonrecursive")
    }

  let overrideFlag = flag =>
    switch flag {
    | Asttypes.Override => Sexp.atom("Override")
    | Fresh => Sexp.atom("Fresh")
    }

  let privateFlag = flag =>
    switch flag {
    | Asttypes.Public => Sexp.atom("Public")
    | Private => Sexp.atom("Private")
    }

  let mutableFlag = flag =>
    switch flag {
    | Asttypes.Immutable => Sexp.atom("Immutable")
    | Mutable => Sexp.atom("Mutable")
    }

  let variance = v =>
    switch v {
    | Asttypes.Covariant => Sexp.atom("Covariant")
    | Contravariant => Sexp.atom("Contravariant")
    | Invariant => Sexp.atom("Invariant")
    }

  let argLabel = lbl =>
    switch lbl {
    | Asttypes.Nolabel => Sexp.atom("Nolabel")
    | Labelled(txt) => Sexp.list(list{Sexp.atom("Labelled"), string(txt)})
    | Optional(txt) => Sexp.list(list{Sexp.atom("Optional"), string(txt)})
    }

  let constant = c => {
    let sexpr = switch c {
    | Pconst_integer(txt, tag) =>
      Sexp.list(list{Sexp.atom("Pconst_integer"), string(txt), optChar(tag)})
    | Pconst_char(c) => Sexp.list(list{Sexp.atom("Pconst_char"), Sexp.atom(Char.escaped(c))})
    | Pconst_string(txt, tag) =>
      Sexp.list(list{
        Sexp.atom("Pconst_string"),
        string(txt),
        switch tag {
        | Some(txt) => Sexp.list(list{Sexp.atom("Some"), string(txt)})
        | None => Sexp.atom("None")
        },
      })
    | Pconst_float(txt, tag) =>
      Sexp.list(list{Sexp.atom("Pconst_float"), string(txt), optChar(tag)})
    }

    Sexp.list(list{Sexp.atom("constant"), sexpr})
  }

  let rec structure = s => Sexp.list(list{Sexp.atom("structure"), ...List.map(structureItem, s)})

  and structureItem = si => {
    let desc = switch si.pstr_desc {
    | Pstr_eval(expr, attrs) =>
      Sexp.list(list{Sexp.atom("Pstr_eval"), expression(expr), attributes(attrs)})
    | Pstr_value(flag, vbs) =>
      Sexp.list(list{
        Sexp.atom("Pstr_value"),
        recFlag(flag),
        Sexp.list(mapEmpty(~f=valueBinding, vbs)),
      })
    | Pstr_primitive(vd) => Sexp.list(list{Sexp.atom("Pstr_primitive"), valueDescription(vd)})
    | Pstr_type(flag, tds) =>
      Sexp.list(list{
        Sexp.atom("Pstr_type"),
        recFlag(flag),
        Sexp.list(mapEmpty(~f=typeDeclaration, tds)),
      })
    | Pstr_typext(typext) => Sexp.list(list{Sexp.atom("Pstr_type"), typeExtension(typext)})
    | Pstr_exception(ec) => Sexp.list(list{Sexp.atom("Pstr_exception"), extensionConstructor(ec)})
    | Pstr_module(mb) => Sexp.list(list{Sexp.atom("Pstr_module"), moduleBinding(mb)})
    | Pstr_recmodule(mbs) =>
      Sexp.list(list{Sexp.atom("Pstr_recmodule"), Sexp.list(mapEmpty(~f=moduleBinding, mbs))})
    | Pstr_modtype(modTypDecl) =>
      Sexp.list(list{Sexp.atom("Pstr_modtype"), moduleTypeDeclaration(modTypDecl)})
    | Pstr_open(openDesc) => Sexp.list(list{Sexp.atom("Pstr_open"), openDescription(openDesc)})
    | Pstr_class(_) => Sexp.atom("Pstr_class")
    | Pstr_class_type(_) => Sexp.atom("Pstr_class_type")
    | Pstr_include(id) => Sexp.list(list{Sexp.atom("Pstr_include"), includeDeclaration(id)})
    | Pstr_attribute(attr) => Sexp.list(list{Sexp.atom("Pstr_attribute"), attribute(attr)})
    | Pstr_extension(ext, attrs) =>
      Sexp.list(list{Sexp.atom("Pstr_extension"), extension(ext), attributes(attrs)})
    }

    Sexp.list(list{Sexp.atom("structure_item"), desc})
  }

  and includeDeclaration = id =>
    Sexp.list(list{
      Sexp.atom("include_declaration"),
      moduleExpression(id.pincl_mod),
      attributes(id.pincl_attributes),
    })

  and openDescription = od =>
    Sexp.list(list{
      Sexp.atom("open_description"),
      longident(od.popen_lid.Asttypes.txt),
      attributes(od.popen_attributes),
    })

  and moduleTypeDeclaration = mtd =>
    Sexp.list(list{
      Sexp.atom("module_type_declaration"),
      string(mtd.pmtd_name.Asttypes.txt),
      switch mtd.pmtd_type {
      | None => Sexp.atom("None")
      | Some(modType) => Sexp.list(list{Sexp.atom("Some"), moduleType(modType)})
      },
      attributes(mtd.pmtd_attributes),
    })

  and moduleBinding = mb =>
    Sexp.list(list{
      Sexp.atom("module_binding"),
      string(mb.pmb_name.Asttypes.txt),
      moduleExpression(mb.pmb_expr),
      attributes(mb.pmb_attributes),
    })

  and moduleExpression = me => {
    let desc = switch me.pmod_desc {
    | Pmod_ident(modName) =>
      Sexp.list(list{Sexp.atom("Pmod_ident"), longident(modName.Asttypes.txt)})
    | Pmod_structure(s) => Sexp.list(list{Sexp.atom("Pmod_structure"), structure(s)})
    | Pmod_functor(lbl, optModType, modExpr) =>
      Sexp.list(list{
        Sexp.atom("Pmod_functor"),
        string(lbl.Asttypes.txt),
        switch optModType {
        | None => Sexp.atom("None")
        | Some(modType) => Sexp.list(list{Sexp.atom("Some"), moduleType(modType)})
        },
        moduleExpression(modExpr),
      })
    | Pmod_apply(callModExpr, modExprArg) =>
      Sexp.list(list{
        Sexp.atom("Pmod_apply"),
        moduleExpression(callModExpr),
        moduleExpression(modExprArg),
      })
    | Pmod_constraint(modExpr, modType) =>
      Sexp.list(list{Sexp.atom("Pmod_constraint"), moduleExpression(modExpr), moduleType(modType)})
    | Pmod_unpack(expr) => Sexp.list(list{Sexp.atom("Pmod_unpack"), expression(expr)})
    | Pmod_extension(ext) => Sexp.list(list{Sexp.atom("Pmod_extension"), extension(ext)})
    }

    Sexp.list(list{Sexp.atom("module_expr"), desc, attributes(me.pmod_attributes)})
  }

  and moduleType = mt => {
    let desc = switch mt.pmty_desc {
    | Pmty_ident(longidentLoc) =>
      Sexp.list(list{Sexp.atom("Pmty_ident"), longident(longidentLoc.Asttypes.txt)})
    | Pmty_signature(s) => Sexp.list(list{Sexp.atom("Pmty_signature"), signature(s)})
    | Pmty_functor(lbl, optModType, modType) =>
      Sexp.list(list{
        Sexp.atom("Pmty_functor"),
        string(lbl.Asttypes.txt),
        switch optModType {
        | None => Sexp.atom("None")
        | Some(modType) => Sexp.list(list{Sexp.atom("Some"), moduleType(modType)})
        },
        moduleType(modType),
      })
    | Pmty_alias(longidentLoc) =>
      Sexp.list(list{Sexp.atom("Pmty_alias"), longident(longidentLoc.Asttypes.txt)})
    | Pmty_extension(ext) => Sexp.list(list{Sexp.atom("Pmty_extension"), extension(ext)})
    | Pmty_typeof(modExpr) => Sexp.list(list{Sexp.atom("Pmty_typeof"), moduleExpression(modExpr)})
    | Pmty_with(modType, withConstraints) =>
      Sexp.list(list{
        Sexp.atom("Pmty_with"),
        moduleType(modType),
        Sexp.list(mapEmpty(~f=withConstraint, withConstraints)),
      })
    }

    Sexp.list(list{Sexp.atom("module_type"), desc, attributes(mt.pmty_attributes)})
  }

  and withConstraint = wc =>
    switch wc {
    | Pwith_type(longidentLoc, td) =>
      Sexp.list(list{
        Sexp.atom("Pmty_with"),
        longident(longidentLoc.Asttypes.txt),
        typeDeclaration(td),
      })
    | Pwith_module(l1, l2) =>
      Sexp.list(list{
        Sexp.atom("Pwith_module"),
        longident(l1.Asttypes.txt),
        longident(l2.Asttypes.txt),
      })
    | Pwith_typesubst(longidentLoc, td) =>
      Sexp.list(list{
        Sexp.atom("Pwith_typesubst"),
        longident(longidentLoc.Asttypes.txt),
        typeDeclaration(td),
      })
    | Pwith_modsubst(l1, l2) =>
      Sexp.list(list{
        Sexp.atom("Pwith_modsubst"),
        longident(l1.Asttypes.txt),
        longident(l2.Asttypes.txt),
      })
    }

  and signature = s => Sexp.list(list{Sexp.atom("signature"), ...List.map(signatureItem, s)})

  and signatureItem = si => {
    let descr = switch si.psig_desc {
    | Psig_value(vd) => Sexp.list(list{Sexp.atom("Psig_value"), valueDescription(vd)})
    | Psig_type(flag, typeDeclarations) =>
      Sexp.list(list{
        Sexp.atom("Psig_type"),
        recFlag(flag),
        Sexp.list(mapEmpty(~f=typeDeclaration, typeDeclarations)),
      })
    | Psig_typext(typExt) => Sexp.list(list{Sexp.atom("Psig_typext"), typeExtension(typExt)})
    | Psig_exception(extConstr) =>
      Sexp.list(list{Sexp.atom("Psig_exception"), extensionConstructor(extConstr)})
    | Psig_module(modDecl) => Sexp.list(list{Sexp.atom("Psig_module"), moduleDeclaration(modDecl)})
    | Psig_recmodule(modDecls) =>
      Sexp.list(list{
        Sexp.atom("Psig_recmodule"),
        Sexp.list(mapEmpty(~f=moduleDeclaration, modDecls)),
      })
    | Psig_modtype(modTypDecl) =>
      Sexp.list(list{Sexp.atom("Psig_modtype"), moduleTypeDeclaration(modTypDecl)})
    | Psig_open(openDesc) => Sexp.list(list{Sexp.atom("Psig_open"), openDescription(openDesc)})
    | Psig_include(inclDecl) =>
      Sexp.list(list{Sexp.atom("Psig_include"), includeDescription(inclDecl)})
    | Psig_class(_) => Sexp.list(list{Sexp.atom("Psig_class")})
    | Psig_class_type(_) => Sexp.list(list{Sexp.atom("Psig_class_type")})
    | Psig_attribute(attr) => Sexp.list(list{Sexp.atom("Psig_attribute"), attribute(attr)})
    | Psig_extension(ext, attrs) =>
      Sexp.list(list{Sexp.atom("Psig_extension"), extension(ext), attributes(attrs)})
    }

    Sexp.list(list{Sexp.atom("signature_item"), descr})
  }

  and includeDescription = id =>
    Sexp.list(list{
      Sexp.atom("include_description"),
      moduleType(id.pincl_mod),
      attributes(id.pincl_attributes),
    })

  and moduleDeclaration = md =>
    Sexp.list(list{
      Sexp.atom("module_declaration"),
      string(md.pmd_name.Asttypes.txt),
      moduleType(md.pmd_type),
      attributes(md.pmd_attributes),
    })

  and valueBinding = vb =>
    Sexp.list(list{
      Sexp.atom("value_binding"),
      pattern(vb.pvb_pat),
      expression(vb.pvb_expr),
      attributes(vb.pvb_attributes),
    })

  and valueDescription = vd =>
    Sexp.list(list{
      Sexp.atom("value_description"),
      string(vd.pval_name.Asttypes.txt),
      coreType(vd.pval_type),
      Sexp.list(mapEmpty(~f=string, vd.pval_prim)),
      attributes(vd.pval_attributes),
    })

  and typeDeclaration = td =>
    Sexp.list(list{
      Sexp.atom("type_declaration"),
      string(td.ptype_name.Asttypes.txt),
      Sexp.list(list{
        Sexp.atom("ptype_params"),
        Sexp.list(
          mapEmpty(
            ~f=((typexpr, var)) => Sexp.list(list{coreType(typexpr), variance(var)}),
            td.ptype_params,
          ),
        ),
      }),
      Sexp.list(list{
        Sexp.atom("ptype_cstrs"),
        Sexp.list(
          mapEmpty(
            ~f=((typ1, typ2, _loc)) => Sexp.list(list{coreType(typ1), coreType(typ2)}),
            td.ptype_cstrs,
          ),
        ),
      }),
      Sexp.list(list{Sexp.atom("ptype_kind"), typeKind(td.ptype_kind)}),
      Sexp.list(list{
        Sexp.atom("ptype_manifest"),
        switch td.ptype_manifest {
        | None => Sexp.atom("None")
        | Some(typ) => Sexp.list(list{Sexp.atom("Some"), coreType(typ)})
        },
      }),
      Sexp.list(list{Sexp.atom("ptype_private"), privateFlag(td.ptype_private)}),
      attributes(td.ptype_attributes),
    })

  and extensionConstructor = ec =>
    Sexp.list(list{
      Sexp.atom("extension_constructor"),
      string(ec.pext_name.Asttypes.txt),
      extensionConstructorKind(ec.pext_kind),
      attributes(ec.pext_attributes),
    })

  and extensionConstructorKind = kind =>
    switch kind {
    | Pext_decl(args, optTypExpr) =>
      Sexp.list(list{
        Sexp.atom("Pext_decl"),
        constructorArguments(args),
        switch optTypExpr {
        | None => Sexp.atom("None")
        | Some(typ) => Sexp.list(list{Sexp.atom("Some"), coreType(typ)})
        },
      })
    | Pext_rebind(longidentLoc) =>
      Sexp.list(list{Sexp.atom("Pext_rebind"), longident(longidentLoc.Asttypes.txt)})
    }

  and typeExtension = te =>
    Sexp.list(list{
      Sexp.atom("type_extension"),
      Sexp.list(list{Sexp.atom("ptyext_path"), longident(te.ptyext_path.Asttypes.txt)}),
      Sexp.list(list{
        Sexp.atom("ptyext_parms"),
        Sexp.list(
          mapEmpty(
            ~f=((typexpr, var)) => Sexp.list(list{coreType(typexpr), variance(var)}),
            te.ptyext_params,
          ),
        ),
      }),
      Sexp.list(list{
        Sexp.atom("ptyext_constructors"),
        Sexp.list(mapEmpty(~f=extensionConstructor, te.ptyext_constructors)),
      }),
      Sexp.list(list{Sexp.atom("ptyext_private"), privateFlag(te.ptyext_private)}),
      attributes(te.ptyext_attributes),
    })

  and typeKind = kind =>
    switch kind {
    | Ptype_abstract => Sexp.atom("Ptype_abstract")
    | Ptype_variant(constrDecls) =>
      Sexp.list(list{
        Sexp.atom("Ptype_variant"),
        Sexp.list(mapEmpty(~f=constructorDeclaration, constrDecls)),
      })
    | Ptype_record(lblDecls) =>
      Sexp.list(list{Sexp.atom("Ptype_record"), Sexp.list(mapEmpty(~f=labelDeclaration, lblDecls))})
    | Ptype_open => Sexp.atom("Ptype_open")
    }

  and constructorDeclaration = cd =>
    Sexp.list(list{
      Sexp.atom("constructor_declaration"),
      string(cd.pcd_name.Asttypes.txt),
      Sexp.list(list{Sexp.atom("pcd_args"), constructorArguments(cd.pcd_args)}),
      Sexp.list(list{
        Sexp.atom("pcd_res"),
        switch cd.pcd_res {
        | None => Sexp.atom("None")
        | Some(typ) => Sexp.list(list{Sexp.atom("Some"), coreType(typ)})
        },
      }),
      attributes(cd.pcd_attributes),
    })

  and constructorArguments = args =>
    switch args {
    | Pcstr_tuple(types) =>
      Sexp.list(list{Sexp.atom("Pcstr_tuple"), Sexp.list(mapEmpty(~f=coreType, types))})
    | Pcstr_record(lds) =>
      Sexp.list(list{Sexp.atom("Pcstr_record"), Sexp.list(mapEmpty(~f=labelDeclaration, lds))})
    }

  and labelDeclaration = ld =>
    Sexp.list(list{
      Sexp.atom("label_declaration"),
      string(ld.pld_name.Asttypes.txt),
      mutableFlag(ld.pld_mutable),
      coreType(ld.pld_type),
      attributes(ld.pld_attributes),
    })

  and expression = expr => {
    let desc = switch expr.pexp_desc {
    | Pexp_ident(longidentLoc) =>
      Sexp.list(list{Sexp.atom("Pexp_ident"), longident(longidentLoc.Asttypes.txt)})
    | Pexp_constant(c) => Sexp.list(list{Sexp.atom("Pexp_constant"), constant(c)})
    | Pexp_let(flag, vbs, expr) =>
      Sexp.list(list{
        Sexp.atom("Pexp_let"),
        recFlag(flag),
        Sexp.list(mapEmpty(~f=valueBinding, vbs)),
        expression(expr),
      })
    | Pexp_function(cases) =>
      Sexp.list(list{Sexp.atom("Pexp_function"), Sexp.list(mapEmpty(~f=case, cases))})
    | Pexp_fun(argLbl, exprOpt, pat, expr) =>
      Sexp.list(list{
        Sexp.atom("Pexp_fun"),
        argLabel(argLbl),
        switch exprOpt {
        | None => Sexp.atom("None")
        | Some(expr) => Sexp.list(list{Sexp.atom("Some"), expression(expr)})
        },
        pattern(pat),
        expression(expr),
      })
    | Pexp_apply(expr, args) =>
      Sexp.list(list{
        Sexp.atom("Pexp_apply"),
        expression(expr),
        Sexp.list(
          mapEmpty(
            ~f=((argLbl, expr)) => Sexp.list(list{argLabel(argLbl), expression(expr)}),
            args,
          ),
        ),
      })
    | Pexp_match(expr, cases) =>
      Sexp.list(list{
        Sexp.atom("Pexp_match"),
        expression(expr),
        Sexp.list(mapEmpty(~f=case, cases)),
      })
    | Pexp_try(expr, cases) =>
      Sexp.list(list{Sexp.atom("Pexp_try"), expression(expr), Sexp.list(mapEmpty(~f=case, cases))})
    | Pexp_tuple(exprs) =>
      Sexp.list(list{Sexp.atom("Pexp_tuple"), Sexp.list(mapEmpty(~f=expression, exprs))})
    | Pexp_construct(longidentLoc, exprOpt) =>
      Sexp.list(list{
        Sexp.atom("Pexp_construct"),
        longident(longidentLoc.Asttypes.txt),
        switch exprOpt {
        | None => Sexp.atom("None")
        | Some(expr) => Sexp.list(list{Sexp.atom("Some"), expression(expr)})
        },
      })
    | Pexp_variant(lbl, exprOpt) =>
      Sexp.list(list{
        Sexp.atom("Pexp_variant"),
        string(lbl),
        switch exprOpt {
        | None => Sexp.atom("None")
        | Some(expr) => Sexp.list(list{Sexp.atom("Some"), expression(expr)})
        },
      })
    | Pexp_record(rows, optExpr) =>
      Sexp.list(list{
        Sexp.atom("Pexp_record"),
        Sexp.list(
          mapEmpty(
            ~f=((longidentLoc, expr)) =>
              Sexp.list(list{longident(longidentLoc.Asttypes.txt), expression(expr)}),
            rows,
          ),
        ),
        switch optExpr {
        | None => Sexp.atom("None")
        | Some(expr) => Sexp.list(list{Sexp.atom("Some"), expression(expr)})
        },
      })
    | Pexp_field(expr, longidentLoc) =>
      Sexp.list(list{
        Sexp.atom("Pexp_field"),
        expression(expr),
        longident(longidentLoc.Asttypes.txt),
      })
    | Pexp_setfield(expr1, longidentLoc, expr2) =>
      Sexp.list(list{
        Sexp.atom("Pexp_setfield"),
        expression(expr1),
        longident(longidentLoc.Asttypes.txt),
        expression(expr2),
      })
    | Pexp_array(exprs) =>
      Sexp.list(list{Sexp.atom("Pexp_array"), Sexp.list(mapEmpty(~f=expression, exprs))})
    | Pexp_ifthenelse(expr1, expr2, optExpr) =>
      Sexp.list(list{
        Sexp.atom("Pexp_ifthenelse"),
        expression(expr1),
        expression(expr2),
        switch optExpr {
        | None => Sexp.atom("None")
        | Some(expr) => Sexp.list(list{Sexp.atom("Some"), expression(expr)})
        },
      })
    | Pexp_sequence(expr1, expr2) =>
      Sexp.list(list{Sexp.atom("Pexp_sequence"), expression(expr1), expression(expr2)})
    | Pexp_while(expr1, expr2) =>
      Sexp.list(list{Sexp.atom("Pexp_while"), expression(expr1), expression(expr2)})
    | Pexp_for(pat, e1, e2, flag, e3) =>
      Sexp.list(list{
        Sexp.atom("Pexp_for"),
        pattern(pat),
        expression(e1),
        expression(e2),
        directionFlag(flag),
        expression(e3),
      })
    | Pexp_constraint(expr, typexpr) =>
      Sexp.list(list{Sexp.atom("Pexp_constraint"), expression(expr), coreType(typexpr)})
    | Pexp_coerce(expr, optTyp, typexpr) =>
      Sexp.list(list{
        Sexp.atom("Pexp_coerce"),
        expression(expr),
        switch optTyp {
        | None => Sexp.atom("None")
        | Some(typ) => Sexp.list(list{Sexp.atom("Some"), coreType(typ)})
        },
        coreType(typexpr),
      })
    | Pexp_send(_) => Sexp.list(list{Sexp.atom("Pexp_send")})
    | Pexp_new(_) => Sexp.list(list{Sexp.atom("Pexp_new")})
    | Pexp_setinstvar(_) => Sexp.list(list{Sexp.atom("Pexp_setinstvar")})
    | Pexp_override(_) => Sexp.list(list{Sexp.atom("Pexp_override")})
    | Pexp_letmodule(modName, modExpr, expr) =>
      Sexp.list(list{
        Sexp.atom("Pexp_letmodule"),
        string(modName.Asttypes.txt),
        moduleExpression(modExpr),
        expression(expr),
      })
    | Pexp_letexception(extConstr, expr) =>
      Sexp.list(list{
        Sexp.atom("Pexp_letexception"),
        extensionConstructor(extConstr),
        expression(expr),
      })
    | Pexp_assert(expr) => Sexp.list(list{Sexp.atom("Pexp_assert"), expression(expr)})
    | Pexp_lazy(expr) => Sexp.list(list{Sexp.atom("Pexp_lazy"), expression(expr)})
    | Pexp_poly(_) => Sexp.list(list{Sexp.atom("Pexp_poly")})
    | Pexp_object(_) => Sexp.list(list{Sexp.atom("Pexp_object")})
    | Pexp_newtype(lbl, expr) =>
      Sexp.list(list{Sexp.atom("Pexp_newtype"), string(lbl.Asttypes.txt), expression(expr)})
    | Pexp_pack(modExpr) => Sexp.list(list{Sexp.atom("Pexp_pack"), moduleExpression(modExpr)})
    | Pexp_open(flag, longidentLoc, expr) =>
      Sexp.list(list{
        Sexp.atom("Pexp_open"),
        overrideFlag(flag),
        longident(longidentLoc.Asttypes.txt),
        expression(expr),
      })
    | Pexp_extension(ext) => Sexp.list(list{Sexp.atom("Pexp_extension"), extension(ext)})
    | Pexp_unreachable => Sexp.atom("Pexp_unreachable")
    }

    Sexp.list(list{Sexp.atom("expression"), desc})
  }

  and case = c =>
    Sexp.list(list{
      Sexp.atom("case"),
      Sexp.list(list{Sexp.atom("pc_lhs"), pattern(c.pc_lhs)}),
      Sexp.list(list{
        Sexp.atom("pc_guard"),
        switch c.pc_guard {
        | None => Sexp.atom("None")
        | Some(expr) => Sexp.list(list{Sexp.atom("Some"), expression(expr)})
        },
      }),
      Sexp.list(list{Sexp.atom("pc_rhs"), expression(c.pc_rhs)}),
    })

  and pattern = p => {
    let descr = switch p.ppat_desc {
    | Ppat_any => Sexp.atom("Ppat_any")
    | Ppat_var(var) => Sexp.list(list{Sexp.atom("Ppat_var"), string(var.Location.txt)})
    | Ppat_alias(p, alias) =>
      Sexp.list(list{Sexp.atom("Ppat_alias"), pattern(p), string(alias.txt)})
    | Ppat_constant(c) => Sexp.list(list{Sexp.atom("Ppat_constant"), constant(c)})
    | Ppat_interval(lo, hi) =>
      Sexp.list(list{Sexp.atom("Ppat_interval"), constant(lo), constant(hi)})
    | Ppat_tuple(patterns) =>
      Sexp.list(list{Sexp.atom("Ppat_tuple"), Sexp.list(mapEmpty(~f=pattern, patterns))})
    | Ppat_construct(longidentLoc, optPattern) =>
      Sexp.list(list{
        Sexp.atom("Ppat_construct"),
        longident(longidentLoc.Location.txt),
        switch optPattern {
        | None => Sexp.atom("None")
        | Some(p) => Sexp.list(list{Sexp.atom("some"), pattern(p)})
        },
      })
    | Ppat_variant(lbl, optPattern) =>
      Sexp.list(list{
        Sexp.atom("Ppat_variant"),
        string(lbl),
        switch optPattern {
        | None => Sexp.atom("None")
        | Some(p) => Sexp.list(list{Sexp.atom("Some"), pattern(p)})
        },
      })
    | Ppat_record(rows, flag) =>
      Sexp.list(list{
        Sexp.atom("Ppat_record"),
        closedFlag(flag),
        Sexp.list(
          mapEmpty(
            ~f=((longidentLoc, p)) =>
              Sexp.list(list{longident(longidentLoc.Location.txt), pattern(p)}),
            rows,
          ),
        ),
      })
    | Ppat_array(patterns) =>
      Sexp.list(list{Sexp.atom("Ppat_array"), Sexp.list(mapEmpty(~f=pattern, patterns))})
    | Ppat_or(p1, p2) => Sexp.list(list{Sexp.atom("Ppat_or"), pattern(p1), pattern(p2)})
    | Ppat_constraint(p, typexpr) =>
      Sexp.list(list{Sexp.atom("Ppat_constraint"), pattern(p), coreType(typexpr)})
    | Ppat_type(longidentLoc) =>
      Sexp.list(list{Sexp.atom("Ppat_type"), longident(longidentLoc.Location.txt)})
    | Ppat_lazy(p) => Sexp.list(list{Sexp.atom("Ppat_lazy"), pattern(p)})
    | Ppat_unpack(stringLoc) =>
      Sexp.list(list{Sexp.atom("Ppat_unpack"), string(stringLoc.Location.txt)})
    | Ppat_exception(p) => Sexp.list(list{Sexp.atom("Ppat_exception"), pattern(p)})
    | Ppat_extension(ext) => Sexp.list(list{Sexp.atom("Ppat_extension"), extension(ext)})
    | Ppat_open(longidentLoc, p) =>
      Sexp.list(list{Sexp.atom("Ppat_open"), longident(longidentLoc.Location.txt), pattern(p)})
    }

    Sexp.list(list{Sexp.atom("pattern"), descr})
  }

  and objectField = field =>
    switch field {
    | Otag(lblLoc, attrs, typexpr) =>
      Sexp.list(list{Sexp.atom("Otag"), string(lblLoc.txt), attributes(attrs), coreType(typexpr)})
    | Oinherit(typexpr) => Sexp.list(list{Sexp.atom("Oinherit"), coreType(typexpr)})
    }

  and rowField = field =>
    switch field {
    | Rtag(labelLoc, attrs, truth, types) =>
      Sexp.list(list{
        Sexp.atom("Rtag"),
        string(labelLoc.txt),
        attributes(attrs),
        Sexp.atom(
          if truth {
            "true"
          } else {
            "false"
          },
        ),
        Sexp.list(mapEmpty(~f=coreType, types)),
      })
    | Rinherit(typexpr) => Sexp.list(list{Sexp.atom("Rinherit"), coreType(typexpr)})
    }

  and packageType = ((modNameLoc, packageConstraints)) =>
    Sexp.list(list{
      Sexp.atom("package_type"),
      longident(modNameLoc.Asttypes.txt),
      Sexp.list(
        mapEmpty(
          ~f=((modNameLoc, typexpr)) =>
            Sexp.list(list{longident(modNameLoc.Asttypes.txt), coreType(typexpr)}),
          packageConstraints,
        ),
      ),
    })

  and coreType = typexpr => {
    let desc = switch typexpr.ptyp_desc {
    | Ptyp_any => Sexp.atom("Ptyp_any")
    | Ptyp_var(var) => Sexp.list(list{Sexp.atom("Ptyp_var"), string(var)})
    | Ptyp_arrow(argLbl, typ1, typ2) =>
      Sexp.list(list{Sexp.atom("Ptyp_arrow"), argLabel(argLbl), coreType(typ1), coreType(typ2)})
    | Ptyp_tuple(types) =>
      Sexp.list(list{Sexp.atom("Ptyp_tuple"), Sexp.list(mapEmpty(~f=coreType, types))})
    | Ptyp_constr(longidentLoc, types) =>
      Sexp.list(list{
        Sexp.atom("Ptyp_constr"),
        longident(longidentLoc.txt),
        Sexp.list(mapEmpty(~f=coreType, types)),
      })
    | Ptyp_alias(typexpr, alias) =>
      Sexp.list(list{Sexp.atom("Ptyp_alias"), coreType(typexpr), string(alias)})
    | Ptyp_object(fields, flag) =>
      Sexp.list(list{
        Sexp.atom("Ptyp_object"),
        closedFlag(flag),
        Sexp.list(mapEmpty(~f=objectField, fields)),
      })
    | Ptyp_class(longidentLoc, types) =>
      Sexp.list(list{
        Sexp.atom("Ptyp_class"),
        longident(longidentLoc.Location.txt),
        Sexp.list(mapEmpty(~f=coreType, types)),
      })
    | Ptyp_variant(fields, flag, optLabels) =>
      Sexp.list(list{
        Sexp.atom("Ptyp_variant"),
        Sexp.list(mapEmpty(~f=rowField, fields)),
        closedFlag(flag),
        switch optLabels {
        | None => Sexp.atom("None")
        | Some(lbls) => Sexp.list(mapEmpty(~f=string, lbls))
        },
      })
    | Ptyp_poly(lbls, typexpr) =>
      Sexp.list(list{
        Sexp.atom("Ptyp_poly"),
        Sexp.list(mapEmpty(~f=lbl => string(lbl.Asttypes.txt), lbls)),
        coreType(typexpr),
      })
    | Ptyp_package(package) => Sexp.list(list{Sexp.atom("Ptyp_package"), packageType(package)})
    | Ptyp_extension(ext) => Sexp.list(list{Sexp.atom("Ptyp_extension"), extension(ext)})
    }

    Sexp.list(list{Sexp.atom("core_type"), desc})
  }

  and payload = p =>
    switch p {
    | PStr(s) => Sexp.list(list{Sexp.atom("PStr"), ...mapEmpty(~f=structureItem, s)})
    | PSig(s) => Sexp.list(list{Sexp.atom("PSig"), signature(s)})
    | PTyp(ct) => Sexp.list(list{Sexp.atom("PTyp"), coreType(ct)})
    | PPat(pat, optExpr) =>
      Sexp.list(list{
        Sexp.atom("PPat"),
        pattern(pat),
        switch optExpr {
        | Some(expr) => Sexp.list(list{Sexp.atom("Some"), expression(expr)})
        | None => Sexp.atom("None")
        },
      })
    }

  and attribute = ((stringLoc, p)) =>
    Sexp.list(list{Sexp.atom("attribute"), Sexp.atom(stringLoc.Asttypes.txt), payload(p)})

  and extension = ((stringLoc, p)) =>
    Sexp.list(list{Sexp.atom("extension"), Sexp.atom(stringLoc.Asttypes.txt), payload(p)})

  and attributes = attrs => {
    let sexprs = mapEmpty(~f=attribute, attrs)
    Sexp.list(list{Sexp.atom("attributes"), ...sexprs})
  }

  let implementation = structure
  let interface = signature
}

module IO: {
  let readFile: string => string
  let readStdin: unit => string
} = {
  /* random chunk size: 2^15, TODO: why do we guess randomly? */
  let chunkSize = 32768

  let readFile = filename => {
    let chan = open_in(filename)
    let buffer = Buffer.create(chunkSize)
    let chunk = (@doesNotRaise Bytes.create)(chunkSize)
    let rec loop = () => {
      let len = try input(chan, chunk, 0, chunkSize) catch {
      | Invalid_argument(_) => 0
      }
      if len === 0 {
        close_in_noerr(chan)
        Buffer.contents(buffer)
      } else {
        Buffer.add_subbytes(buffer, chunk, 0, len)
        loop()
      }
    }

    loop()
  }

  let readStdin = () => {
    let buffer = Buffer.create(chunkSize)
    let chunk = (@doesNotRaise Bytes.create)(chunkSize)
    let rec loop = () => {
      let len = try input(stdin, chunk, 0, chunkSize) catch {
      | Invalid_argument(_) => 0
      }
      if len === 0 {
        close_in_noerr(stdin)
        Buffer.contents(buffer)
      } else {
        Buffer.add_subbytes(buffer, chunk, 0, len)
        loop()
      }
    }

    loop()
  }
}

module CharacterCodes = {
  let eof = -1

  let space = 0x0020
  @live let newline = 0x0A /* \n */
  let lineFeed = 0x0A /* \n */
  let carriageReturn = 0x0D /* \r */
  let lineSeparator = 0x2028
  let paragraphSeparator = 0x2029

  let tab = 0x09

  let bang = 0x21
  let dot = 0x2E
  let colon = 0x3A
  let comma = 0x2C
  let backtick = 0x60
  /* let question = 0x3F */
  let semicolon = 0x3B
  let underscore = 0x5F
  let singleQuote = 0x27
  let doubleQuote = 0x22
  let equal = 0x3D
  let bar = 0x7C
  let tilde = 0x7E
  let question = 0x3F
  let ampersand = 0x26
  let at = 0x40
  let dollar = 0x24
  let percent = 0x25

  let lparen = 0x28
  let rparen = 0x29
  let lbracket = 0x5B
  let rbracket = 0x5D
  let lbrace = 0x7B
  let rbrace = 0x7D

  let forwardslash = 0x2F /* / */
  let backslash = 0x5C /* \ */

  let greaterThan = 0x3E
  let hash = 0x23
  let lessThan = 0x3C

  let minus = 0x2D
  let plus = 0x2B
  let asterisk = 0x2A

  let _0 = 0x30
  @live let _1 = 0x31
  @live let _2 = 0x32
  @live let _3 = 0x33
  @live let _4 = 0x34
  @live let _5 = 0x35
  @live let _6 = 0x36
  @live let _7 = 0x37
  @live let _8 = 0x38
  let _9 = 0x39

  module Lower = {
    let a = 0x61
    let b = 0x62
    @live let c = 0x63
    @live let d = 0x64
    let e = 0x65
    let f = 0x66
    let g = 0x67
    @live let h = 0x68
    @live let i = 0x69
    @live let j = 0x6A
    @live let k = 0x6B
    @live let l = 0x6C
    @live let m = 0x6D
    let n = 0x6E
    let o = 0x6F
    let p = 0x70
    @live let q = 0x71
    let r = 0x72
    @live let s = 0x73
    let t = 0x74
    @live let u = 0x75
    @live let v = 0x76
    @live let w = 0x77
    let x = 0x78
    @live let y = 0x79
    let z = 0x7A
  }

  module Upper = {
    let a = 0x41
    /* let b = 0x42 */
    @live let c = 0x43
    @live let d = 0x44
    @live let e = 0x45
    @live let f = 0x46
    let g = 0x47
    @live let h = 0x48
    @live let i = 0x49
    @live let j = 0x4A
    @live let k = 0x4B
    @live let l = 0x4C
    @live let m = 0x4D
    @live let b = 0x4E
    @live let o = 0x4F
    @live let p = 0x50
    @live let q = 0x51
    @live let r = 0x52
    @live let s = 0x53
    @live let t = 0x54
    @live let u = 0x55
    @live let v = 0x56
    @live let w = 0x57
    @live let x = 0x58
    @live let y = 0x59
    let z = 0x5a
  }

  /* returns lower-case ch, ch should be ascii */
  let lower = ch =>
    /* if ch >= Lower.a && ch <= Lower.z then ch else ch + 32 */
    lor(32, ch)

  let isLetter = ch => (Lower.a <= ch && ch <= Lower.z) || (Upper.a <= ch && ch <= Upper.z)

  let isUpperCase = ch => Upper.a <= ch && ch <= Upper.z

  let isDigit = ch => _0 <= ch && ch <= _9

  let isHex = ch => (_0 <= ch && ch <= _9) || (Lower.a <= lower(ch) && lower(ch) <= Lower.f)

  /*
      // ES5 7.3:
      // The ECMAScript line terminator characters are listed in Table 3.
      //     Table 3: Line Terminator Characters
      //     Code Unit Value     Name                    Formal Name
      //     \u000A              Line Feed               <LF>
      //     \u000D              Carriage Return         <CR>
      //     \u2028              Line separator          <LS>
      //     \u2029              Paragraph separator     <PS>
      // Only the characters in Table 3 are treated as line terminators. Other new line or line
      // breaking characters are treated as white space but not as line terminators.
 */
  let isLineBreak = ch =>
    ch === lineFeed ||
      (ch === carriageReturn ||
      (ch === lineSeparator || ch === paragraphSeparator))

  let digitValue = ch =>
    if _0 <= ch && ch <= _9 {
      ch - 48
    } else if Lower.a <= lower(ch) && lower(ch) <= Lower.f {
      lower(ch) - Lower.a + 10
    } else {
      16
    } /* larger than any legal value */
}

module Comment: {
  type t

  let toString: t => string

  let loc: t => Location.t
  let txt: t => string
  let prevTokEndPos: t => Lexing.position

  let setPrevTokEndPos: (t, Lexing.position) => unit

  let isSingleLineComment: t => bool

  let makeSingleLineComment: (~loc: Location.t, string) => t
  let makeMultiLineComment: (~loc: Location.t, string) => t
  let fromOcamlComment: (~loc: Location.t, ~txt: string, ~prevTokEndPos: Lexing.position) => t
  let trimSpaces: string => string
} = {
  type style =
    | SingleLine
    | MultiLine

  let styleToString = s =>
    switch s {
    | SingleLine => "SingleLine"
    | MultiLine => "MultiLine"
    }

  type t = {
    txt: string,
    style: style,
    loc: Location.t,
    mutable prevTokEndPos: Lexing.position,
  }

  let loc = t => t.loc
  let txt = t => t.txt
  let prevTokEndPos = t => t.prevTokEndPos

  let setPrevTokEndPos = (t, pos) => t.prevTokEndPos = pos

  let isSingleLineComment = t =>
    switch t.style {
    | SingleLine => true
    | MultiLine => false
    }

  let toString = t =>
    Format.sprintf(
      "(txt: %s\nstyle: %s\nlines: %d-%d)",
      t.txt,
      styleToString(t.style),
      t.loc.loc_start.pos_lnum,
      t.loc.loc_end.pos_lnum,
    )

  let makeSingleLineComment = (~loc, txt) => {
    txt: txt,
    loc: loc,
    style: SingleLine,
    prevTokEndPos: Lexing.dummy_pos,
  }

  let makeMultiLineComment = (~loc, txt) => {
    txt: txt,
    loc: loc,
    style: MultiLine,
    prevTokEndPos: Lexing.dummy_pos,
  }

  let fromOcamlComment = (~loc, ~txt, ~prevTokEndPos) => {
    txt: txt,
    loc: loc,
    style: MultiLine,
    prevTokEndPos: prevTokEndPos,
  }

  let trimSpaces = s => {
    let len = String.length(s)
    if len == 0 {
      s
    } else if String.unsafe_get(s, 0) == ' ' || String.unsafe_get(s, len - 1) == ' ' {
      let b = Bytes.of_string(s)
      let i = ref(0)
      while i.contents < len && Bytes.unsafe_get(b, i.contents) == ' ' {
        incr(i)
      }
      let j = ref(len - 1)
      while j.contents >= i.contents && Bytes.unsafe_get(b, j.contents) == ' ' {
        decr(j)
      }
      if j.contents >= i.contents {
        (@doesNotRaise Bytes.sub)(b, i.contents, j.contents - i.contents + 1) |> Bytes.to_string
      } else {
        ""
      }
    } else {
      s
    }
  }
}

module Token = {
  type t =
    | Open
    | True
    | False
    | Character(char)
    | Int({i: string, suffix: option<char>})
    | Float({f: string, suffix: option<char>})
    | String(string)
    | Lident(string)
    | Uident(string)
    | As
    | Dot
    | DotDot
    | DotDotDot
    | Bang
    | Semicolon
    | Let
    | And
    | Rec
    | Underscore
    | SingleQuote
    | Equal
    | EqualEqual
    | EqualEqualEqual
    | Bar
    | Lparen
    | Rparen
    | Lbracket
    | Rbracket
    | Lbrace
    | Rbrace
    | Colon
    | Comma
    | Eof
    | Exception
    | @live Backslash
    | Forwardslash
    | ForwardslashDot
    | Asterisk
    | AsteriskDot
    | Exponentiation
    | Minus
    | MinusDot
    | Plus
    | PlusDot
    | PlusPlus
    | PlusEqual
    | ColonGreaterThan
    | GreaterThan
    | LessThan
    | LessThanSlash
    | Hash
    | HashEqual
    | HashHash
    | Assert
    | Lazy
    | Tilde
    | Question
    | If
    | Else
    | For
    | In
    | To
    | Downto
    | While
    | Switch
    | When
    | EqualGreater
    | MinusGreater
    | External
    | Typ
    | Private
    | Mutable
    | Constraint
    | Include
    | Module
    | Of
    | With
    | Land
    | Lor
    | Band /* Bitwise and: & */
    | BangEqual
    | BangEqualEqual
    | LessEqual
    | GreaterEqual
    | ColonEqual
    | At
    | AtAt
    | Percent
    | PercentPercent
    | Comment(Comment.t)
    | List
    | TemplateTail(string)
    | TemplatePart(string)
    | Backtick
    | BarGreater
    | Try
    | Catch
    | Import
    | Export

  let precedence = x =>
    switch x {
    | HashEqual | ColonEqual => 1
    | Lor => 2
    | Land => 3
    | Equal
    | EqualEqual
    | EqualEqualEqual
    | LessThan
    | GreaterThan
    | BangEqual
    | BangEqualEqual
    | LessEqual
    | GreaterEqual
    | BarGreater => 4
    | Plus | PlusDot | Minus | MinusDot | PlusPlus => 5
    | Asterisk | AsteriskDot | Forwardslash | ForwardslashDot => 6
    | Exponentiation => 7
    | MinusGreater => 8
    | Dot => 9
    | _ => 0
    }

  let toString = x =>
    switch x {
    | Open => "open"
    | True => "true"
    | False => "false"
    | Character(c) => "'" ++ (Char.escaped(c) ++ "'")
    | String(s) => s
    | Lident(str) => str
    | Uident(str) => str
    | Dot => "."
    | DotDot => ".."
    | DotDotDot => "..."
    | Int({i}) => "int " ++ i
    | Float({f}) => "Float: " ++ f
    | Bang => "!"
    | Semicolon => ";"
    | Let => "let"
    | And => "and"
    | Rec => "rec"
    | Underscore => "_"
    | SingleQuote => "'"
    | Equal => "="
    | EqualEqual => "=="
    | EqualEqualEqual => "==="
    | Eof => "eof"
    | Bar => "|"
    | As => "as"
    | Lparen => "("
    | Rparen => ")"
    | Lbracket => "["
    | Rbracket => "]"
    | Lbrace => "{"
    | Rbrace => "}"
    | ColonGreaterThan => ":>"
    | Colon => ":"
    | Comma => ","
    | Minus => "-"
    | MinusDot => "-."
    | Plus => "+"
    | PlusDot => "+."
    | PlusPlus => "++"
    | PlusEqual => "+="
    | Backslash => "\\"
    | Forwardslash => "/"
    | ForwardslashDot => "/."
    | Exception => "exception"
    | Hash => "#"
    | HashHash => "##"
    | HashEqual => "#="
    | GreaterThan => ">"
    | LessThan => "<"
    | LessThanSlash => "</"
    | Asterisk => "*"
    | AsteriskDot => "*."
    | Exponentiation => "**"
    | Assert => "assert"
    | Lazy => "lazy"
    | Tilde => "tilde"
    | Question => "?"
    | If => "if"
    | Else => "else"
    | For => "for"
    | In => "in"
    | To => "to"
    | Downto => "downto"
    | While => "while"
    | Switch => "switch"
    | When => "when"
    | EqualGreater => "=>"
    | MinusGreater => "->"
    | External => "external"
    | Typ => "type"
    | Private => "private"
    | Constraint => "constraint"
    | Mutable => "mutable"
    | Include => "include"
    | Module => "module"
    | Of => "of"
    | With => "with"
    | Lor => "||"
    | Band => "&"
    | Land => "&&"
    | BangEqual => "!="
    | BangEqualEqual => "!=="
    | GreaterEqual => ">="
    | LessEqual => "<="
    | ColonEqual => ":="
    | At => "@"
    | AtAt => "@@"
    | Percent => "%"
    | PercentPercent => "%%"
    | Comment(c) => "Comment(" ++ (Comment.toString(c) ++ ")")
    | List => "list"
    | TemplatePart(text) => text ++ "${"
    | TemplateTail(text) => "TemplateTail(" ++ (text ++ ")")
    | Backtick => "`"
    | BarGreater => "|>"
    | Try => "try"
    | Catch => "catch"
    | Import => "import"
    | Export => "export"
    }

  @raises(Not_found)
  let keywordTable = x =>
    switch x {
    | "true" => True
    | "false" => False
    | "open" => Open
    | "let" => Let
    | "rec" => Rec
    | "and" => And
    | "as" => As
    | "exception" => Exception
    | "assert" => Assert
    | "lazy" => Lazy
    | "if" => If
    | "else" => Else
    | "for" => For
    | "in" => In
    | "to" => To
    | "downto" => Downto
    | "while" => While
    | "switch" => Switch
    | "when" => When
    | "external" => External
    | "type" => Typ
    | "private" => Private
    | "mutable" => Mutable
    | "constraint" => Constraint
    | "include" => Include
    | "module" => Module
    | "of" => Of
    | "list" => List
    | "with" => With
    | "try" => Try
    | "catch" => Catch
    | "import" => Import
    | "export" => Export
    | _ => throw(Not_found)
    }

  let isKeyword = x =>
    switch x {
    | True
    | False
    | Open
    | Let
    | Rec
    | And
    | As
    | Exception
    | Assert
    | Lazy
    | If
    | Else
    | For
    | In
    | To
    | Downto
    | While
    | Switch
    | When
    | External
    | Typ
    | Private
    | Mutable
    | Constraint
    | Include
    | Module
    | Of
    | Land
    | Lor
    | List
    | With
    | Try
    | Catch
    | Import
    | Export => true
    | _ => false
    }

  let lookupKeyword = str =>
    try keywordTable(str) catch {
    | Not_found =>
      if CharacterCodes.isUpperCase(int_of_char(@doesNotRaise String.get(str, 0))) {
        Uident(str)
      } else {
        Lident(str)
      }
    }

  let isKeywordTxt = str =>
    try {
      let _ = keywordTable(str)
      true
    } catch {
    | Not_found => false
    }
}

module Grammar = {
  type t =
    | OpenDescription /* open Belt */
    | @live ModuleLongIdent /* Foo or Foo.Bar */
    | Ternary /* condExpr ? trueExpr : falseExpr */
    | Es6ArrowExpr
    | Jsx
    | JsxAttribute
    | @live JsxChild
    | ExprOperand
    | ExprUnary
    | ExprSetField
    | ExprBinaryAfterOp(Token.t)
    | ExprBlock
    | ExprCall
    | ExprList
    | ExprArrayAccess
    | ExprArrayMutation
    | ExprIf
    | IfCondition
    | IfBranch
    | ElseBranch
    | TypeExpression
    | External
    | PatternMatching
    | PatternMatchCase
    | LetBinding
    | PatternList
    | PatternOcamlList
    | PatternRecord

    | TypeDef
    | TypeConstrName
    | TypeParams
    | @live TypeParam
    | PackageConstraint

    | TypeRepresentation

    | RecordDecl
    | ConstructorDeclaration
    | ParameterList
    | StringFieldDeclarations
    | FieldDeclarations
    | TypExprList
    | FunctorArgs
    | ModExprList
    | TypeParameters
    | RecordRows
    | RecordRowsStringKey
    | ArgumentList
    | Signature
    | Specification
    | Structure
    | Implementation
    | Attribute
    | TypeConstraint
    | Primitive
    | AtomicTypExpr
    | ListExpr
    | JsFfiImport

  let toString = x =>
    switch x {
    | OpenDescription => "an open description"
    | ModuleLongIdent => "a module identifier"
    | Ternary => "a ternary expression"
    | Es6ArrowExpr => "an es6 arrow function"
    | Jsx => "a jsx expression"
    | JsxAttribute => "a jsx attribute"
    | ExprOperand => "a basic expression"
    | ExprUnary => "a unary expression"
    | ExprBinaryAfterOp(op) => "an expression after the operator \"" ++ (Token.toString(op) ++ "\"")
    | ExprIf => "an if expression"
    | IfCondition => "the condition of an if expression"
    | IfBranch => "the true-branch of an if expression"
    | ElseBranch => "the else-branch of an if expression"
    | TypeExpression => "a type"
    | External => "an external"
    | PatternMatching => "the cases of a pattern match"
    | ExprBlock => "a block with expressions"
    | ExprSetField => "a record field mutation"
    | ExprCall => "a function application"
    | ExprArrayAccess => "an array access expression"
    | ExprArrayMutation => "an array mutation"
    | LetBinding => "a let binding"
    | TypeDef => "a type definition"
    | TypeParams => "type parameters"
    | TypeParam => "a type parameter"
    | TypeConstrName => "a type-constructor name"
    | TypeRepresentation => "a type representation"
    | RecordDecl => "a record declaration"
    | PatternMatchCase => "a pattern match case"
    | ConstructorDeclaration => "a constructor declaration"
    | ExprList => "multiple expressions"
    | PatternList => "multiple patterns"
    | PatternOcamlList => "a list pattern"
    | PatternRecord => "a record pattern"
    | ParameterList => "parameters"
    | StringFieldDeclarations => "string field declarations"
    | FieldDeclarations => "field declarations"
    | TypExprList => "list of types"
    | FunctorArgs => "functor arguments"
    | ModExprList => "list of module expressions"
    | TypeParameters => "list of type parameters"
    | RecordRows => "rows of a record"
    | RecordRowsStringKey => "rows of a record with string keys"
    | ArgumentList => "arguments"
    | Signature => "signature"
    | Specification => "specification"
    | Structure => "structure"
    | Implementation => "implementation"
    | Attribute => "an attribute"
    | TypeConstraint => "constraints on a type"
    | Primitive => "an external primitive"
    | AtomicTypExpr => "a type"
    | ListExpr => "an ocaml list expr"
    | PackageConstraint => "a package constraint"
    | JsFfiImport => "js ffi import"
    | JsxChild => "jsx child"
    }

  let isSignatureItemStart = x =>
    switch x {
    | Token.At
    | Let
    | Typ
    | External
    | Exception
    | Open
    | Include
    | Module
    | AtAt
    | PercentPercent => true
    | _ => false
    }

  let isAtomicPatternStart = x =>
    switch x {
    | Token.Int(_)
    | String(_)
    | Character(_)
    | Lparen
    | Lbracket
    | Lbrace
    | Underscore
    | Lident(_)
    | Uident(_)
    | List
    | Exception
    | Lazy
    | Percent => true
    | _ => false
    }

  let isAtomicExprStart = x =>
    switch x {
    | Token.True
    | False
    | Int(_)
    | String(_)
    | Float(_)
    | Character(_)
    | Backtick
    | Uident(_)
    | Lident(_)
    | Hash
    | Lparen
    | List
    | Lbracket
    | Lbrace
    | LessThan
    | Module
    | Percent => true
    | _ => false
    }

  let isAtomicTypExprStart = x =>
    switch x {
    | Token.SingleQuote
    | Underscore
    | Lparen
    | Lbrace
    | Uident(_)
    | Lident(_)
    | List
    | Percent => true
    | _ => false
    }

  let isExprStart = x =>
    switch x {
    | Token.True
    | False
    | Int(_)
    | String(_)
    | Float(_)
    | Character(_)
    | Backtick
    | Underscore
    | Uident(_)
    | Lident(_)
    | Hash
    | Lparen
    | List
    | Module
    | Lbracket
    | Lbrace
    | LessThan
    | Minus
    | MinusDot
    | Plus
    | PlusDot
    | Bang
    | Percent
    | At
    | If
    | Switch
    | While
    | For
    | Assert
    | Lazy
    | Try => true
    | _ => false
    }

  let isJsxAttributeStart = x =>
    switch x {
    | Token.Lident(_) | Question => true
    | _ => false
    }

  let isStructureItemStart = x =>
    switch x {
    | Token.Open
    | Let
    | Typ
    | External
    | Import
    | Export
    | Exception
    | Include
    | Module
    | AtAt
    | PercentPercent
    | At => true
    | t when isExprStart(t) => true
    | _ => false
    }

  let isPatternStart = x =>
    switch x {
    | Token.Int(_)
    | Float(_)
    | String(_)
    | Character(_)
    | True
    | False
    | Minus
    | Plus
    | Lparen
    | Lbracket
    | Lbrace
    | List
    | Underscore
    | Lident(_)
    | Uident(_)
    | Hash
    | HashHash
    | Exception
    | Lazy
    | Percent
    | Module
    | At => true
    | _ => false
    }

  let isParameterStart = x =>
    switch x {
    | Token.Typ | Tilde | Dot => true
    | token when isPatternStart(token) => true
    | _ => false
    }

  /* TODO: overparse Uident ? */
  let isStringFieldDeclStart = x =>
    switch x {
    | Token.String(_) | At => true
    | _ => false
    }

  /* TODO: overparse Uident ? */
  let isFieldDeclStart = x =>
    switch x {
    | Token.At | Mutable | Lident(_) | List => true
    /* recovery, TODO: this is not ideal… */
    | Uident(_) => true
    | t when Token.isKeyword(t) => true
    | _ => false
    }

  let isRecordDeclStart = x =>
    switch x {
    | Token.At | Mutable | Lident(_) | List => true
    | _ => false
    }

  let isTypExprStart = x =>
    switch x {
    | Token.At
    | SingleQuote
    | Underscore
    | Lparen
    | Lbracket
    | Uident(_)
    | Lident(_)
    | List
    | Module
    | Percent
    | Lbrace => true
    | _ => false
    }

  let isTypeParameterStart = x =>
    switch x {
    | Token.Tilde | Dot => true
    | token when isTypExprStart(token) => true
    | _ => false
    }

  let isTypeParamStart = x =>
    switch x {
    | Token.Plus | Minus | SingleQuote | Underscore => true
    | _ => false
    }

  let isFunctorArgStart = x =>
    switch x {
    | Token.At | Uident(_) | Underscore | Percent | Lbrace | Lparen => true
    | _ => false
    }

  let isModExprStart = x =>
    switch x {
    | Token.At | Percent | Uident(_) | Lbrace | Lparen => true
    | _ => false
    }

  let isRecordRowStart = x =>
    switch x {
    | Token.DotDotDot => true
    | Token.Uident(_) | Lident(_) | List => true
    /* TODO */
    | t when Token.isKeyword(t) => true
    | _ => false
    }

  let isRecordRowStringKeyStart = x =>
    switch x {
    | Token.String(_) => true
    | _ => false
    }

  let isArgumentStart = x =>
    switch x {
    | Token.Tilde | Dot | Underscore => true
    | t when isExprStart(t) => true
    | _ => false
    }

  let isPatternMatchStart = x =>
    switch x {
    | Token.Bar => true
    | t when isPatternStart(t) => true
    | _ => false
    }

  let isPatternOcamlListStart = x =>
    switch x {
    | Token.DotDotDot => true
    | t when isPatternStart(t) => true
    | _ => false
    }

  let isPatternRecordItemStart = x =>
    switch x {
    | Token.DotDotDot | Uident(_) | Lident(_) | List | Underscore => true
    | _ => false
    }

  let isAttributeStart = x =>
    switch x {
    | Token.At => true
    | _ => false
    }

  let isJsFfiImportStart = x =>
    switch x {
    | Token.Lident(_) | At => true
    | _ => false
    }

  let isJsxChildStart = isAtomicExprStart

  let isBlockExprStart = x =>
    switch x {
    | Token.At
    | Hash
    | Percent
    | Minus
    | MinusDot
    | Plus
    | PlusDot
    | Bang
    | True
    | False
    | Int(_)
    | String(_)
    | Character(_)
    | Lident(_)
    | Uident(_)
    | Lparen
    | List
    | Lbracket
    | Lbrace
    | Forwardslash
    | Assert
    | Lazy
    | If
    | For
    | While
    | Switch
    | Open
    | Module
    | Exception
    | Let
    | LessThan
    | Backtick
    | Try
    | Underscore => true
    | _ => false
    }

  let isListElement = (grammar, token) =>
    switch grammar {
    | ExprList => token == Token.DotDotDot || isExprStart(token)
    | ListExpr => token == DotDotDot || isExprStart(token)
    | PatternList => token == DotDotDot || isPatternStart(token)
    | ParameterList => isParameterStart(token)
    | StringFieldDeclarations => isStringFieldDeclStart(token)
    | FieldDeclarations => isFieldDeclStart(token)
    | RecordDecl => isRecordDeclStart(token)
    | TypExprList => isTypExprStart(token) || token == Token.LessThan
    | TypeParams => isTypeParamStart(token)
    | FunctorArgs => isFunctorArgStart(token)
    | ModExprList => isModExprStart(token)
    | TypeParameters => isTypeParameterStart(token)
    | RecordRows => isRecordRowStart(token)
    | RecordRowsStringKey => isRecordRowStringKeyStart(token)
    | ArgumentList => isArgumentStart(token)
    | Signature | Specification => isSignatureItemStart(token)
    | Structure | Implementation => isStructureItemStart(token)
    | PatternMatching => isPatternMatchStart(token)
    | PatternOcamlList => isPatternOcamlListStart(token)
    | PatternRecord => isPatternRecordItemStart(token)
    | Attribute => isAttributeStart(token)
    | TypeConstraint => token == Constraint
    | PackageConstraint => token == And
    | ConstructorDeclaration => token == Bar
    | Primitive =>
      switch token {
      | Token.String(_) => true
      | _ => false
      }
    | JsxAttribute => isJsxAttributeStart(token)
    | JsFfiImport => isJsFfiImportStart(token)
    | _ => false
    }

  let isListTerminator = (grammar, token) =>
    switch (grammar, token) {
    | (_, Token.Eof)
    | (ExprList, Rparen | Forwardslash | Rbracket)
    | (ListExpr, Rparen)
    | (ArgumentList, Rparen)
    | (TypExprList, Rparen | Forwardslash | GreaterThan | Equal)
    | (ModExprList, Rparen)
    | (
      PatternList | PatternOcamlList | PatternRecord,
      Forwardslash | Rbracket | Rparen | EqualGreater | In | Equal /* let {x} = foo */,
    )
    | (ExprBlock, Rbrace)
    | (Structure | Signature, Rbrace)
    | (TypeParams, Rparen)
    | (ParameterList, EqualGreater | Lbrace)
    | (JsxAttribute, Forwardslash | GreaterThan)
    | (JsFfiImport, Rbrace)
    | (StringFieldDeclarations, Rbrace) => true

    | (Attribute, token) when token != At => true
    | (TypeConstraint, token) when token != Constraint => true
    | (PackageConstraint, token) when token != And => true
    | (ConstructorDeclaration, token) when token != Bar => true
    | (Primitive, Semicolon) => true
    | (Primitive, token) when isStructureItemStart(token) => true

    | _ => false
    }

  let isPartOfList = (grammar, token) =>
    isListElement(grammar, token) || isListTerminator(grammar, token)
}

module Reporting = {
  module TerminalDoc = {
    type break =
      | Never
      | Always

    type rec document =
      | Nil
      | Group({break: break, doc: document})
      | Text(string)
      | Indent({amount: int, doc: document})
      | Append({doc1: document, doc2: document})

    let group = (~break, doc) => Group({break: break, doc: doc})
    let text = txt => Text(txt)
    let indent = (i, d) => Indent({amount: i, doc: d})
    let append = (d1, d2) => Append({doc1: d1, doc2: d2})
    let nil = Nil

    type rec stack =
      | Empty
      | Cons({doc: document, stack: stack})

    let push = (stack, doc) => Cons({doc: doc, stack: stack})

    type mode =
      | Flat
      | Break

    let toString /* ~width */ = (doc: document) => {
      let buffer = Buffer.create(100)
      let rec loop = (stack, mode, offset) =>
        switch stack {
        | Empty => ()
        | Cons({doc, stack: rest}) =>
          switch doc {
          | Nil => loop(rest, mode, offset)
          | Text(txt) =>
            Buffer.add_string(buffer, txt)
            loop(rest, mode, offset + String.length(txt))
          | Indent({amount: i, doc}) =>
            let indentation = (@doesNotRaise String.make)(i, ' ')
            Buffer.add_string(buffer, indentation)
            loop(push(rest, doc), mode, offset + i)
          | Append({doc1, doc2}) =>
            let rest = push(rest, doc2)
            let rest = push(rest, mode == Flat ? Nil : text("\n"))

            let rest = push(rest, doc1)
            loop(rest, mode, offset)
          | Group({break, doc}) =>
            let rest = push(rest, doc)
            switch break {
            | Always => loop(rest, Break, offset)
            | Never => loop(rest, Flat, offset)
            }
          }
        }

      loop(push(Empty, doc), Flat, 0)
      Buffer.contents(buffer)
    }
  }

  type color =
    | @live NoColor
    | @live Red

  type style = {
    @live underline: bool,
    @live color: color,
  }

  let highlight = (~from, ~len, txt) =>
    if from < 0 || (String.length(txt) === 0 || from >= String.length(txt)) {
      txt
    } else {
      let before = try String.sub(txt, 0, from) catch {
      | Invalid_argument(_) => ""
      }
      let content =
        "\027[31m" ++
        (try String.sub(txt, from, len) catch {
        | Invalid_argument(_) => ""
        } ++
        "\027[0m")

      let after = try String.sub(txt, from + len, String.length(txt) - (from + len)) catch {
      | Invalid_argument(_) => ""
      }
      before ++ (content ++ after)
    }

  let underline = (~from, ~len, txt) => {
    open TerminalDoc
    let indent = (@doesNotRaise String.make)(from, ' ')
    let underline = (@doesNotRaise String.make)(len, '^')
    let line = highlight(~from=0, ~len, underline)
    group(~break=Always, append(text(txt), text(indent ++ line)))
  }

  let rec drop = (n, l) =>
    if n === 1 {
      l
    } else {
      drop(
        n - 1,
        switch l {
        | list{_x, ...xs} => xs
        | _ => l
        },
      )
    }

  let rec take = (n, l) =>
    switch l {
    | _ when n === 0 => list{}
    | list{} => list{}
    | list{x, ...xs} => list{x, ...take(n - 1, xs)}
    }

  /* TODO: cleanup */
  let renderCodeContext = (~missing, src: string, startPos, endPos) => {
    open Lexing
    let startCol = startPos.pos_cnum - startPos.pos_bol
    let endCol = endPos.pos_cnum - startPos.pos_cnum + startCol
    let startLine = max(1, startPos.pos_lnum - 2) /* 2 lines before */
    let lines = String.split_on_char('\n', src)
    let endLine = {
      let len = List.length(lines)
      min(len, startPos.pos_lnum + 3)
    } /* 2 lines after */

    let lines = lines |> drop(startLine) |> take(endLine - startLine) |> Array.of_list

    let renderLine = (x, ix) => {
      let x = if ix == startPos.pos_lnum {
        switch missing {
        | Some(_len) => x ++ @doesNotRaise String.make(10, ' ')
        | None => x
        }
      } else {
        x
      }

      open TerminalDoc
      let rowNr = {
        let txt = string_of_int(ix)
        let len = String.length(txt)
        if ix == startPos.pos_lnum {
          highlight(~from=0, ~len, txt)
        } else {
          txt
        }
      }

      let len = {
        let len = if endCol >= 0 {
          endCol - startCol
        } else {
          1
        }

        if startCol + len > String.length(x) {
          String.length(x) - startCol - 1
        } else {
          len
        }
      }

      let line = if ix == startPos.pos_lnum {
        switch missing {
        | Some(len) =>
          underline(
            ~from=startCol + String.length(String.length(string_of_int(ix)) |> string_of_int) + 5,
            ~len,
            x,
          )
        | None =>
          let len = if startCol + len > String.length(x) {
            String.length(x) - startCol
          } else {
            len
          }

          text(highlight(~from=startCol, ~len, x))
        }
      } else {
        text(x)
      }

      group(~break=Never, append(append(text(rowNr), text(" │")), indent(2, line)))
    }

    let reportDoc = ref(TerminalDoc.nil)

    let linesLen = Array.length(lines)
    for i in 0 to linesLen - 1 {
      let line = try lines[i] catch {
      | Invalid_argument(_) => ""
      }
      reportDoc := {
          open TerminalDoc
          let ix = startLine + i
          group(~break=Always, append(reportDoc.contents, renderLine(line, ix)))
        }
    }

    TerminalDoc.toString(reportDoc.contents)
  }

  type problem =
    | @live Unexpected(Token.t)
    | @live Expected({token: Token.t, pos: Lexing.position, context: option<Grammar.t>})
    | @live Message(string)
    | @live Uident
    | @live Lident
    | @live Unbalanced(Token.t)

  type parseError = (Lexing.position, problem)
}

module Diagnostics: {
  type t
  type category
  type report

  type reportStyle
  let parseReportStyle: string => reportStyle

  let unexpected: (Token.t, list<(Grammar.t, Lexing.position)>) => category
  let expected: (~grammar: Grammar.t=?, Lexing.position, Token.t) => category
  let uident: Token.t => category
  let lident: Token.t => category
  let unclosedString: category
  let unclosedTemplate: category
  let unclosedComment: category
  let unknownUchar: int => category
  let message: string => category

  let make: (~filename: string, ~startPos: Lexing.position, ~endPos: Lexing.position, category) => t

  let stringOfReport: (~style: reportStyle, list<t>, string) => string
} = {
  type category =
    | Unexpected({token: Token.t, context: list<(Grammar.t, Lexing.position)>})
    | Expected({
        context: option<Grammar.t>,
        pos: Lexing.position /* prev token end */,
        token: Token.t,
      })
    | Message(string)
    | Uident(Token.t)
    | Lident(Token.t)
    | UnclosedString
    | UnclosedTemplate
    | UnclosedComment
    | UnknownUchar(int)

  type t = {
    filename: string,
    startPos: Lexing.position,
    endPos: Lexing.position,
    category: category,
  }

  type report = list<t>

  /* TODO: add json here */
  type reportStyle =
    | Pretty
    | Plain

  let parseReportStyle = txt =>
    switch String.lowercase_ascii(txt) {
    | "plain" => Plain
    | _ => Pretty
    }

  let defaultUnexpected = token =>
    "I'm not sure what to parse here when looking at \"" ++ (Token.toString(token) ++ "\".")

  let explain = t =>
    switch t.category {
    | Uident(currentToken) =>
      switch currentToken {
      | Lident(lident) =>
        let guess = String.capitalize_ascii(lident)
        "Did you mean `" ++ (guess ++ ("` instead of `" ++ (lident ++ "`?")))
      | t when Token.isKeyword(t) =>
        let token = Token.toString(t)
        "`" ++ (token ++ "` is a reserved keyword.")
      | _ => "At this point, I'm looking for an uppercased identifier like `Belt` or `Array`"
      }
    | Lident(currentToken) =>
      switch currentToken {
      | Uident(uident) =>
        let guess = String.uncapitalize_ascii(uident)
        "Did you mean `" ++ (guess ++ ("` instead of `" ++ (uident ++ "`?")))
      | t when Token.isKeyword(t) =>
        let token = Token.toString(t)
        "`" ++
        (token ++
        ("` is a reserved keyword. Keywords need to be escaped: \\\"" ++ (token ++ "\"")))
      | Underscore => "`_` isn't a valid name."
      | _ => "I'm expecting an lowercased identifier like `name` or `age`"
      }
    | Message(txt) => txt
    | UnclosedString => "This string is missing a double quote at the end"
    | UnclosedTemplate => "Did you forget to close this template expression with a backtick?"
    | UnclosedComment => "This comment seems to be missing a closing `*/`"
    | UnknownUchar(uchar) =>
      switch uchar {
      | 94 /* ^ */ => "Hmm, not sure what I should do here with this character.\nIf you're trying to deref an expression, use `foo.contents` instead."
      | _ => "Hmm, I have no idea what this character means…"
      }
    | Expected({context, token: t}) =>
      let hint = switch context {
      | Some(grammar) => "It signals the start of " ++ Grammar.toString(grammar)
      | None => ""
      }

      "Did you forget a `" ++ (Token.toString(t) ++ ("` here? " ++ hint))
    | Unexpected({token: t, context: breadcrumbs}) =>
      let name = Token.toString(t)
      switch breadcrumbs {
      | list{(AtomicTypExpr, _), ...breadcrumbs} =>
        switch (breadcrumbs, t) {
        | (
            list{(StringFieldDeclarations | FieldDeclarations, _), ..._},
            String(_) | At | Rbrace | Comma | Eof,
          ) => "I'm missing a type here"
        | (_, t) when Grammar.isStructureItemStart(t) || t == Eof => "Missing a type here"
        | _ => defaultUnexpected(t)
        }
      | list{(ExprOperand, _), ...breadcrumbs} =>
        switch (breadcrumbs, t) {
        | (list{(ExprBlock, _), ..._}, Rbrace) => "It seems that this expression block is empty"
        | (list{(ExprBlock, _), ..._}, Bar) => /* Pattern matching */
          "Looks like there might be an expression missing here"
        | (
            list{(ExprSetField, _), ..._},
            _,
          ) => "It seems that this record field mutation misses an expression"
        | (
            list{(ExprArrayMutation, _), ..._},
            _,
          ) => "Seems that an expression is missing, with what do I mutate the array?"
        | (
            list{(ExprBinaryAfterOp(_) | ExprUnary, _), ..._},
            _,
          ) => "Did you forget to write an expression here?"
        | (list{(Grammar.LetBinding, _), ..._}, _) => "This let-binding misses an expression"
        | (list{_, ..._}, Rbracket | Rbrace) => "Missing expression"
        | _ => "I'm not sure what to parse here when looking at \"" ++ (name ++ "\".")
        }
      | list{(TypeParam, _), ..._} =>
        switch t {
        | Lident(ident) => "Did you mean '" ++ (ident ++ "? A Type parameter starts with a quote.")
        | _ => "I'm not sure what to parse here when looking at \"" ++ (name ++ "\".")
        }
      | _ =>
        /* TODO: match on circumstance to verify Lident needed ? */
        if Token.isKeyword(t) {
          "`" ++
          (name ++
          ("` is a reserved keyword. Keywords need to be escaped: \\\"" ++
          (Token.toString(t) ++
          "\"")))
        } else {
          "I'm not sure what to parse here when looking at \"" ++ (name ++ "\".")
        }
      }
    }

  let toPlainString = (t, buffer) => {
    Buffer.add_string(buffer, t.filename)
    Buffer.add_char(buffer, '(')
    Buffer.add_string(buffer, string_of_int(t.startPos.pos_cnum))
    Buffer.add_char(buffer, ',')
    Buffer.add_string(buffer, string_of_int(t.endPos.pos_cnum))
    Buffer.add_char(buffer, ')')
    Buffer.add_char(buffer, ':')
    Buffer.add_string(buffer, explain(t))
  }

  let toString = (t, src) => {
    open Lexing
    let startchar = t.startPos.pos_cnum - t.startPos.pos_bol
    let endchar = t.endPos.pos_cnum - t.startPos.pos_cnum + startchar
    let locationInfo = Printf.sprintf /* ReasonLanguageServer requires the following format */(
      "File \"%s\", line %d, characters %d-%d:",
      t.filename,
      t.startPos.pos_lnum,
      startchar,
      endchar,
    )

    let code = {
      let missing = switch t.category {
      | Expected({token: t}) => Some(String.length(Token.toString(t)))
      | _ => None
      }

      Reporting.renderCodeContext(~missing, src, t.startPos, t.endPos)
    }

    let explanation = explain(t)
    Printf.sprintf("%s\n\n%s\n\n%s\n\n", locationInfo, code, explanation)
  }

  let make = (~filename, ~startPos, ~endPos, category) => {
    filename: filename,
    startPos: startPos,
    endPos: endPos,
    category: category,
  }

  let stringOfReport = (~style, diagnostics, src) =>
    switch style {
    | Pretty =>
      List.fold_left(
        (report, diagnostic) => report ++ (toString(diagnostic, src) ++ "\n"),
        "\n",
        List.rev(diagnostics),
      )
    | Plain =>
      let buffer = Buffer.create(100)
      List.iter(diagnostic => {
        toPlainString(diagnostic, buffer)
        Buffer.add_char(buffer, '\n')
      }, diagnostics)
      Buffer.contents(buffer)
    }

  let unexpected = (token, context) => Unexpected({token: token, context: context})

  let expected = (~grammar=?, pos, token) => Expected({context: grammar, pos: pos, token: token})

  let uident = currentToken => Uident(currentToken)
  let lident = currentToken => Lident(currentToken)
  let unclosedString = UnclosedString
  let unclosedComment = UnclosedComment
  let unclosedTemplate = UnclosedTemplate
  let unknownUchar = code => UnknownUchar(code)
  let message = txt => Message(txt)
}

/* Collection of utilities to view the ast in a more a convenient form,
 * allowing for easier processing.
 * Example: given a ptyp_arrow type, what are its arguments and what is the
 * returnType? */
module ParsetreeViewer: {
  /* Restructures a nested tree of arrow types into its args & returnType
   * The parsetree contains: a => b => c => d, for printing purposes
   * we restructure the tree into (a, b, c) and its returnType d */
  let arrowType: Parsetree.core_type => (
    Parsetree.attributes,
    list<(Parsetree.attributes, Asttypes.arg_label, Parsetree.core_type)>,
    Parsetree.core_type,
  )

  let functorType: Parsetree.module_type => (
    list<(Parsetree.attributes, Asttypes.loc<string>, option<Parsetree.module_type>)>,
    Parsetree.module_type,
  )

  /* filters @bs out of the provided attributes */
  let processUncurriedAttribute: Parsetree.attributes => (bool, Parsetree.attributes)

  /* if ... else if ... else ... is represented as nested expressions: if ... else { if ... }
   * The purpose of this function is to flatten nested ifs into one sequence.
   * Basically compute: ([if, else if, else if, else if], else) */
  let collectIfExpressions: Parsetree.expression => (
    list<(Parsetree.expression, Parsetree.expression)>,
    option<Parsetree.expression>,
  )

  let collectListExpressions: Parsetree.expression => (
    list<Parsetree.expression>,
    option<Parsetree.expression>,
  )

  type funParamKind =
    | Parameter({
        attrs: Parsetree.attributes,
        lbl: Asttypes.arg_label,
        defaultExpr: option<Parsetree.expression>,
        pat: Parsetree.pattern,
      })
    | NewTypes({attrs: Parsetree.attributes, locs: list<Asttypes.loc<string>>})

  let funExpr: Parsetree.expression => (
    Parsetree.attributes,
    list<funParamKind>,
    Parsetree.expression,
  )

  /* example:
   *  `makeCoordinate({
   *    x: 1,
   *    y: 2,
   *  })`
   *  Notice howe `({` and `})` "hug" or stick to each other */
  let isHuggableExpression: Parsetree.expression => bool

  let isHuggablePattern: Parsetree.pattern => bool

  let isHuggableRhs: Parsetree.expression => bool

  let operatorPrecedence: string => int

  let isUnaryExpression: Parsetree.expression => bool
  let isBinaryOperator: string => bool
  let isBinaryExpression: Parsetree.expression => bool

  let flattenableOperators: (string, string) => bool

  let hasAttributes: Parsetree.attributes => bool

  let isArrayAccess: Parsetree.expression => bool
  let isTernaryExpr: Parsetree.expression => bool

  let collectTernaryParts: Parsetree.expression => (
    list<(Parsetree.expression, Parsetree.expression)>,
    Parsetree.expression,
  )

  let parametersShouldHug: list<funParamKind> => bool

  let filterTernaryAttributes: Parsetree.attributes => Parsetree.attributes

  let isJsxExpression: Parsetree.expression => bool
  let hasJsxAttribute: Parsetree.attributes => bool

  let shouldIndentBinaryExpr: Parsetree.expression => bool
  let shouldInlineRhsBinaryExpr: Parsetree.expression => bool
  let filterPrinteableAttributes: Parsetree.attributes => Parsetree.attributes
  let partitionPrinteableAttributes: Parsetree.attributes => (
    Parsetree.attributes,
    Parsetree.attributes,
  )

  let requiresSpecialCallbackPrintingLastArg: list<(
    Asttypes.arg_label,
    Parsetree.expression,
  )> => bool
  let requiresSpecialCallbackPrintingFirstArg: list<(
    Asttypes.arg_label,
    Parsetree.expression,
  )> => bool

  let modExprApply: Parsetree.module_expr => (list<Parsetree.module_expr>, Parsetree.module_expr)

  let modExprFunctor: Parsetree.module_expr => (
    list<(Parsetree.attributes, Asttypes.loc<string>, option<Parsetree.module_type>)>,
    Parsetree.module_expr,
  )

  let splitGenTypeAttr: Parsetree.attributes => (bool, Parsetree.attributes)

  let collectPatternsFromListConstruct: (
    list<Parsetree.pattern>,
    Parsetree.pattern,
  ) => (list<Parsetree.pattern>, Parsetree.pattern)

  let isBlockExpr: Parsetree.expression => bool

  let isTemplateLiteral: Parsetree.expression => bool

  let collectOrPatternChain: Parsetree.pattern => list<Parsetree.pattern>

  let processBracesAttr: Parsetree.expression => (option<Parsetree.attribute>, Parsetree.expression)

  let filterParsingAttrs: Parsetree.attributes => Parsetree.attributes

  let isBracedExpr: Parsetree.expression => bool

  let isPipeExpr: Parsetree.expression => bool

  let extractValueDescriptionFromModExpr: Parsetree.module_expr => list<Parsetree.value_description>

  type jsImportScope =
    | JsGlobalImport /* nothing */
    | JsModuleImport(string) /* from "path" */
    | JsScopedImport(list<string>) /* window.location */

  let classifyJsImport: Parsetree.value_description => jsImportScope

  /* (__x) => f(a, __x, c) -----> f(a, _, c) */
  let rewriteUnderscoreApply: Parsetree.expression => Parsetree.expression

  /* (__x) => f(a, __x, c) -----> f(a, _, c) */
  let isUnderscoreApplySugar: Parsetree.expression => bool
} = {
  open Parsetree

  let arrowType = ct => {
    let rec process = (attrsBefore, acc, typ) =>
      switch typ {
      | {ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2), ptyp_attributes: list{}} =>
        let arg = (list{}, lbl, typ1)
        process(attrsBefore, list{arg, ...acc}, typ2)
      | {
          ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2),
          ptyp_attributes: list{({txt: "bs"}, _)} as attrs,
        } =>
        let arg = (attrs, lbl, typ1)
        process(attrsBefore, list{arg, ...acc}, typ2)
      | {ptyp_desc: Ptyp_arrow(Nolabel, _typ1, _typ2), ptyp_attributes: _attrs} as returnType =>
        let args = List.rev(acc)
        (attrsBefore, args, returnType)
      | {
          ptyp_desc: Ptyp_arrow((Labelled(_) | Optional(_)) as lbl, typ1, typ2),
          ptyp_attributes: attrs,
        } =>
        let arg = (attrs, lbl, typ1)
        process(attrsBefore, list{arg, ...acc}, typ2)
      | typ => (attrsBefore, List.rev(acc), typ)
      }

    switch ct {
    | {ptyp_desc: Ptyp_arrow(Nolabel, _typ1, _typ2), ptyp_attributes: attrs} as typ =>
      process(attrs, list{}, {...typ, ptyp_attributes: list{}})
    | typ => process(list{}, list{}, typ)
    }
  }

  let functorType = modtype => {
    let rec process = (acc, modtype) =>
      switch modtype {
      | {pmty_desc: Pmty_functor(lbl, argType, returnType), pmty_attributes: attrs} =>
        let arg = (attrs, lbl, argType)
        process(list{arg, ...acc}, returnType)
      | modType => (List.rev(acc), modType)
      }

    process(list{}, modtype)
  }

  let processUncurriedAttribute = attrs => {
    let rec process = (uncurriedSpotted, acc, attrs) =>
      switch attrs {
      | list{} => (uncurriedSpotted, List.rev(acc))
      | list{({Location.txt: "bs"}, _), ...rest} => process(true, acc, rest)
      | list{attr, ...rest} => process(uncurriedSpotted, list{attr, ...acc}, rest)
      }

    process(false, list{}, attrs)
  }

  let collectIfExpressions = expr => {
    let rec collect = (acc, expr) =>
      switch expr.pexp_desc {
      | Pexp_ifthenelse(ifExpr, thenExpr, Some(elseExpr)) =>
        collect(list{(ifExpr, thenExpr), ...acc}, elseExpr)
      | Pexp_ifthenelse(ifExpr, thenExpr, None as elseExpr) =>
        let ifs = List.rev(list{(ifExpr, thenExpr), ...acc})
        (ifs, elseExpr)
      | _ => (List.rev(acc), Some(expr))
      }

    collect(list{}, expr)
  }

  let collectListExpressions = expr => {
    let rec collect = (acc, expr) =>
      switch expr.pexp_desc {
      | Pexp_construct({txt: Longident.Lident("[]")}, _) => (List.rev(acc), None)
      | Pexp_construct(
          {txt: Longident.Lident("::")},
          Some({pexp_desc: Pexp_tuple(list{hd, tail})}),
        ) =>
        collect(list{hd, ...acc}, tail)
      | _ => (List.rev(acc), Some(expr))
      }

    collect(list{}, expr)
  }

  /* (__x) => f(a, __x, c) -----> f(a, _, c) */
  let rewriteUnderscoreApply = expr =>
    switch expr.pexp_desc {
    | Pexp_fun(
        Nolabel,
        None,
        {ppat_desc: Ppat_var({txt: "__x"})},
        {pexp_desc: Pexp_apply(callExpr, args)} as e,
      ) =>
      let newArgs = List.map(arg =>
        switch arg {
        | (lbl, {pexp_desc: Pexp_ident({txt: Longident.Lident("__x")} as lid)} as argExpr) => (
            lbl,
            {...argExpr, pexp_desc: Pexp_ident({...lid, txt: Longident.Lident("_")})},
          )
        | arg => arg
        }
      , args)
      {...e, pexp_desc: Pexp_apply(callExpr, newArgs)}
    | _ => expr
    }

  type funParamKind =
    | Parameter({
        attrs: Parsetree.attributes,
        lbl: Asttypes.arg_label,
        defaultExpr: option<Parsetree.expression>,
        pat: Parsetree.pattern,
      })
    | NewTypes({attrs: Parsetree.attributes, locs: list<Asttypes.loc<string>>})

  let funExpr = expr => {
    /* Turns (type t, type u, type z) into "type t u z" */
    let rec collectNewTypes = (acc, returnExpr) =>
      switch returnExpr {
      | {pexp_desc: Pexp_newtype(stringLoc, returnExpr), pexp_attributes: list{}} =>
        collectNewTypes(list{stringLoc, ...acc}, returnExpr)
      | returnExpr => (List.rev(acc), returnExpr)
      }

    let rec collect = (attrsBefore, acc, expr) =>
      switch expr {
      | {
          pexp_desc:
            Pexp_fun(
              Nolabel,
              None,
              {ppat_desc: Ppat_var({txt: "__x"})},
              {pexp_desc: Pexp_apply(_)},
            ),
        } => (attrsBefore, List.rev(acc), rewriteUnderscoreApply(expr))
      | {pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr), pexp_attributes: list{}} =>
        let parameter = Parameter({
          attrs: list{},
          lbl: lbl,
          defaultExpr: defaultExpr,
          pat: pattern,
        })
        collect(attrsBefore, list{parameter, ...acc}, returnExpr)
      | {pexp_desc: Pexp_newtype(stringLoc, rest), pexp_attributes: attrs} =>
        let (stringLocs, returnExpr) = collectNewTypes(list{stringLoc}, rest)
        let param = NewTypes({attrs: attrs, locs: stringLocs})
        collect(attrsBefore, list{param, ...acc}, returnExpr)
      | {
          pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr),
          pexp_attributes: list{({txt: "bs"}, _)} as attrs,
        } =>
        let parameter = Parameter({
          attrs: attrs,
          lbl: lbl,
          defaultExpr: defaultExpr,
          pat: pattern,
        })
        collect(attrsBefore, list{parameter, ...acc}, returnExpr)
      | {
          pexp_desc: Pexp_fun((Labelled(_) | Optional(_)) as lbl, defaultExpr, pattern, returnExpr),
          pexp_attributes: attrs,
        } =>
        let parameter = Parameter({
          attrs: attrs,
          lbl: lbl,
          defaultExpr: defaultExpr,
          pat: pattern,
        })
        collect(attrsBefore, list{parameter, ...acc}, returnExpr)
      | expr => (attrsBefore, List.rev(acc), expr)
      }

    switch expr {
    | {
        pexp_desc: Pexp_fun(Nolabel, _defaultExpr, _pattern, _returnExpr),
        pexp_attributes: attrs,
      } as expr =>
      collect(attrs, list{}, {...expr, pexp_attributes: list{}})
    | expr => collect(list{}, list{}, expr)
    }
  }

  let processBracesAttr = expr =>
    switch expr.pexp_attributes {
    | list{({txt: "res.braces"}, _) as attr, ...attrs} => (
        Some(attr),
        {...expr, pexp_attributes: attrs},
      )
    | _ => (None, expr)
    }

  let filterParsingAttrs = attrs => List.filter(attr =>
      switch attr {
      | ({Location.txt: "res.ternary" | "res.braces" | "bs" | "res.namedArgLoc"}, _) => false
      | _ => true
      }
    , attrs)

  let isBlockExpr = expr =>
    switch expr.pexp_desc {
    | Pexp_letmodule(_)
    | Pexp_letexception(_)
    | Pexp_let(_)
    | Pexp_open(_)
    | Pexp_sequence(_) => true
    | _ => false
    }

  let isBracedExpr = expr =>
    switch processBracesAttr(expr) {
    | (Some(_), _) => true
    | _ => false
    }

  let isHuggableExpression = expr =>
    switch expr.pexp_desc {
    | Pexp_array(_)
    | Pexp_tuple(_)
    | Pexp_construct({txt: Longident.Lident("::" | "[]")}, _)
    | Pexp_extension({txt: "obj"}, _)
    | Pexp_record(_) => true
    | _ when isBlockExpr(expr) => true
    | _ when isBracedExpr(expr) => true
    | _ => false
    }

  let isHuggableRhs = expr =>
    switch expr.pexp_desc {
    | Pexp_array(_)
    | Pexp_tuple(_)
    | Pexp_construct({txt: Longident.Lident("::" | "[]")}, _)
    | Pexp_extension({txt: "obj"}, _)
    | Pexp_record(_) => true
    | _ when isBracedExpr(expr) => true
    | _ => false
    }

  let isHuggablePattern = pattern =>
    switch pattern.ppat_desc {
    | Ppat_array(_) | Ppat_tuple(_) | Ppat_record(_) | Ppat_construct(_) => true
    | _ => false
    }

  let operatorPrecedence = operator =>
    switch operator {
    | ":=" => 1
    | "||" => 2
    | "&&" => 3
    | "=" | "==" | "<" | ">" | "!=" | "<>" | "!==" | "<=" | ">=" | "|>" => 4
    | "+" | "+." | "-" | "-." | "^" => 5
    | "*" | "*." | "/" | "/." => 6
    | "**" => 7
    | "#" | "##" | "|." => 8
    | _ => 0
    }

  let isUnaryOperator = operator =>
    switch operator {
    | "~+" | "~+." | "~-" | "~-." | "not" => true
    | _ => false
    }

  let isUnaryExpression = expr =>
    switch expr.pexp_desc {
    | Pexp_apply({pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})}, list{(Nolabel, _arg)})
      when isUnaryOperator(operator) => true
    | _ => false
    }

  let isBinaryOperator = operator =>
    switch operator {
    | ":="
    | "||"
    | "&&"
    | "="
    | "=="
    | "<"
    | ">"
    | "!="
    | "!=="
    | "<="
    | ">="
    | "|>"
    | "+"
    | "+."
    | "-"
    | "-."
    | "^"
    | "*"
    | "*."
    | "/"
    | "/."
    | "**"
    | "|."
    | "<>" => true
    | _ => false
    }

  let isBinaryExpression = expr =>
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list{(Nolabel, _operand1), (Nolabel, _operand2)},
      ) when isBinaryOperator(operator) => true
    | _ => false
    }

  let isEqualityOperator = operator =>
    switch operator {
    | "=" | "==" | "<>" | "!=" => true
    | _ => false
    }

  let flattenableOperators = (parentOperator, childOperator) => {
    let precParent = operatorPrecedence(parentOperator)
    let precChild = operatorPrecedence(childOperator)
    if precParent === precChild {
      !(isEqualityOperator(parentOperator) && isEqualityOperator(childOperator))
    } else {
      false
    }
  }

  let hasAttributes = attrs => List.exists(attr =>
      switch attr {
      | ({Location.txt: "bs" | "res.ternary" | "res.braces"}, _) => false
      | _ => true
      }
    , attrs)

  let isArrayAccess = expr =>
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Ldot(Lident("Array"), "get")})},
        list{(Nolabel, _parentExpr), (Nolabel, _memberExpr)},
      ) => true
    | _ => false
    }

  let rec hasTernaryAttribute = attrs =>
    switch attrs {
    | list{} => false
    | list{({Location.txt: "res.ternary"}, _), ..._} => true
    | list{_, ...attrs} => hasTernaryAttribute(attrs)
    }

  let isTernaryExpr = expr =>
    switch expr {
    | {pexp_attributes: attrs, pexp_desc: Pexp_ifthenelse(_)}
      when hasTernaryAttribute(attrs) => true
    | _ => false
    }

  let collectTernaryParts = expr => {
    let rec collect = (acc, expr) =>
      switch expr {
      | {pexp_attributes: attrs, pexp_desc: Pexp_ifthenelse(condition, consequent, Some(alternate))}
        when hasTernaryAttribute(attrs) =>
        collect(list{(condition, consequent), ...acc}, alternate)
      | alternate => (List.rev(acc), alternate)
      }

    collect(list{}, expr)
  }

  let parametersShouldHug = parameters =>
    switch parameters {
    | list{Parameter({attrs: list{}, lbl: Asttypes.Nolabel, defaultExpr: None, pat})}
      when isHuggablePattern(pat) => true
    | _ => false
    }

  let filterTernaryAttributes = attrs => List.filter(attr =>
      switch attr {
      | ({Location.txt: "res.ternary"}, _) => false
      | _ => true
      }
    , attrs)

  let isJsxExpression = expr => {
    let rec loop = attrs =>
      switch attrs {
      | list{} => false
      | list{({Location.txt: "JSX"}, _), ..._} => true
      | list{_, ...attrs} => loop(attrs)
      }

    switch expr.pexp_desc {
    | Pexp_apply(_) => loop(expr.Parsetree.pexp_attributes)
    | _ => false
    }
  }

  let hasJsxAttribute = attributes =>
    switch attributes {
    | list{({Location.txt: "JSX"}, _), ..._} => true
    | _ => false
    }

  let shouldIndentBinaryExpr = expr => {
    let samePrecedenceSubExpression = (operator, subExpression) =>
      switch subExpression {
      | {
          pexp_desc:
            Pexp_apply(
              {pexp_desc: Pexp_ident({txt: Longident.Lident(subOperator)})},
              list{(Nolabel, _lhs), (Nolabel, _rhs)},
            ),
        } when isBinaryOperator(subOperator) =>
        flattenableOperators(operator, subOperator)
      | _ => true
      }

    switch expr {
    | {
        pexp_desc:
          Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
            list{(Nolabel, lhs), (Nolabel, _rhs)},
          ),
      } when isBinaryOperator(operator) =>
      isEqualityOperator(operator) ||
      (!samePrecedenceSubExpression(operator, lhs) ||
      operator == ":=")
    | _ => false
    }
  }

  let shouldInlineRhsBinaryExpr = rhs =>
    switch rhs.pexp_desc {
    | Parsetree.Pexp_constant(_)
    | Pexp_let(_)
    | Pexp_letmodule(_)
    | Pexp_letexception(_)
    | Pexp_sequence(_)
    | Pexp_open(_)
    | Pexp_ifthenelse(_)
    | Pexp_for(_)
    | Pexp_while(_)
    | Pexp_try(_)
    | Pexp_array(_)
    | Pexp_record(_) => true
    | _ => false
    }

  let filterPrinteableAttributes = attrs => List.filter(attr =>
      switch attr {
      | ({Location.txt: "bs" | "res.ternary"}, _) => false
      | _ => true
      }
    , attrs)

  let partitionPrinteableAttributes = attrs => List.partition(attr =>
      switch attr {
      | ({Location.txt: "bs" | "res.ternary"}, _) => false
      | _ => true
      }
    , attrs)

  let requiresSpecialCallbackPrintingLastArg = args => {
    let rec loop = args =>
      switch args {
      | list{} => false
      | list{(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)})} => true
      | list{(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)}), ..._} => false
      | list{_, ...rest} => loop(rest)
      }

    loop(args)
  }

  let requiresSpecialCallbackPrintingFirstArg = args => {
    let rec loop = args =>
      switch args {
      | list{} => true
      | list{(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)}), ..._} => false
      | list{_, ...rest} => loop(rest)
      }

    switch args {
    | list{(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)})} => false
    | list{(_, {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)}), ...rest} => loop(rest)
    | _ => false
    }
  }

  let modExprApply = modExpr => {
    let rec loop = (acc, modExpr) =>
      switch modExpr {
      | {pmod_desc: Pmod_apply(next, arg)} => loop(list{arg, ...acc}, next)
      | _ => (acc, modExpr)
      }

    loop(list{}, modExpr)
  }

  let modExprFunctor = modExpr => {
    let rec loop = (acc, modExpr) =>
      switch modExpr {
      | {pmod_desc: Pmod_functor(lbl, modType, returnModExpr), pmod_attributes: attrs} =>
        let param = (attrs, lbl, modType)
        loop(list{param, ...acc}, returnModExpr)
      | returnModExpr => (List.rev(acc), returnModExpr)
      }

    loop(list{}, modExpr)
  }

  let splitGenTypeAttr = attrs =>
    switch attrs {
    | list{({Location.txt: "genType"}, _), ...attrs} => (true, attrs)
    | attrs => (false, attrs)
    }

  let rec collectPatternsFromListConstruct = (acc, pattern) => {
    open Parsetree
    switch pattern.ppat_desc {
    | Ppat_construct(
        {txt: Longident.Lident("::")},
        Some({ppat_desc: Ppat_tuple(list{pat, rest})}),
      ) =>
      collectPatternsFromListConstruct(list{pat, ...acc}, rest)
    | _ => (List.rev(acc), pattern)
    }
  }

  let rec isTemplateLiteral = expr => {
    let isPexpConstantString = expr =>
      switch expr.pexp_desc {
      | Pexp_constant(Pconst_string(_, Some(_))) => true
      | _ => false
      }

    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident("^")})},
        list{(Nolabel, arg1), (Nolabel, arg2)},
      ) when !(isPexpConstantString(arg1) && isPexpConstantString(arg2)) =>
      isTemplateLiteral(arg1) || isTemplateLiteral(arg2)
    | Pexp_constant(Pconst_string(_, Some(_))) => true
    | _ => false
    }
  }

  /* Blue | Red | Green -> [Blue; Red; Green] */
  let collectOrPatternChain = pat => {
    let rec loop = (pattern, chain) =>
      switch pattern.ppat_desc {
      | Ppat_or(left, right) => loop(left, list{right, ...chain})
      | _ => list{pattern, ...chain}
      }

    loop(pat, list{})
  }

  let isPipeExpr = expr =>
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident("|." | "|>")})},
        list{(Nolabel, _operand1), (Nolabel, _operand2)},
      ) => true
    | _ => false
    }

  let extractValueDescriptionFromModExpr = modExpr => {
    let rec loop = (structure, acc) =>
      switch structure {
      | list{} => List.rev(acc)
      | list{structureItem, ...structure} =>
        switch structureItem.Parsetree.pstr_desc {
        | Pstr_primitive(vd) => loop(structure, list{vd, ...acc})
        | _ => loop(structure, acc)
        }
      }

    switch modExpr.pmod_desc {
    | Pmod_structure(structure) => loop(structure, list{})
    | _ => list{}
    }
  }

  type jsImportScope =
    | JsGlobalImport /* nothing */
    | JsModuleImport(string) /* from "path" */
    | JsScopedImport(list<string>) /* window.location */

  let classifyJsImport = valueDescription => {
    let rec loop = attrs => {
      open Parsetree
      switch attrs {
      | list{} => JsGlobalImport
      | list{
          (
            {Location.txt: "scope"},
            PStr(list{{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Pconst_string(s, _))}, _)}}),
          ),
          ..._,
        } =>
        JsScopedImport(list{s})
      | list{
          (
            {Location.txt: "genType.import"},
            PStr(list{{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Pconst_string(s, _))}, _)}}),
          ),
          ..._,
        } =>
        JsModuleImport(s)
      | list{
          (
            {Location.txt: "scope"},
            PStr(list{{pstr_desc: Pstr_eval({pexp_desc: Pexp_tuple(exprs)}, _)}}),
          ),
          ..._,
        } =>
        let scopes = List.fold_left((acc, curr) =>
          switch curr.Parsetree.pexp_desc {
          | Pexp_constant(Pconst_string(s, _)) => list{s, ...acc}
          | _ => acc
          }
        , list{}, exprs)

        JsScopedImport(List.rev(scopes))
      | list{_, ...attrs} => loop(attrs)
      }
    }

    loop(valueDescription.pval_attributes)
  }

  let isUnderscoreApplySugar = expr =>
    switch expr.pexp_desc {
    | Pexp_fun(
        Nolabel,
        None,
        {ppat_desc: Ppat_var({txt: "__x"})},
        {pexp_desc: Pexp_apply(_)},
      ) => true
    | _ => false
    }
}

module Parens: {
  type kind = Parenthesized | Braced(Location.t) | Nothing

  let expr: Parsetree.expression => kind
  let structureExpr: Parsetree.expression => kind

  let unaryExprOperand: Parsetree.expression => kind

  let binaryExprOperand: (~isLhs: bool, Parsetree.expression) => kind
  let subBinaryExprOperand: (string, string) => bool
  let rhsBinaryExprOperand: (string, Parsetree.expression) => bool
  let flattenOperandRhs: (string, Parsetree.expression) => bool

  let lazyOrAssertExprRhs: Parsetree.expression => kind

  let fieldExpr: Parsetree.expression => kind

  let setFieldExprRhs: Parsetree.expression => kind

  let ternaryOperand: Parsetree.expression => kind

  let jsxPropExpr: Parsetree.expression => kind
  let jsxChildExpr: Parsetree.expression => kind

  let binaryExpr: Parsetree.expression => kind
  let modTypeFunctorReturn: Parsetree.module_type => bool
  let modTypeWithOperand: Parsetree.module_type => bool
  let modExprFunctorConstraint: Parsetree.module_type => bool

  let bracedExpr: Parsetree.expression => bool
  let callExpr: Parsetree.expression => kind

  let includeModExpr: Parsetree.module_expr => bool
} = {
  type kind = Parenthesized | Braced(Location.t) | Nothing

  let expr = expr => {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | _ =>
      switch expr {
      | {
          Parsetree.pexp_desc:
            Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}),
        } =>
        Nothing
      | {pexp_desc: Pexp_constraint(_)} => Parenthesized
      | _ => Nothing
      }
    }
  }

  let callExpr = expr => {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | _ =>
      switch expr {
      | {Parsetree.pexp_attributes: attrs}
        when switch ParsetreeViewer.filterParsingAttrs(attrs) {
        | list{_, ..._} => true
        | list{} => false
        } =>
        Parenthesized
      | _
        when ParsetreeViewer.isUnaryExpression(expr) || ParsetreeViewer.isBinaryExpression(expr) =>
        Parenthesized
      | {
          Parsetree.pexp_desc:
            Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}),
        } =>
        Nothing
      | {pexp_desc: Pexp_fun(_)} when ParsetreeViewer.isUnderscoreApplySugar(expr) => Nothing
      | {
          pexp_desc:
            Pexp_lazy(_)
            | Pexp_assert(_)
            | Pexp_fun(_)
            | Pexp_newtype(_)
            | Pexp_function(_)
            | Pexp_constraint(_)
            | Pexp_setfield(_)
            | Pexp_match(_)
            | Pexp_try(_)
            | Pexp_while(_)
            | Pexp_for(_)
            | Pexp_ifthenelse(_),
        } =>
        Parenthesized
      | _ => Nothing
      }
    }
  }

  let structureExpr = expr => {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | None =>
      switch expr {
      | _
        when ParsetreeViewer.hasAttributes(expr.pexp_attributes) &&
        !ParsetreeViewer.isJsxExpression(expr) =>
        Parenthesized
      | {
          Parsetree.pexp_desc:
            Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}),
        } =>
        Nothing
      | {pexp_desc: Pexp_constraint(_)} => Parenthesized
      | _ => Nothing
      }
    }
  }

  let unaryExprOperand = expr => {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | None =>
      switch expr {
      | {Parsetree.pexp_attributes: attrs}
        when switch ParsetreeViewer.filterParsingAttrs(attrs) {
        | list{_, ..._} => true
        | list{} => false
        } =>
        Parenthesized
      | expr
        when ParsetreeViewer.isUnaryExpression(expr) || ParsetreeViewer.isBinaryExpression(expr) =>
        Parenthesized
      | {pexp_desc: Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)})} =>
        Nothing
      | {pexp_desc: Pexp_fun(_)} when ParsetreeViewer.isUnderscoreApplySugar(expr) => Nothing
      | {
          pexp_desc:
            Pexp_lazy(_)
            | Pexp_assert(_)
            | Pexp_fun(_)
            | Pexp_newtype(_)
            | Pexp_function(_)
            | Pexp_constraint(_)
            | Pexp_setfield(_)
            | Pexp_extension(_)
            | Pexp_match(_)
            | Pexp_try(_)
            | Pexp_while(_)
            | Pexp_for(_)
            | Pexp_ifthenelse(_),
        } =>
        Parenthesized
      | _ => Nothing
      }
    }
  }

  let binaryExprOperand = (~isLhs, expr) => {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | None =>
      switch expr {
      | {
          Parsetree.pexp_desc:
            Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}),
        } =>
        Nothing
      | {pexp_desc: Pexp_fun(_)} when ParsetreeViewer.isUnderscoreApplySugar(expr) => Nothing
      | {pexp_desc: Pexp_constraint(_) | Pexp_fun(_) | Pexp_function(_) | Pexp_newtype(_)} =>
        Parenthesized
      | expr when ParsetreeViewer.isBinaryExpression(expr) => Parenthesized
      | expr when ParsetreeViewer.isTernaryExpr(expr) => Parenthesized
      | {pexp_desc: Pexp_lazy(_) | Pexp_assert(_)} when isLhs => Parenthesized
      | _ => Nothing
      }
    }
  }

  let subBinaryExprOperand = (parentOperator, childOperator) => {
    let precParent = ParsetreeViewer.operatorPrecedence(parentOperator)
    let precChild = ParsetreeViewer.operatorPrecedence(childOperator)
    precParent > precChild ||
      ((precParent === precChild &&
        !ParsetreeViewer.flattenableOperators(parentOperator, childOperator)) ||
      /* a && b || c, add parens to (a && b) for readability, who knows the difference by heart… */
      parentOperator == "||" && childOperator == "&&")
  }

  let rhsBinaryExprOperand = (parentOperator, rhs) =>
    switch rhs.Parsetree.pexp_desc {
    | Parsetree.Pexp_apply(
        {pexp_attributes: list{}, pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list{(_, _left), (_, _right)},
      ) when ParsetreeViewer.isBinaryOperator(operator) =>
      let precParent = ParsetreeViewer.operatorPrecedence(parentOperator)
      let precChild = ParsetreeViewer.operatorPrecedence(operator)
      precParent === precChild
    | _ => false
    }

  let flattenOperandRhs = (parentOperator, rhs) =>
    switch rhs.Parsetree.pexp_desc {
    | Parsetree.Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list{(_, _left), (_, _right)},
      ) when ParsetreeViewer.isBinaryOperator(operator) =>
      let precParent = ParsetreeViewer.operatorPrecedence(parentOperator)
      let precChild = ParsetreeViewer.operatorPrecedence(operator)
      precParent >= precChild || rhs.pexp_attributes != list{}
    | Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}) => false
    | Pexp_fun(_) when ParsetreeViewer.isUnderscoreApplySugar(rhs) => false
    | Pexp_fun(_) | Pexp_newtype(_) | Pexp_setfield(_) | Pexp_constraint(_) => true
    | _ when ParsetreeViewer.isTernaryExpr(rhs) => true
    | _ => false
    }

  let lazyOrAssertExprRhs = expr => {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | None =>
      switch expr {
      | {Parsetree.pexp_attributes: attrs}
        when switch ParsetreeViewer.filterParsingAttrs(attrs) {
        | list{_, ..._} => true
        | list{} => false
        } =>
        Parenthesized
      | expr when ParsetreeViewer.isBinaryExpression(expr) => Parenthesized
      | {pexp_desc: Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)})} =>
        Nothing
      | {pexp_desc: Pexp_fun(_)} when ParsetreeViewer.isUnderscoreApplySugar(expr) => Nothing
      | {
          pexp_desc:
            Pexp_lazy(_)
            | Pexp_assert(_)
            | Pexp_fun(_)
            | Pexp_newtype(_)
            | Pexp_function(_)
            | Pexp_constraint(_)
            | Pexp_setfield(_)
            | Pexp_match(_)
            | Pexp_try(_)
            | Pexp_while(_)
            | Pexp_for(_)
            | Pexp_ifthenelse(_),
        } =>
        Parenthesized
      | _ => Nothing
      }
    }
  }

  let isNegativeConstant = constant => {
    let isNeg = txt => {
      let len = String.length(txt)
      len > 0 && (@doesNotRaise String.get)(txt, 0) == '-'
    }

    switch constant {
    | Parsetree.Pconst_integer(i, _) | Pconst_float(i, _) when isNeg(i) => true
    | _ => false
    }
  }

  let fieldExpr = expr => {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | None =>
      switch expr {
      | {Parsetree.pexp_attributes: attrs}
        when switch ParsetreeViewer.filterParsingAttrs(attrs) {
        | list{_, ..._} => true
        | list{} => false
        } =>
        Parenthesized
      | expr
        when ParsetreeViewer.isBinaryExpression(expr) || ParsetreeViewer.isUnaryExpression(expr) =>
        Parenthesized
      | {pexp_desc: Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)})} =>
        Nothing
      | {pexp_desc: Pexp_constant(c)} when isNegativeConstant(c) => Parenthesized
      | {pexp_desc: Pexp_fun(_)} when ParsetreeViewer.isUnderscoreApplySugar(expr) => Nothing
      | {
          pexp_desc:
            Pexp_lazy(_)
            | Pexp_assert(_)
            | Pexp_extension(_)
            | Pexp_fun(_)
            | Pexp_newtype(_)
            | Pexp_function(_)
            | Pexp_constraint(_)
            | Pexp_setfield(_)
            | Pexp_match(_)
            | Pexp_try(_)
            | Pexp_while(_)
            | Pexp_for(_)
            | Pexp_ifthenelse(_),
        } =>
        Parenthesized
      | _ => Nothing
      }
    }
  }

  let setFieldExprRhs = expr => {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | None =>
      switch expr {
      | {
          Parsetree.pexp_desc:
            Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}),
        } =>
        Nothing
      | {pexp_desc: Pexp_constraint(_)} => Parenthesized
      | _ => Nothing
      }
    }
  }

  let ternaryOperand = expr => {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | None =>
      switch expr {
      | {
          Parsetree.pexp_desc:
            Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}),
        } =>
        Nothing
      | {pexp_desc: Pexp_constraint(_)} => Parenthesized
      | {pexp_desc: Pexp_fun(_) | Pexp_newtype(_)} =>
        let (_attrsOnArrow, _parameters, returnExpr) = ParsetreeViewer.funExpr(expr)
        switch returnExpr.pexp_desc {
        | Pexp_constraint(_) => Parenthesized
        | _ => Nothing
        }
      | _ => Nothing
      }
    }
  }

  let startsWithMinus = txt => {
    let len = String.length(txt)
    if len === 0 {
      false
    } else {
      let s = (@doesNotRaise String.get)(txt, 0)
      s == '-'
    }
  }

  let jsxPropExpr = expr =>
    switch expr.Parsetree.pexp_desc {
    | Parsetree.Pexp_let(_)
    | Pexp_sequence(_)
    | Pexp_letexception(_)
    | Pexp_letmodule(_)
    | Pexp_open(_) =>
      Nothing
    | _ =>
      let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
      switch optBraces {
      | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
      | None =>
        switch expr {
        | {
            Parsetree.pexp_desc: Pexp_constant(Pconst_integer(x, _) | Pconst_float(x, _)),
            pexp_attributes: list{},
          } when startsWithMinus(x) =>
          Parenthesized
        | {
            Parsetree.pexp_desc:
              Pexp_ident(_)
              | Pexp_constant(_)
              | Pexp_field(_)
              | Pexp_construct(_)
              | Pexp_variant(_)
              | Pexp_array(_)
              | Pexp_pack(_)
              | Pexp_record(_)
              | Pexp_extension(_)
              | Pexp_letmodule(_)
              | Pexp_letexception(_)
              | Pexp_open(_)
              | Pexp_sequence(_)
              | Pexp_let(_)
              | Pexp_tuple(_),
            pexp_attributes: list{},
          } =>
          Nothing
        | {
            Parsetree.pexp_desc:
              Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}),
            pexp_attributes: list{},
          } =>
          Nothing
        | _ => Parenthesized
        }
      }
    }

  let jsxChildExpr = expr =>
    switch expr.Parsetree.pexp_desc {
    | Parsetree.Pexp_let(_)
    | Pexp_sequence(_)
    | Pexp_letexception(_)
    | Pexp_letmodule(_)
    | Pexp_open(_) =>
      Nothing
    | _ =>
      let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
      switch optBraces {
      | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
      | _ =>
        switch expr {
        | {
            Parsetree.pexp_desc: Pexp_constant(Pconst_integer(x, _) | Pconst_float(x, _)),
            pexp_attributes: list{},
          } when startsWithMinus(x) =>
          Parenthesized
        | {
            Parsetree.pexp_desc:
              Pexp_ident(_)
              | Pexp_constant(_)
              | Pexp_field(_)
              | Pexp_construct(_)
              | Pexp_variant(_)
              | Pexp_array(_)
              | Pexp_pack(_)
              | Pexp_record(_)
              | Pexp_extension(_)
              | Pexp_letmodule(_)
              | Pexp_letexception(_)
              | Pexp_open(_)
              | Pexp_sequence(_)
              | Pexp_let(_),
            pexp_attributes: list{},
          } =>
          Nothing
        | {
            Parsetree.pexp_desc:
              Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}),
            pexp_attributes: list{},
          } =>
          Nothing
        | expr when ParsetreeViewer.isJsxExpression(expr) => Nothing
        | _ => Parenthesized
        }
      }
    }

  let binaryExpr = expr => {
    let (optBraces, _) = ParsetreeViewer.processBracesAttr(expr)
    switch optBraces {
    | Some({Location.loc: bracesLoc}, _) => Braced(bracesLoc)
    | None =>
      switch expr {
      | {Parsetree.pexp_attributes: list{_, ..._}} as expr
        when ParsetreeViewer.isBinaryExpression(expr) =>
        Parenthesized
      | _ => Nothing
      }
    }
  }

  let modTypeFunctorReturn = modType =>
    switch modType {
    | {Parsetree.pmty_desc: Pmty_with(_)} => true
    | _ => false
    }

  /* Add parens for readability:
       module type Functor = SetLike => Set with type t = A.t
     This is actually:
       module type Functor = (SetLike => Set) with type t = A.t
 */
  let modTypeWithOperand = modType =>
    switch modType {
    | {Parsetree.pmty_desc: Pmty_functor(_)} => true
    | _ => false
    }

  let modExprFunctorConstraint = modType =>
    switch modType {
    | {Parsetree.pmty_desc: Pmty_functor(_) | Pmty_with(_)} => true
    | _ => false
    }

  let bracedExpr = expr =>
    switch expr.Parsetree.pexp_desc {
    | Pexp_constraint({pexp_desc: Pexp_pack(_)}, {ptyp_desc: Ptyp_package(_)}) => false
    | Pexp_constraint(_) => true
    | _ => false
    }

  let includeModExpr = modExpr =>
    switch modExpr.Parsetree.pmod_desc {
    | Parsetree.Pmod_constraint(_) => true
    | _ => false
    }
}

module CommentTable = {
  type t = {
    leading: Hashtbl.t<Location.t, list<Comment.t>>,
    inside: Hashtbl.t<Location.t, list<Comment.t>>,
    trailing: Hashtbl.t<Location.t, list<Comment.t>>,
  }

  let make = () => {
    leading: Hashtbl.create(100),
    inside: Hashtbl.create(100),
    trailing: Hashtbl.create(100),
  }

  let empty = make()

  @live
  let log = t => {
    open Location
    let leadingStuff = Hashtbl.fold((k: Location.t, v: list<Comment.t>, acc) => {
      let loc = Doc.concat(list{
        Doc.lbracket,
        Doc.text(string_of_int(k.loc_start.pos_lnum)),
        Doc.text(":"),
        Doc.text(string_of_int(k.loc_start.pos_cnum - k.loc_start.pos_bol)),
        Doc.text("-"),
        Doc.text(string_of_int(k.loc_end.pos_lnum)),
        Doc.text(":"),
        Doc.text(string_of_int(k.loc_end.pos_cnum - k.loc_end.pos_bol)),
        Doc.rbracket,
      })
      let doc = Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list{
          loc,
          Doc.indent(
            Doc.concat(list{
              Doc.line,
              Doc.join(~sep=Doc.comma, List.map(c => Doc.text(Comment.txt(c)), v)),
            }),
          ),
          Doc.line,
        }),
      )
      list{doc, ...acc}
    }, t.leading, list{})

    let trailingStuff = Hashtbl.fold((k: Location.t, v: list<Comment.t>, acc) => {
      let loc = Doc.concat(list{
        Doc.lbracket,
        Doc.text(string_of_int(k.loc_start.pos_lnum)),
        Doc.text(":"),
        Doc.text(string_of_int(k.loc_start.pos_cnum - k.loc_start.pos_bol)),
        Doc.text("-"),
        Doc.text(string_of_int(k.loc_end.pos_lnum)),
        Doc.text(":"),
        Doc.text(string_of_int(k.loc_end.pos_cnum - k.loc_end.pos_bol)),
        Doc.rbracket,
      })
      let doc = Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list{
          loc,
          Doc.indent(
            Doc.concat(list{
              Doc.line,
              Doc.join(
                ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                List.map(c => Doc.text(Comment.txt(c)), v),
              ),
            }),
          ),
          Doc.line,
        }),
      )
      list{doc, ...acc}
    }, t.trailing, list{})

    Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat(list{
        Doc.text("leading comments:"),
        Doc.line,
        Doc.indent(Doc.concat(leadingStuff)),
        Doc.line,
        Doc.line,
        Doc.text("trailing comments:"),
        Doc.indent(Doc.concat(trailingStuff)),
        Doc.line,
        Doc.line,
      }),
    )
    |> Doc.toString(~width=80)
    |> print_endline
  }
  let attach = (tbl, loc, comments) =>
    switch comments {
    | list{} => ()
    | comments => Hashtbl.replace(tbl, loc, comments)
    }

  let partitionByLoc = (comments, loc) => {
    let rec loop = ((leading, inside, trailing), comments) => {
      open Location
      switch comments {
      | list{comment, ...rest} =>
        let cmtLoc = Comment.loc(comment)
        if cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum {
          loop((list{comment, ...leading}, inside, trailing), rest)
        } else if cmtLoc.loc_start.pos_cnum >= loc.loc_end.pos_cnum {
          loop((leading, inside, list{comment, ...trailing}), rest)
        } else {
          loop((leading, list{comment, ...inside}, trailing), rest)
        }
      | list{} => (List.rev(leading), List.rev(inside), List.rev(trailing))
      }
    }

    loop((list{}, list{}, list{}), comments)
  }

  let partitionLeadingTrailing = (comments, loc) => {
    let rec loop = ((leading, trailing), comments) => {
      open Location
      switch comments {
      | list{comment, ...rest} =>
        let cmtLoc = Comment.loc(comment)
        if cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum {
          loop((list{comment, ...leading}, trailing), rest)
        } else {
          loop((leading, list{comment, ...trailing}), rest)
        }
      | list{} => (List.rev(leading), List.rev(trailing))
      }
    }

    loop((list{}, list{}), comments)
  }

  let partitionByOnSameLine = (loc, comments) => {
    let rec loop = ((onSameLine, onOtherLine), comments) => {
      open Location
      switch comments {
      | list{} => (List.rev(onSameLine), List.rev(onOtherLine))
      | list{comment, ...rest} =>
        let cmtLoc = Comment.loc(comment)
        if cmtLoc.loc_start.pos_lnum === loc.loc_end.pos_lnum {
          loop((list{comment, ...onSameLine}, onOtherLine), rest)
        } else {
          loop((onSameLine, list{comment, ...onOtherLine}), rest)
        }
      }
    }

    loop((list{}, list{}), comments)
  }

  let partitionAdjacentTrailing = (loc1, comments) => {
    open Location
    open Lexing
    let rec loop = (~prevEndPos, afterLoc1, comments) =>
      switch comments {
      | list{} => (List.rev(afterLoc1), list{})
      | list{comment, ...rest} as comments =>
        let cmtPrevEndPos = Comment.prevTokEndPos(comment)
        if prevEndPos.Lexing.pos_cnum === cmtPrevEndPos.pos_cnum {
          let commentEnd = Comment.loc(comment).loc_end
          loop(~prevEndPos=commentEnd, list{comment, ...afterLoc1}, rest)
        } else {
          (List.rev(afterLoc1), comments)
        }
      }

    loop(~prevEndPos=loc1.loc_end, list{}, comments)
  }

  let rec collectListPatterns = (acc, pattern) => {
    open Parsetree
    switch pattern.ppat_desc {
    | Ppat_construct(
        {txt: Longident.Lident("::")},
        Some({ppat_desc: Ppat_tuple(list{pat, rest})}),
      ) =>
      collectListPatterns(list{pat, ...acc}, rest)
    | Ppat_construct({txt: Longident.Lident("[]")}, None) => List.rev(acc)
    | _ => List.rev(list{pattern, ...acc})
    }
  }

  let rec collectListExprs = (acc, expr) => {
    open Parsetree
    switch expr.pexp_desc {
    | Pexp_construct(
        {txt: Longident.Lident("::")},
        Some({pexp_desc: Pexp_tuple(list{expr, rest})}),
      ) =>
      collectListExprs(list{expr, ...acc}, rest)
    | Pexp_construct({txt: Longident.Lident("[]")}, _) => List.rev(acc)
    | _ => List.rev(list{expr, ...acc})
    }
  }

  /* TODO: use ParsetreeViewer */
  let arrowType = ct => {
    open Parsetree
    let rec process = (attrsBefore, acc, typ) =>
      switch typ {
      | {ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2), ptyp_attributes: list{}} =>
        let arg = (list{}, lbl, typ1)
        process(attrsBefore, list{arg, ...acc}, typ2)
      | {
          ptyp_desc: Ptyp_arrow(Nolabel as lbl, typ1, typ2),
          ptyp_attributes: list{({txt: "bs"}, _)} as attrs,
        } =>
        let arg = (attrs, lbl, typ1)
        process(attrsBefore, list{arg, ...acc}, typ2)
      | {ptyp_desc: Ptyp_arrow(Nolabel, _typ1, _typ2), ptyp_attributes: _attrs} as returnType =>
        let args = List.rev(acc)
        (attrsBefore, args, returnType)
      | {
          ptyp_desc: Ptyp_arrow((Labelled(_) | Optional(_)) as lbl, typ1, typ2),
          ptyp_attributes: attrs,
        } =>
        let arg = (attrs, lbl, typ1)
        process(attrsBefore, list{arg, ...acc}, typ2)
      | typ => (attrsBefore, List.rev(acc), typ)
      }

    switch ct {
    | {ptyp_desc: Ptyp_arrow(Nolabel, _typ1, _typ2), ptyp_attributes: attrs} as typ =>
      process(attrs, list{}, {...typ, ptyp_attributes: list{}})
    | typ => process(list{}, list{}, typ)
    }
  }

  /* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? */
  let modExprApply = modExpr => {
    let rec loop = (acc, modExpr) =>
      switch modExpr {
      | {Parsetree.pmod_desc: Pmod_apply(next, arg)} => loop(list{arg, ...acc}, next)
      | _ => list{modExpr, ...acc}
      }

    loop(list{}, modExpr)
  }

  /* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? */
  let modExprFunctor = modExpr => {
    let rec loop = (acc, modExpr) =>
      switch modExpr {
      | {Parsetree.pmod_desc: Pmod_functor(lbl, modType, returnModExpr), pmod_attributes: attrs} =>
        let param = (attrs, lbl, modType)
        loop(list{param, ...acc}, returnModExpr)
      | returnModExpr => (List.rev(acc), returnModExpr)
      }

    loop(list{}, modExpr)
  }

  let functorType = modtype => {
    let rec process = (acc, modtype) =>
      switch modtype {
      | {Parsetree.pmty_desc: Pmty_functor(lbl, argType, returnType), pmty_attributes: attrs} =>
        let arg = (attrs, lbl, argType)
        process(list{arg, ...acc}, returnType)
      | modType => (List.rev(acc), modType)
      }

    process(list{}, modtype)
  }

  let funExpr = expr => {
    open Parsetree
    /* Turns (type t, type u, type z) into "type t u z" */
    let rec collectNewTypes = (acc, returnExpr) =>
      switch returnExpr {
      | {pexp_desc: Pexp_newtype(stringLoc, returnExpr), pexp_attributes: list{}} =>
        collectNewTypes(list{stringLoc, ...acc}, returnExpr)
      | returnExpr =>
        let loc = switch (acc, List.rev(acc)) {
        | (list{_startLoc, ..._}, list{endLoc, ..._}) => {
            ...endLoc.loc,
            loc_end: endLoc.loc.loc_end,
          }
        | _ => Location.none
        }

        let txt = List.fold_right((curr, acc) => acc ++ (" " ++ curr.Location.txt), acc, "type")
        (Location.mkloc(txt, loc), returnExpr)
      }

    /* For simplicity reason Pexp_newtype gets converted to a Nolabel parameter,
     * otherwise this function would need to return a variant:
     * | NormalParamater(...)
     * | NewType(...)
     * This complicates printing with an extra variant/boxing/allocation for a code-path
     * that is not often used. Lets just keep it simple for now */
    let rec collect = (attrsBefore, acc, expr) =>
      switch expr {
      | {pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr), pexp_attributes: list{}} =>
        let parameter = (list{}, lbl, defaultExpr, pattern)
        collect(attrsBefore, list{parameter, ...acc}, returnExpr)
      | {pexp_desc: Pexp_newtype(stringLoc, rest), pexp_attributes: attrs} =>
        let (var, returnExpr) = collectNewTypes(list{stringLoc}, rest)
        let parameter = (attrs, Asttypes.Nolabel, None, Ast_helper.Pat.var(~loc=stringLoc.loc, var))
        collect(attrsBefore, list{parameter, ...acc}, returnExpr)
      | {
          pexp_desc: Pexp_fun(lbl, defaultExpr, pattern, returnExpr),
          pexp_attributes: list{({txt: "bs"}, _)} as attrs,
        } =>
        let parameter = (attrs, lbl, defaultExpr, pattern)
        collect(attrsBefore, list{parameter, ...acc}, returnExpr)
      | {
          pexp_desc: Pexp_fun((Labelled(_) | Optional(_)) as lbl, defaultExpr, pattern, returnExpr),
          pexp_attributes: attrs,
        } =>
        let parameter = (attrs, lbl, defaultExpr, pattern)
        collect(attrsBefore, list{parameter, ...acc}, returnExpr)
      | expr => (attrsBefore, List.rev(acc), expr)
      }

    switch expr {
    | {
        pexp_desc: Pexp_fun(Nolabel, _defaultExpr, _pattern, _returnExpr),
        pexp_attributes: attrs,
      } as expr =>
      collect(attrs, list{}, {...expr, pexp_attributes: list{}})
    | expr => collect(list{}, list{}, expr)
    }
  }

  let rec isBlockExpr = expr => {
    open Parsetree
    switch expr.pexp_desc {
    | Pexp_letmodule(_)
    | Pexp_letexception(_)
    | Pexp_let(_)
    | Pexp_open(_)
    | Pexp_sequence(_) => true
    | Pexp_apply(callExpr, _) when isBlockExpr(callExpr) => true
    | Pexp_constraint(expr, _) when isBlockExpr(expr) => true
    | Pexp_field(expr, _) when isBlockExpr(expr) => true
    | Pexp_setfield(expr, _, _) when isBlockExpr(expr) => true
    | _ => false
    }
  }

  let rec walkStructure = (s, t, comments) =>
    switch s {
    | _ when comments == list{} => ()
    | list{} => attach(t.inside, Location.none, comments)
    | s => walkList(~getLoc=n => n.Parsetree.pstr_loc, ~walkNode=walkStructureItem, s, t, comments)
    }

  and walkStructureItem = (si, t, comments) =>
    switch si.Parsetree.pstr_desc {
    | _ when comments == list{} => ()
    | Pstr_primitive(valueDescription) => walkValueDescription(valueDescription, t, comments)
    | Pstr_open(openDescription) => walkOpenDescription(openDescription, t, comments)
    | Pstr_value(_, valueBindings) => walkValueBindings(valueBindings, t, comments)
    | Pstr_type(_, typeDeclarations) => walkTypeDeclarations(typeDeclarations, t, comments)
    | Pstr_eval(expr, _) => walkExpr(expr, t, comments)
    | Pstr_module(moduleBinding) => walkModuleBinding(moduleBinding, t, comments)
    | Pstr_recmodule(moduleBindings) =>
      walkList(
        ~getLoc=mb => mb.Parsetree.pmb_loc,
        ~walkNode=walkModuleBinding,
        moduleBindings,
        t,
        comments,
      )
    | Pstr_modtype(modTypDecl) => walkModuleTypeDeclaration(modTypDecl, t, comments)
    | Pstr_attribute(attribute) => walkAttribute(attribute, t, comments)
    | Pstr_extension(extension, _) => walkExtension(extension, t, comments)
    | Pstr_include(includeDeclaration) => walkIncludeDeclaration(includeDeclaration, t, comments)
    | Pstr_exception(extensionConstructor) => walkExtConstr(extensionConstructor, t, comments)
    | Pstr_typext(typeExtension) => walkTypeExtension(typeExtension, t, comments)
    | Pstr_class_type(_) | Pstr_class(_) => ()
    }

  and walkValueDescription = (vd, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(comments, vd.pval_name.loc)
    attach(t.leading, vd.pval_name.loc, leading)
    let (afterName, rest) = partitionAdjacentTrailing(vd.pval_name.loc, trailing)
    attach(t.trailing, vd.pval_name.loc, afterName)
    let (before, inside, after) = partitionByLoc(rest, vd.pval_type.ptyp_loc)

    attach(t.leading, vd.pval_type.ptyp_loc, before)
    walkTypExpr(vd.pval_type, t, inside)
    attach(t.trailing, vd.pval_type.ptyp_loc, after)
  }

  and walkTypeExtension = (te, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(comments, te.ptyext_path.loc)
    attach(t.leading, te.ptyext_path.loc, leading)
    let (afterPath, rest) = partitionAdjacentTrailing(te.ptyext_path.loc, trailing)
    attach(t.trailing, te.ptyext_path.loc, afterPath)

    /* type params */
    let rest = switch te.ptyext_params {
    | list{} => rest
    | typeParams =>
      visitListButContinueWithRemainingComments(
        ~getLoc=((typexpr, _variance)) => typexpr.Parsetree.ptyp_loc,
        ~walkNode=walkTypeParam,
        ~newlineDelimited=false,
        typeParams,
        t,
        rest,
      )
    }

    walkList(
      ~getLoc=n => n.Parsetree.pext_loc,
      ~walkNode=walkExtConstr,
      te.ptyext_constructors,
      t,
      rest,
    )
  }

  and walkIncludeDeclaration = (inclDecl, t, comments) => {
    let (before, inside, after) = partitionByLoc(comments, inclDecl.pincl_mod.pmod_loc)
    attach(t.leading, inclDecl.pincl_mod.pmod_loc, before)
    walkModExpr(inclDecl.pincl_mod, t, inside)
    attach(t.trailing, inclDecl.pincl_mod.pmod_loc, after)
  }

  and walkModuleTypeDeclaration = (mtd, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(comments, mtd.pmtd_name.loc)
    attach(t.leading, mtd.pmtd_name.loc, leading)
    switch mtd.pmtd_type {
    | None => attach(t.trailing, mtd.pmtd_name.loc, trailing)
    | Some(modType) =>
      let (afterName, rest) = partitionAdjacentTrailing(mtd.pmtd_name.loc, trailing)
      attach(t.trailing, mtd.pmtd_name.loc, afterName)
      let (before, inside, after) = partitionByLoc(rest, modType.pmty_loc)
      attach(t.leading, modType.pmty_loc, before)
      walkModType(modType, t, inside)
      attach(t.trailing, modType.pmty_loc, after)
    }
  }

  and walkModuleBinding = (mb, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(comments, mb.pmb_name.loc)
    attach(t.leading, mb.pmb_name.loc, leading)
    let (afterName, rest) = partitionAdjacentTrailing(mb.pmb_name.loc, trailing)
    attach(t.trailing, mb.pmb_name.loc, afterName)
    let (leading, inside, trailing) = partitionByLoc(rest, mb.pmb_expr.pmod_loc)
    switch mb.pmb_expr.pmod_desc {
    | Pmod_constraint(_) => walkModExpr(mb.pmb_expr, t, List.concat(list{leading, inside}))
    | _ =>
      attach(t.leading, mb.pmb_expr.pmod_loc, leading)
      walkModExpr(mb.pmb_expr, t, inside)
    }
    attach(t.trailing, mb.pmb_expr.pmod_loc, trailing)
  }

  and walkSignature = (signature, t, comments) =>
    switch signature {
    | _ when comments == list{} => ()
    | list{} => attach(t.inside, Location.none, comments)
    | _s =>
      walkList(
        ~getLoc=n => n.Parsetree.psig_loc,
        ~walkNode=walkSignatureItem,
        signature,
        t,
        comments,
      )
    }

  and walkSignatureItem = (si, t, comments) =>
    switch si.psig_desc {
    | _ when comments == list{} => ()
    | Psig_value(valueDescription) => walkValueDescription(valueDescription, t, comments)
    | Psig_type(_, typeDeclarations) => walkTypeDeclarations(typeDeclarations, t, comments)
    | Psig_typext(typeExtension) => walkTypeExtension(typeExtension, t, comments)
    | Psig_exception(extensionConstructor) => walkExtConstr(extensionConstructor, t, comments)
    | Psig_module(moduleDeclaration) => walkModuleDeclaration(moduleDeclaration, t, comments)
    | Psig_recmodule(moduleDeclarations) =>
      walkList(
        ~getLoc=n => n.Parsetree.pmd_loc,
        ~walkNode=walkModuleDeclaration,
        moduleDeclarations,
        t,
        comments,
      )
    | Psig_modtype(moduleTypeDeclaration) =>
      walkModuleTypeDeclaration(moduleTypeDeclaration, t, comments)
    | Psig_open(openDescription) => walkOpenDescription(openDescription, t, comments)
    | Psig_include(includeDescription) => walkIncludeDescription(includeDescription, t, comments)
    | Psig_attribute(attribute) => walkAttribute(attribute, t, comments)
    | Psig_extension(extension, _) => walkExtension(extension, t, comments)
    | Psig_class(_) | Psig_class_type(_) => ()
    }

  and walkIncludeDescription = (id, t, comments) => {
    let (before, inside, after) = partitionByLoc(comments, id.pincl_mod.pmty_loc)
    attach(t.leading, id.pincl_mod.pmty_loc, before)
    walkModType(id.pincl_mod, t, inside)
    attach(t.trailing, id.pincl_mod.pmty_loc, after)
  }

  and walkModuleDeclaration = (md, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(comments, md.pmd_name.loc)
    attach(t.leading, md.pmd_name.loc, leading)
    let (afterName, rest) = partitionAdjacentTrailing(md.pmd_name.loc, trailing)
    attach(t.trailing, md.pmd_name.loc, afterName)
    let (leading, inside, trailing) = partitionByLoc(rest, md.pmd_type.pmty_loc)
    attach(t.leading, md.pmd_type.pmty_loc, leading)
    walkModType(md.pmd_type, t, inside)
    attach(t.trailing, md.pmd_type.pmty_loc, trailing)
  }

  and walkList: 'node. (
    ~prevLoc: Location.t=?,
    ~getLoc: 'node => Location.t,
    ~walkNode: ('node, t, list<Comment.t>) => unit,
    list<'node>,
    t,
    list<Comment.t>,
  ) => unit = (~prevLoc=?, ~getLoc, ~walkNode, l, t, comments) => {
    open Location
    switch l {
    | _ when comments == list{} => ()
    | list{} =>
      switch prevLoc {
      | Some(loc) => attach(t.trailing, loc, comments)
      | None => ()
      }
    | list{node, ...rest} =>
      let currLoc = getLoc(node)
      let (leading, inside, trailing) = partitionByLoc(comments, currLoc)
      switch prevLoc {
      | None =>
        /* first node, all leading comments attach here */
        attach(t.leading, currLoc, leading)
      | Some(prevLoc) =>
        /* Same line */
        if prevLoc.loc_end.pos_lnum === currLoc.loc_start.pos_lnum {
          let (afterPrev, beforeCurr) = partitionAdjacentTrailing(prevLoc, leading)
          let () = attach(t.trailing, prevLoc, afterPrev)
          attach(t.leading, currLoc, beforeCurr)
        } else {
          let (onSameLineAsPrev, afterPrev) = partitionByOnSameLine(prevLoc, leading)
          let () = attach(t.trailing, prevLoc, onSameLineAsPrev)
          let (leading, _inside, _trailing) = partitionByLoc(afterPrev, currLoc)
          attach(t.leading, currLoc, leading)
        }
      }
      walkNode(node, t, inside)
      walkList(~prevLoc=currLoc, ~getLoc, ~walkNode, rest, t, trailing)
    }
  }

  /* The parsetree doesn't always contain location info about the opening or
   * closing token of a "list-of-things". This routine visits the whole list,
   * but returns any remaining comments that likely fall after the whole list. */
  and visitListButContinueWithRemainingComments: 'node. (
    ~prevLoc: Location.t=?,
    ~newlineDelimited: bool,
    ~getLoc: 'node => Location.t,
    ~walkNode: ('node, t, list<Comment.t>) => unit,
    list<'node>,
    t,
    list<Comment.t>,
  ) => list<Comment.t> = (~prevLoc=?, ~newlineDelimited, ~getLoc, ~walkNode, l, t, comments) => {
    open Location
    switch l {
    | _ when comments == list{} => list{}
    | list{} =>
      switch prevLoc {
      | Some(loc) =>
        let (afterPrev, rest) = if newlineDelimited {
          partitionByOnSameLine(loc, comments)
        } else {
          partitionAdjacentTrailing(loc, comments)
        }

        attach(t.trailing, loc, afterPrev)
        rest
      | None => comments
      }
    | list{node, ...rest} =>
      let currLoc = getLoc(node)
      let (leading, inside, trailing) = partitionByLoc(comments, currLoc)
      let () = switch prevLoc {
      | None =>
        /* first node, all leading comments attach here */
        attach(t.leading, currLoc, leading)
        ()
      | Some(prevLoc) =>
        /* Same line */
        if prevLoc.loc_end.pos_lnum === currLoc.loc_start.pos_lnum {
          let (afterPrev, beforeCurr) = partitionAdjacentTrailing(prevLoc, leading)
          let () = attach(t.trailing, prevLoc, afterPrev)
          let () = attach(t.leading, currLoc, beforeCurr)
        } else {
          let (onSameLineAsPrev, afterPrev) = partitionByOnSameLine(prevLoc, leading)
          let () = attach(t.trailing, prevLoc, onSameLineAsPrev)
          let (leading, _inside, _trailing) = partitionByLoc(afterPrev, currLoc)
          let () = attach(t.leading, currLoc, leading)
        }
      }

      walkNode(node, t, inside)
      visitListButContinueWithRemainingComments(
        ~prevLoc=currLoc,
        ~getLoc,
        ~walkNode,
        ~newlineDelimited,
        rest,
        t,
        trailing,
      )
    }
  }

  and walkValueBindings = (vbs, t, comments) =>
    walkList(~getLoc=n => n.Parsetree.pvb_loc, ~walkNode=walkValueBinding, vbs, t, comments)

  and walkOpenDescription = (openDescription, t, comments) => {
    let loc = openDescription.popen_lid.loc
    let (leading, trailing) = partitionLeadingTrailing(comments, loc)
    attach(t.leading, loc, leading)
    attach(t.trailing, loc, trailing)
  }

  and walkTypeDeclarations = (typeDeclarations, t, comments) =>
    walkList(
      ~getLoc=n => n.Parsetree.ptype_loc,
      ~walkNode=walkTypeDeclaration,
      typeDeclarations,
      t,
      comments,
    )

  and walkTypeParam = ((typexpr, _variance), t, comments) => walkTypExpr(typexpr, t, comments)

  and walkTypeDeclaration = (td, t, comments) => {
    let (beforeName, rest) = partitionLeadingTrailing(comments, td.ptype_name.loc)
    attach(t.leading, td.ptype_name.loc, beforeName)

    let (afterName, rest) = partitionAdjacentTrailing(td.ptype_name.loc, rest)
    attach(t.trailing, td.ptype_name.loc, afterName)

    /* type params */
    let rest = switch td.ptype_params {
    | list{} => rest
    | typeParams =>
      visitListButContinueWithRemainingComments(
        ~getLoc=((typexpr, _variance)) => typexpr.Parsetree.ptyp_loc,
        ~walkNode=walkTypeParam,
        ~newlineDelimited=false,
        typeParams,
        t,
        rest,
      )
    }

    /* manifest:  = typexpr */
    let rest = switch td.ptype_manifest {
    | Some(typexpr) =>
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      let (afterTyp, rest) = partitionAdjacentTrailing(typexpr.Parsetree.ptyp_loc, afterTyp)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
      rest
    | None => rest
    }

    let rest = switch td.ptype_kind {
    | Ptype_abstract | Ptype_open => rest
    | Ptype_record(labelDeclarations) =>
      let () = walkList(
        ~getLoc=ld => ld.Parsetree.pld_loc,
        ~walkNode=walkLabelDeclaration,
        labelDeclarations,
        t,
        rest,
      )

      list{}
    | Ptype_variant(constructorDeclarations) =>
      walkConstructorDeclarations(constructorDeclarations, t, rest)
    }

    attach(t.trailing, td.ptype_loc, rest)
  }

  and walkLabelDeclarations = (lds, t, comments) =>
    visitListButContinueWithRemainingComments(
      ~getLoc=ld => ld.Parsetree.pld_loc,
      ~walkNode=walkLabelDeclaration,
      ~newlineDelimited=false,
      lds,
      t,
      comments,
    )

  and walkLabelDeclaration = (ld, t, comments) => {
    let (beforeName, rest) = partitionLeadingTrailing(comments, ld.pld_name.loc)
    attach(t.leading, ld.pld_name.loc, beforeName)
    let (afterName, rest) = partitionAdjacentTrailing(ld.pld_name.loc, rest)
    attach(t.trailing, ld.pld_name.loc, afterName)
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, ld.pld_type.ptyp_loc)
    attach(t.leading, ld.pld_type.ptyp_loc, beforeTyp)
    walkTypExpr(ld.pld_type, t, insideTyp)
    attach(t.trailing, ld.pld_type.ptyp_loc, afterTyp)
  }

  and walkConstructorDeclarations = (cds, t, comments) =>
    visitListButContinueWithRemainingComments(
      ~getLoc=cd => cd.Parsetree.pcd_loc,
      ~walkNode=walkConstructorDeclaration,
      ~newlineDelimited=false,
      cds,
      t,
      comments,
    )

  and walkConstructorDeclaration = (cd, t, comments) => {
    let (beforeName, rest) = partitionLeadingTrailing(comments, cd.pcd_name.loc)
    attach(t.leading, cd.pcd_name.loc, beforeName)
    let (afterName, rest) = partitionAdjacentTrailing(cd.pcd_name.loc, rest)
    attach(t.trailing, cd.pcd_name.loc, afterName)
    let rest = walkConstructorArguments(cd.pcd_args, t, rest)

    let rest = switch cd.pcd_res {
    | Some(typexpr) =>
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      let (afterTyp, rest) = partitionAdjacentTrailing(typexpr.Parsetree.ptyp_loc, afterTyp)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
      rest
    | None => rest
    }

    attach(t.trailing, cd.pcd_loc, rest)
  }

  and walkConstructorArguments = (args, t, comments) =>
    switch args {
    | Pcstr_tuple(typexprs) =>
      visitListButContinueWithRemainingComments(
        ~getLoc=n => n.Parsetree.ptyp_loc,
        ~walkNode=walkTypExpr,
        ~newlineDelimited=false,
        typexprs,
        t,
        comments,
      )
    | Pcstr_record(labelDeclarations) => walkLabelDeclarations(labelDeclarations, t, comments)
    }

  and walkValueBinding = (vb, t, comments) => {
    open Location

    let vb = {
      open Parsetree
      switch (vb.pvb_pat, vb.pvb_expr) {
      | (
          {ppat_desc: Ppat_constraint(pat, {ptyp_desc: Ptyp_poly(list{}, t)})},
          {pexp_desc: Pexp_constraint(expr, _typ)},
        ) => {
          ...vb,
          pvb_pat: Ast_helper.Pat.constraint_(
            ~loc={...pat.ppat_loc, loc_end: t.Parsetree.ptyp_loc.loc_end},
            pat,
            t,
          ),
          pvb_expr: expr,
        }
      | (
          {ppat_desc: Ppat_constraint(pat, {ptyp_desc: Ptyp_poly(list{_, ..._}, t)})},
          {pexp_desc: Pexp_fun(_)},
        ) => {
          ...vb,
          pvb_pat: {
            ...vb.pvb_pat,
            ppat_loc: {...pat.ppat_loc, loc_end: t.ptyp_loc.loc_end},
          },
        }
      | _ => vb
      }
    }

    let patternLoc = vb.Parsetree.pvb_pat.ppat_loc
    let exprLoc = vb.Parsetree.pvb_expr.pexp_loc

    let (leading, inside, trailing) = partitionByLoc(comments, patternLoc)

    /* everything before start of pattern can only be leading on the pattern:
     *   let |* before *| a = 1 */
    attach(t.leading, patternLoc, leading)
    walkPattern(vb.Parsetree.pvb_pat, t, inside)
    /* let pattern = expr     -> pattern and expr on the same line */
    /* if patternLoc.loc_end.pos_lnum == exprLoc.loc_start.pos_lnum then ( */
    let (afterPat, surroundingExpr) = partitionAdjacentTrailing(patternLoc, trailing)

    attach(t.trailing, patternLoc, afterPat)
    let (beforeExpr, insideExpr, afterExpr) = partitionByLoc(surroundingExpr, exprLoc)
    if isBlockExpr(vb.pvb_expr) {
      walkExpr(vb.pvb_expr, t, List.concat(list{beforeExpr, insideExpr, afterExpr}))
    } else {
      attach(t.leading, exprLoc, beforeExpr)
      walkExpr(vb.Parsetree.pvb_expr, t, insideExpr)
      attach(t.trailing, exprLoc, afterExpr)
    }
  }

  and walkExpr = (expr, t, comments) => {
    open Location
    switch expr.Parsetree.pexp_desc {
    | _ when comments == list{} => ()
    | Pexp_constant(_) =>
      let (leading, trailing) = partitionLeadingTrailing(comments, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, leading)
      attach(t.trailing, expr.pexp_loc, trailing)
    | Pexp_ident(longident) =>
      let (leading, trailing) = partitionLeadingTrailing(comments, longident.loc)
      attach(t.leading, longident.loc, leading)
      attach(t.trailing, longident.loc, trailing)
    | Pexp_let(_recFlag, valueBindings, expr2) =>
      let comments = visitListButContinueWithRemainingComments(~getLoc=n =>
        if n.Parsetree.pvb_pat.ppat_loc.loc_ghost {
          n.pvb_expr.pexp_loc
        } else {
          n.Parsetree.pvb_loc
        }
      , ~walkNode=walkValueBinding, ~newlineDelimited=true, valueBindings, t, comments)

      if isBlockExpr(expr2) {
        walkExpr(expr2, t, comments)
      } else {
        let (leading, inside, trailing) = partitionByLoc(comments, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_sequence(expr1, expr2) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr1.pexp_loc)
      let comments = if isBlockExpr(expr1) {
        let (afterExpr, comments) = partitionByOnSameLine(expr1.pexp_loc, trailing)
        walkExpr(expr1, t, List.concat(list{leading, inside, afterExpr}))
        comments
      } else {
        attach(t.leading, expr1.pexp_loc, leading)
        walkExpr(expr1, t, inside)
        let (afterExpr, comments) = partitionByOnSameLine(expr1.pexp_loc, trailing)
        attach(t.trailing, expr1.pexp_loc, afterExpr)
        comments
      }
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, comments)
      } else {
        let (leading, inside, trailing) = partitionByLoc(comments, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_open(_override, longident, expr2) =>
      let (leading, comments) = partitionLeadingTrailing(comments, expr.pexp_loc)
      attach(t.leading, {...expr.pexp_loc, loc_end: longident.loc.loc_end}, leading)
      let (leading, trailing) = partitionLeadingTrailing(comments, longident.loc)
      attach(t.leading, longident.loc, leading)
      let (afterLongident, rest) = partitionByOnSameLine(longident.loc, trailing)
      attach(t.trailing, longident.loc, afterLongident)
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_extension(
        {txt: "obj"},
        PStr(list{{pstr_desc: Pstr_eval({pexp_desc: Pexp_record(rows, _)}, list{})}}),
      ) =>
      walkList(~getLoc=((longident, expr): (Asttypes.loc<Longident.t>, Parsetree.expression)) => {
        ...longident.loc,
        loc_end: expr.pexp_loc.loc_end,
      }, ~walkNode=walkExprRecordRow, rows, t, comments)
    | Pexp_extension(extension) => walkExtension(extension, t, comments)
    | Pexp_letexception(extensionConstructor, expr2) =>
      let (leading, comments) = partitionLeadingTrailing(comments, expr.pexp_loc)
      attach(t.leading, {...expr.pexp_loc, loc_end: extensionConstructor.pext_loc.loc_end}, leading)
      let (leading, inside, trailing) = partitionByLoc(comments, extensionConstructor.pext_loc)
      attach(t.leading, extensionConstructor.pext_loc, leading)
      walkExtConstr(extensionConstructor, t, inside)
      let (afterExtConstr, rest) = partitionByOnSameLine(extensionConstructor.pext_loc, trailing)
      attach(t.trailing, extensionConstructor.pext_loc, afterExtConstr)
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_letmodule(stringLoc, modExpr, expr2) =>
      let (leading, comments) = partitionLeadingTrailing(comments, expr.pexp_loc)
      attach(t.leading, {...expr.pexp_loc, loc_end: modExpr.pmod_loc.loc_end}, leading)
      let (leading, trailing) = partitionLeadingTrailing(comments, stringLoc.loc)
      attach(t.leading, stringLoc.loc, leading)
      let (afterString, rest) = partitionAdjacentTrailing(stringLoc.loc, trailing)
      attach(t.trailing, stringLoc.loc, afterString)
      let (beforeModExpr, insideModExpr, afterModExpr) = partitionByLoc(rest, modExpr.pmod_loc)
      attach(t.leading, modExpr.pmod_loc, beforeModExpr)
      walkModExpr(modExpr, t, insideModExpr)
      let (afterModExpr, rest) = partitionByOnSameLine(modExpr.pmod_loc, afterModExpr)
      attach(t.trailing, modExpr.pmod_loc, afterModExpr)
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_assert(expr) | Pexp_lazy(expr) =>
      if isBlockExpr(expr) {
        walkExpr(expr, t, comments)
      } else {
        let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
        attach(t.leading, expr.pexp_loc, leading)
        walkExpr(expr, t, inside)
        attach(t.trailing, expr.pexp_loc, trailing)
      }
    | Pexp_coerce(expr, optTypexpr, typexpr) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, leading)
      walkExpr(expr, t, inside)
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
      attach(t.trailing, expr.pexp_loc, afterExpr)
      let rest = switch optTypexpr {
      | Some(typexpr) =>
        let (leading, inside, trailing) = partitionByLoc(comments, typexpr.ptyp_loc)
        attach(t.leading, typexpr.ptyp_loc, leading)
        walkTypExpr(typexpr, t, inside)
        let (afterTyp, rest) = partitionAdjacentTrailing(typexpr.ptyp_loc, trailing)
        attach(t.trailing, typexpr.ptyp_loc, afterTyp)
        rest
      | None => rest
      }

      let (leading, inside, trailing) = partitionByLoc(rest, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, leading)
      walkTypExpr(typexpr, t, inside)
      attach(t.trailing, typexpr.ptyp_loc, trailing)
    | Pexp_constraint(expr, typexpr) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, leading)
      walkExpr(expr, t, inside)
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
      attach(t.trailing, expr.pexp_loc, afterExpr)
      let (leading, inside, trailing) = partitionByLoc(rest, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, leading)
      walkTypExpr(typexpr, t, inside)
      attach(t.trailing, typexpr.ptyp_loc, trailing)
    | Pexp_tuple(list{}) | Pexp_array(list{}) | Pexp_construct({txt: Longident.Lident("[]")}, _) =>
      attach(t.inside, expr.pexp_loc, comments)
    | Pexp_construct({txt: Longident.Lident("::")}, _) =>
      walkList(
        ~getLoc=n => n.Parsetree.pexp_loc,
        ~walkNode=walkExpr,
        collectListExprs(list{}, expr),
        t,
        comments,
      )
    | Pexp_construct(longident, args) =>
      let (leading, trailing) = partitionLeadingTrailing(comments, longident.loc)
      attach(t.leading, longident.loc, leading)
      switch args {
      | Some(expr) =>
        let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, trailing)
        attach(t.trailing, longident.loc, afterLongident)
        walkExpr(expr, t, rest)
      | None => attach(t.trailing, longident.loc, trailing)
      }
    | Pexp_variant(_label, None) => ()
    | Pexp_variant(_label, Some(expr)) => walkExpr(expr, t, comments)
    | Pexp_array(exprs) | Pexp_tuple(exprs) =>
      walkList(~getLoc=n => n.Parsetree.pexp_loc, ~walkNode=walkExpr, exprs, t, comments)
    | Pexp_record(rows, spreadExpr) =>
      let comments = switch spreadExpr {
      | None => comments
      | Some(expr) =>
        let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
        attach(t.leading, expr.pexp_loc, leading)
        walkExpr(expr, t, inside)
        let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
        attach(t.trailing, expr.pexp_loc, afterExpr)
        rest
      }

      walkList(~getLoc=((longident, expr): (Asttypes.loc<Longident.t>, Parsetree.expression)) => {
        ...longident.loc,
        loc_end: expr.pexp_loc.loc_end,
      }, ~walkNode=walkExprRecordRow, rows, t, comments)
    | Pexp_field(expr, longident) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
      let trailing = if isBlockExpr(expr) {
        let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
        walkExpr(expr, t, List.concat(list{leading, inside, afterExpr}))
        rest
      } else {
        attach(t.leading, expr.pexp_loc, leading)
        walkExpr(expr, t, inside)
        trailing
      }
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, trailing)
      attach(t.trailing, expr.pexp_loc, afterExpr)
      let (leading, trailing) = partitionLeadingTrailing(rest, longident.loc)
      attach(t.leading, longident.loc, leading)
      attach(t.trailing, longident.loc, trailing)
    | Pexp_setfield(expr1, longident, expr2) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr1.pexp_loc)
      let rest = if isBlockExpr(expr1) {
        let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, trailing)
        walkExpr(expr1, t, List.concat(list{leading, inside, afterExpr}))
        rest
      } else {
        let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, trailing)
        attach(t.leading, expr1.pexp_loc, leading)
        walkExpr(expr1, t, inside)
        attach(t.trailing, expr1.pexp_loc, afterExpr)
        rest
      }
      let (beforeLongident, afterLongident) = partitionLeadingTrailing(rest, longident.loc)
      attach(t.leading, longident.loc, beforeLongident)
      let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, afterLongident)
      attach(t.trailing, longident.loc, afterLongident)
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_ifthenelse(ifExpr, thenExpr, elseExpr) =>
      let (leading, inside, trailing) = partitionByLoc(comments, ifExpr.pexp_loc)
      let comments = if isBlockExpr(ifExpr) {
        let (afterExpr, comments) = partitionAdjacentTrailing(ifExpr.pexp_loc, trailing)
        walkExpr(ifExpr, t, List.concat(list{leading, inside, afterExpr}))
        comments
      } else {
        attach(t.leading, ifExpr.pexp_loc, leading)
        walkExpr(ifExpr, t, inside)
        let (afterExpr, comments) = partitionAdjacentTrailing(ifExpr.pexp_loc, trailing)
        attach(t.trailing, ifExpr.pexp_loc, afterExpr)
        comments
      }
      let (leading, inside, trailing) = partitionByLoc(comments, thenExpr.pexp_loc)
      let comments = if isBlockExpr(thenExpr) {
        let (afterExpr, trailing) = partitionAdjacentTrailing(thenExpr.pexp_loc, trailing)
        walkExpr(thenExpr, t, List.concat(list{leading, inside, afterExpr}))
        trailing
      } else {
        attach(t.leading, thenExpr.pexp_loc, leading)
        walkExpr(thenExpr, t, inside)
        let (afterExpr, comments) = partitionAdjacentTrailing(thenExpr.pexp_loc, trailing)
        attach(t.trailing, thenExpr.pexp_loc, afterExpr)
        comments
      }
      switch elseExpr {
      | None => ()
      | Some(expr) =>
        if isBlockExpr(expr) {
          walkExpr(expr, t, comments)
        } else {
          let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
          attach(t.leading, expr.pexp_loc, leading)
          walkExpr(expr, t, inside)
          attach(t.trailing, expr.pexp_loc, trailing)
        }
      }
    | Pexp_while(expr1, expr2) =>
      let (leading, inside, trailing) = partitionByLoc(comments, expr1.pexp_loc)
      let rest = if isBlockExpr(expr1) {
        let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, trailing)
        walkExpr(expr1, t, List.concat(list{leading, inside, afterExpr}))
        rest
      } else {
        attach(t.leading, expr1.pexp_loc, leading)
        walkExpr(expr1, t, inside)
        let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, trailing)
        attach(t.trailing, expr1.pexp_loc, afterExpr)
        rest
      }
      if isBlockExpr(expr2) {
        walkExpr(expr2, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
        attach(t.leading, expr2.pexp_loc, leading)
        walkExpr(expr2, t, inside)
        attach(t.trailing, expr2.pexp_loc, trailing)
      }
    | Pexp_for(pat, expr1, expr2, _, expr3) =>
      let (leading, inside, trailing) = partitionByLoc(comments, pat.ppat_loc)
      attach(t.leading, pat.ppat_loc, leading)
      walkPattern(pat, t, inside)
      let (afterPat, rest) = partitionAdjacentTrailing(pat.ppat_loc, trailing)
      attach(t.trailing, pat.ppat_loc, afterPat)
      let (leading, inside, trailing) = partitionByLoc(rest, expr1.pexp_loc)
      attach(t.leading, expr1.pexp_loc, leading)
      walkExpr(expr1, t, inside)
      let (afterExpr, rest) = partitionAdjacentTrailing(expr1.pexp_loc, trailing)
      attach(t.trailing, expr1.pexp_loc, afterExpr)
      let (leading, inside, trailing) = partitionByLoc(rest, expr2.pexp_loc)
      attach(t.leading, expr2.pexp_loc, leading)
      walkExpr(expr2, t, inside)
      let (afterExpr, rest) = partitionAdjacentTrailing(expr2.pexp_loc, trailing)
      attach(t.trailing, expr2.pexp_loc, afterExpr)
      if isBlockExpr(expr3) {
        walkExpr(expr3, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr3.pexp_loc)
        attach(t.leading, expr3.pexp_loc, leading)
        walkExpr(expr3, t, inside)
        attach(t.trailing, expr3.pexp_loc, trailing)
      }
    | Pexp_pack(modExpr) =>
      let (before, inside, after) = partitionByLoc(comments, modExpr.pmod_loc)
      attach(t.leading, modExpr.pmod_loc, before)
      walkModExpr(modExpr, t, inside)
      attach(t.trailing, modExpr.pmod_loc, after)
    | Pexp_match(expr, cases) | Pexp_try(expr, cases) =>
      let (before, inside, after) = partitionByLoc(comments, expr.pexp_loc)
      let after = if isBlockExpr(expr) {
        let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, after)
        walkExpr(expr, t, List.concat(list{before, inside, afterExpr}))
        rest
      } else {
        attach(t.leading, expr.pexp_loc, before)
        walkExpr(expr, t, inside)
        after
      }
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, after)
      attach(t.trailing, expr.pexp_loc, afterExpr)
      walkList(~getLoc=n => {
        ...n.Parsetree.pc_lhs.ppat_loc,
        loc_end: n.pc_rhs.pexp_loc.loc_end,
      }, ~walkNode=walkCase, cases, t, rest)
    /* unary expression: todo use parsetreeviewer */
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident("~+" | "~+." | "~-" | "~-." | "not" | "!")})},
        list{(Nolabel, argExpr)},
      ) =>
      let (before, inside, after) = partitionByLoc(comments, argExpr.pexp_loc)
      attach(t.leading, argExpr.pexp_loc, before)
      walkExpr(argExpr, t, inside)
      attach(t.trailing, argExpr.pexp_loc, after)
    /* binary expression */
    | Pexp_apply(
        {
          pexp_desc:
            Pexp_ident({
              txt:
                Longident.Lident(
                  ":="
                  | "||"
                  | "&&"
                  | "="
                  | "=="
                  | "<"
                  | ">"
                  | "!="
                  | "!=="
                  | "<="
                  | ">="
                  | "|>"
                  | "+"
                  | "+."
                  | "-"
                  | "-."
                  | "++"
                  | "^"
                  | "*"
                  | "*."
                  | "/"
                  | "/."
                  | "**"
                  | "|."
                  | "<>",
                ),
            }),
        },
        list{(Nolabel, operand1), (Nolabel, operand2)},
      ) =>
      let (before, inside, after) = partitionByLoc(comments, operand1.pexp_loc)
      attach(t.leading, operand1.pexp_loc, before)
      walkExpr(operand1, t, inside)
      let (afterOperand1, rest) = partitionAdjacentTrailing(operand1.pexp_loc, after)
      attach(t.trailing, operand1.pexp_loc, afterOperand1)
      let (before, inside, after) = partitionByLoc(rest, operand2.pexp_loc)
      attach(t.leading, operand2.pexp_loc, before)
      walkExpr(operand2, t, inside) /* (List.concat [inside; after]); */
      attach(t.trailing, operand2.pexp_loc, after)
    | Pexp_apply(callExpr, arguments) =>
      let (before, inside, after) = partitionByLoc(comments, callExpr.pexp_loc)
      let after = if isBlockExpr(callExpr) {
        let (afterExpr, rest) = partitionAdjacentTrailing(callExpr.pexp_loc, after)
        walkExpr(callExpr, t, List.concat(list{before, inside, afterExpr}))
        rest
      } else {
        attach(t.leading, callExpr.pexp_loc, before)
        walkExpr(callExpr, t, inside)
        after
      }
      let (afterExpr, rest) = partitionAdjacentTrailing(callExpr.pexp_loc, after)
      attach(t.trailing, callExpr.pexp_loc, afterExpr)
      walkList(~getLoc=((_argLabel, expr)) => {
        let loc = switch expr.Parsetree.pexp_attributes {
        | list{({Location.txt: "res.namedArgLoc", loc}, _), ..._attrs} => {
            ...loc,
            loc_end: expr.pexp_loc.loc_end,
          }
        | _ => expr.pexp_loc
        }

        loc
      }, ~walkNode=walkExprArgument, arguments, t, rest)
    | Pexp_fun(_, _, _, _) | Pexp_newtype(_) =>
      let (_, parameters, returnExpr) = funExpr(expr)
      let comments = visitListButContinueWithRemainingComments(
        ~newlineDelimited=false,
        ~walkNode=walkExprPararameter,
        ~getLoc=((_attrs, _argLbl, exprOpt, pattern)) => {
          open Parsetree
          let startPos = switch pattern.ppat_attributes {
          | list{({Location.txt: "res.namedArgLoc", loc}, _), ..._attrs} => loc.loc_start
          | _ => pattern.ppat_loc.loc_start
          }

          switch exprOpt {
          | None => {...pattern.ppat_loc, loc_start: startPos}
          | Some(expr) => {
              ...pattern.ppat_loc,
              loc_start: startPos,
              loc_end: expr.pexp_loc.loc_end,
            }
          }
        },
        parameters,
        t,
        comments,
      )

      switch returnExpr.pexp_desc {
      | Pexp_constraint(expr, typ)
        when expr.pexp_loc.loc_start.pos_cnum >= typ.ptyp_loc.loc_end.pos_cnum =>
        let (leading, inside, trailing) = partitionByLoc(comments, typ.ptyp_loc)
        attach(t.leading, typ.ptyp_loc, leading)
        walkTypExpr(typ, t, inside)
        let (afterTyp, comments) = partitionAdjacentTrailing(typ.ptyp_loc, trailing)
        attach(t.trailing, typ.ptyp_loc, afterTyp)
        if isBlockExpr(expr) {
          walkExpr(expr, t, comments)
        } else {
          let (leading, inside, trailing) = partitionByLoc(comments, expr.pexp_loc)
          attach(t.leading, expr.pexp_loc, leading)
          walkExpr(expr, t, inside)
          attach(t.trailing, expr.pexp_loc, trailing)
        }
      | _ =>
        if isBlockExpr(returnExpr) {
          walkExpr(returnExpr, t, comments)
        } else {
          let (leading, inside, trailing) = partitionByLoc(comments, returnExpr.pexp_loc)
          attach(t.leading, returnExpr.pexp_loc, leading)
          walkExpr(returnExpr, t, inside)
          attach(t.trailing, returnExpr.pexp_loc, trailing)
        }
      }
    | _ => ()
    }
  }

  and walkExprPararameter = ((_attrs, _argLbl, exprOpt, pattern), t, comments) => {
    let (leading, inside, trailing) = partitionByLoc(comments, pattern.ppat_loc)
    attach(t.leading, pattern.ppat_loc, leading)
    walkPattern(pattern, t, inside)
    switch exprOpt {
    | Some(expr) =>
      let (_afterPat, rest) = partitionAdjacentTrailing(pattern.ppat_loc, trailing)
      attach(t.trailing, pattern.ppat_loc, trailing)
      if isBlockExpr(expr) {
        walkExpr(expr, t, rest)
      } else {
        let (leading, inside, trailing) = partitionByLoc(rest, expr.pexp_loc)
        attach(t.leading, expr.pexp_loc, leading)
        walkExpr(expr, t, inside)
        attach(t.trailing, expr.pexp_loc, trailing)
      }
    | None => attach(t.trailing, pattern.ppat_loc, trailing)
    }
  }

  and walkExprArgument = ((_argLabel, expr), t, comments) =>
    switch expr.Parsetree.pexp_attributes {
    | list{({Location.txt: "res.namedArgLoc", loc}, _), ..._attrs} =>
      let (leading, trailing) = partitionLeadingTrailing(comments, loc)
      attach(t.leading, loc, leading)
      let (afterLabel, rest) = partitionAdjacentTrailing(loc, trailing)
      attach(t.trailing, loc, afterLabel)
      let (before, inside, after) = partitionByLoc(rest, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, before)
      walkExpr(expr, t, inside)
      attach(t.trailing, expr.pexp_loc, after)
    | _ =>
      let (before, inside, after) = partitionByLoc(comments, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, before)
      walkExpr(expr, t, inside)
      attach(t.trailing, expr.pexp_loc, after)
    }

  and walkCase = (case, t, comments) => {
    let (before, inside, after) = partitionByLoc(comments, case.pc_lhs.ppat_loc)
    /* cases don't have a location on their own, leading comments should go
     * after the bar on the pattern */
    walkPattern(case.pc_lhs, t, List.concat(list{before, inside}))
    let (afterPat, rest) = partitionAdjacentTrailing(case.pc_lhs.ppat_loc, after)
    attach(t.trailing, case.pc_lhs.ppat_loc, afterPat)
    let comments = switch case.pc_guard {
    | Some(expr) =>
      let (before, inside, after) = partitionByLoc(rest, expr.pexp_loc)
      let (afterExpr, rest) = partitionAdjacentTrailing(expr.pexp_loc, after)
      if isBlockExpr(expr) {
        walkExpr(expr, t, List.concat(list{before, inside, afterExpr}))
      } else {
        attach(t.leading, expr.pexp_loc, before)
        walkExpr(expr, t, inside)
        attach(t.trailing, expr.pexp_loc, afterExpr)
      }
      rest
    | None => rest
    }

    if isBlockExpr(case.pc_rhs) {
      walkExpr(case.pc_rhs, t, comments)
    } else {
      let (before, inside, after) = partitionByLoc(comments, case.pc_rhs.pexp_loc)
      attach(t.leading, case.pc_rhs.pexp_loc, before)
      walkExpr(case.pc_rhs, t, inside)
      attach(t.trailing, case.pc_rhs.pexp_loc, after)
    }
  }

  and walkExprRecordRow = ((longident, expr), t, comments) => {
    let (beforeLongident, afterLongident) = partitionLeadingTrailing(comments, longident.loc)

    attach(t.leading, longident.loc, beforeLongident)
    let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, afterLongident)
    attach(t.trailing, longident.loc, afterLongident)
    let (leading, inside, trailing) = partitionByLoc(rest, expr.pexp_loc)
    attach(t.leading, expr.pexp_loc, leading)
    walkExpr(expr, t, inside)
    attach(t.trailing, expr.pexp_loc, trailing)
  }

  and walkExtConstr = (extConstr, t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(comments, extConstr.pext_name.loc)
    attach(t.leading, extConstr.pext_name.loc, leading)
    let (afterName, rest) = partitionAdjacentTrailing(extConstr.pext_name.loc, trailing)
    attach(t.trailing, extConstr.pext_name.loc, afterName)
    walkExtensionConstructorKind(extConstr.pext_kind, t, rest)
  }

  and walkExtensionConstructorKind = (kind, t, comments) =>
    switch kind {
    | Pext_rebind(longident) =>
      let (leading, trailing) = partitionLeadingTrailing(comments, longident.loc)
      attach(t.leading, longident.loc, leading)
      attach(t.trailing, longident.loc, trailing)
    | Pext_decl(constructorArguments, maybeTypExpr) =>
      let rest = walkConstructorArguments(constructorArguments, t, comments)
      switch maybeTypExpr {
      | None => ()
      | Some(typexpr) =>
        let (before, inside, after) = partitionByLoc(rest, typexpr.ptyp_loc)
        attach(t.leading, typexpr.ptyp_loc, before)
        walkTypExpr(typexpr, t, inside)
        attach(t.trailing, typexpr.ptyp_loc, after)
      }
    }

  and walkModExpr = (modExpr, t, comments) =>
    switch modExpr.pmod_desc {
    | Pmod_ident(longident) =>
      let (before, after) = partitionLeadingTrailing(comments, longident.loc)
      attach(t.leading, longident.loc, before)
      attach(t.trailing, longident.loc, after)
    | Pmod_structure(structure) => walkStructure(structure, t, comments)
    | Pmod_extension(extension) => walkExtension(extension, t, comments)
    | Pmod_unpack(expr) =>
      let (before, inside, after) = partitionByLoc(comments, expr.pexp_loc)
      attach(t.leading, expr.pexp_loc, before)
      walkExpr(expr, t, inside)
      attach(t.trailing, expr.pexp_loc, after)
    | Pmod_constraint(modexpr, modtype) =>
      if modtype.pmty_loc.loc_start >= modexpr.pmod_loc.loc_end {
        let (before, inside, after) = partitionByLoc(comments, modexpr.pmod_loc)
        attach(t.leading, modexpr.pmod_loc, before)
        walkModExpr(modexpr, t, inside)
        let (after, rest) = partitionAdjacentTrailing(modexpr.pmod_loc, after)
        attach(t.trailing, modexpr.pmod_loc, after)
        let (before, inside, after) = partitionByLoc(rest, modtype.pmty_loc)
        attach(t.leading, modtype.pmty_loc, before)
        walkModType(modtype, t, inside)
        attach(t.trailing, modtype.pmty_loc, after)
      } else {
        let (before, inside, after) = partitionByLoc(comments, modtype.pmty_loc)
        attach(t.leading, modtype.pmty_loc, before)
        walkModType(modtype, t, inside)
        let (after, rest) = partitionAdjacentTrailing(modtype.pmty_loc, after)
        attach(t.trailing, modtype.pmty_loc, after)
        let (before, inside, after) = partitionByLoc(rest, modexpr.pmod_loc)
        attach(t.leading, modexpr.pmod_loc, before)
        walkModExpr(modexpr, t, inside)
        attach(t.trailing, modexpr.pmod_loc, after)
      }
    | Pmod_apply(_callModExpr, _argModExpr) =>
      let modExprs = modExprApply(modExpr)
      walkList(~getLoc=n => n.Parsetree.pmod_loc, ~walkNode=walkModExpr, modExprs, t, comments)
    | Pmod_functor(_) =>
      let (parameters, returnModExpr) = modExprFunctor(modExpr)
      let comments = visitListButContinueWithRemainingComments(~getLoc=((_, lbl, modTypeOption)) =>
        switch modTypeOption {
        | None => lbl.Asttypes.loc
        | Some(modType) => {...lbl.loc, loc_end: modType.Parsetree.pmty_loc.loc_end}
        }
      , ~walkNode=walkModExprParameter, ~newlineDelimited=false, parameters, t, comments)

      switch returnModExpr.pmod_desc {
      | Pmod_constraint(modExpr, modType)
        when modType.pmty_loc.loc_end.pos_cnum <= modExpr.pmod_loc.loc_start.pos_cnum =>
        let (before, inside, after) = partitionByLoc(comments, modType.pmty_loc)
        attach(t.leading, modType.pmty_loc, before)
        walkModType(modType, t, inside)
        let (after, rest) = partitionAdjacentTrailing(modType.pmty_loc, after)
        attach(t.trailing, modType.pmty_loc, after)
        let (before, inside, after) = partitionByLoc(rest, modExpr.pmod_loc)
        attach(t.leading, modExpr.pmod_loc, before)
        walkModExpr(modExpr, t, inside)
        attach(t.trailing, modExpr.pmod_loc, after)
      | _ =>
        let (before, inside, after) = partitionByLoc(comments, returnModExpr.pmod_loc)
        attach(t.leading, returnModExpr.pmod_loc, before)
        walkModExpr(returnModExpr, t, inside)
        attach(t.trailing, returnModExpr.pmod_loc, after)
      }
    }

  and walkModExprParameter = (parameter, t, comments) => {
    let (_attrs, lbl, modTypeOption) = parameter
    let (leading, trailing) = partitionLeadingTrailing(comments, lbl.loc)
    attach(t.leading, lbl.loc, leading)
    switch modTypeOption {
    | None => attach(t.trailing, lbl.loc, trailing)
    | Some(modType) =>
      let (afterLbl, rest) = partitionAdjacentTrailing(lbl.loc, trailing)
      attach(t.trailing, lbl.loc, afterLbl)
      let (before, inside, after) = partitionByLoc(rest, modType.pmty_loc)
      attach(t.leading, modType.pmty_loc, before)
      walkModType(modType, t, inside)
      attach(t.trailing, modType.pmty_loc, after)
    }
  }

  and walkModType = (modType, t, comments) =>
    switch modType.pmty_desc {
    | Pmty_ident(longident) | Pmty_alias(longident) =>
      let (leading, trailing) = partitionLeadingTrailing(comments, longident.loc)
      attach(t.leading, longident.loc, leading)
      attach(t.trailing, longident.loc, trailing)
    | Pmty_signature(signature) => walkSignature(signature, t, comments)
    | Pmty_extension(extension) => walkExtension(extension, t, comments)
    | Pmty_typeof(modExpr) =>
      let (before, inside, after) = partitionByLoc(comments, modExpr.pmod_loc)
      attach(t.leading, modExpr.pmod_loc, before)
      walkModExpr(modExpr, t, inside)
      attach(t.trailing, modExpr.pmod_loc, after)
    | Pmty_with(modType, _withConstraints) =>
      let (before, inside, after) = partitionByLoc(comments, modType.pmty_loc)
      attach(t.leading, modType.pmty_loc, before)
      walkModType(modType, t, inside)
      attach(t.trailing, modType.pmty_loc, after)
    /* TODO: withConstraints */
    | Pmty_functor(_) =>
      let (parameters, returnModType) = functorType(modType)
      let comments = visitListButContinueWithRemainingComments(~getLoc=((_, lbl, modTypeOption)) =>
        switch modTypeOption {
        | None => lbl.Asttypes.loc
        | Some(modType) =>
          if lbl.txt == "_" {
            modType.Parsetree.pmty_loc
          } else {
            {...lbl.loc, loc_end: modType.Parsetree.pmty_loc.loc_end}
          }
        }
      , ~walkNode=walkModTypeParameter, ~newlineDelimited=false, parameters, t, comments)

      let (before, inside, after) = partitionByLoc(comments, returnModType.pmty_loc)
      attach(t.leading, returnModType.pmty_loc, before)
      walkModType(returnModType, t, inside)
      attach(t.trailing, returnModType.pmty_loc, after)
    }

  and walkModTypeParameter = ((_, lbl, modTypeOption), t, comments) => {
    let (leading, trailing) = partitionLeadingTrailing(comments, lbl.loc)
    attach(t.leading, lbl.loc, leading)
    switch modTypeOption {
    | None => attach(t.trailing, lbl.loc, trailing)
    | Some(modType) =>
      let (afterLbl, rest) = partitionAdjacentTrailing(lbl.loc, trailing)
      attach(t.trailing, lbl.loc, afterLbl)
      let (before, inside, after) = partitionByLoc(rest, modType.pmty_loc)
      attach(t.leading, modType.pmty_loc, before)
      walkModType(modType, t, inside)
      attach(t.trailing, modType.pmty_loc, after)
    }
  }

  and walkPattern = (pat, t, comments) => {
    open Location
    switch pat.Parsetree.ppat_desc {
    | _ when comments == list{} => ()
    | Ppat_alias(pat, alias) =>
      let (leading, inside, trailing) = partitionByLoc(comments, pat.ppat_loc)
      attach(t.leading, pat.ppat_loc, leading)
      walkPattern(pat, t, inside)
      let (afterPat, rest) = partitionAdjacentTrailing(pat.ppat_loc, trailing)
      attach(t.leading, pat.ppat_loc, leading)
      attach(t.trailing, pat.ppat_loc, afterPat)
      let (beforeAlias, afterAlias) = partitionLeadingTrailing(rest, alias.loc)
      attach(t.leading, alias.loc, beforeAlias)
      attach(t.trailing, alias.loc, afterAlias)
    | Ppat_tuple(list{})
    | Ppat_array(list{})
    | Ppat_construct({txt: Longident.Lident("()")}, _)
    | Ppat_construct({txt: Longident.Lident("[]")}, _) =>
      attach(t.inside, pat.ppat_loc, comments)
    | Ppat_array(patterns) =>
      walkList(~getLoc=n => n.Parsetree.ppat_loc, ~walkNode=walkPattern, patterns, t, comments)
    | Ppat_tuple(patterns) =>
      walkList(~getLoc=n => n.Parsetree.ppat_loc, ~walkNode=walkPattern, patterns, t, comments)
    | Ppat_construct({txt: Longident.Lident("::")}, _) =>
      walkList(
        ~getLoc=n => n.Parsetree.ppat_loc,
        ~walkNode=walkPattern,
        collectListPatterns(list{}, pat),
        t,
        comments,
      )
    | Ppat_construct(constr, None) =>
      let (beforeConstr, afterConstr) = partitionLeadingTrailing(comments, constr.loc)

      attach(t.leading, constr.loc, beforeConstr)
      attach(t.trailing, constr.loc, afterConstr)
    | Ppat_construct(constr, Some(pat)) =>
      let (leading, trailing) = partitionLeadingTrailing(comments, constr.loc)
      attach(t.leading, constr.loc, leading)
      let (leading, inside, trailing) = partitionByLoc(trailing, pat.ppat_loc)
      attach(t.leading, pat.ppat_loc, leading)
      walkPattern(pat, t, inside)
      attach(t.trailing, pat.ppat_loc, trailing)
    | Ppat_variant(_label, None) => ()
    | Ppat_variant(_label, Some(pat)) => walkPattern(pat, t, comments)
    | Ppat_type(_) => ()
    | Ppat_record(recordRows, _) => walkList(~getLoc=((longidentLoc, pattern): (
        Asttypes.loc<Longident.t>,
        Parsetree.pattern,
      )) => {
        ...longidentLoc.loc,
        loc_end: pattern.Parsetree.ppat_loc.loc_end,
      }, ~walkNode=walkPatternRecordRow, recordRows, t, comments)
    | Ppat_or(pattern1, pattern2) =>
      let (beforePattern1, insidePattern1, afterPattern1) = partitionByLoc(
        comments,
        pattern1.ppat_loc,
      )

      attach(t.leading, pattern1.ppat_loc, beforePattern1)
      walkPattern(pattern1, t, insidePattern1)
      let (afterPattern1, rest) = partitionAdjacentTrailing(pattern1.ppat_loc, afterPattern1)

      attach(t.trailing, pattern1.ppat_loc, afterPattern1)
      let (beforePattern2, insidePattern2, afterPattern2) = partitionByLoc(rest, pattern2.ppat_loc)

      attach(t.leading, pattern2.ppat_loc, beforePattern2)
      walkPattern(pattern2, t, insidePattern2)
      attach(t.trailing, pattern2.ppat_loc, afterPattern2)
    | Ppat_constraint(pattern, typ) =>
      let (beforePattern, insidePattern, afterPattern) = partitionByLoc(comments, pattern.ppat_loc)

      attach(t.leading, pattern.ppat_loc, beforePattern)
      walkPattern(pattern, t, insidePattern)
      let (afterPattern, rest) = partitionAdjacentTrailing(pattern.ppat_loc, afterPattern)

      attach(t.trailing, pattern.ppat_loc, afterPattern)
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typ.ptyp_loc)

      attach(t.leading, typ.ptyp_loc, beforeTyp)
      walkTypExpr(typ, t, insideTyp)
      attach(t.trailing, typ.ptyp_loc, afterTyp)
    | Ppat_lazy(pattern) | Ppat_exception(pattern) =>
      let (leading, inside, trailing) = partitionByLoc(comments, pattern.ppat_loc)
      attach(t.leading, pattern.ppat_loc, leading)
      walkPattern(pattern, t, inside)
      attach(t.trailing, pattern.ppat_loc, trailing)
    | Ppat_unpack(stringLoc) =>
      let (leading, trailing) = partitionLeadingTrailing(comments, stringLoc.loc)
      attach(t.leading, stringLoc.loc, leading)
      attach(t.trailing, stringLoc.loc, trailing)
    | Ppat_extension(extension) => walkExtension(extension, t, comments)
    | _ => ()
    }
  }

  /* name: firstName */
  and walkPatternRecordRow = (row, t, comments) =>
    switch row {
    /* punned {x} */
    | (
        {Location.txt: Longident.Lident(ident), loc: longidentLoc},
        {Parsetree.ppat_desc: Ppat_var({txt, _})},
      ) when ident == txt =>
      let (beforeLbl, afterLbl) = partitionLeadingTrailing(comments, longidentLoc)

      attach(t.leading, longidentLoc, beforeLbl)
      attach(t.trailing, longidentLoc, afterLbl)
    | (longident, pattern) =>
      let (beforeLbl, afterLbl) = partitionLeadingTrailing(comments, longident.loc)

      attach(t.leading, longident.loc, beforeLbl)
      let (afterLbl, rest) = partitionAdjacentTrailing(longident.loc, afterLbl)
      attach(t.trailing, longident.loc, afterLbl)
      let (leading, inside, trailing) = partitionByLoc(rest, pattern.ppat_loc)
      attach(t.leading, pattern.ppat_loc, leading)
      walkPattern(pattern, t, inside)
      attach(t.trailing, pattern.ppat_loc, trailing)
    }

  and walkTypExpr = (typ, t, comments) =>
    switch typ.Parsetree.ptyp_desc {
    | _ when comments == list{} => ()
    | Ptyp_tuple(typexprs) =>
      walkList(~getLoc=n => n.Parsetree.ptyp_loc, ~walkNode=walkTypExpr, typexprs, t, comments)
    | Ptyp_extension(extension) => walkExtension(extension, t, comments)
    | Ptyp_package(packageType) => walkPackageType(packageType, t, comments)
    | Ptyp_alias(typexpr, _alias) =>
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(comments, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
    | Ptyp_poly(strings, typexpr) =>
      let comments = visitListButContinueWithRemainingComments(
        ~getLoc=n => n.Asttypes.loc,
        ~walkNode=(longident, t, comments) => {
          let (beforeLongident, afterLongident) = partitionLeadingTrailing(comments, longident.loc)
          attach(t.leading, longident.loc, beforeLongident)
          attach(t.trailing, longident.loc, afterLongident)
        },
        ~newlineDelimited=false,
        strings,
        t,
        comments,
      )

      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(comments, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
    | Ptyp_constr(longident, typexprs) =>
      let (beforeLongident, _afterLongident) = partitionLeadingTrailing(comments, longident.loc)
      let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, comments)
      attach(t.leading, longident.loc, beforeLongident)
      attach(t.trailing, longident.loc, afterLongident)
      walkList(~getLoc=n => n.Parsetree.ptyp_loc, ~walkNode=walkTypExpr, typexprs, t, rest)
    | Ptyp_arrow(_) =>
      let (_, parameters, typexpr) = arrowType(typ)
      let comments = walkTypeParameters(parameters, t, comments)
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(comments, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
    | Ptyp_object(fields, _) => walkTypObjectFields(fields, t, comments)
    | _ => ()
    }

  and walkTypObjectFields = (fields, t, comments) => walkList(~getLoc=field =>
      switch field {
      | Parsetree.Otag(lbl, _, typ) => {...lbl.loc, loc_end: typ.ptyp_loc.loc_end}
      | _ => Location.none
      }
    , ~walkNode=walkTypObjectField, fields, t, comments)

  and walkTypObjectField = (field, t, comments) =>
    switch field {
    | Otag(lbl, _, typexpr) =>
      let (beforeLbl, afterLbl) = partitionLeadingTrailing(comments, lbl.loc)
      attach(t.leading, lbl.loc, beforeLbl)
      let (afterLbl, rest) = partitionAdjacentTrailing(lbl.loc, afterLbl)
      attach(t.trailing, lbl.loc, afterLbl)
      let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typexpr.ptyp_loc)
      attach(t.leading, typexpr.ptyp_loc, beforeTyp)
      walkTypExpr(typexpr, t, insideTyp)
      attach(t.trailing, typexpr.ptyp_loc, afterTyp)
    | _ => ()
    }

  and walkTypeParameters = (typeParameters, t, comments) =>
    visitListButContinueWithRemainingComments(
      ~getLoc=((_, _, typexpr)) => typexpr.Parsetree.ptyp_loc,
      ~walkNode=walkTypeParameter,
      ~newlineDelimited=false,
      typeParameters,
      t,
      comments,
    )

  and walkTypeParameter = ((_attrs, _lbl, typexpr), t, comments) => {
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(comments, typexpr.ptyp_loc)
    attach(t.leading, typexpr.ptyp_loc, beforeTyp)
    walkTypExpr(typexpr, t, insideTyp)
    attach(t.trailing, typexpr.ptyp_loc, afterTyp)
  }

  and walkPackageType = (packageType, t, comments) => {
    let (longident, packageConstraints) = packageType
    let (beforeLongident, afterLongident) = partitionLeadingTrailing(comments, longident.loc)
    attach(t.leading, longident.loc, beforeLongident)
    let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, afterLongident)
    attach(t.trailing, longident.loc, afterLongident)
    walkPackageConstraints(packageConstraints, t, rest)
  }

  and walkPackageConstraints = (packageConstraints, t, comments) => walkList(~getLoc=((
      longident,
      typexpr,
    )) => {
      ...longident.Asttypes.loc,
      loc_end: typexpr.Parsetree.ptyp_loc.loc_end,
    }, ~walkNode=walkPackageConstraint, packageConstraints, t, comments)

  and walkPackageConstraint = (packageConstraint, t, comments) => {
    let (longident, typexpr) = packageConstraint
    let (beforeLongident, afterLongident) = partitionLeadingTrailing(comments, longident.loc)
    attach(t.leading, longident.loc, beforeLongident)
    let (afterLongident, rest) = partitionAdjacentTrailing(longident.loc, afterLongident)
    attach(t.trailing, longident.loc, afterLongident)
    let (beforeTyp, insideTyp, afterTyp) = partitionByLoc(rest, typexpr.ptyp_loc)
    attach(t.leading, typexpr.ptyp_loc, beforeTyp)
    walkTypExpr(typexpr, t, insideTyp)
    attach(t.trailing, typexpr.ptyp_loc, afterTyp)
  }

  and walkExtension = (extension, t, comments) => {
    let (id, payload) = extension
    let (beforeId, afterId) = partitionLeadingTrailing(comments, id.loc)
    attach(t.leading, id.loc, beforeId)
    let (afterId, rest) = partitionAdjacentTrailing(id.loc, afterId)
    attach(t.trailing, id.loc, afterId)
    walkPayload(payload, t, rest)
  }

  and walkAttribute = ((id, payload), t, comments) => {
    let (beforeId, afterId) = partitionLeadingTrailing(comments, id.loc)
    attach(t.leading, id.loc, beforeId)
    let (afterId, rest) = partitionAdjacentTrailing(id.loc, afterId)
    attach(t.trailing, id.loc, afterId)
    walkPayload(payload, t, rest)
  }

  and walkPayload = (payload, t, comments) =>
    switch payload {
    | PStr(s) => walkStructure(s, t, comments)
    | _ => ()
    }
}

module Printer = {
  let addParens = doc =>
    Doc.group(
      Doc.concat(list{
        Doc.lparen,
        Doc.indent(Doc.concat(list{Doc.softLine, doc})),
        Doc.softLine,
        Doc.rparen,
      }),
    )

  let addBraces = doc => Doc.group(Doc.concat(list{Doc.lbrace, doc, Doc.rbrace}))

  let getFirstLeadingComment = (tbl, loc) =>
    switch Hashtbl.find(tbl.CommentTable.leading, loc) {
    | list{comment, ..._} => Some(comment)
    | list{} => None
    | exception Not_found => None
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
        let trailingSpace = if String.length(line) > 0 {
          Doc.space
        } else {
          Doc.nil
        }
        List.rev(list{trailingSpace, doc, ...acc}) |> Doc.concat
      | list{line, ...lines} =>
        let line = String.trim(line)
        let len = String.length(line)
        if len > 0 && (@doesNotRaise String.get)(line, 0) === '*' {
          let doc = Doc.text(" " ++ String.trim(line))
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
        if String.length(firstLine) > 0 && !String.equal(firstLine, "*") {
          Doc.space
        } else {
          Doc.nil
        },
        indentStars(rest, list{Doc.hardLine, Doc.text(firstLine)}),
        Doc.text("*/"),
      })
    }
  }

  let printTrailingComment = (nodeLoc: Location.t, comment) => {
    let singleLine = Comment.isSingleLineComment(comment)
    let content = {
      let txt = Comment.txt(comment)
      if singleLine {
        Doc.text("// " ++ String.trim(txt))
      } else {
        printMultilineCommentContent(txt)
      }
    }

    let diff = {
      let cmtStart = Comment.loc(comment).loc_start
      let prevTokEndPos = Comment.prevTokEndPos(comment)
      cmtStart.pos_lnum - prevTokEndPos.pos_lnum
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
        Doc.text("// " ++ String.trim(txt))
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
    let rec loop = (acc, comments) =>
      switch comments {
      | list{} => Doc.concat(List.rev(acc))
      | list{comment, ...comments} =>
        let cmtDoc = printTrailingComment(loc, comment)
        loop(list{cmtDoc, ...acc}, comments)
      }

    switch Hashtbl.find(tbl, loc) {
    | exception Not_found => node
    | list{} => node
    | list{_first, ..._} as comments =>
      /* Remove comments from tbl: Some ast nodes have the same location.
       * We only want to print comments once */
      Hashtbl.remove(tbl, loc)
      let cmtsDoc = loop(list{}, comments)
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

  let classifyIdentContent = (~allowUident=false, txt) => {
    let len = String.length(txt)
    let rec go = i =>
      if i === len {
        NormalIdent
      } else {
        let c = String.unsafe_get(txt, i)
        if (
          i === 0 &&
            !(
              (allowUident && (c >= 'A' && c <= 'Z')) ||
                ((c >= 'a' && c <= 'z') ||
                (c == '_' || (c >= '0' && c <= '9')))
            )
        ) {
          ExoticIdent
        } else if (
          !(
            (c >= 'a' && c <= 'z') ||
              ((c >= 'A' && c <= 'Z') ||
              (c == '\'' || (c == '_' || c >= '0' && c <= '9')))
          )
        ) {
          ExoticIdent
        } else {
          go(i + 1)
        }
      }

    if Token.isKeywordTxt(txt) && txt != "list" {
      ExoticIdent
    } else {
      go(0)
    }
  }

  let printIdentLike = (~allowUident=?, txt) =>
    switch classifyIdentContent(~allowUident?, txt) {
    | ExoticIdent => Doc.concat(list{Doc.text("\\\""), Doc.text(txt), Doc.text("\"")})
    | NormalIdent => Doc.text(txt)
    }

  let printLident = l =>
    switch l {
    | Longident.Lident(txt) => printIdentLike(txt)
    | Longident.Ldot(path, txt) =>
      let txts = Longident.flatten(path)
      Doc.concat(list{
        Doc.join(~sep=Doc.dot, List.map(Doc.text, txts)),
        Doc.dot,
        printIdentLike(txt),
      })
    | _ => Doc.text("printLident: Longident.Lapply is not supported")
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

  let printConstant = c =>
    switch c {
    | Parsetree.Pconst_integer(s, suffix) =>
      switch suffix {
      | Some(c) => Doc.text(s ++ Char.escaped(c))
      | None => Doc.text(s)
      }
    | Pconst_string(txt, None) => Doc.text("\"" ++ (txt ++ "\""))
    | Pconst_string(txt, Some(prefix)) =>
      Doc.concat(list{
        if prefix == "" {
          Doc.nil
        } else {
          Doc.text(prefix)
        },
        Doc.text("`" ++ (txt ++ "`")),
      })
    | Pconst_float(s, _) => Doc.text(s)
    | Pconst_char(c) => Doc.text("'" ++ (Char.escaped(c) ++ "'"))
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

      Doc.concat(list{printAttributes(attrs), exprDoc})
    | Pstr_attribute(attr) =>
      Doc.concat(list{Doc.text("@"), printAttributeWithComments(attr, cmtTbl)})
    | Pstr_extension(extension, attrs) =>
      Doc.concat(list{
        printAttributes(attrs),
        Doc.concat(list{printExtensionWithComments(~atModuleLvl=true, extension, cmtTbl)}),
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
        printAttributes(~loc=te.ptyext_path.loc, te.ptyext_attributes),
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
      printAttributes(~loc=moduleBinding.pmb_name.loc, moduleBinding.pmb_attributes),
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
      printAttributes(modTypeDecl.pmtd_attributes),
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
        printAttributes(~loc=longident.loc, modType.pmty_attributes),
        printLongidentLocation(longident, cmtTbl),
      })
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
      Doc.concat(list{printAttributes(modType.pmty_attributes), signatureDoc})
    | Pmty_functor(_) =>
      let (parameters, returnType) = ParsetreeViewer.functorType(modType)
      let parametersDoc = switch parameters {
      | list{} => Doc.nil
      | list{(attrs, {Location.txt: "_", loc}, Some(modType))} =>
        let cmtLoc = {...loc, loc_end: modType.Parsetree.pmty_loc.loc_end}

        let attrs = switch attrs {
        | list{} => Doc.nil
        | attrs =>
          Doc.concat(list{Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)), Doc.line})
        }
        let doc = Doc.concat(list{attrs, printModType(modType, cmtTbl)})
        printComments(doc, cmtTbl, cmtLoc)
      | params =>
        Doc.group(
          Doc.concat(list{
            Doc.lparen,
            Doc.indent(
              Doc.concat(list{
                Doc.softLine,
                Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(((
                    attrs,
                    lbl,
                    modType,
                  )) => {
                    let cmtLoc = switch modType {
                    | None => lbl.Asttypes.loc
                    | Some(modType) => {
                        ...lbl.Asttypes.loc,
                        loc_end: modType.Parsetree.pmty_loc.loc_end,
                      }
                    }

                    let attrs = switch attrs {
                    | list{} => Doc.nil
                    | attrs =>
                      Doc.concat(list{
                        Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
                        Doc.line,
                      })
                    }
                    let lblDoc = if lbl.Location.txt == "_" {
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
                  }, params)),
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
    | Pmty_extension(extension) => printExtensionWithComments(~atModuleLvl=false, extension, cmtTbl)
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
        printAttributes(modType.pmty_attributes)
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
    | Psig_attribute(attr) =>
      Doc.concat(list{Doc.text("@"), printAttributeWithComments(attr, cmtTbl)})
    | Psig_extension(extension, attrs) =>
      Doc.concat(list{
        printAttributes(attrs),
        Doc.concat(list{printExtensionWithComments(~atModuleLvl=true, extension, cmtTbl)}),
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
      printAttributes(~loc=md.pmd_name.loc, md.pmd_attributes),
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
      printAttributes(~loc=md.pmd_name.loc, md.pmd_attributes),
      Doc.text("module "),
      printComments(Doc.text(md.pmd_name.txt), cmtTbl, md.pmd_name.loc),
      body,
    })
  }

  and printOpenDescription = (openDescription: Parsetree.open_description, p) =>
    Doc.concat(list{
      printAttributes(openDescription.popen_attributes),
      Doc.text("open"),
      switch openDescription.popen_override {
      | Asttypes.Fresh => Doc.space
      | Asttypes.Override => Doc.text("! ")
      },
      printLongidentLocation(openDescription.popen_lid, p),
    })

  and printIncludeDescription = (includeDescription: Parsetree.include_description, cmtTbl) =>
    Doc.concat(list{
      printAttributes(includeDescription.pincl_attributes),
      Doc.text("include "),
      printModType(includeDescription.pincl_mod, cmtTbl),
    })

  and printIncludeDeclaration = (includeDeclaration: Parsetree.include_declaration, cmtTbl) => {
    let isJsFfiImport = List.exists(attr =>
      switch attr {
      | ({Location.txt: "ns.jsFfi"}, _) => true
      | _ => false
      }
    , includeDeclaration.pincl_attributes)

    if isJsFfiImport {
      printJsFfiImportDeclaration(includeDeclaration, cmtTbl)
    } else {
      Doc.concat(list{
        printAttributes(includeDeclaration.pincl_attributes),
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
    }
  }

  and printJsFfiImport = (valueDescription: Parsetree.value_description, cmtTbl) => {
    let attrs = List.filter(attr =>
      switch attr {
      | ({Location.txt: "val" | "genType.import" | "scope"}, _) => false
      | _ => true
      }
    , valueDescription.pval_attributes)
    let (ident, alias) = switch valueDescription.pval_prim {
    | list{primitive, ..._} =>
      if primitive != valueDescription.pval_name.txt {
        (
          printIdentLike(primitive),
          Doc.concat(list{Doc.text(" as "), printIdentLike(valueDescription.pval_name.txt)}),
        )
      } else {
        (printIdentLike(primitive), Doc.nil)
      }
    | _ => (printIdentLike(valueDescription.pval_name.txt), Doc.nil)
    }

    Doc.concat(list{
      printAttributes(~loc=valueDescription.pval_name.loc, attrs),
      ident,
      alias,
      Doc.text(": "),
      printTypExpr(valueDescription.pval_type, cmtTbl),
    })
  }

  and printJsFfiImportScope = (scope: ParsetreeViewer.jsImportScope) =>
    switch scope {
    | JsGlobalImport => Doc.nil
    | JsModuleImport(modName) =>
      Doc.concat(list{Doc.text(" from "), Doc.doubleQuote, Doc.text(modName), Doc.doubleQuote})
    | JsScopedImport(idents) =>
      Doc.concat(list{Doc.text(" from "), Doc.join(~sep=Doc.dot, List.map(Doc.text, idents))})
    }

  and printJsFfiImportDeclaration = (includeDeclaration: Parsetree.include_declaration, cmtTbl) => {
    let attrs = List.filter(attr =>
      switch attr {
      | ({Location.txt: "ns.jsFfi"}, _) => false
      | _ => true
      }
    , includeDeclaration.pincl_attributes)

    let imports = ParsetreeViewer.extractValueDescriptionFromModExpr(includeDeclaration.pincl_mod)
    let scope = switch imports {
    | list{vd, ..._} => ParsetreeViewer.classifyJsImport(vd)
    | list{} => ParsetreeViewer.JsGlobalImport
    }

    let scopeDoc = printJsFfiImportScope(scope)
    Doc.group(
      Doc.concat(list{
        printAttributes(attrs),
        Doc.text("import "),
        Doc.group(
          Doc.concat(list{
            Doc.lbrace,
            Doc.indent(
              Doc.concat(list{
                Doc.softLine,
                Doc.join(
                  ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                  List.map(vd => printJsFfiImport(vd, cmtTbl), imports),
                ),
              }),
            ),
            Doc.trailingComma,
            Doc.softLine,
            Doc.rbrace,
          }),
        ),
        scopeDoc,
      }),
    )
  }

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

    Doc.group(
      Doc.concat(list{
        printAttributes(valueDescription.pval_attributes),
        Doc.text(
          if isExternal {
            "external "
          } else {
            "let "
          },
        ),
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
    let (hasGenType, attrs) = ParsetreeViewer.splitGenTypeAttr(td.ptype_attributes)
    let attrs = printAttributes(~loc=td.ptype_loc, attrs)
    let prefix = if i > 0 {
      Doc.concat(list{
        Doc.text("and "),
        if hasGenType {
          Doc.text("export ")
        } else {
          Doc.nil
        },
      })
    } else {
      Doc.concat(list{
        Doc.text(
          if hasGenType {
            "export type "
          } else {
            "type "
          },
        ),
        recFlag,
      })
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
    let (hasGenType, attrs) = ParsetreeViewer.splitGenTypeAttr(td.ptype_attributes)
    let attrs = printAttributes(~loc=td.ptype_loc, attrs)
    let prefix = if i > 0 {
      Doc.concat(list{
        Doc.text("and "),
        if hasGenType {
          Doc.text("export ")
        } else {
          Doc.nil
        },
      })
    } else {
      Doc.concat(list{
        Doc.text(
          if hasGenType {
            "export type "
          } else {
            "type "
          },
        ),
        recFlag,
      })
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
    let attrs = printAttributes(cd.pcd_attributes)
    let bar = if i > 0 {
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
    let attrs = printAttributes(~loc=ld.pld_name.loc, ld.pld_attributes)
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
    | Ptyp_var(var) => Doc.concat(list{Doc.text("'"), printIdentLike(var)})
    | Ptyp_extension(extension) => printExtensionWithComments(~atModuleLvl=false, extension, cmtTbl)
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

      Doc.concat(list{
        typ,
        Doc.text(" as "),
        Doc.concat(list{Doc.text("'"), printIdentLike(alias)}),
      })
    | Ptyp_constr(
        {txt: Longident.Ldot(Longident.Lident("Js"), "t")},
        list{{ptyp_desc: Ptyp_object(_fields, _openFlag)} as typ},
      ) =>
      let bsObject = printTypExpr(typ, cmtTbl)
      switch typExpr.ptyp_attributes {
      | list{} => bsObject
      | attrs =>
        Doc.concat(list{
          Doc.group(Doc.join(~sep=Doc.line, List.map(printAttribute, attrs))),
          Doc.space,
          printTypExpr(typ, cmtTbl),
        })
      }
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
      | list{{
          Parsetree.ptyp_desc:
            Ptyp_constr(
              {txt: Longident.Ldot(Longident.Lident("Js"), "t")},
              list{{ptyp_desc: Ptyp_object(fields, openFlag)}},
            ),
        }} =>
        Doc.concat(list{
          constrName,
          Doc.lessThan,
          printBsObjectSugar(~inline=true, fields, openFlag, cmtTbl),
          Doc.greaterThan,
        })
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
      | list{(list{}, Nolabel, n)} when !isUncurried =>
        let hasAttrsBefore = !(attrs == list{})
        let attrs = if hasAttrsBefore {
          Doc.concat(list{
            Doc.join(~sep=Doc.line, List.map(printAttribute, attrsBefore)),
            Doc.space,
          })
        } else {
          Doc.nil
        }

        let typDoc = {
          let doc = printTypExpr(n, cmtTbl)
          switch n.ptyp_desc {
          | Ptyp_arrow(_) | Ptyp_tuple(_) => addParens(doc)
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
        let attrs = switch attrs {
        | list{} => Doc.nil
        | attrs =>
          Doc.concat(list{Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)), Doc.space})
        }

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
    | Ptyp_object(fields, openFlag) => printBsObjectSugar(~inline=false, fields, openFlag, cmtTbl)
    | Ptyp_poly(list{}, typ) => printTypExpr(typ, cmtTbl)
    | Ptyp_poly(stringLocs, typ) => Doc.concat(list{Doc.join(~sep=Doc.space, List.map(({
            Location.txt: txt,
            loc,
          }) => {
            let doc = Doc.concat(list{Doc.text("'"), Doc.text(txt)})
            printComments(doc, cmtTbl, loc)
          }, stringLocs)), Doc.dot, Doc.space, printTypExpr(typ, cmtTbl)})
    | Ptyp_package(packageType) =>
      printPackageType(~printModuleKeywordAndParens=true, packageType, cmtTbl)
    | Ptyp_class(_) => Doc.text("classes are not supported in types")
    | Ptyp_variant(rowFields, closedFlag, labelsOpt) =>
      let printRowField = x =>
        switch x {
        | Parsetree.Rtag({txt}, attrs, true, list{}) =>
          Doc.concat(list{
            printAttributes(attrs),
            Doc.concat(list{Doc.text("#"), printIdentLike(~allowUident=true, txt)}),
          })
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
              printAttributes(attrs),
              Doc.concat(list{Doc.text("#"), printIdentLike(~allowUident=true, txt)}),
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
        Doc.concat(list{Doc.text("| "), cases})
      }
      let openingSymbol = if closedFlag == Open {
        Doc.greaterThan
      } else if labelsOpt == None {
        Doc.nil
      } else {
        Doc.lessThan
      }
      let hasLabels = labelsOpt != None && labelsOpt != Some(list{})
      let labels = switch labelsOpt {
      | None | Some(list{}) => Doc.nil
      | Some(labels) =>
        Doc.concat(
          List.map(
            label =>
              Doc.concat(list{Doc.line, Doc.text("#"), printIdentLike(~allowUident=true, label)}),
            labels,
          ),
        )
      }

      let closingSymbol = if hasLabels {
        Doc.text(" >")
      } else {
        Doc.nil
      }
      Doc.group(
        Doc.concat(list{
          Doc.lbracket,
          openingSymbol,
          Doc.line,
          cases,
          closingSymbol,
          labels,
          Doc.line,
          Doc.rbracket,
        }),
      )
    }

    let shouldPrintItsOwnAttributes = switch typExpr.ptyp_desc {
    | Ptyp_arrow(_) /* es6 arrow types print their own attributes */
    | Ptyp_constr({txt: Longident.Ldot(Longident.Lident("Js"), "t")}, _) => true
    | _ => false
    }

    let doc = switch typExpr.ptyp_attributes {
    | list{_, ..._} as attrs when !shouldPrintItsOwnAttributes =>
      Doc.group(Doc.concat(list{printAttributes(attrs), renderedType}))
    | _ => renderedType
    }

    printComments(doc, cmtTbl, typExpr.ptyp_loc)
  }

  and printBsObjectSugar = (~inline, fields, openFlag, cmtTbl) => {
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
        | Open => Doc.dotdot
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
        printAttributes(~loc=labelLoc.loc, attrs),
        lbl,
        Doc.text(": "),
        printTypExpr(typ, cmtTbl),
      })
      let cmtLoc = {...labelLoc.loc, loc_end: typ.ptyp_loc.loc_end}
      printComments(doc, cmtTbl, cmtLoc)
    | _ => Doc.nil
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
    let attrs = switch attrs {
    | list{} => Doc.nil
    | attrs => Doc.concat(list{Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)), Doc.line})
    }
    let label = switch lbl {
    | Asttypes.Nolabel => Doc.nil
    | Labelled(lbl) => Doc.concat(list{Doc.text("~"), printIdentLike(lbl), Doc.text(": ")})
    | Optional(lbl) => Doc.concat(list{Doc.text("~"), printIdentLike(lbl), Doc.text(": ")})
    }

    let optionalIndicator = switch lbl {
    | Asttypes.Nolabel | Labelled(_) => Doc.nil
    | Optional(_lbl) => Doc.text("=?")
    }

    let doc = Doc.group(
      Doc.concat(list{uncurried, attrs, label, printTypExpr(typ, cmtTbl), optionalIndicator}),
    )
    printComments(doc, cmtTbl, typ.ptyp_loc)
  }

  and printValueBinding = (~recFlag, vb, cmtTbl, i) => {
    let (hasGenType, attrs) = ParsetreeViewer.splitGenTypeAttr(vb.pvb_attributes)
    let attrs = printAttributes(~loc=vb.pvb_pat.ppat_loc, attrs)
    let header = if i === 0 {
      Doc.concat(list{
        if hasGenType {
          Doc.text("export ")
        } else {
          Doc.text("let ")
        },
        recFlag,
      })
    } else {
      Doc.concat(list{
        Doc.text("and "),
        if hasGenType {
          Doc.text("export ")
        } else {
          Doc.nil
        },
      })
    }

    switch vb {
    | {
        pvb_pat: {ppat_desc: Ppat_constraint(pattern, {ptyp_desc: Ptyp_poly(_)})},
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
      | _ => Doc.nil
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

      if ParsetreeViewer.isPipeExpr(vb.pvb_expr) {
        Doc.customLayout(list{
          Doc.group(
            Doc.concat(list{
              attrs,
              header,
              printPattern(vb.pvb_pat, cmtTbl),
              Doc.text(" ="),
              Doc.space,
              printedExpr,
            }),
          ),
          Doc.group(
            Doc.concat(list{
              attrs,
              header,
              printPattern(vb.pvb_pat, cmtTbl),
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
              pexp_attributes: list{({Location.txt: "res.ternary"}, _)},
              pexp_desc: Pexp_ifthenelse(ifExpr, _, _),
            } =>
            ParsetreeViewer.isBinaryExpression(ifExpr) ||
            ParsetreeViewer.hasAttributes(ifExpr.pexp_attributes)
          | {pexp_desc: Pexp_newtype(_)} => false
          | e =>
            ParsetreeViewer.hasAttributes(e.pexp_attributes) || ParsetreeViewer.isArrayAccess(e)
          }
        }

        Doc.group(
          Doc.concat(list{
            attrs,
            header,
            printPattern(vb.pvb_pat, cmtTbl),
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

  and printExtensionWithComments = (~atModuleLvl, (stringLoc, payload), cmtTbl) => {
    let extName = {
      let doc = Doc.concat(list{
        Doc.text("%"),
        if atModuleLvl {
          Doc.text("%")
        } else {
          Doc.nil
        },
        Doc.text(stringLoc.Location.txt),
      })
      printComments(doc, cmtTbl, stringLoc.Location.loc)
    }

    switch payload {
    | Parsetree.PStr(list{{pstr_desc: Pstr_eval(expr, attrs)}}) =>
      let exprDoc = printExpressionWithComments(expr, cmtTbl)
      let needsParens = switch attrs {
      | list{} => false
      | _ => true
      }
      Doc.group(
        Doc.concat(list{
          extName,
          addParens(
            Doc.concat(list{
              printAttributes(attrs),
              if needsParens {
                addParens(exprDoc)
              } else {
                exprDoc
              },
            }),
          ),
        }),
      )
    | _ => extName
    }
  }

  and printPattern = (p: Parsetree.pattern, cmtTbl) => {
    let patternWithoutAttributes = switch p.ppat_desc {
    | Ppat_any => Doc.text("_")
    | Ppat_var(var) => printIdentLike(var.txt)
    | Ppat_constant(c) => printConstant(c)
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
      Doc.concat(list{Doc.text("list["), printCommentsInside(cmtTbl, p.ppat_loc), Doc.rbracket})
    | Ppat_construct({txt: Longident.Lident("::")}, _) =>
      let (patterns, tail) = ParsetreeViewer.collectPatternsFromListConstruct(list{}, p)
      let shouldHug = switch (patterns, tail) {
      | (list{pat}, {ppat_desc: Ppat_construct({txt: Longident.Lident("[]")}, _)})
        when ParsetreeViewer.isHuggablePattern(pat) => true
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
          Doc.text("list["),
          if shouldHug {
            children
          } else {
            Doc.concat(list{
              Doc.indent(children),
              Doc.ifBreaks(Doc.text(","), Doc.nil),
              Doc.softLine,
            })
          },
          Doc.rbracket,
        }),
      )
    | Ppat_construct(constrName, constructorArgs) =>
      let constrName = printLongident(constrName.txt)
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
    | Ppat_variant(label, None) =>
      Doc.concat(list{Doc.text("#"), printIdentLike(~allowUident=true, label)})
    | Ppat_variant(label, variantArgs) =>
      let variantName = Doc.concat(list{Doc.text("#"), printIdentLike(~allowUident=true, label)})
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
    | Ppat_type(ident) => Doc.concat(list{Doc.text("##"), printIdentPath(ident, cmtTbl)})
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
      Doc.group(Doc.concat(docs))
    | Ppat_extension(ext) => printExtensionWithComments(~atModuleLvl=false, ext, cmtTbl)
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
    | attrs => Doc.group(Doc.concat(list{printAttributes(attrs), patternWithoutAttributes}))
    }

    printComments(doc, cmtTbl, p.ppat_loc)
  }

  and printPatternRecordRow = (row, cmtTbl) =>
    switch row {
    /* punned {x} */
    | (
        {Location.txt: Longident.Lident(ident)} as longident,
        {Parsetree.ppat_desc: Ppat_var({txt, _})},
      ) when ident == txt =>
      printLidentPath(longident, cmtTbl)
    | (longident, pattern) =>
      let locForComments = {
        ...longident.loc,
        loc_end: pattern.Parsetree.ppat_loc.loc_end,
      }
      let doc = Doc.group(
        Doc.concat(list{
          printLidentPath(longident, cmtTbl),
          Doc.text(": "),
          Doc.indent(Doc.concat(list{Doc.softLine, printPattern(pattern, cmtTbl)})),
        }),
      )
      printComments(doc, cmtTbl, locForComments)
    }

  and printExpressionWithComments = (expr, cmtTbl) => {
    let doc = printExpression(expr, cmtTbl)
    printComments(doc, cmtTbl, expr.Parsetree.pexp_loc)
  }

  and printExpression = (e: Parsetree.expression, cmtTbl) => {
    let printedExpression = switch e.pexp_desc {
    | Parsetree.Pexp_constant(c) => printConstant(c)
    | Pexp_construct(_) when ParsetreeViewer.hasJsxAttribute(e.pexp_attributes) =>
      printJsxFragment(e, cmtTbl)
    | Pexp_construct({txt: Longident.Lident("()")}, _) => Doc.text("()")
    | Pexp_construct({txt: Longident.Lident("[]")}, _) =>
      Doc.concat(list{Doc.text("list["), printCommentsInside(cmtTbl, e.pexp_loc), Doc.rbracket})
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
          Doc.text("list["),
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
          Doc.rbracket,
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
      let variantName = Doc.concat(list{Doc.text("#"), printIdentLike(~allowUident=true, label)})
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
                List.map(row => printRecordRow(row, cmtTbl), rows),
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
          {txt: "obj"},
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
      | extension => printExtensionWithComments(~atModuleLvl=false, extension, cmtTbl)
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
    | Pexp_ifthenelse(_ifExpr, _thenExpr, _elseExpr) =>
      if ParsetreeViewer.isTernaryExpr(e) {
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
          printAttributes(attrs),
          if needsParens {
            addParens(ternaryDoc)
          } else {
            ternaryDoc
          },
        })
      } else {
        let (ifs, elseExpr) = ParsetreeViewer.collectIfExpressions(e)
        let ifDocs = Doc.join(~sep=Doc.space, List.mapi((i, (ifExpr, thenExpr)) => {
            let ifTxt = if i > 0 {
              Doc.text("else if ")
            } else {
              Doc.text("if ")
            }
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
          }, ifs))
        let elseDoc = switch elseExpr {
        | None => Doc.nil
        | Some(expr) =>
          Doc.concat(list{Doc.text(" else "), printExpressionBlock(~braces=true, expr, cmtTbl)})
        }

        Doc.concat(list{printAttributes(e.pexp_attributes), ifDocs, elseDoc})
      }
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
    | Pexp_letexception(_extensionConstructor, _expr) =>
      printExpressionBlock(~braces=true, e, cmtTbl)
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
    | Pexp_open(_overrideFlag, _longidentLoc, _expr) =>
      printExpressionBlock(~braces=true, e, cmtTbl)
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
        ~inCallback=false,
        ~uncurried,
        ~hasConstraint,
        parameters,
        cmtTbl,
      )

      let returnExprDoc = {
        let (optBraces, _) = ParsetreeViewer.processBracesAttr(returnExpr)
        let shouldInline = switch (returnExpr.pexp_desc, optBraces) {
        | (_, Some(_)) => true
        | (Pexp_array(_) | Pexp_tuple(_) | Pexp_construct(_, Some(_)) | Pexp_record(_), _) => true
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
      | Some(typ) => Doc.concat(list{Doc.text(": "), printTypExpr(typ, cmtTbl)})
      | _ => Doc.nil
      }

      let attrs = printAttributes(attrs)
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
    | Pexp_function(cases) =>
      Doc.concat(list{Doc.text("x => switch x "), printCases(cases, cmtTbl)})
    | Pexp_coerce(expr, typOpt, typ) =>
      let docExpr = printExpressionWithComments(expr, cmtTbl)
      let docTyp = printTypExpr(typ, cmtTbl)
      let ofType = switch typOpt {
      | None => Doc.nil
      | Some(typ1) => Doc.concat(list{Doc.text(": "), printTypExpr(typ1, cmtTbl)})
      }

      Doc.concat(list{Doc.lparen, docExpr, ofType, Doc.text(" :> "), docTyp, Doc.rparen})
    | Pexp_send(_) => Doc.text("Pexp_send not implemented in printer")
    | Pexp_new(_) => Doc.text("Pexp_new not implemented in printer")
    | Pexp_setinstvar(_) => Doc.text("Pexp_setinstvar not implemented in printer")
    | Pexp_override(_) => Doc.text("Pexp_override not implemented in printer")
    | Pexp_poly(_) => Doc.text("Pexp_poly not implemented in printer")
    | Pexp_object(_) => Doc.text("Pexp_object not implemented in printer")
    }

    let shouldPrintItsOwnAttributes = switch e.pexp_desc {
    | Pexp_apply(_) | Pexp_fun(_) | Pexp_newtype(_) | Pexp_setfield(_) | Pexp_ifthenelse(_) => true
    | Pexp_construct(_) when ParsetreeViewer.hasJsxAttribute(e.pexp_attributes) => true
    | _ => false
    }

    switch e.pexp_attributes {
    | list{} => printedExpression
    | attrs when !shouldPrintItsOwnAttributes =>
      Doc.group(Doc.concat(list{printAttributes(attrs), printedExpression}))
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
      | (Pexp_array(_) | Pexp_tuple(_) | Pexp_construct(_, Some(_)) | Pexp_record(_), _) => true
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
              if inCallback {
                Doc.softLine
              } else {
                Doc.nil
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

    Doc.group(
      Doc.concat(list{
        printAttributes(attrs),
        parametersDoc,
        typConstraintDoc,
        Doc.text(" =>"),
        returnExprDoc,
      }),
    )
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
    | attrs => Doc.group(Doc.concat(list{printAttributes(attrs), doc}))
    }

    printComments(doc, cmtTbl, loc)
  }

  and printTemplateLiteral = (expr, cmtTbl) => {
    let tag = ref("j")
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
        Doc.text(txt)
      | _ =>
        let doc = printExpressionWithComments(expr, cmtTbl)
        Doc.concat(list{Doc.text("${"), doc, Doc.rbrace})
      }
    }

    let content = walkExpr(expr)
    Doc.concat(list{
      if tag.contents == "j" {
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
      | "|." => "->"
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
              pexp_desc:
                Pexp_apply(
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
                let (_, rightAttrs) = ParsetreeViewer.partitionPrinteableAttributes(
                  right.pexp_attributes,
                )

                let doc = printExpressionWithComments(
                  {...right, pexp_attributes: rightAttrs},
                  cmtTbl,
                )

                let doc = if Parens.flattenOperandRhs(parentOperator, right) {
                  Doc.concat(list{Doc.lparen, doc, Doc.rparen})
                } else {
                  doc
                }

                let printeableAttrs = ParsetreeViewer.filterPrinteableAttributes(
                  right.pexp_attributes,
                )

                Doc.concat(list{printAttributes(printeableAttrs), doc})
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
              Doc.concat(list{printAttributes(expr.pexp_attributes), doc})
            }
          | _ => assert false
          }
        } else {
          switch expr.pexp_desc {
          | Pexp_setfield(lhs, field, rhs) =>
            let doc = printSetFieldExpr(
              expr.pexp_attributes,
              lhs,
              field,
              rhs,
              expr.pexp_loc,
              cmtTbl,
            )
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
            | attrs => Doc.group(Doc.concat(list{printAttributes(attrs), doc}))
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
      )
      when !(ParsetreeViewer.isBinaryExpression(lhs) || ParsetreeViewer.isBinaryExpression(rhs)) =>
      let lhsDoc = printOperand(~isLhs=true, lhs, op)
      let rhsDoc = printOperand(~isLhs=false, rhs, op)
      Doc.group(
        Doc.concat(list{
          lhsDoc,
          switch op {
          | "|." => Doc.text("->")
          | "|>" => Doc.text(" |> ")
          | _ => assert false
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
            printBinaryOperator(
              ~inlineRhs=ParsetreeViewer.shouldInlineRhsBinaryExpr(rhs),
              operator,
            ),
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
          printAttributes(expr.pexp_attributes),
          switch Parens.binaryExpr({
            ...expr,
            pexp_attributes: List.filter(attr =>
              switch attr {
              | ({Location.txt: "res.braces"}, _) => false
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
        | Pexp_ident(lident) =>
          printComments(printLongident(lident.txt), cmtTbl, memberExpr.pexp_loc)
        | _ => printExpressionWithComments(memberExpr, cmtTbl)
        }

        Doc.concat(list{Doc.text("\""), memberDoc, Doc.text("\"")})
      }

      Doc.group(
        Doc.concat(list{
          printAttributes(expr.pexp_attributes),
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
      let shouldIndent =
        !ParsetreeViewer.isBracedExpr(rhs) && ParsetreeViewer.isBinaryExpression(rhs)
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
      | attrs => Doc.group(Doc.concat(list{printAttributes(attrs), doc}))
      }
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Ldot(Lident("Array"), "get")})},
        list{(Nolabel, parentExpr), (Nolabel, memberExpr)},
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
          printAttributes(expr.pexp_attributes),
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
            pexp_attributes: list{({Location.txt: "res.ternary"}, _)},
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
          printAttributes(expr.pexp_attributes),
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
    | Pexp_apply({pexp_desc: Pexp_ident(lident)}, args)
      when ParsetreeViewer.isJsxExpression(expr) =>
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

        Doc.concat(list{printAttributes(attrs), callExprDoc, argsDoc})
      } else if ParsetreeViewer.requiresSpecialCallbackPrintingLastArg(args) {
        let argsDoc = printArgumentsWithCallbackInLastPosition(~uncurried, args, cmtTbl)

        Doc.concat(list{printAttributes(attrs), callExprDoc, argsDoc})
      } else {
        let argsDoc = printArguments(~uncurried, args, cmtTbl)
        Doc.concat(list{printAttributes(attrs), callExprDoc, argsDoc})
      }
    | _ => assert false
    }

  and printJsxExpression = (lident, args, cmtTbl) => {
    let name = printJsxName(lident)
    let (formattedProps, children) = printJsxProps(args, cmtTbl)
    /* <div className="test" /> */
    let isSelfClosing = switch children {
    | list{} => true
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
            Doc.indent(Doc.concat(list{Doc.line, printJsxChildren(children, cmtTbl)})),
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
    let (children, _) = ParsetreeViewer.collectListExpressions(expr)
    Doc.group(
      Doc.concat(list{
        opening,
        switch children {
        | list{} => Doc.nil
        | children => Doc.indent(Doc.concat(list{Doc.line, printJsxChildren(children, cmtTbl)}))
        },
        Doc.line,
        closing,
      }),
    )
  }

  and printJsxChildren = (children: list<Parsetree.expression>, cmtTbl) =>
    Doc.group(Doc.join(~sep=Doc.line, List.map(expr => {
          let exprDoc = printExpressionWithComments(expr, cmtTbl)
          switch Parens.jsxChildExpr(expr) {
          | Parenthesized | Braced(_) =>
            /* {(20: int)} make sure that we also protect the expression inside */
            addBraces(
              if Parens.bracedExpr(expr) {
                addParens(exprDoc)
              } else {
                exprDoc
              },
            )
          | Nothing => exprDoc
          }
        }, children)))

  and printJsxProps = (args, cmtTbl) => {
    let rec loop = (props, args) =>
      switch args {
      | list{} => (Doc.nil, list{})
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
          | props =>
            Doc.concat(list{Doc.line, Doc.group(Doc.join(~sep=Doc.line, props |> List.rev))})
          },
        )
        let (children, _) = ParsetreeViewer.collectListExpressions(children)
        (formattedProps, children)
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
          Parsetree.pexp_attributes: list{({Location.txt: "res.namedArgLoc", loc: argLoc}, _)},
          pexp_desc: Pexp_ident({txt: Longident.Lident(ident)}),
        },
      ) when lblTxt == ident /* jsx punning */ =>
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
      ) when lblTxt == ident /* jsx punning when printing from Reason */ =>
      switch lbl {
      | Nolabel => Doc.nil
      | Labelled(_lbl) => printIdentLike(ident)
      | Optional(_lbl) => Doc.concat(list{Doc.question, printIdentLike(ident)})
      }
    | (lbl, expr) =>
      let (argLoc, expr) = switch expr.pexp_attributes {
      | list{({Location.txt: "res.namedArgLoc", loc}, _), ...attrs} => (
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
        let doc = printExpression(expr, cmtTbl)
        switch Parens.jsxPropExpr(expr) {
        | Parenthesized | Braced(_) =>
          /* {(20: int)} make sure that we also protect the expression inside */
          addBraces(
            if Parens.bracedExpr(expr) {
              addParens(doc)
            } else {
              doc
            },
          )
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
    let (callback, printedArgs) = switch args {
    | list{(lbl, expr), ...args} =>
      let lblDoc = switch lbl {
      | Asttypes.Nolabel => Doc.nil
      | Asttypes.Labelled(txt) => Doc.concat(list{Doc.tilde, printIdentLike(txt), Doc.equal})
      | Asttypes.Optional(txt) =>
        Doc.concat(list{Doc.tilde, printIdentLike(txt), Doc.equal, Doc.question})
      }

      let callback = Doc.concat(list{lblDoc, printPexpFun(~inCallback=true, expr, cmtTbl)})
      let printedArgs =
        List.map(arg => printArgument(arg, cmtTbl), args) |> Doc.join(
          ~sep=Doc.concat(list{Doc.comma, Doc.line}),
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
    let breakAllArgs = printArguments(~uncurried, args, cmtTbl)
    Doc.customLayout(list{fitsOnOneLine, breakAllArgs})
  }

  and printArgumentsWithCallbackInLastPosition = (~uncurried, args, cmtTbl) => {
    let rec loop = (acc, args) =>
      switch args {
      | list{} => (Doc.nil, Doc.nil)
      | list{(lbl, expr)} =>
        let lblDoc = switch lbl {
        | Asttypes.Nolabel => Doc.nil
        | Asttypes.Labelled(txt) => Doc.concat(list{Doc.tilde, printIdentLike(txt), Doc.equal})
        | Asttypes.Optional(txt) =>
          Doc.concat(list{Doc.tilde, printIdentLike(txt), Doc.equal, Doc.question})
        }

        let callback = printPexpFun(~inCallback=true, expr, cmtTbl)
        (Doc.concat(List.rev(acc)), Doc.concat(list{lblDoc, callback}))
      | list{arg, ...args} =>
        let argDoc = printArgument(arg, cmtTbl)
        loop(list{Doc.line, Doc.comma, argDoc, ...acc}, args)
      }

    let (printedArgs, callback) = loop(list{}, args)

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
      Doc.softLine,
      printedArgs,
      Doc.breakableGroup(~forceBreak=true, callback),
      Doc.softLine,
      Doc.rparen,
    })

    /* Thing.map(
     *   arg1,
     *   arg2,
     *   arg3,
     *   (param1, parm2) => doStuff(param1, parm2)
     * )
     */
    let breakAllArgs = printArguments(~uncurried, args, cmtTbl)
    Doc.customLayout(list{fitsOnOneLine, arugmentsFitOnOneLine, breakAllArgs})
  }

  and printArguments = (
    ~uncurried,
    args: list<(Asttypes.arg_label, Parsetree.expression)>,
    cmtTbl,
  ) =>
    switch args {
    | list{(Nolabel, {pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, _)})} =>
      if uncurried {
        Doc.text("(.)")
      } else {
        Doc.text("()")
      }
    | list{(Nolabel, arg)} when ParsetreeViewer.isHuggableExpression(arg) =>
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
          Doc.text("(.")
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
          pexp_attributes: list{} | list{({Location.txt: "res.namedArgLoc"}, _)},
        } as argExpr,
      ) when lbl == name && !ParsetreeViewer.isBracedExpr(argExpr) =>
      let loc = switch arg.pexp_attributes {
      | list{({Location.txt: "res.namedArgLoc", loc}, _), ..._} => loc
      | _ => arg.pexp_loc
      }

      let doc = Doc.concat(list{Doc.tilde, printIdentLike(lbl)})
      printComments(doc, cmtTbl, loc)

    /* ~a: int (punned) */
    | (
        Asttypes.Labelled(lbl),
        {
          pexp_desc:
            Pexp_constraint({pexp_desc: Pexp_ident({txt: Longident.Lident(name)})} as argExpr, typ),
          pexp_loc,
          pexp_attributes: (list{} | list{({Location.txt: "res.namedArgLoc"}, _)}) as attrs,
        },
      ) when lbl == name && !ParsetreeViewer.isBracedExpr(argExpr) =>
      let loc = switch attrs {
      | list{({Location.txt: "res.namedArgLoc", loc}, _), ..._} => {
          ...loc,
          loc_end: pexp_loc.loc_end,
        }
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
          pexp_attributes: list{} | list{({Location.txt: "res.namedArgLoc"}, _)},
        },
      ) when lbl == name =>
      let loc = switch arg.pexp_attributes {
      | list{({Location.txt: "res.namedArgLoc", loc}, _), ..._} => loc
      | _ => arg.pexp_loc
      }

      let doc = Doc.concat(list{Doc.tilde, printIdentLike(lbl), Doc.question})
      printComments(doc, cmtTbl, loc)
    | (_lbl, expr) =>
      let (argLoc, expr) = switch expr.pexp_attributes {
      | list{({Location.txt: "res.namedArgLoc", loc}, _), ...attrs} => (
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
      Doc.concat(list{Doc.lbrace, Doc.concat(list{Doc.line, printList(~getLoc=n => {
            ...n.Parsetree.pc_lhs.ppat_loc,
            loc_end: switch ParsetreeViewer.processBracesAttr(n.Parsetree.pc_rhs) {
            | (None, _) => n.pc_rhs.pexp_loc.loc_end
            | (Some({loc}, _), _) => loc.Location.loc_end
            },
          }, ~print=printCase, ~nodes=cases, cmtTbl)}), Doc.line, Doc.rbrace}),
    )

  and printCase = (case: Parsetree.case, cmtTbl) => {
    let rhs = switch case.pc_rhs.pexp_desc {
    | Pexp_let(_) | Pexp_letmodule(_) | Pexp_letexception(_) | Pexp_open(_) | Pexp_sequence(_) =>
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
        Doc.concat(list{Doc.line, Doc.text("when "), printExpressionWithComments(expr, cmtTbl)}),
      )
    }

    let shouldInlineRhs = switch case.pc_rhs.pexp_desc {
    | Pexp_construct({txt: Longident.Lident("()" | "true" | "false")}, _)
    | Pexp_constant(_)
    | Pexp_ident(_) => true
    | _ when ParsetreeViewer.isHuggableRhs(case.pc_rhs) => true
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
      })} when !uncurried =>
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
      })} when !uncurried =>
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
      })} when !uncurried =>
      Doc.text("()")
    /* let f = (~greeting, ~from as hometown, ~x=?) => () */
    | parameters =>
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
          ~sep=Doc.concat(list{
            Doc.comma,
            if inCallback {
              Doc.space
            } else {
              Doc.line
            },
          }),
          List.map(p => printExpFunParameter(p, cmtTbl), parameters),
        ),
      })
      Doc.group(
        Doc.concat(list{
          lparen,
          if shouldHug || inCallback {
            printedParamaters
          } else {
            Doc.indent(printedParamaters)
          },
          if shouldHug || inCallback {
            Doc.nil
          } else {
            Doc.concat(list{Doc.trailingComma, Doc.softLine})
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
          printAttributes(attrs),
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
      let attrs = printAttributes(attrs)
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
            ppat_attributes: list{} | list{({Location.txt: "res.namedArgLoc"}, _)},
          },
        ) when lbl == stringLoc.txt =>
        /* ~d */
        Doc.concat(list{Doc.text("~"), printIdentLike(lbl)})
      | (
          Asttypes.Labelled(lbl) | Optional(lbl),
          {
            ppat_desc: Ppat_constraint({ppat_desc: Ppat_var({txt})}, typ),
            ppat_attributes: list{} | list{({Location.txt: "res.namedArgLoc"}, _)},
          },
        ) when lbl == txt =>
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
        | list{({Location.txt: "res.namedArgLoc", loc}, _), ..._} => {
            ...loc,
            loc_end: pattern.ppat_loc.loc_end,
          }
        | _ => pattern.ppat_loc
        }
      | Some(expr) =>
        let startPos = switch pattern.ppat_attributes {
        | list{({Location.txt: "res.namedArgLoc", loc}, _), ..._} => loc.loc_start
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

  and printRecordRow = ((lbl, expr), cmtTbl) => {
    let cmtLoc = {...lbl.loc, loc_end: expr.pexp_loc.loc_end}
    let doc = Doc.group(
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
      }),
    )
    printComments(doc, cmtTbl, cmtLoc)
  }

  and printBsObjectRow = ((lbl, expr), cmtTbl) => {
    let cmtLoc = {...lbl.loc, loc_end: expr.pexp_loc.loc_end}
    let lblDoc = {
      let doc = Doc.concat(list{Doc.text("\""), printLongident(lbl.txt), Doc.text("\"")})
      printComments(doc, cmtTbl, lbl.loc)
    }

    let doc = Doc.concat(list{lblDoc, Doc.text(": "), printExpressionWithComments(expr, cmtTbl)})
    printComments(doc, cmtTbl, cmtLoc)
  }

  /* The optional loc indicates whether we need to print the attributes in
   * relation to some location. In practise this means the following:
   *  `@attr type t = string` -> on the same line, print on the same line
   *  `@attr
   *   type t = string` -> attr is on prev line, print the attributes
   *   with a line break between, we respect the users' original layout */
  and printAttributes = (~loc=?, attrs: Parsetree.attributes) =>
    switch ParsetreeViewer.filterParsingAttrs(attrs) {
    | list{} => Doc.nil
    | attrs =>
      let lineBreak = switch loc {
      | None => Doc.line
      | Some(loc) =>
        switch List.rev(attrs) {
        | list{({loc: firstLoc}, _), ..._}
          when loc.loc_start.pos_lnum > firstLoc.loc_end.pos_lnum => Doc.hardLine
        | _ => Doc.line
        }
      }

      Doc.concat(list{
        Doc.group(Doc.join(~sep=Doc.line, List.map(printAttribute, attrs))),
        lineBreak,
      })
    }

  and printAttribute = ((id, payload): Parsetree.attribute) => {
    let attrName = Doc.concat(list{Doc.text("@"), Doc.text(id.txt)})
    switch payload {
    | PStr(list{{pstr_desc: Pstr_eval(expr, attrs)}}) =>
      let exprDoc = printExpression(expr, CommentTable.empty)
      let needsParens = switch attrs {
      | list{} => false
      | _ => true
      }
      Doc.group(
        Doc.concat(list{
          attrName,
          addParens(
            Doc.concat(list{
              printAttributes(attrs),
              if needsParens {
                addParens(exprDoc)
              } else {
                exprDoc
              },
            }),
          ),
        }),
      )
    | PTyp(typ) =>
      Doc.group(
        Doc.concat(list{
          attrName,
          Doc.lparen,
          Doc.indent(
            Doc.concat(list{Doc.softLine, Doc.text(": "), printTypExpr(typ, CommentTable.empty)}),
          ),
          Doc.softLine,
          Doc.rparen,
        }),
      )
    | _ => attrName
    }
  }

  and printAttributeWithComments = ((id, payload): Parsetree.attribute, cmtTbl) => {
    let attrName = Doc.text("@" ++ id.txt)
    switch payload {
    | PStr(list{{pstr_desc: Pstr_eval(expr, attrs)}}) =>
      let exprDoc = printExpressionWithComments(expr, cmtTbl)
      let needsParens = switch attrs {
      | list{} => false
      | _ => true
      }
      Doc.group(
        Doc.concat(list{
          attrName,
          addParens(
            Doc.concat(list{
              printAttributes(attrs),
              if needsParens {
                addParens(exprDoc)
              } else {
                exprDoc
              },
            }),
          ),
        }),
      )
    | _ => attrName
    }
  }

  and printModExpr = (modExpr, cmtTbl) => {
    let doc = switch modExpr.pmod_desc {
    | Pmod_ident(longidentLoc) => printLongidentLocation(longidentLoc, cmtTbl)
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
    | Pmod_extension(extension) => printExtensionWithComments(~atModuleLvl=false, extension, cmtTbl)
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
      let attrs = switch attrs {
      | list{} => Doc.nil
      | attrs =>
        Doc.concat(list{Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)), Doc.line})
      }
      Doc.group(Doc.concat(list{attrs, Doc.text("()")}))
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

    let attrs = switch attrs {
    | list{} => Doc.nil
    | attrs => Doc.concat(list{Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)), Doc.line})
    }
    let lblDoc = {
      let doc = Doc.text(lbl.txt)
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
      Doc.concat(list{printAttributes(constr.pext_attributes), Doc.text("exception "), name, kind}),
    )
    printComments(doc, cmtTbl, constr.pext_loc)
  }

  and printExtensionConstructor = (constr: Parsetree.extension_constructor, cmtTbl, i) => {
    let attrs = printAttributes(constr.pext_attributes)
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

  let printImplementation = (~width, s: Parsetree.structure, comments) => {
    let cmtTbl = CommentTable.make()
    CommentTable.walkStructure(s, cmtTbl, comments)
    /* CommentTable.log cmtTbl; */
    let doc = printStructure(s, cmtTbl)
    /* Doc.debug doc; */
    let stringDoc = Doc.toString(~width, doc)
    print_string(stringDoc)
  }

  let printInterface = (~width, s: Parsetree.signature, comments) => {
    let cmtTbl = CommentTable.make()
    CommentTable.walkSignature(s, cmtTbl, comments)
    let stringDoc = Doc.toString(~width, printSignature(s, cmtTbl))
    print_string(stringDoc)
  }
}

module Scanner = {
  type mode = Template | Jsx | Diamond

  type t = {
    filename: string,
    src: bytes,
    mutable err: (
      ~startPos: Lexing.position,
      ~endPos: Lexing.position,
      Diagnostics.category,
    ) => unit,
    mutable ch: int /* current character */,
    mutable offset: int /* character offset */,
    mutable rdOffset: int /* reading offset (position after current character) */,
    mutable lineOffset: int /* current line offset */,
    mutable lnum: int /* current line number */,
    mutable mode: list<mode>,
  }

  let setDiamondMode = scanner => scanner.mode = list{Diamond, ...scanner.mode}

  let setTemplateMode = scanner => scanner.mode = list{Template, ...scanner.mode}

  let setJsxMode = scanner => scanner.mode = list{Jsx, ...scanner.mode}

  let popMode = (scanner, mode) =>
    switch scanner.mode {
    | list{m, ...ms} when m == mode => scanner.mode = ms
    | _ => ()
    }

  let inDiamondMode = scanner =>
    switch scanner.mode {
    | list{Diamond, ..._} => true
    | _ => false
    }

  let inJsxMode = scanner =>
    switch scanner.mode {
    | list{Jsx, ..._} => true
    | _ => false
    }

  let inTemplateMode = scanner =>
    switch scanner.mode {
    | list{Template, ..._} => true
    | _ => false
    }

  let position = scanner => {
    open Lexing
    {
      pos_fname: scanner.filename,
      /* line number */
      pos_lnum: scanner.lnum,
      /* offset of the beginning of the line (number
       of characters between the beginning of the scanner and the beginning
       of the line) */
      pos_bol: scanner.lineOffset,
      /* [pos_cnum] is the offset of the position (number of
       characters between the beginning of the scanner and the position). */
      pos_cnum: scanner.offset,
    }
  }

  let next = scanner =>
    if scanner.rdOffset < Bytes.length(scanner.src) {
      scanner.offset = scanner.rdOffset
      let ch = (@doesNotRaise Bytes.get)(scanner.src, scanner.rdOffset)
      scanner.rdOffset = scanner.rdOffset + 1
      scanner.ch = int_of_char(ch)
    } else {
      scanner.offset = Bytes.length(scanner.src)
      scanner.ch = -1
    }

  let peek = scanner =>
    if scanner.rdOffset < Bytes.length(scanner.src) {
      int_of_char(Bytes.unsafe_get(scanner.src, scanner.rdOffset))
    } else {
      -1
    }

  let make = (b, filename) => {
    let scanner = {
      filename: filename,
      src: b,
      err: (~startPos as _, ~endPos as _, _) => (),
      ch: CharacterCodes.space,
      offset: 0,
      rdOffset: 0,
      lineOffset: 0,
      lnum: 1,
      mode: list{},
    }
    next(scanner)
    scanner
  }

  let skipWhitespace = scanner => {
    let rec scan = () =>
      if scanner.ch === CharacterCodes.space || scanner.ch === CharacterCodes.tab {
        next(scanner)
        scan()
      } else if CharacterCodes.isLineBreak(scanner.ch) {
        scanner.lineOffset = scanner.offset + 1
        scanner.lnum = scanner.lnum + 1
        next(scanner)
        scan()
      } else {
        ()
      }

    scan()
  }

  let scanIdentifier = scanner => {
    let startOff = scanner.offset
    while (
      CharacterCodes.isLetter(scanner.ch) ||
      (CharacterCodes.isDigit(scanner.ch) ||
      (CharacterCodes.underscore === scanner.ch || CharacterCodes.singleQuote === scanner.ch))
    ) {
      next(scanner)
    }
    let str = Bytes.sub_string(scanner.src, startOff, scanner.offset - startOff)
    Token.lookupKeyword(str)
  }

  let scanDigits = (scanner, ~base) =>
    if base <= 10 {
      while CharacterCodes.isDigit(scanner.ch) || scanner.ch === CharacterCodes.underscore {
        next(scanner)
      }
    } else {
      while CharacterCodes.isHex(scanner.ch) || scanner.ch === CharacterCodes.underscore {
        next(scanner)
      }
    }

  /* float: (0…9) { 0…9∣ _ } [. { 0…9∣ _ }] [(e∣ E) [+∣ -] (0…9) { 0…9∣ _ }] */
  let scanNumber = scanner => {
    let startOff = scanner.offset

    /* integer part */
    let (base, _prefix) = if scanner.ch !== CharacterCodes.dot {
      if scanner.ch === CharacterCodes._0 {
        next(scanner)
        let ch = CharacterCodes.lower(scanner.ch)
        if ch === CharacterCodes.Lower.x {
          next(scanner)
          (16, 'x')
        } else if ch === CharacterCodes.Lower.o {
          next(scanner)
          (8, 'o')
        } else if ch === CharacterCodes.Lower.b {
          next(scanner)
          (2, 'b')
        } else {
          (8, '0')
        }
      } else {
        (10, ' ')
      }
    } else {
      (10, ' ')
    }

    scanDigits(scanner, ~base)

    /*  */
    let isFloat = if CharacterCodes.dot === scanner.ch {
      next(scanner)
      scanDigits(scanner, ~base)
      true
    } else {
      false
    }

    /* exponent part */
    let isFloat = if {
      let exp = CharacterCodes.lower(scanner.ch)
      exp === CharacterCodes.Lower.e || exp === CharacterCodes.Lower.p
    } {
      next(scanner)
      if scanner.ch === CharacterCodes.plus || scanner.ch === CharacterCodes.minus {
        next(scanner)
      }
      scanDigits(scanner, ~base)
      true
    } else {
      isFloat
    }

    let literal = Bytes.sub_string(scanner.src, startOff, scanner.offset - startOff)

    /* suffix */
    let suffix = if (
      (scanner.ch >= CharacterCodes.Lower.g && scanner.ch <= CharacterCodes.Lower.z) ||
        (scanner.ch >= CharacterCodes.Upper.g && scanner.ch <= CharacterCodes.Upper.z)
    ) {
      let ch = scanner.ch
      next(scanner)
      Some(Char.unsafe_chr(ch))
    } else {
      None
    }

    if isFloat {
      Token.Float({f: literal, suffix: suffix})
    } else {
      Token.Int({i: literal, suffix: suffix})
    }
  }

  let scanExoticIdentifier = scanner => {
    next(scanner)
    let buffer = Buffer.create(20)
    let startPos = position(scanner)

    let rec scan = () =>
      if scanner.ch === CharacterCodes.eof {
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.message("Did you forget a \" here?"))
      } else if scanner.ch === CharacterCodes.doubleQuote {
        next(scanner)
      } else if CharacterCodes.isLineBreak(scanner.ch) {
        scanner.lineOffset = scanner.offset + 1
        scanner.lnum = scanner.lnum + 1
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.message("Did you forget a \" here?"))
        next(scanner)
      } else {
        Buffer.add_char(buffer, (@doesNotRaise Char.chr)(scanner.ch))
        next(scanner)
        scan()
      }

    scan()
    Token.Lident(Buffer.contents(buffer))
  }

  let scanStringEscapeSequence = (~startPos, scanner) =>
    /* \ already consumed */
    if (
      CharacterCodes.Lower.n === scanner.ch ||
        (CharacterCodes.Lower.t === scanner.ch ||
        (CharacterCodes.Lower.b === scanner.ch ||
          (CharacterCodes.Lower.r === scanner.ch ||
          (CharacterCodes.backslash === scanner.ch ||
            (CharacterCodes.space === scanner.ch ||
            (CharacterCodes.singleQuote === scanner.ch ||
              CharacterCodes.doubleQuote === scanner.ch))))))
    ) {
      next(scanner)
    } else {
      let (n, base, max) = if CharacterCodes.isDigit(scanner.ch) {
        /* decimal */
        (3, 10, 255)
      } else if scanner.ch === CharacterCodes.Lower.o {
        /* octal */
        let () = next(scanner)
        (3, 8, 255)
      } else if scanner.ch === CharacterCodes.Lower.x {
        /* hex */
        let () = next(scanner)
        (2, 16, 255)
      } else {
        /* unknown escape sequence
         * TODO: we should warn the user here. Let's not make it a hard error for now, for reason compat */
        /* let pos = position scanner in */
        /* let () = */
        /* let msg = if scanner.ch == -1 then */
        /* "unclosed escape sequence" */
        /* else "unknown escape sequence" */
        /* in */
        /* scanner.err ~startPos ~endPos:pos (Diagnostics.message msg) */
        /* in */
        (-1, -1, -1)
      }

      if n < 0 {
        ()
      } else {
        let rec while_ = (n, x) =>
          if n === 0 {
            x
          } else {
            let d = CharacterCodes.digitValue(scanner.ch)
            if d >= base {
              let pos = position(scanner)
              let msg = if scanner.ch === -1 {
                "unclosed escape sequence"
              } else {
                "unknown escape sequence"
              }

              scanner.err(~startPos, ~endPos=pos, Diagnostics.message(msg))
              -1
            } else {
              let () = next(scanner)
              while_(n - 1, x * base + d)
            }
          }

        let x = while_(n, 0)
        if x > max {
          let pos = position(scanner)
          let msg = "invalid escape sequence (value too high)"
          scanner.err(~startPos, ~endPos=pos, Diagnostics.message(msg))
          ()
        }
      }
    }

  let scanString = scanner => {
    let offs = scanner.offset

    let startPos = position(scanner)
    let rec scan = () =>
      if scanner.ch === CharacterCodes.eof {
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.unclosedString)
      } else if scanner.ch === CharacterCodes.doubleQuote {
        next(scanner)
      } else if scanner.ch === CharacterCodes.backslash {
        let startPos = position(scanner)
        next(scanner)
        scanStringEscapeSequence(~startPos, scanner)
        scan()
      } else if CharacterCodes.isLineBreak(scanner.ch) {
        scanner.lineOffset = scanner.offset + 1
        scanner.lnum = scanner.lnum + 1
        next(scanner)
        scan()
      } else {
        next(scanner)
        scan()
      }

    scan()
    Token.String(Bytes.sub_string(scanner.src, offs, scanner.offset - offs - 1))
  }

  /* I wonder if this gets inlined */
  let convertNumber = (scanner, ~n, ~base) => {
    let x = ref(0)
    for _ in n downto 1 {
      let d = CharacterCodes.digitValue(scanner.ch)
      x := x.contents * base + d
      next(scanner)
    }
    x.contents
  }

  let scanEscape = scanner => {
    /* let offset = scanner.offset in */
    let c = switch scanner.ch {
    | 98 /* b */ =>
      next(scanner)
      '\b'
    | 110 /* n */ =>
      next(scanner)
      '\n'
    | 114 /* r */ =>
      next(scanner)
      '\r'
    | 116 /* t */ =>
      next(scanner)
      '\t'
    | ch when CharacterCodes.isDigit(ch) =>
      let x = convertNumber(scanner, ~n=3, ~base=10)
      (@doesNotRaise Char.chr)(x)
    | ch when ch === CharacterCodes.Lower.x =>
      next(scanner)
      let x = convertNumber(scanner, ~n=2, ~base=16)
      (@doesNotRaise Char.chr)(x)
    | ch when ch === CharacterCodes.Lower.o =>
      next(scanner)
      let x = convertNumber(scanner, ~n=3, ~base=8)
      (@doesNotRaise Char.chr)(x)
    | ch =>
      next(scanner)
      (@doesNotRaise Char.chr)(ch)
    }

    next(scanner) /* Consume \' */
    Token.Character(c)
  }

  let scanSingleLineComment = scanner => {
    let startOff = scanner.offset
    let startPos = position(scanner)
    while !CharacterCodes.isLineBreak(scanner.ch) && scanner.ch >= 0 {
      next(scanner)
    }
    let endPos = position(scanner)
    Token.Comment(
      Comment.makeSingleLineComment(
        ~loc={
          open Location
          {loc_start: startPos, loc_end: endPos, loc_ghost: false}
        },
        Bytes.sub_string(scanner.src, startOff, scanner.offset - startOff),
      ),
    )
  }

  let scanMultiLineComment = scanner => {
    let startOff = scanner.offset
    let startPos = position(scanner)
    let rec scan = (~depth, ()) =>
      if scanner.ch === CharacterCodes.asterisk && peek(scanner) === CharacterCodes.forwardslash {
        next(scanner)
        next(scanner)
        if depth > 0 {
          scan(~depth=depth - 1, ())
        } else {
          ()
        }
      } else if scanner.ch === CharacterCodes.eof {
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.unclosedComment)
      } else if (
        scanner.ch === CharacterCodes.forwardslash && peek(scanner) === CharacterCodes.asterisk
      ) {
        next(scanner)
        next(scanner)
        scan(~depth=depth + 1, ())
      } else {
        if CharacterCodes.isLineBreak(scanner.ch) {
          scanner.lineOffset = scanner.offset + 1
          scanner.lnum = scanner.lnum + 1
        }
        next(scanner)
        scan(~depth, ())
      }

    scan(~depth=0, ())
    Token.Comment(
      Comment.makeMultiLineComment(
        ~loc={
          open Location
          {loc_start: startPos, loc_end: position(scanner), loc_ghost: false}
        },
        Bytes.sub_string(scanner.src, startOff, scanner.offset - 2 - startOff),
      ),
    )
  }

  let scanTemplate = scanner => {
    let startOff = scanner.offset
    let startPos = position(scanner)

    let rec scan = () =>
      if scanner.ch === CharacterCodes.eof {
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.unclosedTemplate)
        popMode(scanner, Template)
        Token.TemplateTail(Bytes.sub_string(scanner.src, startOff, scanner.offset - 2 - startOff))
      } else if scanner.ch === CharacterCodes.backslash {
        next(scanner)
        if (
          scanner.ch === CharacterCodes.backtick ||
            (scanner.ch === CharacterCodes.backslash ||
            scanner.ch === CharacterCodes.dollar)
        ) {
          next(scanner)
        }
        scan()
      } else if scanner.ch === CharacterCodes.backtick {
        next(scanner)
        let contents = Bytes.sub_string(scanner.src, startOff, scanner.offset - 1 - startOff)

        popMode(scanner, Template)
        Token.TemplateTail(contents)
      } else if scanner.ch === CharacterCodes.dollar && peek(scanner) === CharacterCodes.lbrace {
        next(scanner) /* consume $ */
        next(scanner) /* consume { */
        let contents = Bytes.sub_string(scanner.src, startOff, scanner.offset - 2 - startOff)

        popMode(scanner, Template)
        Token.TemplatePart(contents)
      } else {
        if CharacterCodes.isLineBreak(scanner.ch) {
          scanner.lineOffset = scanner.offset + 1
          scanner.lnum = scanner.lnum + 1
        }
        next(scanner)
        scan()
      }

    scan()
  }

  let rec scan = scanner => {
    if !inTemplateMode(scanner) {
      skipWhitespace(scanner)
    }
    let startPos = position(scanner)
    let ch = scanner.ch
    let token = if inTemplateMode(scanner) {
      scanTemplate(scanner)
    } else if ch === CharacterCodes.underscore {
      let nextCh = peek(scanner)
      if (
        nextCh === CharacterCodes.underscore ||
          (CharacterCodes.isDigit(nextCh) ||
          CharacterCodes.isLetter(nextCh))
      ) {
        scanIdentifier(scanner)
      } else {
        next(scanner)
        Token.Underscore
      }
    } else if CharacterCodes.isLetter(ch) {
      scanIdentifier(scanner)
    } else if CharacterCodes.isDigit(ch) {
      scanNumber(scanner)
    } else {
      next(scanner)
      if ch === CharacterCodes.dot {
        if scanner.ch === CharacterCodes.dot {
          next(scanner)
          if scanner.ch === CharacterCodes.dot {
            next(scanner)
            Token.DotDotDot
          } else {
            Token.DotDot
          }
        } else {
          Token.Dot
        }
      } else if ch === CharacterCodes.doubleQuote {
        scanString(scanner)
      } else if ch === CharacterCodes.singleQuote {
        if (
          scanner.ch === CharacterCodes.backslash && !(peek(scanner) === CharacterCodes.doubleQuote)
        ) {
          /* start of exotic ident */

          next(scanner)
          scanEscape(scanner)
        } else if peek(scanner) === CharacterCodes.singleQuote {
          let ch = scanner.ch
          next(scanner)
          next(scanner)
          Token.Character((@doesNotRaise Char.chr)(ch))
        } else {
          SingleQuote
        }
      } else if ch === CharacterCodes.bang {
        if scanner.ch === CharacterCodes.equal {
          next(scanner)
          if scanner.ch === CharacterCodes.equal {
            next(scanner)
            Token.BangEqualEqual
          } else {
            Token.BangEqual
          }
        } else {
          Token.Bang
        }
      } else if ch === CharacterCodes.semicolon {
        Token.Semicolon
      } else if ch === CharacterCodes.equal {
        if scanner.ch === CharacterCodes.greaterThan {
          next(scanner)
          Token.EqualGreater
        } else if scanner.ch === CharacterCodes.equal {
          next(scanner)
          if scanner.ch === CharacterCodes.equal {
            next(scanner)
            Token.EqualEqualEqual
          } else {
            Token.EqualEqual
          }
        } else {
          Token.Equal
        }
      } else if ch === CharacterCodes.bar {
        if scanner.ch === CharacterCodes.bar {
          next(scanner)
          Token.Lor
        } else if scanner.ch === CharacterCodes.greaterThan {
          next(scanner)
          Token.BarGreater
        } else {
          Token.Bar
        }
      } else if ch === CharacterCodes.ampersand {
        if scanner.ch === CharacterCodes.ampersand {
          next(scanner)
          Token.Land
        } else {
          Token.Band
        }
      } else if ch === CharacterCodes.lparen {
        Token.Lparen
      } else if ch === CharacterCodes.rparen {
        Token.Rparen
      } else if ch === CharacterCodes.lbracket {
        Token.Lbracket
      } else if ch === CharacterCodes.rbracket {
        Token.Rbracket
      } else if ch === CharacterCodes.lbrace {
        Token.Lbrace
      } else if ch === CharacterCodes.rbrace {
        Token.Rbrace
      } else if ch === CharacterCodes.comma {
        Token.Comma
      } else if ch === CharacterCodes.colon {
        if scanner.ch === CharacterCodes.equal {
          next(scanner)
          Token.ColonEqual
        } else if scanner.ch === CharacterCodes.greaterThan {
          next(scanner)
          Token.ColonGreaterThan
        } else {
          Token.Colon
        }
      } else if ch === CharacterCodes.backslash {
        scanExoticIdentifier(scanner)
      } else if ch === CharacterCodes.forwardslash {
        if scanner.ch === CharacterCodes.forwardslash {
          next(scanner)
          scanSingleLineComment(scanner)
        } else if scanner.ch === CharacterCodes.asterisk {
          next(scanner)
          scanMultiLineComment(scanner)
        } else if scanner.ch === CharacterCodes.dot {
          next(scanner)
          Token.ForwardslashDot
        } else {
          Token.Forwardslash
        }
      } else if ch === CharacterCodes.minus {
        if scanner.ch === CharacterCodes.dot {
          next(scanner)
          Token.MinusDot
        } else if scanner.ch === CharacterCodes.greaterThan {
          next(scanner)
          Token.MinusGreater
        } else {
          Token.Minus
        }
      } else if ch === CharacterCodes.plus {
        if scanner.ch === CharacterCodes.dot {
          next(scanner)
          Token.PlusDot
        } else if scanner.ch === CharacterCodes.plus {
          next(scanner)
          Token.PlusPlus
        } else if scanner.ch === CharacterCodes.equal {
          next(scanner)
          Token.PlusEqual
        } else {
          Token.Plus
        }
      } else if ch === CharacterCodes.greaterThan {
        if scanner.ch === CharacterCodes.equal && !inDiamondMode(scanner) {
          next(scanner)
          Token.GreaterEqual
        } else {
          Token.GreaterThan
        }
      } else if ch === CharacterCodes.lessThan {
        /* Imagine the following: <div><
         * < indicates the start of a new jsx-element, the parser expects
         * the name of a new element after the <
         * Example: <div> <div
         * But what if we have a / here: example </ in  <div></div>
         * This signals a closing element. To simulate the two-token lookahead,
         * the </ is emitted as a single new token LessThanSlash */
        if inJsxMode(scanner) {
          skipWhitespace(scanner)
          if scanner.ch === CharacterCodes.forwardslash {
            let () = next(scanner)
            Token.LessThanSlash
          } else {
            Token.LessThan
          }
        } else if scanner.ch === CharacterCodes.equal {
          next(scanner)
          Token.LessEqual
        } else {
          Token.LessThan
        }
      } else if ch === CharacterCodes.hash {
        if scanner.ch === CharacterCodes.hash {
          next(scanner)
          Token.HashHash
        } else if scanner.ch === CharacterCodes.equal {
          next(scanner)
          Token.HashEqual
        } else {
          Token.Hash
        }
      } else if ch === CharacterCodes.asterisk {
        if scanner.ch === CharacterCodes.asterisk {
          next(scanner)
          Token.Exponentiation
        } else if scanner.ch === CharacterCodes.dot {
          next(scanner)
          Token.AsteriskDot
        } else {
          Token.Asterisk
        }
      } else if ch === CharacterCodes.tilde {
        Token.Tilde
      } else if ch === CharacterCodes.question {
        Token.Question
      } else if ch === CharacterCodes.at {
        if scanner.ch === CharacterCodes.at {
          next(scanner)
          Token.AtAt
        } else {
          Token.At
        }
      } else if ch === CharacterCodes.percent {
        if scanner.ch === CharacterCodes.percent {
          next(scanner)
          Token.PercentPercent
        } else {
          Token.Percent
        }
      } else if ch === CharacterCodes.backtick {
        Token.Backtick
      } else if ch === -1 {
        Token.Eof
      } else {
        /* if we arrive here, we're dealing with an unkown character,
         * report the error and continue scanning… */
        let endPos = position(scanner)
        scanner.err(~startPos, ~endPos, Diagnostics.unknownUchar(ch))
        let (_, _, token) = scan(scanner)
        token
      }
    }
    let endPos = position(scanner)
    (startPos, endPos, token)
  }

  /* Imagine: <div> <Navbar /> <
   * is `<` the start of a jsx-child? <div …
   * or is it the start of a closing tag?  </div>
   * reconsiderLessThan peeks at the next token and
   * determines the correct token to disambiguate */
  let reconsiderLessThan = scanner => {
    /* < consumed */
    skipWhitespace(scanner)
    if scanner.ch === CharacterCodes.forwardslash {
      let () = next(scanner)
      Token.LessThanSlash
    } else {
      Token.LessThan
    }
  }

  /* If an operator has whitespace around both sides, it's a binary operator */
  let isBinaryOp = (src, startCnum, endCnum) =>
    if startCnum === 0 {
      false
    } else {
      let leftOk = {
        let c = startCnum - 1 |> (@doesNotRaise Bytes.get)(src) |> Char.code

        c === CharacterCodes.space || (c === CharacterCodes.tab || CharacterCodes.isLineBreak(c))
      }

      let rightOk = {
        let c = if endCnum === Bytes.length(src) {
          -1
        } else {
          endCnum |> (@doesNotRaise Bytes.get)(src) |> Char.code
        }

        c === CharacterCodes.space ||
          (c === CharacterCodes.tab ||
          (CharacterCodes.isLineBreak(c) || c === CharacterCodes.eof))
      }

      leftOk && rightOk
    }
}

/* AST for js externals */
module JsFfi = {
  type scope =
    | Global
    | Module(string) /* module("path") */
    | Scope(Longident.t) /* scope(/"window", "location"/) */

  type label_declaration = {
    @live jld_attributes: Parsetree.attributes,
    jld_name: string,
    jld_alias: string,
    jld_type: Parsetree.core_type,
    jld_loc: Location.t,
  }

  type importSpec =
    | Default(label_declaration)
    | Spec(list<label_declaration>)

  type import_description = {
    jid_loc: Location.t,
    jid_spec: importSpec,
    jid_scope: scope,
    jid_attributes: Parsetree.attributes,
  }

  let decl = (~attrs, ~loc, ~name, ~alias, ~typ) => {
    jld_loc: loc,
    jld_attributes: attrs,
    jld_name: name,
    jld_alias: alias,
    jld_type: typ,
  }

  let importDescr = (~attrs, ~scope, ~importSpec, ~loc) => {
    jid_loc: loc,
    jid_spec: importSpec,
    jid_scope: scope,
    jid_attributes: attrs,
  }

  let toParsetree = importDescr => {
    let bsVal = (Location.mknoloc("val"), Parsetree.PStr(list{}))
    let attrs = switch importDescr.jid_scope {
    | Global => list{bsVal}
    /* @genType.import("./MyMath"),
     * @genType.import(/"./MyMath", "default"/) */
    | Module(s) =>
      let structure = list{
        Parsetree.Pconst_string(s, None) |> Ast_helper.Exp.constant |> Ast_helper.Str.eval,
      }
      let genType = (Location.mknoloc("genType.import"), Parsetree.PStr(structure))
      list{genType}
    | Scope(longident) =>
      let structureItem = {
        let expr = switch Longident.flatten(longident) |> List.map(s =>
          Ast_helper.Exp.constant(Parsetree.Pconst_string(s, None))
        ) {
        | list{expr} => expr
        | list{} as exprs | _ as exprs => exprs |> Ast_helper.Exp.tuple
        }

        Ast_helper.Str.eval(expr)
      }

      let bsScope = (Location.mknoloc("scope"), Parsetree.PStr(list{structureItem}))
      list{bsVal, bsScope}
    }

    let valueDescrs = switch importDescr.jid_spec {
    | Default(decl) =>
      let prim = list{decl.jld_name}
      let allAttrs = List.concat(list{attrs, importDescr.jid_attributes}) |> List.map(attr =>
        switch attr {
        | (
            {Location.txt: "genType.import"} as id,
            Parsetree.PStr(list{{pstr_desc: Parsetree.Pstr_eval(moduleName, _)}}),
          ) =>
          let default = Parsetree.Pconst_string("default", None) |> Ast_helper.Exp.constant

          let structureItem =
            list{moduleName, default} |> Ast_helper.Exp.tuple |> Ast_helper.Str.eval

          (id, Parsetree.PStr(list{structureItem}))
        | attr => attr
        }
      )

      list{
        Ast_helper.Val.mk(
          ~loc=importDescr.jid_loc,
          ~prim,
          ~attrs=allAttrs,
          Location.mknoloc(decl.jld_alias),
          decl.jld_type,
        ) |> Ast_helper.Str.primitive,
      }
    | Spec(decls) => List.map(decl => {
        let prim = list{decl.jld_name}
        let allAttrs = List.concat(list{attrs, decl.jld_attributes})
        Ast_helper.Val.mk(
          ~loc=importDescr.jid_loc,
          ~prim,
          ~attrs=allAttrs,
          Location.mknoloc(decl.jld_alias),
          decl.jld_type,
        ) |> Ast_helper.Str.primitive(~loc=decl.jld_loc)
      }, decls)
    }

    let jsFfiAttr = (Location.mknoloc("ns.jsFfi"), Parsetree.PStr(list{}))
    Ast_helper.Mod.structure(~loc=importDescr.jid_loc, valueDescrs)
    |> Ast_helper.Incl.mk(~attrs=list{jsFfiAttr}, ~loc=importDescr.jid_loc)
    |> Ast_helper.Str.include_(~loc=importDescr.jid_loc)
  }
}

module ParsetreeCompatibility = {
  let concatLongidents = (l1, l2) => {
    let parts1 = Longident.flatten(l1)
    let parts2 = Longident.flatten(l2)
    switch List.concat(list{parts1, parts2}) |> Longident.unflatten {
    | Some(longident) => longident
    | None => l2
    }
  }

  /* TODO: support nested open's ? */
  let rec rewritePpatOpen = (longidentOpen, pat) => {
    open Parsetree
    switch pat.ppat_desc {
    | Ppat_array(list{first, ...rest}) => /* Color.[Red, Blue, Green] -> [Color.Red, Blue, Green] */
      {...pat, ppat_desc: Ppat_array(list{rewritePpatOpen(longidentOpen, first), ...rest})}
    | Ppat_tuple(list{first, ...rest}) => /* Color.(Red, Blue, Green) -> (Color.Red, Blue, Green) */
      {...pat, ppat_desc: Ppat_tuple(list{rewritePpatOpen(longidentOpen, first), ...rest})}
    | Ppat_construct(
        {txt: Longident.Lident("::")} as listConstructor,
        Some({ppat_desc: Ppat_tuple(list{pat, ...rest})} as element),
      ) => /* Color.(list{Red, Blue, Green}) -> list{Color.Red, Blue, Green} */
      {
        ...pat,
        ppat_desc: Ppat_construct(
          listConstructor,
          Some({
            ...element,
            ppat_desc: Ppat_tuple(list{rewritePpatOpen(longidentOpen, pat), ...rest}),
          }),
        ),
      }
    | Ppat_construct(
        {txt: constructor} as longidentLoc,
        optPattern,
      ) => /* Foo.(Bar(a)) -> Foo.Bar(a) */
      {
        ...pat,
        ppat_desc: Ppat_construct(
          {...longidentLoc, txt: concatLongidents(longidentOpen, constructor)},
          optPattern,
        ),
      }
    | Ppat_record(list{({txt: lbl} as longidentLoc, firstPat), ...rest}, flag) =>
      /* Foo.{x} -> {Foo.x: x} */
      let firstRow = ({...longidentLoc, txt: concatLongidents(longidentOpen, lbl)}, firstPat)
      {...pat, ppat_desc: Ppat_record(list{firstRow, ...rest}, flag)}
    | Ppat_or(pat1, pat2) => {
        ...pat,
        ppat_desc: Ppat_or(
          rewritePpatOpen(longidentOpen, pat1),
          rewritePpatOpen(longidentOpen, pat2),
        ),
      }
    | Ppat_constraint(pattern, typ) => {
        ...pat,
        ppat_desc: Ppat_constraint(rewritePpatOpen(longidentOpen, pattern), typ),
      }
    | Ppat_type({txt: constructor} as longidentLoc) => {
        ...pat,
        ppat_desc: Ppat_type({
          ...longidentLoc,
          txt: concatLongidents(longidentOpen, constructor),
        }),
      }
    | Ppat_lazy(p) => {...pat, ppat_desc: Ppat_lazy(rewritePpatOpen(longidentOpen, p))}
    | Ppat_exception(p) => {...pat, ppat_desc: Ppat_exception(rewritePpatOpen(longidentOpen, p))}
    | _ => pat
    }
  }

  let rec rewriteReasonFastPipe = expr => {
    open Parsetree
    switch expr.pexp_desc {
    | Pexp_apply(
        {
          pexp_desc:
            Pexp_apply(
              {pexp_desc: Pexp_ident({txt: Longident.Lident("|.")})} as op,
              list{(Asttypes.Nolabel, lhs), (Nolabel, rhs)},
            ),
          pexp_attributes: subAttrs,
        },
        args,
      ) =>
      let rhsLoc = {...rhs.pexp_loc, loc_end: expr.pexp_loc.loc_end}
      let newLhs = {
        let expr = rewriteReasonFastPipe(lhs)
        {...expr, pexp_attributes: subAttrs}
      }

      let allArgs = list{
        (Asttypes.Nolabel, newLhs),
        (Asttypes.Nolabel, Ast_helper.Exp.apply(~loc=rhsLoc, rhs, args)),
      }

      Ast_helper.Exp.apply(~attrs=expr.pexp_attributes, ~loc=expr.pexp_loc, op, allArgs)
    | _ => expr
    }
  }

  let makeReasonArityMapper = (~forPrinter) => {
    open Ast_mapper
    {
      ...default_mapper,
      expr: (mapper, expr) =>
        switch expr {
        /* Don't mind this case, Reason doesn't handle this. */
        /* | {pexp_desc = Pexp_variant (lbl, args); pexp_loc; pexp_attributes} -> */
        /* let newArgs = match args with */
        /* | (Some {pexp_desc = Pexp_tuple [{pexp_desc = Pexp_tuple _ } as sp]}) as args-> */
        /* if forPrinter then args else Some sp */
        /* | Some {pexp_desc = Pexp_tuple [sp]} -> Some sp */
        /* | _ -> args */
        /* in */
        /* default_mapper.expr mapper {pexp_desc=Pexp_variant(lbl, newArgs); pexp_loc; pexp_attributes} */
        | {pexp_desc: Pexp_construct(lid, args), pexp_loc, pexp_attributes} =>
          let newArgs = switch args {
          | Some({pexp_desc: Pexp_tuple(list{{pexp_desc: Pexp_tuple(_)} as sp})}) as args =>
            if forPrinter {
              args
            } else {
              Some(sp)
            }
          | Some({pexp_desc: Pexp_tuple(list{sp})}) => Some(sp)
          | _ => args
          }

          default_mapper.expr(
            mapper,
            {
              pexp_desc: Pexp_construct(lid, newArgs),
              pexp_loc: pexp_loc,
              pexp_attributes: pexp_attributes,
            },
          )
        | expr => default_mapper.expr(mapper, rewriteReasonFastPipe(expr))
        },
      pat: (mapper, pattern) =>
        switch pattern {
        /* Don't mind this case, Reason doesn't handle this. */
        /* | {ppat_desc = Ppat_variant (lbl, args); ppat_loc; ppat_attributes} -> */
        /* let newArgs = match args with */
        /* | (Some {ppat_desc = Ppat_tuple [{ppat_desc = Ppat_tuple _} as sp]}) as args -> */
        /* if forPrinter then args else Some sp */
        /* | Some {ppat_desc = Ppat_tuple [sp]} -> Some sp */
        /* | _ -> args */
        /* in */
        /* default_mapper.pat mapper {ppat_desc = Ppat_variant (lbl, newArgs); ppat_loc; ppat_attributes;} */
        | {ppat_desc: Ppat_construct(lid, args), ppat_loc, ppat_attributes} =>
          let new_args = switch args {
          | Some({ppat_desc: Ppat_tuple(list{{ppat_desc: Ppat_tuple(_)} as sp})}) as args =>
            if forPrinter {
              args
            } else {
              Some(sp)
            }
          | Some({ppat_desc: Ppat_tuple(list{sp})}) => Some(sp)
          | _ => args
          }
          default_mapper.pat(
            mapper,
            {
              ppat_desc: Ppat_construct(lid, new_args),
              ppat_loc: ppat_loc,
              ppat_attributes: ppat_attributes,
            },
          )
        | x => default_mapper.pat(mapper, x)
        },
    }
  }

  let escapeTemplateLiteral = s => {
    let len = String.length(s)
    let b = Buffer.create(len)
    let i = ref(0)
    while i.contents < len {
      let c = (@doesNotRaise String.get)(s, i.contents)
      if c == '`' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, '`')
        incr(i)
      } else if c == '$' {
        if i.contents + 1 < len {
          let c2 = (@doesNotRaise String.get)(s, i.contents + 1)
          if c2 == '{' {
            Buffer.add_char(b, '\\')
            Buffer.add_char(b, '$')
            Buffer.add_char(b, '{')
          } else {
            Buffer.add_char(b, c)
            Buffer.add_char(b, c2)
          }
          i := i.contents + 2
        } else {
          Buffer.add_char(b, c)
          incr(i)
        }
      } else if c == '\\' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, '\\')
        incr(i)
      } else {
        Buffer.add_char(b, c)
        incr(i)
      }
    }
    Buffer.contents(b)
  }

  let escapeStringContents = s => {
    let len = String.length(s)
    let b = Buffer.create(len)

    let i = ref(0)

    while i.contents < len {
      let c = String.unsafe_get(s, i.contents)
      if c == '\\' {
        incr(i)
        Buffer.add_char(b, c)
        let c = String.unsafe_get(s, i.contents)
        if i.contents < len {
          let () = Buffer.add_char(b, c)
          incr(i)
        } else {
          ()
        }
      } else if c == '"' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, c)
        incr(i)
      } else {
        Buffer.add_char(b, c)
        incr(i)
      }
    }
    Buffer.contents(b)
  }

  let looksLikeRecursiveTypeDeclaration = typeDeclaration => {
    open Parsetree
    let name = typeDeclaration.ptype_name.txt
    let rec checkKind = kind =>
      switch kind {
      | Ptype_abstract | Ptype_open => false
      | Ptype_variant(constructorDeclarations) =>
        List.exists(checkConstructorDeclaration, constructorDeclarations)
      | Ptype_record(labelDeclarations) => List.exists(checkLabelDeclaration, labelDeclarations)
      }

    and checkConstructorDeclaration = constrDecl =>
      checkConstructorArguments(constrDecl.pcd_args) ||
      switch constrDecl.pcd_res {
      | Some(typexpr) => checkTypExpr(typexpr)
      | None => false
      }

    and checkLabelDeclaration = labelDeclaration => checkTypExpr(labelDeclaration.pld_type)

    and checkConstructorArguments = constrArg =>
      switch constrArg {
      | Pcstr_tuple(types) => List.exists(checkTypExpr, types)
      | Pcstr_record(labelDeclarations) => List.exists(checkLabelDeclaration, labelDeclarations)
      }

    and checkTypExpr = typ =>
      switch typ.ptyp_desc {
      | Ptyp_any => false
      | Ptyp_var(_) => false
      | Ptyp_object(_) => false
      | Ptyp_class(_) => false
      | Ptyp_package(_) => false
      | Ptyp_extension(_) => false
      | Ptyp_arrow(_lbl, typ1, typ2) => checkTypExpr(typ1) || checkTypExpr(typ2)
      | Ptyp_tuple(types) => List.exists(checkTypExpr, types)
      | Ptyp_constr({txt: longident}, types) =>
        switch longident {
        | Lident(ident) => ident == name
        | _ => false
        } ||
        List.exists(checkTypExpr, types)
      | Ptyp_alias(typ, _) => checkTypExpr(typ)
      | Ptyp_variant(rowFields, _, _) => List.exists(checkRowFields, rowFields)
      | Ptyp_poly(_, typ) => checkTypExpr(typ)
      }

    and checkRowFields = rowField =>
      switch rowField {
      | Rtag(_, _, _, types) => List.exists(checkTypExpr, types)
      | Rinherit(typexpr) => checkTypExpr(typexpr)
      }

    checkKind(typeDeclaration.ptype_kind)
  }

  let filterReasonRawLiteral = attrs => List.filter(attr =>
      switch attr {
      | ({Location.txt: "reason.raw_literal"}, _) => false
      | _ => true
      }
    , attrs)

  let stringLiteralMapper = stringData => {
    let isSameLocation = (l1, l2) => {
      open Location
      l1.loc_start.pos_cnum === l2.loc_start.pos_cnum
    }

    let remainingStringData = stringData
    open Ast_mapper
    {
      ...default_mapper,
      expr: (mapper, expr) =>
        switch expr.pexp_desc {
        | Pexp_constant(Pconst_string(_txt, None)) =>
          switch List.find_opt(
            ((_stringData, stringLoc)) => isSameLocation(stringLoc, expr.pexp_loc),
            remainingStringData,
          ) {
          | Some(stringData, _) =>
            let stringData = {
              let attr = List.find_opt(attr =>
                switch attr {
                | ({Location.txt: "reason.raw_literal"}, _) => true
                | _ => false
                }
              , expr.pexp_attributes)
              switch attr {
              | Some(
                  _,
                  PStr(list{{
                    pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Pconst_string(raw, _))}, _),
                  }}),
                ) => raw
              | _ => (@doesNotRaise String.sub)(stringData, 1, String.length(stringData) - 2)
              }
            }

            {
              ...expr,
              pexp_attributes: filterReasonRawLiteral(expr.pexp_attributes),
              pexp_desc: Pexp_constant(Pconst_string(stringData, None)),
            }
          | None => default_mapper.expr(mapper, expr)
          }
        | _ => default_mapper.expr(mapper, expr)
        },
    }
  }

  let normalize = {
    open Ast_mapper
    {
      ...default_mapper,
      attributes: (mapper, attrs) => attrs |> List.filter(attr =>
          switch attr {
          | (
              {Location.txt: "reason.preserve_braces" | "explicit_arity" | "implicity_arity"},
              _,
            ) => false
          | _ => true
          }
        ) |> default_mapper.attributes(mapper),
      pat: (mapper, p) =>
        switch p.ppat_desc {
        | Ppat_open({txt: longidentOpen}, pattern) =>
          let p = rewritePpatOpen(longidentOpen, pattern)
          default_mapper.pat(mapper, p)
        | _ => default_mapper.pat(mapper, p)
        },
      expr: (mapper, expr) =>
        switch expr.pexp_desc {
        | Pexp_constant(Pconst_string(txt, None)) =>
          let raw = escapeStringContents(txt)
          let s = Parsetree.Pconst_string(raw, None)
          let expr = Ast_helper.Exp.constant(~attrs=expr.pexp_attributes, ~loc=expr.pexp_loc, s)

          expr
        | Pexp_constant(Pconst_string(txt, tag)) =>
          let s = Parsetree.Pconst_string(escapeTemplateLiteral(txt), tag)
          Ast_helper.Exp.constant(
            ~attrs=mapper.attributes(mapper, expr.pexp_attributes),
            ~loc=expr.pexp_loc,
            s,
          )
        | Pexp_function(cases) =>
          let loc = switch (cases, List.rev(cases)) {
          | (list{first, ..._}, list{last, ..._}) => {
              ...first.pc_lhs.ppat_loc,
              loc_end: last.pc_rhs.pexp_loc.loc_end,
            }
          | _ => Location.none
          }

          Ast_helper.Exp.fun_(
            ~loc,
            Asttypes.Nolabel,
            None,
            Ast_helper.Pat.var(Location.mknoloc("x")),
            Ast_helper.Exp.match_(
              ~loc,
              Ast_helper.Exp.ident(Location.mknoloc(Longident.Lident("x"))),
              default_mapper.cases(mapper, cases),
            ),
          )
        | Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Longident.Lident("!")})},
            list{(Asttypes.Nolabel, operand)},
          ) =>
          /* turn `!foo` into `foo.contents` */
          Ast_helper.Exp.field(
            ~loc=expr.pexp_loc,
            ~attrs=expr.pexp_attributes,
            operand,
            Location.mknoloc(Longident.Lident("contents")),
          )
        | Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Longident.Lident("##")})} as op,
            list{
              (Asttypes.Nolabel, lhs),
              (Nolabel, {pexp_desc: Pexp_constant(Pconst_string(txt, None))} as stringExpr),
            },
          ) =>
          let ident = Ast_helper.Exp.ident(
            ~loc=stringExpr.pexp_loc,
            Location.mkloc(Longident.Lident(txt), stringExpr.pexp_loc),
          )

          Ast_helper.Exp.apply(
            ~loc=expr.pexp_loc,
            ~attrs=expr.pexp_attributes,
            op,
            list{(Asttypes.Nolabel, lhs), (Nolabel, ident)},
          )
        | Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Longident.Lident("@@")})},
            list{(Asttypes.Nolabel, callExpr), (Nolabel, argExpr)},
          ) =>
          Ast_helper.Exp.apply(
            mapper.expr(mapper, callExpr),
            list{(Asttypes.Nolabel, mapper.expr(mapper, argExpr))},
          )
        | Pexp_apply(
            {pexp_desc: Pexp_ident({txt: Longident.Lident("@")})},
            list{(Nolabel, arg1), (Nolabel, arg2)},
          ) =>
          let listConcat = Longident.Ldot(Longident.Lident("List"), "append")
          Ast_helper.Exp.apply(
            Ast_helper.Exp.ident(Location.mknoloc(listConcat)),
            list{(Nolabel, mapper.expr(mapper, arg1)), (Nolabel, mapper.expr(mapper, arg2))},
          )
        | Pexp_match(
            condition,
            list{
              {
                pc_lhs: {ppat_desc: Ppat_construct({txt: Longident.Lident("true")}, None)},
                pc_rhs: thenExpr,
              },
              {
                pc_lhs: {ppat_desc: Ppat_construct({txt: Longident.Lident("false")}, None)},
                pc_rhs: elseExpr,
              },
            },
          ) =>
          let ternaryMarker = (Location.mknoloc("res.ternary"), Parsetree.PStr(list{}))
          Ast_helper.Exp.ifthenelse(
            ~loc=expr.pexp_loc,
            ~attrs=list{ternaryMarker, ...expr.pexp_attributes},
            default_mapper.expr(mapper, condition),
            default_mapper.expr(mapper, thenExpr),
            Some(default_mapper.expr(mapper, elseExpr)),
          )
        | _ => default_mapper.expr(mapper, expr)
        },
      structure_item: (mapper, structureItem) =>
        switch structureItem.pstr_desc {
        /* heuristic: if we have multiple type declarations, mark them recursive */
        | Pstr_type(recFlag, typeDeclarations) =>
          let flag = switch typeDeclarations {
          | list{td} =>
            if looksLikeRecursiveTypeDeclaration(td) {
              Asttypes.Recursive
            } else {
              Asttypes.Nonrecursive
            }
          | _ => recFlag
          }

          {
            ...structureItem,
            pstr_desc: Pstr_type(
              flag,
              List.map(
                typeDeclaration => default_mapper.type_declaration(mapper, typeDeclaration),
                typeDeclarations,
              ),
            ),
          }
        | _ => default_mapper.structure_item(mapper, structureItem)
        },
      signature_item: (mapper, signatureItem) =>
        switch signatureItem.psig_desc {
        /* heuristic: if we have multiple type declarations, mark them recursive */
        | Psig_type(recFlag, typeDeclarations) =>
          let flag = switch typeDeclarations {
          | list{td} =>
            if looksLikeRecursiveTypeDeclaration(td) {
              Asttypes.Recursive
            } else {
              Asttypes.Nonrecursive
            }
          | _ => recFlag
          }

          {
            ...signatureItem,
            psig_desc: Psig_type(
              flag,
              List.map(
                typeDeclaration => default_mapper.type_declaration(mapper, typeDeclaration),
                typeDeclarations,
              ),
            ),
          }
        | _ => default_mapper.signature_item(mapper, signatureItem)
        },
      value_binding: (mapper, vb) =>
        switch vb {
        | {
            pvb_pat: {ppat_desc: Ppat_var(_)} as pat,
            pvb_expr: {pexp_loc: expr_loc, pexp_desc: Pexp_constraint(expr, typ)},
          } when expr_loc.loc_ghost =>
          /* let t: t = (expr : t) -> let t: t = expr */
          let typ = default_mapper.typ(mapper, typ)
          let pat = default_mapper.pat(mapper, pat)
          let expr = mapper.expr(mapper, expr)
          let newPattern = Ast_helper.Pat.constraint_(
            ~loc={...pat.ppat_loc, loc_end: typ.ptyp_loc.loc_end},
            pat,
            typ,
          )
          {
            ...vb,
            pvb_pat: newPattern,
            pvb_expr: expr,
            pvb_attributes: default_mapper.attributes(mapper, vb.pvb_attributes),
          }
        | {
            pvb_pat: {ppat_desc: Ppat_constraint(pat, {ptyp_desc: Ptyp_poly(list{}, _)})},
            pvb_expr: {pexp_loc: expr_loc, pexp_desc: Pexp_constraint(expr, typ)},
          } when expr_loc.loc_ghost =>
          /* let t: . t = (expr : t) -> let t: t = expr */
          let typ = default_mapper.typ(mapper, typ)
          let pat = default_mapper.pat(mapper, pat)
          let expr = mapper.expr(mapper, expr)
          let newPattern = Ast_helper.Pat.constraint_(
            ~loc={...pat.ppat_loc, loc_end: typ.ptyp_loc.loc_end},
            pat,
            typ,
          )
          {
            ...vb,
            pvb_pat: newPattern,
            pvb_expr: expr,
            pvb_attributes: default_mapper.attributes(mapper, vb.pvb_attributes),
          }
        | _ => default_mapper.value_binding(mapper, vb)
        },
    }
  }

  let normalizeReasonArityStructure = (~forPrinter, s) => {
    let mapper = makeReasonArityMapper(~forPrinter)
    mapper.Ast_mapper.structure(mapper, s)
  }

  let normalizeReasonAritySignature = (~forPrinter, s) => {
    let mapper = makeReasonArityMapper(~forPrinter)
    mapper.Ast_mapper.signature(mapper, s)
  }

  let structure = s => normalize.Ast_mapper.structure(normalize, s)
  let signature = s => normalize.Ast_mapper.signature(normalize, s)

  let replaceStringLiteralStructure = (stringData, structure) => {
    let mapper = stringLiteralMapper(stringData)
    mapper.Ast_mapper.structure(mapper, structure)
  }

  let replaceStringLiteralSignature = (stringData, signature) => {
    let mapper = stringLiteralMapper(stringData)
    mapper.Ast_mapper.signature(mapper, signature)
  }
}

module OcamlParser = Parser

module Parser = {
  type mode = ParseForTypeChecker | Default

  type regionStatus = Report | Silent

  type t = {
    mode: mode,
    mutable scanner: Scanner.t,
    mutable token: Token.t,
    mutable startPos: Lexing.position,
    mutable endPos: Lexing.position,
    mutable prevEndPos: Lexing.position,
    mutable breadcrumbs: list<(Grammar.t, Lexing.position)>,
    mutable errors: list<Reporting.parseError>,
    mutable diagnostics: list<Diagnostics.t>,
    mutable comments: list<Comment.t>,
    mutable regions: list<ref<regionStatus>>,
  }

  let err = (~startPos=?, ~endPos=?, p, error) => {
    let d = Diagnostics.make(
      ~filename=p.scanner.filename,
      ~startPos=switch startPos {
      | Some(pos) => pos
      | None => p.startPos
      },
      ~endPos=switch endPos {
      | Some(pos) => pos
      | None => p.endPos
      },
      error,
    )

    try if List.hd(p.regions).contents == Report {
      p.diagnostics = list{d, ...p.diagnostics}
      List.hd(p.regions) := Silent
    } catch {
    | Failure(_) => ()
    }
  }

  let beginRegion = p => p.regions = list{ref(Report), ...p.regions}
  let endRegion = p =>
    try p.regions = List.tl(p.regions) catch {
    | Failure(_) => ()
    }

  /* Advance to the next non-comment token and store any encountered comment
   * in the parser's state. Every comment contains the end position of it's
   * previous token to facilite comment interleaving */
  let rec next = (~prevEndPos=?, p) => {
    let prevEndPos = switch prevEndPos {
    | Some(pos) => pos
    | None => p.endPos
    }
    let (startPos, endPos, token) = Scanner.scan(p.scanner)
    switch token {
    | Comment(c) =>
      Comment.setPrevTokEndPos(c, p.endPos)
      p.comments = list{c, ...p.comments}
      p.prevEndPos = p.endPos
      p.endPos = endPos
      next(~prevEndPos, p)
    | _ =>
      p.token = token

      /* p.prevEndPos <- prevEndPos; */
      p.prevEndPos = prevEndPos
      p.startPos = startPos
      p.endPos = endPos
    }
  }

  let checkProgress = (~prevEndPos, ~result, p) =>
    if p.endPos === prevEndPos {
      None
    } else {
      Some(result)
    }

  let make = (~mode=ParseForTypeChecker, src, filename) => {
    let scanner = Scanner.make(Bytes.of_string(src), filename)
    let parserState = {
      mode: mode,
      scanner: scanner,
      token: Token.Eof,
      startPos: Lexing.dummy_pos,
      prevEndPos: Lexing.dummy_pos,
      endPos: Lexing.dummy_pos,
      breadcrumbs: list{},
      errors: list{},
      diagnostics: list{},
      comments: list{},
      regions: list{ref(Report)},
    }
    parserState.scanner.err = (~startPos, ~endPos, error) => {
      let diagnostic = Diagnostics.make(~filename, ~startPos, ~endPos, error)

      parserState.diagnostics = list{diagnostic, ...parserState.diagnostics}
    }
    next(parserState)
    parserState
  }

  let leaveBreadcrumb = (p, circumstance) => {
    let crumb = (circumstance, p.startPos)
    p.breadcrumbs = list{crumb, ...p.breadcrumbs}
  }

  let eatBreadcrumb = p =>
    switch p.breadcrumbs {
    | list{} => ()
    | list{_, ...crumbs} => p.breadcrumbs = crumbs
    }

  let optional = (p, token) =>
    if p.token == token {
      let () = next(p)
      true
    } else {
      false
    }

  let expect = (~grammar=?, token, p) =>
    if p.token == token {
      next(p)
    } else {
      let error = Diagnostics.expected(~grammar?, p.prevEndPos, token)
      err(~startPos=p.prevEndPos, p, error)
    }

  /* Don't use immutable copies here, it trashes certain heuristics
   * in the ocaml compiler, resulting in massive slowdowns of the parser */
  let lookahead = (p, callback) => {
    let err = p.scanner.err
    let ch = p.scanner.ch
    let offset = p.scanner.offset
    let rdOffset = p.scanner.rdOffset
    let lineOffset = p.scanner.lineOffset
    let lnum = p.scanner.lnum
    let mode = p.scanner.mode
    let token = p.token
    let startPos = p.startPos
    let endPos = p.endPos
    let prevEndPos = p.prevEndPos
    let breadcrumbs = p.breadcrumbs
    let errors = p.errors
    let diagnostics = p.diagnostics
    let comments = p.comments

    let res = callback(p)

    p.scanner.err = err
    p.scanner.ch = ch
    p.scanner.offset = offset
    p.scanner.rdOffset = rdOffset
    p.scanner.lineOffset = lineOffset
    p.scanner.lnum = lnum
    p.scanner.mode = mode
    p.token = token
    p.startPos = startPos
    p.endPos = endPos
    p.prevEndPos = prevEndPos
    p.breadcrumbs = breadcrumbs
    p.errors = errors
    p.diagnostics = diagnostics
    p.comments = comments

    res
  }
}

module NapkinScript = {
  let mkLoc = (startLoc, endLoc) => {
    open Location
    {
      loc_start: startLoc,
      loc_end: endLoc,
      loc_ghost: false,
    }
  }

  module Recover = {
    type action = option<unit> /* None is abort, Some () is retry */

    let defaultExpr = () => {
      let id = Location.mknoloc("napkinscript.exprhole")
      Ast_helper.Exp.mk(Pexp_extension(id, PStr(list{})))
    }

    let defaultType = () => {
      let id = Location.mknoloc("napkinscript.typehole")
      Ast_helper.Typ.extension((id, PStr(list{})))
    }

    let defaultPattern = () => {
      let id = Location.mknoloc("napkinscript.patternhole")
      Ast_helper.Pat.extension((id, PStr(list{})))
    }
    /* Ast_helper.Pat.any  () */

    let defaultModuleExpr = () => Ast_helper.Mod.structure(list{})
    let defaultModuleType = () => Ast_helper.Mty.signature(list{})

    let recoverEqualGreater = p => {
      Parser.expect(EqualGreater, p)
      switch p.Parser.token {
      | MinusGreater => Parser.next(p)
      | _ => ()
      }
    }

    let shouldAbortListParse = p => {
      let rec check = breadcrumbs =>
        switch breadcrumbs {
        | list{} => false
        | list{(grammar, _), ...rest} =>
          if Grammar.isPartOfList(grammar, p.Parser.token) {
            true
          } else {
            check(rest)
          }
        }

      check(p.breadcrumbs)
    }
  }

  module ErrorMessages = {
    let listPatternSpread = "List pattern matches only supports one `...` spread, at the end.
Explanation: a list spread at the tail is efficient, but a spread in the middle would create new list{s}; out of performance concern, our pattern matching currently guarantees to never create new intermediate data."

    let recordPatternSpread = "Record's `...` spread is not supported in pattern matches.
Explanation: you can't collect a subset of a record's field into its own record, since a record needs an explicit declaration and that subset wouldn't have one.
Solution: you need to pull out each field you want explicitly."

    @live let recordPatternUnderscore = "Record patterns only support one `_`, at the end."

    let arrayPatternSpread = "Array's `...` spread is not supported in pattern matches.
Explanation: such spread would create a subarray; out of performance concern, our pattern matching currently guarantees to never create new intermediate data.
Solution: if it's to validate the first few elements, use a `when` clause + Array size check + `get` checks on the current pattern. If it's to obtain a subarray, use `Array.sub` or `Belt.Array.slice`."

    let arrayExprSpread = "Arrays can't use the `...` spread currently. Please use `concat` or other Array helpers."

    let recordExprSpread = "Records can only have one `...` spread, at the beginning.
Explanation: since records have a known, fixed shape, a spread like `{a, ...b}` wouldn't make sense, as `b` would override every field of `a` anyway."

    let listExprSpread = "Lists can only have one `...` spread, and at the end.
Explanation: lists are singly-linked list, where a node contains a value and points to the next node. `list{a, ...bc}` efficiently creates a new item and links `bc` as its next nodes. `[...bc, a]` would be expensive, as it'd need to traverse `bc` and prepend each item to `a` one by one. We therefore disallow such syntax sugar.
Solution: directly use `concat`."

    let variantIdent = "A polymorphic variant (e.g. #id) must start with an alphabetical letter."
  }

  let jsxAttr = (Location.mknoloc("JSX"), Parsetree.PStr(list{}))
  let uncurryAttr = (Location.mknoloc("bs"), Parsetree.PStr(list{}))
  let ternaryAttr = (Location.mknoloc("res.ternary"), Parsetree.PStr(list{}))
  let makeBracesAttr = loc => (Location.mkloc("res.braces", loc), Parsetree.PStr(list{}))

  type typDefOrExt =
    | TypeDef({recFlag: Asttypes.rec_flag, types: list<Parsetree.type_declaration>})
    | TypeExt(Parsetree.type_extension)

  type labelledParameter =
    | TermParameter({
        uncurried: bool,
        attrs: Parsetree.attributes,
        label: Asttypes.arg_label,
        expr: option<Parsetree.expression>,
        pat: Parsetree.pattern,
        pos: Lexing.position,
      })
    | TypeParameter({
        uncurried: bool,
        attrs: Parsetree.attributes,
        locs: list<Location.loc<string>>,
        pos: Lexing.position,
      })

  type recordPatternItem =
    | PatUnderscore
    | PatField((Ast_helper.lid, Parsetree.pattern))

  type context =
    | OrdinaryExpr
    | TernaryTrueBranchExpr
    | WhenExpr

  let getClosingToken = x =>
    switch x {
    | Token.Lparen => Token.Rparen
    | Lbrace => Rbrace
    | Lbracket => Rbracket
    | _ => assert false
    }

  let rec goToClosing = (closingToken, state) =>
    switch (state.Parser.token, closingToken) {
    | (Rparen, Token.Rparen) | (Rbrace, Rbrace) | (Rbracket, Rbracket) =>
      Parser.next(state)
      ()
    | ((Token.Lbracket | Lparen | Lbrace) as t, _) =>
      Parser.next(state)
      goToClosing(getClosingToken(t), state)
      goToClosing(closingToken, state)
    | (Rparen | Token.Rbrace | Rbracket | Eof, _) => () /* TODO: how do report errors here? */
    | _ =>
      Parser.next(state)
      goToClosing(closingToken, state)
    }

  /* Madness */
  let isEs6ArrowExpression = (~inTernary, p) => Parser.lookahead(p, state =>
      switch state.Parser.token {
      | Lident(_) | List | Underscore =>
        Parser.next(state)
        switch state.Parser.token {
        /* Don't think that this valid
         * Imagine: let x = (a: int)
         * This is a parenthesized expression with a type constraint, wait for
         * the arrow */
        /* | Colon when not inTernary -> true */
        | EqualGreater => true
        | _ => false
        }
      | Lparen =>
        let prevEndPos = state.prevEndPos
        Parser.next(state)
        switch state.token {
        | Rparen =>
          Parser.next(state)
          switch state.Parser.token {
          | Colon when !inTernary => true
          | EqualGreater => true
          | _ => false
          }
        | Dot /* uncurried */ => true
        | Tilde => true
        | Backtick => false /* (` always indicates the start of an expr, can't be es6 parameter */
        | _ =>
          goToClosing(Rparen, state)
          switch state.Parser.token {
          | EqualGreater => true
          /* | Lbrace TODO: detect missing =>, is this possible? */
          | Colon when !inTernary => true
          | Rparen => /* imagine having something as :
             * switch colour {
             * | Red
             *    when l == l'
             *    || (&Clflags.classic && (l == Nolabel && !is_optional(l'))) => (t1, t2)
             * We'll arrive at the outer rparen just before the =>.
             * This is not an es6 arrow.
             * */
            false
          | _ =>
            Parser.next(state)
            /* error recovery, peek at the next token,
             * (elements, providerId] => {
             *  in the example above, we have an unbalanced ] here
             */
            switch state.Parser.token {
            | EqualGreater when state.startPos.pos_lnum === prevEndPos.pos_lnum => true
            | _ => false
            }
          }
        }
      | _ => false
      }
    )

  let isEs6ArrowFunctor = p => Parser.lookahead(p, state =>
      switch state.Parser.token {
      /* | Uident _ | Underscore -> */
      /* Parser.next state; */
      /* begin match state.Parser.token with */
      /* | EqualGreater -> true */
      /* | _ -> false */
      /* end */
      | Lparen =>
        Parser.next(state)
        switch state.token {
        | Rparen =>
          Parser.next(state)
          switch state.token {
          | Colon | EqualGreater => true
          | _ => false
          }
        | _ =>
          goToClosing(Rparen, state)
          switch state.Parser.token {
          | EqualGreater | Lbrace => true
          | Colon => true
          | _ => false
          }
        }
      | _ => false
      }
    )

  let isEs6ArrowType = p => Parser.lookahead(p, state =>
      switch state.Parser.token {
      | Lparen =>
        Parser.next(state)
        switch state.Parser.token {
        | Rparen =>
          Parser.next(state)
          switch state.Parser.token {
          | EqualGreater => true
          | _ => false
          }
        | Tilde | Dot => true
        | _ =>
          goToClosing(Rparen, state)
          switch state.Parser.token {
          | EqualGreater => true
          | _ => false
          }
        }
      | Tilde => true
      | _ => false
      }
    )

  let buildLongident = words =>
    switch List.rev(words) {
    | list{} => assert false
    | list{hd, ...tl} => List.fold_left((p, s) => Longident.Ldot(p, s), Lident(hd), tl)
    }

  let makeInfixOperator = (p, token, startPos, endPos) => {
    let stringifiedToken = if token == Token.MinusGreater {
      "|."
    } else if token == Token.PlusPlus {
      "^"
    } else if token == Token.BangEqual {
      "<>"
    } else if token == Token.BangEqualEqual {
      "!="
    } else if token == Token.Equal {
      /* TODO: could have a totally different meaning like x->fooSet(y) */
      Parser.err(~startPos, ~endPos, p, Diagnostics.message("Did you mean `==` here?"))
      "="
    } else if token == Token.EqualEqual {
      "="
    } else if token == Token.EqualEqualEqual {
      "=="
    } else {
      Token.toString(token)
    }

    let loc = mkLoc(startPos, endPos)
    let operator = Location.mkloc(Longident.Lident(stringifiedToken), loc)

    Ast_helper.Exp.ident(~loc, operator)
  }

  let negateString = s =>
    if String.length(s) > 0 && @doesNotRaise String.get(s, 0) == '-' {
      (@doesNotRaise String.sub)(s, 1, String.length(s) - 1)
    } else {
      "-" ++ s
    }

  let makeUnaryExpr = (startPos, tokenEnd, token, operand) =>
    switch (token, operand.Parsetree.pexp_desc) {
    | (Token.Plus | PlusDot, Pexp_constant(Pconst_integer(_) | Pconst_float(_))) => operand
    | (Minus, Pexp_constant(Pconst_integer(n, m))) => {
        ...operand,
        pexp_desc: Pexp_constant(Pconst_integer(negateString(n), m)),
      }
    | (Minus | MinusDot, Pexp_constant(Pconst_float(n, m))) => {
        ...operand,
        pexp_desc: Pexp_constant(Pconst_float(negateString(n), m)),
      }
    | (Token.Plus | PlusDot | Minus | MinusDot, _) =>
      let tokenLoc = mkLoc(startPos, tokenEnd)
      let operator = "~" ++ Token.toString(token)
      Ast_helper.Exp.apply(
        ~loc=mkLoc(startPos, operand.Parsetree.pexp_loc.loc_end),
        Ast_helper.Exp.ident(~loc=tokenLoc, Location.mkloc(Longident.Lident(operator), tokenLoc)),
        list{(Nolabel, operand)},
      )
    | (Token.Bang, _) =>
      let tokenLoc = mkLoc(startPos, tokenEnd)
      Ast_helper.Exp.apply(
        ~loc=mkLoc(startPos, operand.Parsetree.pexp_loc.loc_end),
        Ast_helper.Exp.ident(~loc=tokenLoc, Location.mkloc(Longident.Lident("not"), tokenLoc)),
        list{(Nolabel, operand)},
      )
    | _ => operand
    }

  let makeListExpression = (loc, seq, extOpt) => {
    let rec handleSeq = x =>
      switch x {
      | list{} =>
        switch extOpt {
        | Some(ext) => ext
        | None =>
          let loc = {...loc, Location.loc_ghost: true}
          let nil = Location.mkloc(Longident.Lident("[]"), loc)
          Ast_helper.Exp.construct(~loc, nil, None)
        }
      | list{e1, ...el} =>
        let exp_el = handleSeq(el)
        let loc = mkLoc(e1.Parsetree.pexp_loc.Location.loc_start, exp_el.pexp_loc.loc_end)

        let arg = Ast_helper.Exp.tuple(~loc, list{e1, exp_el})
        Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("::"), loc), Some(arg))
      }

    let expr = handleSeq(seq)
    {...expr, pexp_loc: loc}
  }

  let makeListPattern = (loc, seq, ext_opt) => {
    let rec handle_seq = x =>
      switch x {
      | list{} =>
        let base_case = switch ext_opt {
        | Some(ext) => ext
        | None =>
          let loc = {...loc, Location.loc_ghost: true}
          let nil = {Location.txt: Longident.Lident("[]"), loc: loc}
          Ast_helper.Pat.construct(~loc, nil, None)
        }

        base_case
      | list{p1, ...pl} =>
        let pat_pl = handle_seq(pl)
        let loc = mkLoc(p1.Parsetree.ppat_loc.loc_start, pat_pl.ppat_loc.loc_end)
        let arg = Ast_helper.Pat.mk(~loc, Ppat_tuple(list{p1, pat_pl}))
        Ast_helper.Pat.mk(
          ~loc,
          Ppat_construct(Location.mkloc(Longident.Lident("::"), loc), Some(arg)),
        )
      }

    handle_seq(seq)
  }

  /* {"foo": bar} -> Js.t({. foo: bar})
   * {.. "foo": bar} -> Js.t({.. foo: bar})
   * {..} -> Js.t({..}) */
  let makeBsObjType = (~attrs, ~loc, ~closed, rows) => {
    let obj = Ast_helper.Typ.object_(~loc, rows, closed)
    let jsDotTCtor = Location.mkloc(Longident.Ldot(Longident.Lident("Js"), "t"), loc)

    Ast_helper.Typ.constr(~loc, ~attrs, jsDotTCtor, list{obj})
  }

  /* TODO: diagnostic reporting */
  let lidentOfPath = longident =>
    switch Longident.flatten(longident) |> List.rev {
    | list{} => ""
    | list{ident, ..._} => ident
    }

  let makeNewtypes = (~attrs, ~loc, newtypes, exp) => {
    let expr = List.fold_right(
      (newtype, exp) => Ast_helper.Exp.mk(~loc, Pexp_newtype(newtype, exp)),
      newtypes,
      exp,
    )
    {...expr, pexp_attributes: attrs}
  }

  /* locally abstract types syntax sugar
   * Transforms
   *  let f: type t u v. = (foo : list</t, u, v/>) => ...
   * into
   *  let f = (type t u v. foo : list</t, u, v/>) => ...
   */
  let wrapTypeAnnotation = (~loc, newtypes, core_type, body) => {
    let exp = makeNewtypes(
      ~attrs=list{},
      ~loc,
      newtypes,
      Ast_helper.Exp.constraint_(~loc, body, core_type),
    )

    let typ = Ast_helper.Typ.poly(
      ~loc,
      newtypes,
      Ast_helper.Typ.varify_constructors(newtypes, core_type),
    )

    (exp, typ)
  }

  @ocaml.doc(
    "
    * process the occurrence of _ in the arguments of a function application
    * replace _ with a new variable, currently __x, in the arguments
    * return a wrapping function that wraps ((__x) => ...) around an expression
    * e.g. foo(_, 3) becomes (__x) => foo(__x, 3)
    "
  )
  let processUnderscoreApplication = args => {
    open Parsetree
    let exp_question = ref(None)
    let hidden_var = "__x"
    let check_arg = ((lab, exp) as arg) =>
      switch exp.pexp_desc {
      | Pexp_ident({txt: Lident("_")} as id) =>
        let new_id = Location.mkloc(Longident.Lident(hidden_var), id.loc)
        let new_exp = Ast_helper.Exp.mk(Pexp_ident(new_id), ~loc=exp.pexp_loc)
        exp_question := Some(new_exp)
        (lab, new_exp)
      | _ => arg
      }

    let args = List.map(check_arg, args)
    let wrap = exp_apply =>
      switch exp_question.contents {
      | Some({pexp_loc: loc}) =>
        let pattern = Ast_helper.Pat.mk(Ppat_var(Location.mkloc(hidden_var, loc)), ~loc)
        Ast_helper.Exp.mk(Pexp_fun(Nolabel, None, pattern, exp_apply), ~loc)
      | None => exp_apply
      }

    (args, wrap)
  }

  let rec parseLident = p => {
    let recoverLident = p =>
      if Token.isKeyword(p.Parser.token) && p.Parser.prevEndPos.pos_lnum === p.startPos.pos_lnum {
        Parser.err(p, Diagnostics.lident(p.Parser.token))
        Parser.next(p)
        None
      } else {
        let rec loop = p =>
          if !Recover.shouldAbortListParse(p) {
            Parser.next(p)
            loop(p)
          }

        Parser.next(p)
        loop(p)
        switch p.Parser.token {
        | Lident(_) => Some()
        | _ => None
        }
      }

    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Lident(ident) =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      (ident, loc)
    | List =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      ("list", loc)
    | _ =>
      switch recoverLident(p) {
      | Some() => parseLident(p)
      | None => ("_", mkLoc(startPos, p.prevEndPos))
      }
    }
  }

  let parseIdent = (~msg, ~startPos, p) =>
    switch p.Parser.token {
    | Lident(ident) | Uident(ident) =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      (ident, loc)
    | List =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      ("list", loc)
    | _token =>
      Parser.err(p, Diagnostics.message(msg))
      Parser.next(p)
      ("_", mkLoc(startPos, p.prevEndPos))
    }

  let parseHashIdent = (~startPos, p) => {
    Parser.expect(Hash, p)
    parseIdent(~startPos, ~msg=ErrorMessages.variantIdent, p)
  }

  /* Ldot (Ldot (Lident "Foo", "Bar"), "baz") */
  let parseValuePath = p => {
    let startPos = p.Parser.startPos
    let rec aux = (p, path) =>
      switch p.Parser.token {
      | List => Longident.Ldot(path, "list")
      | Lident(ident) => Longident.Ldot(path, ident)
      | Uident(uident) =>
        Parser.next(p)
        Parser.expect(Dot, p)
        aux(p, Ldot(path, uident))
      | token =>
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        Longident.Lident("_")
      }

    let ident = switch p.Parser.token {
    | List => Longident.Lident("list")
    | Lident(ident) => Longident.Lident(ident)
    | Uident(ident) =>
      Parser.next(p)
      Parser.expect(Dot, p)
      aux(p, Lident(ident))
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Longident.Lident("_")
    }

    Parser.next(p)
    Location.mkloc(ident, mkLoc(startPos, p.prevEndPos))
  }

  let parseValuePathTail = (p, startPos, ident) => {
    let rec loop = (p, path) =>
      switch p.Parser.token {
      | Lident(ident) =>
        Parser.next(p)
        Location.mkloc(Longident.Ldot(path, ident), mkLoc(startPos, p.prevEndPos))
      | List =>
        Parser.next(p)
        Location.mkloc(Longident.Ldot(path, "list"), mkLoc(startPos, p.prevEndPos))
      | Uident(ident) =>
        Parser.next(p)
        Parser.expect(Dot, p)
        loop(p, Longident.Ldot(path, ident))
      | token =>
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        Location.mknoloc(path)
      }

    loop(p, ident)
  }

  let parseModuleLongIdentTail = (~lowercase, p, startPos, ident) => {
    let rec loop = (p, acc) =>
      switch p.Parser.token {
      | List when lowercase =>
        Parser.next(p)
        let lident = Longident.Ldot(acc, "list")
        Location.mkloc(lident, mkLoc(startPos, p.prevEndPos))
      | Lident(ident) when lowercase =>
        Parser.next(p)
        let lident = Longident.Ldot(acc, ident)
        Location.mkloc(lident, mkLoc(startPos, p.prevEndPos))
      | Uident(ident) =>
        Parser.next(p)
        let endPos = p.prevEndPos
        let lident = Longident.Ldot(acc, ident)
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          loop(p, lident)
        | _ => Location.mkloc(lident, mkLoc(startPos, endPos))
        }
      | t =>
        Parser.err(p, Diagnostics.uident(t))
        Location.mkloc(acc, mkLoc(startPos, p.prevEndPos))
      }

    loop(p, ident)
  }

  /* Parses module identifiers:
       Foo
       Foo.Bar */
  let parseModuleLongIdent = (~lowercase, p) => {
    /* Parser.leaveBreadcrumb p Reporting.ModuleLongIdent; */
    let startPos = p.Parser.startPos
    let moduleIdent = switch p.Parser.token {
    | List when lowercase =>
      let loc = mkLoc(startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(Longident.Lident("list"), loc)
    | Lident(ident) when lowercase =>
      let loc = mkLoc(startPos, p.endPos)
      let lident = Longident.Lident(ident)
      Parser.next(p)
      Location.mkloc(lident, loc)
    | Uident(ident) =>
      let lident = Longident.Lident(ident)
      let endPos = p.endPos
      Parser.next(p)
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        parseModuleLongIdentTail(~lowercase, p, startPos, lident)
      | _ => Location.mkloc(lident, mkLoc(startPos, endPos))
      }
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mkloc(Longident.Lident("_"), mkLoc(startPos, p.prevEndPos))
    }

    /* Parser.eatBreadcrumb p; */
    moduleIdent
  }

  /* `window.location` or `Math` or `Foo.Bar` */
  let parseIdentPath = p => {
    let rec loop = (p, acc) =>
      switch p.Parser.token {
      | Uident(ident) | Lident(ident) =>
        Parser.next(p)
        let lident = Longident.Ldot(acc, ident)
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          loop(p, lident)
        | _ => lident
        }
      | _t => acc
      }

    switch p.Parser.token {
    | Lident(ident) | Uident(ident) =>
      Parser.next(p)
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        loop(p, Longident.Lident(ident))
      | _ => Longident.Lident(ident)
      }
    | _ => Longident.Lident("_")
    }
  }

  let verifyJsxOpeningClosingName = (p, nameExpr) => {
    let closing = switch p.Parser.token {
    | Lident(lident) =>
      Parser.next(p)
      Longident.Lident(lident)
    | Uident(_) => parseModuleLongIdent(~lowercase=false, p).txt
    | _ => Longident.Lident("")
    }

    switch nameExpr.Parsetree.pexp_desc {
    | Pexp_ident(openingIdent) =>
      let opening = {
        let withoutCreateElement =
          Longident.flatten(openingIdent.txt) |> List.filter(s => s != "createElement")

        switch Longident.unflatten(withoutCreateElement) {
        | Some(li) => li
        | None => Longident.Lident("")
        }
      }

      opening == closing
    | _ => assert false
    }
  }

  let string_of_pexp_ident = nameExpr =>
    switch nameExpr.Parsetree.pexp_desc {
    | Pexp_ident(openingIdent) =>
      Longident.flatten(openingIdent.txt)
      |> List.filter(s => s != "createElement")
      |> String.concat(".")
    | _ => ""
    }

  /* open-def ::=
   *   | open module-path
   *   | open! module-path */
  let parseOpenDescription = (~attrs, p) => {
    Parser.leaveBreadcrumb(p, Grammar.OpenDescription)
    let startPos = p.Parser.startPos
    Parser.expect(Open, p)
    let override = if Parser.optional(p, Token.Bang) {
      Asttypes.Override
    } else {
      Asttypes.Fresh
    }

    let modident = parseModuleLongIdent(~lowercase=false, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Parser.eatBreadcrumb(p)
    Ast_helper.Opn.mk(~loc, ~attrs, ~override, modident)
  }

  let hexValue = x =>
    switch x {
    | '0' .. '9' => Char.code(x) - 48
    | 'A' .. 'Z' => Char.code(x) - 55
    | 'a' .. 'z' => Char.code(x) - 97
    | _ => 16
    }

  let parseStringLiteral = s => {
    let len = String.length(s)
    let b = Buffer.create(String.length(s))

    let rec loop = i =>
      if i == len {
        ()
      } else {
        let c = String.unsafe_get(s, i)
        switch c {
        | '\\' as c =>
          let nextIx = i + 1
          if nextIx < len {
            let nextChar = String.unsafe_get(s, nextIx)
            switch nextChar {
            | 'n' =>
              Buffer.add_char(b, '\n')
              loop(nextIx + 1)
            | 'r' =>
              Buffer.add_char(b, '\r')
              loop(nextIx + 1)
            | 'b' =>
              Buffer.add_char(b, '\b')
              loop(nextIx + 1)
            | 't' =>
              Buffer.add_char(b, '\t')
              loop(nextIx + 1)
            | '\\' as c =>
              Buffer.add_char(b, c)
              loop(nextIx + 1)
            | ' ' as c =>
              Buffer.add_char(b, c)
              loop(nextIx + 1)
            | '\'' as c =>
              Buffer.add_char(b, c)
              loop(nextIx + 1)
            | '"' as c =>
              Buffer.add_char(b, c)
              loop(nextIx + 1)
            | '0' .. '9' =>
              if nextIx + 2 < len {
                let c0 = nextChar
                let c1 = String.unsafe_get(s, nextIx + 1)
                let c2 = String.unsafe_get(s, nextIx + 2)
                let c =
                  100 * (Char.code(c0) - 48) + 10 * (Char.code(c1) - 48) + (Char.code(c2) - 48)

                if c < 0 || c > 255 {
                  Buffer.add_char(b, '\\')
                  Buffer.add_char(b, c0)
                  Buffer.add_char(b, c1)
                  Buffer.add_char(b, c2)
                  loop(nextIx + 3)
                } else {
                  Buffer.add_char(b, Char.unsafe_chr(c))
                  loop(nextIx + 3)
                }
              } else {
                Buffer.add_char(b, '\\')
                Buffer.add_char(b, nextChar)
                loop(nextIx + 1)
              }
            | 'o' =>
              if nextIx + 3 < len {
                let c0 = String.unsafe_get(s, nextIx + 1)
                let c1 = String.unsafe_get(s, nextIx + 2)
                let c2 = String.unsafe_get(s, nextIx + 3)
                let c = 64 * (Char.code(c0) - 48) + 8 * (Char.code(c1) - 48) + (Char.code(c2) - 48)

                if c < 0 || c > 255 {
                  Buffer.add_char(b, '\\')
                  Buffer.add_char(b, '0')
                  Buffer.add_char(b, c0)
                  Buffer.add_char(b, c1)
                  Buffer.add_char(b, c2)
                  loop(nextIx + 4)
                } else {
                  Buffer.add_char(b, Char.unsafe_chr(c))
                  loop(nextIx + 4)
                }
              } else {
                Buffer.add_char(b, '\\')
                Buffer.add_char(b, nextChar)
                loop(nextIx + 1)
              }
            | 'x' as c =>
              if nextIx + 2 < len {
                let c0 = String.unsafe_get(s, nextIx + 1)
                let c1 = String.unsafe_get(s, nextIx + 2)
                let c = 16 * hexValue(c0) + hexValue(c1)
                if c < 0 || c > 255 {
                  Buffer.add_char(b, '\\')
                  Buffer.add_char(b, 'x')
                  Buffer.add_char(b, c0)
                  Buffer.add_char(b, c1)
                  loop(nextIx + 3)
                } else {
                  Buffer.add_char(b, Char.unsafe_chr(c))
                  loop(nextIx + 3)
                }
              } else {
                Buffer.add_char(b, '\\')
                Buffer.add_char(b, c)
                loop(nextIx + 2)
              }
            | _ =>
              Buffer.add_char(b, c)
              Buffer.add_char(b, nextChar)
              loop(nextIx + 1)
            }
          } else {
            Buffer.add_char(b, c)
            ()
          }
        | c =>
          Buffer.add_char(b, c)
          loop(i + 1)
        }
      }

    loop(0)
    Buffer.contents(b)
  }

  let parseTemplateStringLiteral = s => {
    let len = String.length(s)
    let b = Buffer.create(len)

    let rec loop = i =>
      if i < len {
        let c = String.unsafe_get(s, i)
        switch c {
        | '\\' as c =>
          if i + 1 < len {
            let nextChar = String.unsafe_get(s, i + 1)
            switch nextChar {
            | '\\' as c =>
              Buffer.add_char(b, c)
              loop(i + 2)
            | '$' as c =>
              Buffer.add_char(b, c)
              loop(i + 2)
            | '`' as c =>
              Buffer.add_char(b, c)
              loop(i + 2)
            | c =>
              Buffer.add_char(b, '\\')
              Buffer.add_char(b, c)
              loop(i + 2)
            }
          } else {
            Buffer.add_char(b, c)
          }

        | c =>
          Buffer.add_char(b, c)
          loop(i + 1)
        }
      } else {
        ()
      }

    loop(0)
    Buffer.contents(b)
  }

  /* constant	::=	integer-literal */
  /* ∣	 float-literal */
  /* ∣	 string-literal */
  let parseConstant = p => {
    let isNegative = switch p.Parser.token {
    | Token.Minus =>
      Parser.next(p)
      true
    | Plus =>
      Parser.next(p)
      false
    | _ => false
    }

    let constant = switch p.Parser.token {
    | Int({i, suffix}) =>
      let intTxt = if isNegative {
        "-" ++ i
      } else {
        i
      }
      Parsetree.Pconst_integer(intTxt, suffix)
    | Float({f, suffix}) =>
      let floatTxt = if isNegative {
        "-" ++ f
      } else {
        f
      }
      Parsetree.Pconst_float(floatTxt, suffix)
    | String(s) =>
      let txt = if p.mode == ParseForTypeChecker {
        parseStringLiteral(s)
      } else {
        s
      }

      Pconst_string(txt, None)
    | Character(c) => Pconst_char(c)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Pconst_string("", None)
    }

    Parser.next(p)
    constant
  }

  let parseCommaDelimitedRegion = (p, ~grammar, ~closing, ~f) => {
    Parser.leaveBreadcrumb(p, grammar)
    let rec loop = nodes =>
      switch f(p) {
      | Some(node) =>
        switch p.Parser.token {
        | Comma =>
          Parser.next(p)
          loop(list{node, ...nodes})
        | token when token == closing || token == Eof => List.rev(list{node, ...nodes})
        | _ =>
          if !(p.token == Eof || (p.token == closing || Recover.shouldAbortListParse(p))) {
            Parser.expect(Comma, p)
          }
          if p.token == Semicolon {
            Parser.next(p)
          }
          loop(list{node, ...nodes})
        }
      | None =>
        if p.token == Eof || (p.token == closing || Recover.shouldAbortListParse(p)) {
          List.rev(nodes)
        } else {
          Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
          Parser.next(p)
          loop(nodes)
        }
      }

    let nodes = loop(list{})
    Parser.eatBreadcrumb(p)
    nodes
  }

  let parseCommaDelimitedReversedList = (p, ~grammar, ~closing, ~f) => {
    Parser.leaveBreadcrumb(p, grammar)
    let rec loop = nodes =>
      switch f(p) {
      | Some(node) =>
        switch p.Parser.token {
        | Comma =>
          Parser.next(p)
          loop(list{node, ...nodes})
        | token when token == closing || token == Eof => list{node, ...nodes}
        | _ =>
          if !(p.token == Eof || (p.token == closing || Recover.shouldAbortListParse(p))) {
            Parser.expect(Comma, p)
          }
          if p.token == Semicolon {
            Parser.next(p)
          }
          loop(list{node, ...nodes})
        }
      | None =>
        if p.token == Eof || (p.token == closing || Recover.shouldAbortListParse(p)) {
          nodes
        } else {
          Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
          Parser.next(p)
          loop(nodes)
        }
      }

    let nodes = loop(list{})
    Parser.eatBreadcrumb(p)
    nodes
  }

  let parseDelimitedRegion = (p, ~grammar, ~closing, ~f) => {
    Parser.leaveBreadcrumb(p, grammar)
    let rec loop = nodes =>
      switch f(p) {
      | Some(node) => loop(list{node, ...nodes})
      | None =>
        if p.Parser.token == Token.Eof || (p.token == closing || Recover.shouldAbortListParse(p)) {
          List.rev(nodes)
        } else {
          Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
          Parser.next(p)
          loop(nodes)
        }
      }

    let nodes = loop(list{})
    Parser.eatBreadcrumb(p)
    nodes
  }

  let parseRegion = (p, ~grammar, ~f) => {
    Parser.leaveBreadcrumb(p, grammar)
    let rec loop = nodes =>
      switch f(p) {
      | Some(node) => loop(list{node, ...nodes})
      | None =>
        if p.Parser.token == Token.Eof || Recover.shouldAbortListParse(p) {
          List.rev(nodes)
        } else {
          Parser.err(p, Diagnostics.unexpected(p.token, p.breadcrumbs))
          Parser.next(p)
          loop(nodes)
        }
      }

    let nodes = loop(list{})
    Parser.eatBreadcrumb(p)
    nodes
  }

  /* let-binding	::=	pattern =  expr */
  /* ∣	 value-name  { parameter }  [: typexpr]  [:> typexpr] =  expr */
  /* ∣	 value-name :  poly-typexpr =  expr */

  /* pattern	::=	value-name */
  /* ∣	 _ */
  /* ∣	 constant */
  /* ∣	 pattern as  value-name */
  /* ∣	 ( pattern ) */
  /* ∣	 ( pattern :  typexpr ) */
  /* ∣	 pattern |  pattern */
  /* ∣	 constr  pattern */
  /* ∣	 #variant variant-pattern */
  /* ∣	 ##type */
  /* ∣	 / pattern  { , pattern }+  / */
  /* ∣	 { field  [: typexpr]  [= pattern] { ; field  [: typexpr]  [= pattern] }  [; _ ] [ ; ] } */
  /* ∣	 [ pattern  { ; pattern }  [ ; ] ] */
  /* ∣	 pattern ::  pattern */
  /* ∣	 [| pattern  { ; pattern }  [ ; ] |] */
  /* ∣	 char-literal ..  char-literal */
  /* 	∣	 exception pattern */
  let rec parsePattern = (~alias=true, ~or_=true, p) => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    let pat = switch p.Parser.token {
    | (True | False) as token =>
      let endPos = p.endPos
      Parser.next(p)
      let loc = mkLoc(startPos, endPos)
      Ast_helper.Pat.construct(
        ~loc,
        Location.mkloc(Longident.Lident(Token.toString(token)), loc),
        None,
      )
    | Int(_) | String(_) | Float(_) | Character(_) | Minus | Plus =>
      let c = parseConstant(p)
      switch p.token {
      | DotDot =>
        Parser.next(p)
        let c2 = parseConstant(p)
        Ast_helper.Pat.interval(~loc=mkLoc(startPos, p.prevEndPos), c, c2)
      | _ => Ast_helper.Pat.constant(~loc=mkLoc(startPos, p.prevEndPos), c)
      }
    | Lparen =>
      Parser.next(p)
      switch p.token {
      | Rparen =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let lid = Location.mkloc(Longident.Lident("()"), loc)
        Ast_helper.Pat.construct(~loc, lid, None)
      | _ =>
        let pat = parseConstrainedPattern(p)
        switch p.token {
        | Comma =>
          Parser.next(p)
          parseTuplePattern(~attrs, ~first=pat, ~startPos, p)
        | _ =>
          Parser.expect(Rparen, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          {...pat, ppat_loc: loc}
        }
      }
    | Lbracket => parseArrayPattern(~attrs, p)
    | Lbrace => parseRecordPattern(~attrs, p)
    | Underscore =>
      let endPos = p.endPos
      let loc = mkLoc(startPos, endPos)
      Parser.next(p)
      Ast_helper.Pat.any(~loc, ~attrs, ())
    | Lident(ident) =>
      let endPos = p.endPos
      let loc = mkLoc(startPos, endPos)
      Parser.next(p)
      Ast_helper.Pat.var(~loc, ~attrs, Location.mkloc(ident, loc))
    | Uident(_) =>
      let constr = parseModuleLongIdent(~lowercase=false, p)
      switch p.Parser.token {
      | Lparen => parseConstructorPatternArgs(p, constr, startPos, attrs)
      | _ => Ast_helper.Pat.construct(~loc=constr.loc, ~attrs, constr, None)
      }
    | Hash =>
      let (ident, loc) = parseHashIdent(~startPos, p)
      switch p.Parser.token {
      | Lparen => parseVariantPatternArgs(p, ident, startPos, attrs)
      | _ => Ast_helper.Pat.variant(~loc, ~attrs, ident, None)
      }
    | HashHash =>
      Parser.next(p)
      let ident = parseValuePath(p)
      let loc = mkLoc(startPos, ident.loc.loc_end)
      Ast_helper.Pat.type_(~loc, ~attrs, ident)
    | Exception =>
      Parser.next(p)
      let pat = parsePattern(~alias=false, ~or_=false, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Pat.exception_(~loc, ~attrs, pat)
    | Lazy =>
      Parser.next(p)
      let pat = parsePattern(~alias=false, ~or_=false, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Pat.lazy_(~loc, ~attrs, pat)
    | List =>
      Parser.next(p)
      switch p.token {
      | Lbracket => parseListPattern(~startPos, ~attrs, p)
      | _ =>
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Pat.var(~loc, ~attrs, Location.mkloc("list", loc))
      }
    | Module => parseModulePattern(~attrs, p)
    | Percent =>
      let extension = parseExtension(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Pat.extension(~loc, ~attrs, extension)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      switch skipTokensAndMaybeRetry(p, ~isStartOfGrammar=Grammar.isAtomicPatternStart) {
      | None => Recover.defaultPattern()
      | Some() => parsePattern(p)
      }
    }

    let pat = if alias {
      parseAliasPattern(~attrs, pat, p)
    } else {
      pat
    }
    if or_ {
      parseOrPattern(pat, p)
    } else {
      pat
    }
  }

  and skipTokensAndMaybeRetry = (p, ~isStartOfGrammar) =>
    if Token.isKeyword(p.Parser.token) && p.Parser.prevEndPos.pos_lnum === p.startPos.pos_lnum {
      Parser.next(p)
      None
    } else if Recover.shouldAbortListParse(p) {
      if isStartOfGrammar(p.Parser.token) {
        Parser.next(p)
        Some()
      } else {
        None
      }
    } else {
      Parser.next(p)
      let rec loop = p =>
        if !Recover.shouldAbortListParse(p) {
          Parser.next(p)
          loop(p)
        }
      loop(p)
      if isStartOfGrammar(p.Parser.token) {
        Some()
      } else {
        None
      }
    }

  /* alias ::= pattern as lident */
  and parseAliasPattern = (~attrs, pattern, p) =>
    switch p.Parser.token {
    | As =>
      Parser.next(p)
      let (name, loc) = parseLident(p)
      let name = Location.mkloc(name, loc)
      Ast_helper.Pat.alias(~loc={...pattern.ppat_loc, loc_end: p.prevEndPos}, ~attrs, pattern, name)
    | _ => pattern
    }

  /* or ::= pattern | pattern
   * precedence: Red | Blue | Green is interpreted as (Red | Blue) | Green */
  and parseOrPattern = (pattern1, p) => {
    let rec loop = pattern1 =>
      switch p.Parser.token {
      | Bar =>
        Parser.next(p)
        let pattern2 = parsePattern(~or_=false, p)
        let loc = {
          ...pattern1.Parsetree.ppat_loc,
          loc_end: pattern2.ppat_loc.loc_end,
        }
        loop(Ast_helper.Pat.or_(~loc, pattern1, pattern2))
      | _ => pattern1
      }

    loop(pattern1)
  }

  and parseNonSpreadPattern = (~msg, p) => {
    let () = switch p.Parser.token {
    | DotDotDot =>
      Parser.err(p, Diagnostics.message(msg))
      Parser.next(p)
    | _ => ()
    }

    switch p.Parser.token {
    | token when Grammar.isPatternStart(token) =>
      let pat = parsePattern(p)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(pat.ppat_loc.loc_start, typ.Parsetree.ptyp_loc.loc_end)
        Some(Ast_helper.Pat.constraint_(~loc, pat, typ))
      | _ => Some(pat)
      }
    | _ => None
    }
  }

  and parseConstrainedPattern = p => {
    let pat = parsePattern(p)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let typ = parseTypExpr(p)
      let loc = mkLoc(pat.ppat_loc.loc_start, typ.Parsetree.ptyp_loc.loc_end)
      Ast_helper.Pat.constraint_(~loc, pat, typ)
    | _ => pat
    }
  }

  and parseConstrainedPatternRegion = p =>
    switch p.Parser.token {
    | token when Grammar.isPatternStart(token) => Some(parseConstrainedPattern(p))
    | _ => None
    }

  /* field ::=
   *   | longident
   *   | longident : pattern
   *   | longident as lident
   *
   *  row ::=
   *	 | field ,
   *	 | field , _
   *	 | field , _,
   */
  and parseRecordPatternField = p => {
    let startPos = p.Parser.startPos
    let label = parseValuePath(p)
    let pattern = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      parsePattern(p)
    | _ => Ast_helper.Pat.var(~loc=label.loc, Location.mkloc(Longident.last(label.txt), label.loc))
    }

    switch p.token {
    | As =>
      Parser.next(p)
      let (name, loc) = parseLident(p)
      let name = Location.mkloc(name, loc)
      let aliasPattern = Ast_helper.Pat.alias(~loc=mkLoc(startPos, p.prevEndPos), pattern, name)

      (Location.mkloc(label.txt, mkLoc(startPos, aliasPattern.ppat_loc.loc_end)), aliasPattern)
    | _ => (label, pattern)
    }
  }

  /* TODO: there are better representations than PatField|Underscore ? */
  and parseRecordPatternItem = p =>
    switch p.Parser.token {
    | DotDotDot =>
      Parser.next(p)
      Some(true, PatField(parseRecordPatternField(p)))
    | Uident(_) | Lident(_) => Some(false, PatField(parseRecordPatternField(p)))
    | Underscore =>
      Parser.next(p)
      Some(false, PatUnderscore)
    | _ => None
    }

  and parseRecordPattern = (~attrs, p) => {
    let startPos = p.startPos
    Parser.expect(Lbrace, p)
    let rawFields = parseCommaDelimitedReversedList(
      p,
      ~grammar=PatternRecord,
      ~closing=Rbrace,
      ~f=parseRecordPatternItem,
    )

    Parser.expect(Rbrace, p)
    let (fields, closedFlag) = {
      let (rawFields, flag) = switch rawFields {
      | list{(_hasSpread, PatUnderscore), ...rest} => (rest, Asttypes.Open)
      | rawFields => (rawFields, Asttypes.Closed)
      }

      List.fold_left(((fields, flag), curr) => {
        let (hasSpread, field) = curr
        switch field {
        | PatField(field) =>
          if hasSpread {
            let (_, pattern) = field
            Parser.err(
              ~startPos=pattern.Parsetree.ppat_loc.loc_start,
              p,
              Diagnostics.message(ErrorMessages.recordPatternSpread),
            )
          }
          (list{field, ...fields}, flag)
        | PatUnderscore => (fields, flag)
        }
      }, (list{}, flag), rawFields)
    }

    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Pat.record(~loc, ~attrs, fields, closedFlag)
  }

  and parseTuplePattern = (~attrs, ~first, ~startPos, p) => {
    let patterns = parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.PatternList,
      ~closing=Rparen,
      ~f=parseConstrainedPatternRegion,
    )

    Parser.expect(Rparen, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Pat.tuple(~loc, ~attrs, list{first, ...patterns})
  }

  and parsePatternRegion = p =>
    switch p.Parser.token {
    | DotDotDot =>
      Parser.next(p)
      Some(true, parseConstrainedPattern(p))
    | token when Grammar.isPatternStart(token) => Some(false, parseConstrainedPattern(p))
    | _ => None
    }

  and parseModulePattern = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Module, p)
    Parser.expect(Lparen, p)
    let uident = switch p.token {
    | Uident(uident) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(uident, loc)
    | _ =>
      /* TODO: error recovery */
      Location.mknoloc("_")
    }

    switch p.token {
    | Colon =>
      let colonStart = p.Parser.startPos
      Parser.next(p)
      let packageTypAttrs = parseAttributes(p)
      let packageType = parsePackageType(~startPos=colonStart, ~attrs=packageTypAttrs, p)
      Parser.expect(Rparen, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let unpack = Ast_helper.Pat.unpack(~loc=uident.loc, uident)
      Ast_helper.Pat.constraint_(~loc, ~attrs, unpack, packageType)
    | _ =>
      Parser.expect(Rparen, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Pat.unpack(~loc, ~attrs, uident)
    }
  }

  and parseListPattern = (~startPos, ~attrs, p) => {
    Parser.expect(Lbracket, p)
    let listPatterns = parseCommaDelimitedReversedList(
      p,
      ~grammar=Grammar.PatternOcamlList,
      ~closing=Rbracket,
      ~f=parsePatternRegion,
    )

    Parser.expect(Rbracket, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let filterSpread = ((hasSpread, pattern)) =>
      if hasSpread {
        Parser.err(
          ~startPos=pattern.Parsetree.ppat_loc.loc_start,
          p,
          Diagnostics.message(ErrorMessages.listPatternSpread),
        )
        pattern
      } else {
        pattern
      }

    switch listPatterns {
    | list{(true, pattern), ...patterns} =>
      let patterns = patterns |> List.map(filterSpread) |> List.rev
      let pat = makeListPattern(loc, patterns, Some(pattern))
      {...pat, ppat_loc: loc, ppat_attributes: attrs}
    | patterns =>
      let patterns = patterns |> List.map(filterSpread) |> List.rev
      let pat = makeListPattern(loc, patterns, None)
      {...pat, ppat_loc: loc, ppat_attributes: attrs}
    }
  }

  and parseArrayPattern = (~attrs, p) => {
    let startPos = p.startPos
    Parser.expect(Lbracket, p)
    let patterns = parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.PatternList,
      ~closing=Rbracket,
      ~f=parseNonSpreadPattern(~msg=ErrorMessages.arrayPatternSpread),
    )

    Parser.expect(Rbracket, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Pat.array(~loc, ~attrs, patterns)
  }

  and parseConstructorPatternArgs = (p, constr, startPos, attrs) => {
    let lparen = p.startPos
    Parser.expect(Lparen, p)
    let args = parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.PatternList,
      ~closing=Rparen,
      ~f=parseConstrainedPatternRegion,
    )

    Parser.expect(Rparen, p)
    let args = switch args {
    | list{} =>
      let loc = mkLoc(lparen, p.prevEndPos)
      Some(Ast_helper.Pat.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None))
    | list{{ppat_desc: Ppat_tuple(_)} as pat} as patterns =>
      if p.mode == ParseForTypeChecker {
        /* Some(1, 2) for type-checker */
        Some(pat)
      } else {
        /* Some((1, 2)) for printer */
        Some(Ast_helper.Pat.tuple(~loc=mkLoc(lparen, p.endPos), patterns))
      }
    | list{pattern} => Some(pattern)
    | patterns => Some(Ast_helper.Pat.tuple(~loc=mkLoc(lparen, p.endPos), patterns))
    }

    Ast_helper.Pat.construct(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, constr, args)
  }

  and parseVariantPatternArgs = (p, ident, startPos, attrs) => {
    let lparen = p.startPos
    Parser.expect(Lparen, p)
    let patterns = parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.PatternList,
      ~closing=Rparen,
      ~f=parseConstrainedPatternRegion,
    )
    let args = switch patterns {
    | list{{ppat_desc: Ppat_tuple(_)} as pat} as patterns =>
      if p.mode == ParseForTypeChecker {
        /* #ident(1, 2) for type-checker */
        Some(pat)
      } else {
        /* #ident((1, 2)) for printer */
        Some(Ast_helper.Pat.tuple(~loc=mkLoc(lparen, p.endPos), patterns))
      }
    | list{pattern} => Some(pattern)
    | patterns => Some(Ast_helper.Pat.tuple(~loc=mkLoc(lparen, p.endPos), patterns))
    }

    Parser.expect(Rparen, p)
    Ast_helper.Pat.variant(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, ident, args)
  }

  and parseExpr = (~context=OrdinaryExpr, p) => {
    let expr = parseOperandExpr(~context, p)
    let expr = parseBinaryExpr(~context, ~a=expr, p, 1)
    parseTernaryExpr(expr, p)
  }

  /* expr ? expr : expr */
  and parseTernaryExpr = (leftOperand, p) =>
    switch p.Parser.token {
    | Question =>
      Parser.leaveBreadcrumb(p, Grammar.Ternary)
      Parser.next(p)
      let trueBranch = parseExpr(~context=TernaryTrueBranchExpr, p)
      Parser.expect(Colon, p)
      let falseBranch = parseExpr(p)
      Parser.eatBreadcrumb(p)
      let loc = {
        ...leftOperand.Parsetree.pexp_loc,
        loc_start: leftOperand.pexp_loc.loc_start,
        loc_end: falseBranch.Parsetree.pexp_loc.loc_end,
      }
      Ast_helper.Exp.ifthenelse(
        ~attrs=list{ternaryAttr},
        ~loc,
        leftOperand,
        trueBranch,
        Some(falseBranch),
      )
    | _ => leftOperand
    }

  and parseEs6ArrowExpression = (~parameters=?, p) => {
    let startPos = p.Parser.startPos
    Parser.leaveBreadcrumb(p, Grammar.Es6ArrowExpr)
    let parameters = switch parameters {
    | Some(params) => params
    | None => parseParameters(p)
    }

    let returnType = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      Some(parseTypExpr(~es6Arrow=false, p))
    | _ => None
    }

    Parser.expect(EqualGreater, p)
    let body = {
      let expr = parseExpr(p)
      switch returnType {
      | Some(typ) =>
        Ast_helper.Exp.constraint_(
          ~loc=mkLoc(expr.pexp_loc.loc_start, typ.Parsetree.ptyp_loc.loc_end),
          expr,
          typ,
        )
      | None => expr
      }
    }

    Parser.eatBreadcrumb(p)
    let endPos = p.prevEndPos
    let arrowExpr = List.fold_right((parameter, expr) =>
      switch parameter {
      | TermParameter({uncurried, attrs, label: lbl, expr: defaultExpr, pat, pos: startPos}) =>
        let attrs = if uncurried {
          list{uncurryAttr, ...attrs}
        } else {
          attrs
        }
        Ast_helper.Exp.fun_(~loc=mkLoc(startPos, endPos), ~attrs, lbl, defaultExpr, pat, expr)
      | TypeParameter({uncurried, attrs, locs: newtypes, pos: startPos}) =>
        let attrs = if uncurried {
          list{uncurryAttr, ...attrs}
        } else {
          attrs
        }
        makeNewtypes(~attrs, ~loc=mkLoc(startPos, endPos), newtypes, expr)
      }
    , parameters, body)

    {...arrowExpr, pexp_loc: {...arrowExpr.pexp_loc, loc_start: startPos}}
  }

  /*
   * uncurried_parameter ::=
   *   | . parameter
   *
   * parameter ::=
   *   | pattern
   *   | pattern : type
   *   | ~ labelName
   *   | ~ labelName as pattern
   *   | ~ labelName as pattern : type
   *   | ~ labelName = expr
   *   | ~ labelName as pattern = expr
   *   | ~ labelName as pattern : type = expr
   *   | ~ labelName = ?
   *   | ~ labelName as pattern = ?
   *   | ~ labelName as pattern : type = ?
   *
   * labelName ::= lident
   */
  and parseParameter = p =>
    if (
      p.Parser.token == Token.Typ ||
        (p.token == Tilde ||
        (p.token == Dot || Grammar.isPatternStart(p.token)))
    ) {
      let startPos = p.Parser.startPos
      let uncurried = Parser.optional(p, Token.Dot)
      /* two scenarios:
       *   attrs ~lbl ...
       *   attrs pattern
       * Attributes before a labelled arg, indicate that it's on the whole arrow expr
       * Otherwise it's part of the pattern
       * */
      let attrs = parseAttributes(p)
      if p.Parser.token == Typ {
        Parser.next(p)
        let lidents = parseLidentList(p)
        Some(TypeParameter({uncurried: uncurried, attrs: attrs, locs: lidents, pos: startPos}))
      } else {
        let (attrs, lbl, pat) = switch p.Parser.token {
        | Tilde =>
          Parser.next(p)
          let (lblName, loc) = parseLident(p)
          let propLocAttr = (Location.mkloc("res.namedArgLoc", loc), Parsetree.PStr(list{}))
          switch p.Parser.token {
          | Comma | Equal | Rparen =>
            let loc = mkLoc(startPos, p.prevEndPos)
            (
              attrs,
              Asttypes.Labelled(lblName),
              Ast_helper.Pat.var(~attrs=list{propLocAttr}, ~loc, Location.mkloc(lblName, loc)),
            )
          | Colon =>
            let lblEnd = p.prevEndPos
            Parser.next(p)
            let typ = parseTypExpr(p)
            let loc = mkLoc(startPos, lblEnd)
            let pat = {
              let pat = Ast_helper.Pat.var(~loc, Location.mkloc(lblName, loc))
              let loc = mkLoc(startPos, p.prevEndPos)
              Ast_helper.Pat.constraint_(~attrs=list{propLocAttr}, ~loc, pat, typ)
            }
            (attrs, Asttypes.Labelled(lblName), pat)
          | As =>
            Parser.next(p)
            let pat = {
              let pat = parseConstrainedPattern(p)
              {...pat, ppat_attributes: list{propLocAttr, ...pat.ppat_attributes}}
            }

            (attrs, Asttypes.Labelled(lblName), pat)
          | t =>
            Parser.err(p, Diagnostics.unexpected(t, p.breadcrumbs))
            let loc = mkLoc(startPos, p.prevEndPos)
            (
              attrs,
              Asttypes.Labelled(lblName),
              Ast_helper.Pat.var(~loc, Location.mkloc(lblName, loc)),
            )
          }
        | _ =>
          let pattern = parseConstrainedPattern(p)
          let attrs = List.concat(list{attrs, pattern.ppat_attributes})
          (list{}, Asttypes.Nolabel, {...pattern, ppat_attributes: attrs})
        }

        switch p.Parser.token {
        | Equal =>
          Parser.next(p)
          let lbl = switch lbl {
          | Asttypes.Labelled(lblName) => Asttypes.Optional(lblName)
          | Asttypes.Optional(_) as lbl => lbl
          | Asttypes.Nolabel => Asttypes.Nolabel
          }

          switch p.Parser.token {
          | Question =>
            Parser.next(p)
            Some(
              TermParameter({
                uncurried: uncurried,
                attrs: attrs,
                label: lbl,
                expr: None,
                pat: pat,
                pos: startPos,
              }),
            )
          | _ =>
            let expr = parseConstrainedOrCoercedExpr(p)
            Some(
              TermParameter({
                uncurried: uncurried,
                attrs: attrs,
                label: lbl,
                expr: Some(expr),
                pat: pat,
                pos: startPos,
              }),
            )
          }
        | _ =>
          Some(
            TermParameter({
              uncurried: uncurried,
              attrs: attrs,
              label: lbl,
              expr: None,
              pat: pat,
              pos: startPos,
            }),
          )
        }
      }
    } else {
      None
    }

  and parseParameterList = p => {
    let parameters = parseCommaDelimitedRegion(
      ~grammar=Grammar.ParameterList,
      ~f=parseParameter,
      ~closing=Rparen,
      p,
    )

    Parser.expect(Rparen, p)
    parameters
  }

  /* parameters ::=
   *   | _
   *   | lident
   *   | ()
   *   | (.)
   *   | ( parameter {, parameter} [,] )
   */
  and parseParameters = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Lident(ident) =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.Parser.prevEndPos)
      list{
        TermParameter({
          uncurried: false,
          attrs: list{},
          label: Asttypes.Nolabel,
          expr: None,
          pat: Ast_helper.Pat.var(~loc, Location.mkloc(ident, loc)),
          pos: startPos,
        }),
      }
    | List =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.Parser.prevEndPos)
      list{
        TermParameter({
          uncurried: false,
          attrs: list{},
          label: Asttypes.Nolabel,
          expr: None,
          pat: Ast_helper.Pat.var(~loc, Location.mkloc("list", loc)),
          pos: startPos,
        }),
      }
    | Underscore =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.Parser.prevEndPos)
      list{
        TermParameter({
          uncurried: false,
          attrs: list{},
          label: Asttypes.Nolabel,
          expr: None,
          pat: Ast_helper.Pat.any(~loc, ()),
          pos: startPos,
        }),
      }
    | Lparen =>
      Parser.next(p)
      switch p.Parser.token {
      | Rparen =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.Parser.prevEndPos)
        let unitPattern = Ast_helper.Pat.construct(
          ~loc,
          Location.mkloc(Longident.Lident("()"), loc),
          None,
        )

        list{
          TermParameter({
            uncurried: false,
            attrs: list{},
            label: Asttypes.Nolabel,
            expr: None,
            pat: unitPattern,
            pos: startPos,
          }),
        }
      | Dot =>
        Parser.next(p)
        switch p.token {
        | Rparen =>
          Parser.next(p)
          let loc = mkLoc(startPos, p.Parser.prevEndPos)
          let unitPattern = Ast_helper.Pat.construct(
            ~loc,
            Location.mkloc(Longident.Lident("()"), loc),
            None,
          )

          list{
            TermParameter({
              uncurried: true,
              attrs: list{},
              label: Asttypes.Nolabel,
              expr: None,
              pat: unitPattern,
              pos: startPos,
            }),
          }
        | _ =>
          switch parseParameterList(p) {
          | list{
              TermParameter({attrs, label: lbl, expr: defaultExpr, pat: pattern, pos: startPos}),
              ...rest,
            } => list{
              TermParameter({
                uncurried: true,
                attrs: attrs,
                label: lbl,
                expr: defaultExpr,
                pat: pattern,
                pos: startPos,
              }),
              ...rest,
            }
          | parameters => parameters
          }
        }
      | _ => parseParameterList(p)
      }
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      list{}
    }
  }

  and parseCoercedExpr = (~expr: Parsetree.expression, p) => {
    Parser.expect(ColonGreaterThan, p)
    let typ = parseTypExpr(p)
    let loc = mkLoc(expr.pexp_loc.loc_start, p.prevEndPos)
    Ast_helper.Exp.coerce(~loc, expr, None, typ)
  }

  and parseConstrainedOrCoercedExpr = p => {
    let expr = parseExpr(p)
    switch p.Parser.token {
    | ColonGreaterThan => parseCoercedExpr(~expr, p)
    | Colon =>
      Parser.next(p)
      switch p.token {
      | _ =>
        let typ = parseTypExpr(p)
        let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
        let expr = Ast_helper.Exp.constraint_(~loc, expr, typ)
        switch p.token {
        | ColonGreaterThan => parseCoercedExpr(~expr, p)
        | _ => expr
        }
      }
    | _ => expr
    }
  }

  and parseConstrainedExprRegion = p =>
    switch p.Parser.token {
    | token when Grammar.isExprStart(token) =>
      let expr = parseExpr(p)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
        Some(Ast_helper.Exp.constraint_(~loc, expr, typ))
      | _ => Some(expr)
      }
    | _ => None
    }

  /* Atomic expressions represent unambiguous expressions.
   * This means that regardless of the context, these expressions
   * are always interpreted correctly. */
  and parseAtomicExpr = p => {
    Parser.leaveBreadcrumb(p, Grammar.ExprOperand)
    let startPos = p.Parser.startPos
    let expr = switch p.Parser.token {
    | (True | False) as token =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.construct(
        ~loc,
        Location.mkloc(Longident.Lident(Token.toString(token)), loc),
        None,
      )
    | Int(_) | String(_) | Float(_) | Character(_) =>
      let c = parseConstant(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.constant(~loc, c)
    | Backtick =>
      let expr = parseTemplateExpr(p)
      {...expr, pexp_loc: mkLoc(startPos, p.prevEndPos)}
    | Uident(_) | Lident(_) => parseValueOrConstructor(p)
    | Hash => parsePolyVariantExpr(p)
    | Lparen =>
      Parser.next(p)
      switch p.Parser.token {
      | Rparen =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None)
      | _t =>
        let expr = parseConstrainedOrCoercedExpr(p)
        switch p.token {
        | Comma =>
          Parser.next(p)
          parseTupleExpr(~startPos, ~first=expr, p)
        | _ =>
          Parser.expect(Rparen, p)
          expr
        /* {expr with pexp_loc = mkLoc startPos p.prevEndPos}
         * What does this location mean here? It means that when there's
         * a parenthesized we keep the location here for whitespace interleaving.
         * Without the closing paren in the location there will always be an extra
         * line. For now we don't include it, because it does weird things
         * with for comments. */
        }
      }
    | List =>
      Parser.next(p)
      switch p.token {
      | Lbracket => parseListExpr(~startPos, p)
      | _ =>
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Exp.ident(~loc, Location.mkloc(Longident.Lident("list"), loc))
      }
    | Module =>
      Parser.next(p)
      parseFirstClassModuleExpr(~startPos, p)
    | Lbracket => parseArrayExp(p)
    | Lbrace => parseBracedOrRecordExpr(p)
    | LessThan => parseJsx(p)
    | Percent =>
      let extension = parseExtension(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.extension(~loc, extension)
    | Underscore as token =>
      /* This case is for error recovery. Not sure if it's the correct place */
      Parser.err(p, Diagnostics.lident(token))
      Parser.next(p)
      Recover.defaultExpr()
    | token =>
      let errPos = p.prevEndPos
      switch skipTokensAndMaybeRetry(p, ~isStartOfGrammar=Grammar.isAtomicExprStart) {
      | None =>
        Parser.err(~startPos=errPos, p, Diagnostics.unexpected(token, p.breadcrumbs))
        Recover.defaultExpr()
      | Some() => parseAtomicExpr(p)
      }
    }

    Parser.eatBreadcrumb(p)
    expr
  }

  /* module(module-expr)
   * module(module-expr : package-type) */
  and parseFirstClassModuleExpr = (~startPos, p) => {
    Parser.expect(Lparen, p)

    let modExpr = parseModuleExpr(p)
    let modEndLoc = p.prevEndPos
    switch p.Parser.token {
    | Colon =>
      let colonStart = p.Parser.startPos
      Parser.next(p)
      let attrs = parseAttributes(p)
      let packageType = parsePackageType(~startPos=colonStart, ~attrs, p)
      Parser.expect(Rparen, p)
      let loc = mkLoc(startPos, modEndLoc)
      let firstClassModule = Ast_helper.Exp.pack(~loc, modExpr)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.constraint_(~loc, firstClassModule, packageType)
    | _ =>
      Parser.expect(Rparen, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.pack(~loc, modExpr)
    }
  }

  and parseBracketAccess = (p, expr, startPos) => {
    Parser.leaveBreadcrumb(p, Grammar.ExprArrayAccess)
    let lbracket = p.startPos
    Parser.next(p)
    let stringStart = p.startPos
    switch p.Parser.token {
    | String(s) =>
      Parser.next(p)
      let stringEnd = p.prevEndPos
      Parser.expect(Rbracket, p)
      let rbracket = p.prevEndPos
      let e = {
        let identLoc = mkLoc(stringStart, stringEnd)
        let loc = mkLoc(lbracket, rbracket)
        Ast_helper.Exp.apply(
          ~loc,
          Ast_helper.Exp.ident(~loc, Location.mkloc(Longident.Lident("##"), loc)),
          list{
            (Nolabel, expr),
            (
              Nolabel,
              Ast_helper.Exp.ident(~loc=identLoc, Location.mkloc(Longident.Lident(s), identLoc)),
            ),
          },
        )
      }

      let e = parsePrimaryExpr(~operand=e, p)
      let equalStart = p.startPos
      switch p.token {
      | Equal =>
        Parser.next(p)
        let equalEnd = p.prevEndPos
        let rhsExpr = parseExpr(p)
        let loc = mkLoc(startPos, rhsExpr.pexp_loc.loc_end)
        let operatorLoc = mkLoc(equalStart, equalEnd)
        Ast_helper.Exp.apply(
          ~loc,
          Ast_helper.Exp.ident(
            ~loc=operatorLoc,
            Location.mkloc(Longident.Lident("#="), operatorLoc),
          ),
          list{(Nolabel, e), (Nolabel, rhsExpr)},
        )
      | _ => e
      }
    | _ =>
      let accessExpr = parseConstrainedOrCoercedExpr(p)
      Parser.expect(Rbracket, p)
      let rbracket = p.prevEndPos
      let arrayLoc = mkLoc(lbracket, rbracket)
      switch p.token {
      | Equal =>
        Parser.leaveBreadcrumb(p, ExprArrayMutation)
        Parser.next(p)
        let rhsExpr = parseExpr(p)
        let arraySet = Location.mkloc(Longident.Ldot(Lident("Array"), "set"), arrayLoc)

        let endPos = p.prevEndPos
        let arraySet = Ast_helper.Exp.apply(
          ~loc=mkLoc(startPos, endPos),
          Ast_helper.Exp.ident(~loc=arrayLoc, arraySet),
          list{(Nolabel, expr), (Nolabel, accessExpr), (Nolabel, rhsExpr)},
        )

        Parser.eatBreadcrumb(p)
        arraySet
      | _ =>
        let endPos = p.prevEndPos
        let e = Ast_helper.Exp.apply(
          ~loc=mkLoc(startPos, endPos),
          Ast_helper.Exp.ident(
            ~loc=arrayLoc,
            Location.mkloc(Longident.Ldot(Lident("Array"), "get"), arrayLoc),
          ),
          list{(Nolabel, expr), (Nolabel, accessExpr)},
        )

        Parser.eatBreadcrumb(p)
        parsePrimaryExpr(~operand=e, p)
      }
    }
  }

  /* * A primary expression represents
   *  - atomic-expr
   *  - john.age
   *  - array[0]
   *  - applyFunctionTo(arg1, arg2)
   *
   *  The "operand" represents the expression that is operated on
   */
  and parsePrimaryExpr = (~operand, ~noCall=false, p) => {
    let startPos = operand.pexp_loc.loc_start
    let rec loop = (p, expr) =>
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        let lident = parseValuePath(p)
        switch p.Parser.token {
        | Equal when noCall == false =>
          Parser.leaveBreadcrumb(p, Grammar.ExprSetField)
          Parser.next(p)
          let targetExpr = parseExpr(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let setfield = Ast_helper.Exp.setfield(~loc, expr, lident, targetExpr)
          Parser.eatBreadcrumb(p)
          setfield
        | _ =>
          let endPos = p.prevEndPos
          let loc = mkLoc(startPos, endPos)
          loop(p, Ast_helper.Exp.field(~loc, expr, lident))
        }
      | Lbracket when noCall == false && p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
        parseBracketAccess(p, expr, startPos)
      | Lparen when noCall == false && p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
        loop(p, parseCallExpr(p, expr))
      | Backtick when noCall == false && p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
        switch expr.pexp_desc {
        | Pexp_ident({txt: Longident.Lident(ident)}) => parseTemplateExpr(~prefix=ident, p)
        | _ =>
          Parser.err(
            ~startPos=expr.pexp_loc.loc_start,
            ~endPos=expr.pexp_loc.loc_end,
            p,
            Diagnostics.message(
              "Tagged template literals are currently restricted to identifiers like: json`null`.",
            ),
          )
          parseTemplateExpr(p)
        }
      | _ => expr
      }

    loop(p, operand)
  }

  /* a unary expression is an expression with only one operand and
   * unary operator. Examples:
   *   -1
   *   !condition
   *   -. 1.6
   */
  and parseUnaryExpr = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | (Minus | MinusDot | Plus | PlusDot | Bang) as token =>
      Parser.leaveBreadcrumb(p, Grammar.ExprUnary)
      let tokenEnd = p.endPos
      Parser.next(p)
      let operand = parseUnaryExpr(p)
      let unaryExpr = makeUnaryExpr(startPos, tokenEnd, token, operand)
      Parser.eatBreadcrumb(p)
      unaryExpr
    | _ => parsePrimaryExpr(~operand=parseAtomicExpr(p), p)
    }
  }

  /* Represents an "operand" in a binary expression.
   * If you have `a + b`, `a` and `b` both represent
   * the operands of the binary expression with opeartor `+` */
  and parseOperandExpr = (~context, p) => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    let expr = switch p.Parser.token {
    | Assert =>
      Parser.next(p)
      let expr = parseUnaryExpr(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.assert_(~loc, expr)
    | Lazy =>
      Parser.next(p)
      let expr = parseUnaryExpr(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.lazy_(~loc, expr)
    | Try => parseTryExpression(p)
    | If => parseIfExpression(p)
    | For => parseForExpression(p)
    | While => parseWhileExpression(p)
    | Switch => parseSwitchExpression(p)
    | _ =>
      if (
        context !== WhenExpr && isEs6ArrowExpression(~inTernary=context == TernaryTrueBranchExpr, p)
      ) {
        parseEs6ArrowExpression(p)
      } else {
        parseUnaryExpr(p)
      }
    }

    /* let endPos = p.Parser.prevEndPos in */
    {
      ...expr,
      pexp_attributes: List.concat(list{expr.Parsetree.pexp_attributes, attrs}),
      /* pexp_loc = mkLoc startPos endPos */
    }
  }

  /* a binary expression is an expression that combines two expressions with an
   * operator. Examples:
   *    a + b
   *    f(x) |> g(y)
   */
  and parseBinaryExpr = (~context=OrdinaryExpr, ~a=?, p, prec) => {
    let a = switch a {
    | Some(e) => e
    | None => parseOperandExpr(~context, p)
    }

    let rec loop = a => {
      let token = p.Parser.token
      let tokenPrec = switch token {
      /* Can the minus be interpreted as a binary operator? Or is it a unary?
       * let w = {
       *   x
       *   -10
       * }
       * vs
       * let w = {
       *   width
       *   - gap
       * }
       *
       * First case is unary, second is a binary operator.
       * See Scanner.isBinaryOp */
      | Minus | MinusDot | LessThan
        when !Scanner.isBinaryOp(p.scanner.src, p.startPos.pos_cnum, p.endPos.pos_cnum) &&
        p.startPos.pos_lnum > p.prevEndPos.pos_lnum => -1
      | token => Token.precedence(token)
      }

      if tokenPrec < prec {
        a
      } else {
        Parser.leaveBreadcrumb(p, Grammar.ExprBinaryAfterOp(token))
        let startPos = p.startPos
        Parser.next(p)
        let endPos = p.prevEndPos
        let b = parseBinaryExpr(~context, p, tokenPrec + 1)
        let loc = mkLoc(a.Parsetree.pexp_loc.loc_start, b.pexp_loc.loc_end)
        let expr = Ast_helper.Exp.apply(
          ~loc,
          makeInfixOperator(p, token, startPos, endPos),
          list{(Nolabel, a), (Nolabel, b)},
        )

        loop(expr)
      }
    }

    loop(a)
  }

  /* If we even need this, determines if < might be the start of jsx. Not 100% complete */
  /* and isStartOfJsx p = */
  /* Parser.lookahead p (fun p -> */
  /* match p.Parser.token with */
  /* | LessThan -> */
  /* Parser.next p; */
  /* begin match p.token with */
  /* | GreaterThan (* <> *) -> true */
  /* | Lident _ | Uident _ | List -> */
  /* ignore (parseJsxName p); */
  /* begin match p.token with */
  /* | GreaterThan (* <div> *) -> true */
  /* | Question (*<Component ? *) -> true */
  /* | Lident _ | List -> */
  /* Parser.next p; */
  /* begin match p.token with */
  /* | Equal (* <Component handleClick= *) -> true */
  /* | _ -> false (* TODO *) */
  /* end */
  /* | Forwardslash (* <Component / *)-> */
  /* Parser.next p; */
  /* begin match p.token with */
  /* | GreaterThan (* <Component /> *) -> true */
  /* | _ -> false */
  /* end */
  /* | _ -> */
  /* false */
  /* end */
  /* | _ -> false */
  /* end */
  /* | _ -> false */
  /* ) */

  and parseTemplateExpr = (~prefix="", p) => {
    let hiddenOperator = {
      let op = Location.mknoloc(Longident.Lident("^"))
      Ast_helper.Exp.ident(op)
    }

    let rec loop = (acc, p) => {
      let startPos = p.Parser.startPos
      switch p.Parser.token {
      | TemplateTail(txt) =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        if String.length(txt) > 0 {
          let txt = if p.mode == ParseForTypeChecker {
            parseTemplateStringLiteral(txt)
          } else {
            txt
          }
          let str = Ast_helper.Exp.constant(~loc, Pconst_string(txt, Some(prefix)))
          Ast_helper.Exp.apply(~loc, hiddenOperator, list{(Nolabel, acc), (Nolabel, str)})
        } else {
          acc
        }
      | TemplatePart(txt) =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let expr = parseExprBlock(p)
        let fullLoc = mkLoc(startPos, p.prevEndPos)
        Scanner.setTemplateMode(p.scanner)
        Parser.expect(Rbrace, p)
        let txt = if p.mode == ParseForTypeChecker {
          parseTemplateStringLiteral(txt)
        } else {
          txt
        }
        let str = Ast_helper.Exp.constant(~loc, Pconst_string(txt, Some(prefix)))
        let next = {
          let a = if String.length(txt) > 0 {
            Ast_helper.Exp.apply(~loc=fullLoc, hiddenOperator, list{(Nolabel, acc), (Nolabel, str)})
          } else {
            acc
          }

          Ast_helper.Exp.apply(~loc=fullLoc, hiddenOperator, list{(Nolabel, a), (Nolabel, expr)})
        }

        loop(next, p)
      | token =>
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        acc
      }
    }

    Scanner.setTemplateMode(p.scanner)
    Parser.expect(Backtick, p)
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | TemplateTail(txt) =>
      let loc = mkLoc(startPos, p.endPos)
      Parser.next(p)
      let txt = if p.mode == ParseForTypeChecker {
        parseTemplateStringLiteral(txt)
      } else {
        txt
      }
      Ast_helper.Exp.constant(~loc, Pconst_string(txt, Some(prefix)))
    | TemplatePart(txt) =>
      let constantLoc = mkLoc(startPos, p.endPos)
      Parser.next(p)
      let expr = parseExprBlock(p)
      let fullLoc = mkLoc(startPos, p.prevEndPos)
      Scanner.setTemplateMode(p.scanner)
      Parser.expect(Rbrace, p)
      let txt = if p.mode == ParseForTypeChecker {
        parseTemplateStringLiteral(txt)
      } else {
        txt
      }
      let str = Ast_helper.Exp.constant(~loc=constantLoc, Pconst_string(txt, Some(prefix)))
      let next = if String.length(txt) > 0 {
        Ast_helper.Exp.apply(~loc=fullLoc, hiddenOperator, list{(Nolabel, str), (Nolabel, expr)})
      } else {
        expr
      }

      loop(next, p)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Ast_helper.Exp.constant(Pconst_string("", None))
    }
  }

  /* Overparse: let f = a : int => a + 1, is it (a : int) => or (a): int =>
   * Also overparse constraints:
   *  let x = {
   *    let a = 1
   *    a + pi: int
   *  }
   *
   *  We want to give a nice error message in these cases
   * */
  and overParseConstrainedOrCoercedOrArrowExpression = (p, expr) =>
    switch p.Parser.token {
    | ColonGreaterThan => parseCoercedExpr(~expr, p)
    | Colon =>
      Parser.next(p)
      let typ = parseTypExpr(~es6Arrow=false, p)
      switch p.Parser.token {
      | EqualGreater =>
        Parser.next(p)
        let body = parseExpr(p)
        let pat = switch expr.pexp_desc {
        | Pexp_ident(longident) =>
          Ast_helper.Pat.var(
            ~loc=expr.pexp_loc,
            Location.mkloc(Longident.flatten(longident.txt) |> String.concat("."), longident.loc),
          )
        /* TODO: can we convert more expressions to patterns? */
        | _ => Ast_helper.Pat.var(~loc=expr.pexp_loc, Location.mkloc("pattern", expr.pexp_loc))
        }

        let arrow1 = Ast_helper.Exp.fun_(
          ~loc=mkLoc(expr.pexp_loc.loc_start, body.pexp_loc.loc_end),
          Asttypes.Nolabel,
          None,
          pat,
          Ast_helper.Exp.constraint_(body, typ),
        )

        let arrow2 = Ast_helper.Exp.fun_(
          ~loc=mkLoc(expr.pexp_loc.loc_start, body.pexp_loc.loc_end),
          Asttypes.Nolabel,
          None,
          Ast_helper.Pat.constraint_(pat, typ),
          body,
        )

        let msg =
          Doc.breakableGroup(
            ~forceBreak=true,
            Doc.concat(list{
              Doc.text("Did you mean to annotate the parameter type or the return type?"),
              Doc.indent(
                Doc.concat(list{
                  Doc.line,
                  Doc.text("1) "),
                  Printer.printExpression(arrow1, CommentTable.empty),
                  Doc.line,
                  Doc.text("2) "),
                  Printer.printExpression(arrow2, CommentTable.empty),
                }),
              ),
            }),
          ) |> Doc.toString(~width=80)

        Parser.err(
          ~startPos=expr.pexp_loc.loc_start,
          ~endPos=body.pexp_loc.loc_end,
          p,
          Diagnostics.message(msg),
        )
        arrow1
      | _ =>
        open Parsetree
        let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
        let expr = Ast_helper.Exp.constraint_(~loc, expr, typ)
        let () = Parser.err(
          ~startPos=expr.pexp_loc.loc_start,
          ~endPos=typ.ptyp_loc.loc_end,
          p,
          Diagnostics.message(
            Doc.breakableGroup(
              ~forceBreak=true,
              Doc.concat(list{
                Doc.text("Expressions with type constraints need to be wrapped in parens:"),
                Doc.indent(
                  Doc.concat(list{
                    Doc.line,
                    Printer.addParens(Printer.printExpression(expr, CommentTable.empty)),
                  }),
                ),
              }),
            ) |> Doc.toString(~width=80),
          ),
        )

        expr
      }
    | _ => expr
    }

  and parseLetBindingBody = (~startPos, ~attrs, p) => {
    Parser.beginRegion(p)
    Parser.leaveBreadcrumb(p, Grammar.LetBinding)
    let (pat, exp) = {
      let pat = parsePattern(p)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        switch p.token {
        | Typ =>
          /* locally abstract types */
          Parser.next(p)
          let newtypes = parseLidentList(p)
          Parser.expect(Dot, p)
          let typ = parseTypExpr(p)
          Parser.expect(Equal, p)
          let expr = parseExpr(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let (exp, poly) = wrapTypeAnnotation(~loc, newtypes, typ, expr)
          let pat = Ast_helper.Pat.constraint_(~loc, pat, poly)
          (pat, exp)
        | _ =>
          let polyType = parsePolyTypeExpr(p)
          let loc = {...pat.ppat_loc, loc_end: polyType.Parsetree.ptyp_loc.loc_end}
          let pat = Ast_helper.Pat.constraint_(~loc, pat, polyType)
          Parser.expect(Token.Equal, p)
          let exp = parseExpr(p)
          let exp = overParseConstrainedOrCoercedOrArrowExpression(p, exp)
          (pat, exp)
        }
      | _ =>
        Parser.expect(Token.Equal, p)
        let exp = overParseConstrainedOrCoercedOrArrowExpression(p, parseExpr(p))
        (pat, exp)
      }
    }

    let loc = mkLoc(startPos, p.prevEndPos)
    let vb = Ast_helper.Vb.mk(~loc, ~attrs, pat, exp)
    Parser.eatBreadcrumb(p)
    Parser.endRegion(p)
    vb
  }

  /* TODO: find a better way? Is it possible?
   * let a = 1
   * @attr
   * and b = 2
   *
   * The problem is that without semi we need a lookahead to determine
   * if the attr is on the letbinding or the start of a new thing
   *
   * let a = 1
   * @attr
   * let b = 1
   *
   * Here @attr should attach to something "new": `let b = 1`
   * The parser state is forked, which is quite expensive…
   */
  and parseAttributesAndBinding = (p: Parser.t) => {
    let err = p.scanner.err
    let ch = p.scanner.ch
    let offset = p.scanner.offset
    let rdOffset = p.scanner.rdOffset
    let lineOffset = p.scanner.lineOffset
    let lnum = p.scanner.lnum
    let mode = p.scanner.mode
    let token = p.token
    let startPos = p.startPos
    let endPos = p.endPos
    let prevEndPos = p.prevEndPos
    let breadcrumbs = p.breadcrumbs
    let errors = p.errors
    let diagnostics = p.diagnostics
    let comments = p.comments

    switch p.Parser.token {
    | At =>
      let attrs = parseAttributes(p)
      switch p.Parser.token {
      | And => attrs
      | _ =>
        p.scanner.err = err
        p.scanner.ch = ch
        p.scanner.offset = offset
        p.scanner.rdOffset = rdOffset
        p.scanner.lineOffset = lineOffset
        p.scanner.lnum = lnum
        p.scanner.mode = mode
        p.token = token
        p.startPos = startPos
        p.endPos = endPos
        p.prevEndPos = prevEndPos
        p.breadcrumbs = breadcrumbs
        p.errors = errors
        p.diagnostics = diagnostics
        p.comments = comments
        list{}
      }
    | _ => list{}
    }
  }

  /* definition	::=	let [rec] let-binding  { and let-binding } */
  and parseLetBindings = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.optional(p, Let) |> ignore
    let recFlag = if Parser.optional(p, Token.Rec) {
      Asttypes.Recursive
    } else {
      Asttypes.Nonrecursive
    }

    let first = parseLetBindingBody(~startPos, ~attrs, p)

    let rec loop = (p, bindings) => {
      let startPos = p.Parser.startPos
      let attrs = parseAttributesAndBinding(p)
      switch p.Parser.token {
      | And =>
        Parser.next(p)
        let attrs = switch p.token {
        | Export =>
          let exportLoc = mkLoc(p.startPos, p.endPos)
          Parser.next(p)
          let genTypeAttr = (Location.mkloc("genType", exportLoc), Parsetree.PStr(list{}))
          list{genTypeAttr, ...attrs}
        | _ => attrs
        }

        ignore(Parser.optional(p, Let)) /* overparse for fault tolerance */
        let letBinding = parseLetBindingBody(~startPos, ~attrs, p)
        loop(p, list{letBinding, ...bindings})
      | _ => List.rev(bindings)
      }
    }

    (recFlag, loop(p, list{first}))
  }

  /*
   * div -> div
   * Foo -> Foo.createElement
   * Foo.Bar -> Foo.Bar.createElement
   */
  and parseJsxName = p => {
    let longident = switch p.Parser.token {
    | Lident(ident) =>
      let identStart = p.startPos
      let identEnd = p.endPos
      Parser.next(p)
      let loc = mkLoc(identStart, identEnd)
      Location.mkloc(Longident.Lident(ident), loc)
    | Uident(_) =>
      let longident = parseModuleLongIdent(~lowercase=false, p)
      Location.mkloc(Longident.Ldot(longident.txt, "createElement"), longident.loc)
    | _ =>
      let msg = "A jsx name should start with a lowercase or uppercase identifier, like: div in <div /> or Navbar in <Navbar />"

      Parser.err(p, Diagnostics.message(msg))
      Location.mknoloc(Longident.Lident("_"))
    }

    Ast_helper.Exp.ident(~loc=longident.loc, longident)
  }

  and parseJsxOpeningOrSelfClosingElement = (~startPos, p) => {
    let jsxStartPos = p.Parser.startPos
    let name = parseJsxName(p)
    let jsxProps = parseJsxProps(p)
    let children = switch p.Parser.token {
    | Forwardslash =>
      /* <foo a=b /> */
      let childrenStartPos = p.Parser.startPos
      Parser.next(p)
      let childrenEndPos = p.Parser.startPos
      Parser.expect(GreaterThan, p)
      let loc = mkLoc(childrenStartPos, childrenEndPos)
      makeListExpression(loc, list{}, None) /* no children */
    | GreaterThan =>
      /* <foo a=b> bar </foo> */
      let childrenStartPos = p.Parser.startPos
      Scanner.setJsxMode(p.scanner)
      Parser.next(p)
      let (spread, children) = parseJsxChildren(p)
      let childrenEndPos = p.Parser.startPos
      let () = switch p.token {
      | LessThanSlash => Parser.next(p)
      | LessThan =>
        Parser.next(p)
        Parser.expect(Forwardslash, p)
      | token when Grammar.isStructureItemStart(token) => ()
      | _ => Parser.expect(LessThanSlash, p)
      }

      switch p.Parser.token {
      | Lident(_) | Uident(_) when verifyJsxOpeningClosingName(p, name) =>
        Parser.expect(GreaterThan, p)
        let loc = mkLoc(childrenStartPos, childrenEndPos)
        switch (spread, children) {
        | (true, list{child, ..._}) => child
        | _ => makeListExpression(loc, children, None)
        }
      | token =>
        let () = if Grammar.isStructureItemStart(token) {
          let closing = "</" ++ (string_of_pexp_ident(name) ++ ">")
          let msg = Diagnostics.message("Missing " ++ closing)
          Parser.err(~startPos, ~endPos=p.prevEndPos, p, msg)
        } else {
          let opening = "</" ++ (string_of_pexp_ident(name) ++ ">")
          let msg =
            "Closing jsx name should be the same as the opening name. Did you mean " ++
            (opening ++
            " ?")
          Parser.err(~startPos, ~endPos=p.prevEndPos, p, Diagnostics.message(msg))
          Parser.expect(GreaterThan, p)
        }

        let loc = mkLoc(childrenStartPos, childrenEndPos)
        switch (spread, children) {
        | (true, list{child, ..._}) => child
        | _ => makeListExpression(loc, children, None)
        }
      }
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      makeListExpression(Location.none, list{}, None)
    }

    let jsxEndPos = p.prevEndPos
    let loc = mkLoc(jsxStartPos, jsxEndPos)
    Ast_helper.Exp.apply(
      ~loc,
      name,
      List.concat(list{
        jsxProps,
        list{
          (Asttypes.Labelled("children"), children),
          (
            Asttypes.Nolabel,
            Ast_helper.Exp.construct(Location.mknoloc(Longident.Lident("()")), None),
          ),
        },
      }),
    )
  }

  /*
   *  jsx ::=
   *    | <> jsx-children </>
   *    | <element-name {jsx-prop} />
   *    | <element-name {jsx-prop}> jsx-children </element-name>
   *
   *  jsx-children ::= primary-expr*          * => 0 or more
   */
  and parseJsx = p => {
    Parser.leaveBreadcrumb(p, Grammar.Jsx)
    let startPos = p.Parser.startPos
    Parser.expect(LessThan, p)
    let jsxExpr = switch p.Parser.token {
    | Lident(_) | Uident(_) => parseJsxOpeningOrSelfClosingElement(~startPos, p)
    | GreaterThan =>
      /* fragment: <> foo </> */
      parseJsxFragment(p)
    | _ => parseJsxName(p)
    }

    {...jsxExpr, pexp_attributes: list{jsxAttr}}
  }

  /*
   * jsx-fragment ::=
   *  | <> </>
   *  | <> jsx-children </>
   */
  and parseJsxFragment = p => {
    let childrenStartPos = p.Parser.startPos
    Scanner.setJsxMode(p.scanner)
    Parser.expect(GreaterThan, p)
    let (_spread, children) = parseJsxChildren(p)
    let childrenEndPos = p.Parser.startPos
    Parser.expect(LessThanSlash, p)
    Parser.expect(GreaterThan, p)
    let loc = mkLoc(childrenStartPos, childrenEndPos)
    makeListExpression(loc, children, None)
  }

  /*
   * jsx-prop ::=
   *   |  lident
   *   | ?lident
   *   |  lident =  jsx_expr
   *   |  lident = ?jsx_expr
   */
  and parseJsxProp = p => {
    Parser.leaveBreadcrumb(p, Grammar.JsxAttribute)
    switch p.Parser.token {
    | Question | Lident(_) =>
      let optional = Parser.optional(p, Question)
      let (name, loc) = parseLident(p)
      let propLocAttr = (Location.mkloc("res.namedArgLoc", loc), Parsetree.PStr(list{}))
      /* optional punning: <foo ?a /> */
      if optional {
        Some(
          Asttypes.Optional(name),
          Ast_helper.Exp.ident(
            ~attrs=list{propLocAttr},
            ~loc,
            Location.mkloc(Longident.Lident(name), loc),
          ),
        )
      } else {
        switch p.Parser.token {
        | Equal =>
          Parser.next(p)
          /* no punning */
          let optional = Parser.optional(p, Question)
          let attrExpr = {
            let e = parsePrimaryExpr(~operand=parseAtomicExpr(p), p)
            {...e, pexp_attributes: list{propLocAttr, ...e.pexp_attributes}}
          }

          let label = if optional {
            Asttypes.Optional(name)
          } else {
            Asttypes.Labelled(name)
          }

          Some(label, attrExpr)
        | _ =>
          let attrExpr = Ast_helper.Exp.ident(
            ~loc,
            ~attrs=list{propLocAttr},
            Location.mknoloc(Longident.Lident(name)),
          )
          let label = if optional {
            Asttypes.Optional(name)
          } else {
            Asttypes.Labelled(name)
          }

          Some(label, attrExpr)
        }
      }
    | _ => None
    }
  }

  and parseJsxProps = p => parseRegion(~grammar=Grammar.JsxAttribute, ~f=parseJsxProp, p)

  and parseJsxChildren = p => {
    let rec loop = (p, children) =>
      switch p.Parser.token {
      | Token.Eof | LessThanSlash =>
        Scanner.popMode(p.scanner, Jsx)
        List.rev(children)
      | LessThan =>
        /* Imagine: <div> <Navbar /> <
         * is `<` the start of a jsx-child? <div …
         * or is it the start of a closing tag?  </div>
         * reconsiderLessThan peeks at the next token and
         * determines the correct token to disambiguate */
        let token = Scanner.reconsiderLessThan(p.scanner)
        if token == LessThan {
          let child = parsePrimaryExpr(~operand=parseAtomicExpr(p), ~noCall=true, p)
          loop(p, list{child, ...children})
        } else {
          /* LessThanSlash */
          let () = p.token = token
          let () = Scanner.popMode(p.scanner, Jsx)
          List.rev(children)
        }
      | token when Grammar.isJsxChildStart(token) =>
        let () = Scanner.popMode(p.scanner, Jsx)
        let child = parsePrimaryExpr(~operand=parseAtomicExpr(p), ~noCall=true, p)
        loop(p, list{child, ...children})
      | _ =>
        Scanner.popMode(p.scanner, Jsx)
        List.rev(children)
      }

    switch p.Parser.token {
    | DotDotDot =>
      Parser.next(p)
      (true, list{parsePrimaryExpr(~operand=parseAtomicExpr(p), ~noCall=true, p)})
    | _ => (false, loop(p, list{}))
    }
  }

  and parseBracedOrRecordExpr = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lbrace, p)
    switch p.Parser.token {
    | Rbrace =>
      Parser.err(p, Diagnostics.unexpected(Rbrace, p.breadcrumbs))
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let braces = makeBracesAttr(loc)
      Ast_helper.Exp.construct(
        ~attrs=list{braces},
        ~loc,
        Location.mkloc(Longident.Lident("()"), loc),
        None,
      )
    | DotDotDot =>
      /* beginning of record spread, parse record */
      Parser.next(p)
      let spreadExpr = parseConstrainedOrCoercedExpr(p)
      Parser.expect(Comma, p)
      let expr = parseRecordExpr(~startPos, ~spread=Some(spreadExpr), list{}, p)
      Parser.expect(Rbrace, p)
      expr
    | String(s) =>
      let field = {
        let loc = mkLoc(p.startPos, p.endPos)
        Parser.next(p)
        Location.mkloc(Longident.Lident(s), loc)
      }

      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let fieldExpr = parseExpr(p)
        Parser.optional(p, Comma) |> ignore
        let expr = parseRecordExprWithStringKeys(~startPos, (field, fieldExpr), p)
        Parser.expect(Rbrace, p)
        expr
      | _ =>
        let constant = Ast_helper.Exp.constant(~loc=field.loc, Parsetree.Pconst_string(s, None))
        let a = parsePrimaryExpr(~operand=constant, p)
        let e = parseBinaryExpr(~a, p, 1)
        let e = parseTernaryExpr(e, p)
        switch p.Parser.token {
        | Semicolon =>
          Parser.next(p)
          let expr = parseExprBlock(~first=e, p)
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
        | Rbrace =>
          Parser.next(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...e, pexp_attributes: list{braces, ...e.pexp_attributes}}
        | _ =>
          let expr = parseExprBlock(~first=e, p)
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
        }
      }
    | Uident(_) | Lident(_) =>
      let valueOrConstructor = parseValueOrConstructor(p)
      switch valueOrConstructor.pexp_desc {
      | Pexp_ident(pathIdent) =>
        let identEndPos = p.prevEndPos
        switch p.Parser.token {
        | Comma =>
          Parser.next(p)
          let expr = parseRecordExpr(~startPos, list{(pathIdent, valueOrConstructor)}, p)
          Parser.expect(Rbrace, p)
          expr
        | Colon =>
          Parser.next(p)
          let fieldExpr = parseExpr(p)
          switch p.token {
          | Rbrace =>
            Parser.next(p)
            let loc = mkLoc(startPos, p.prevEndPos)
            Ast_helper.Exp.record(~loc, list{(pathIdent, fieldExpr)}, None)
          | _ =>
            Parser.expect(Comma, p)
            let expr = parseRecordExpr(~startPos, list{(pathIdent, fieldExpr)}, p)
            Parser.expect(Rbrace, p)
            expr
          }
        /* error case */
        | Lident(_) =>
          if p.prevEndPos.pos_lnum < p.startPos.pos_lnum {
            Parser.expect(Comma, p)
            let expr = parseRecordExpr(~startPos, list{(pathIdent, valueOrConstructor)}, p)
            Parser.expect(Rbrace, p)
            expr
          } else {
            Parser.expect(Colon, p)
            let expr = parseRecordExpr(~startPos, list{(pathIdent, valueOrConstructor)}, p)
            Parser.expect(Rbrace, p)
            expr
          }
        | Semicolon =>
          Parser.next(p)
          let expr = parseExprBlock(~first=Ast_helper.Exp.ident(pathIdent), p)
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
        | Rbrace =>
          Parser.next(p)
          let expr = Ast_helper.Exp.ident(~loc=pathIdent.loc, pathIdent)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
        | EqualGreater =>
          let loc = mkLoc(startPos, identEndPos)
          let ident = Location.mkloc(Longident.last(pathIdent.txt), loc)
          let a = parseEs6ArrowExpression(
            ~parameters=list{
              TermParameter({
                uncurried: false,
                attrs: list{},
                label: Asttypes.Nolabel,
                expr: None,
                pat: Ast_helper.Pat.var(ident),
                pos: startPos,
              }),
            },
            p,
          )

          let e = parseBinaryExpr(~a, p, 1)
          let e = parseTernaryExpr(e, p)
          switch p.Parser.token {
          | Semicolon =>
            Parser.next(p)
            let expr = parseExprBlock(~first=e, p)
            Parser.expect(Rbrace, p)
            let loc = mkLoc(startPos, p.prevEndPos)
            let braces = makeBracesAttr(loc)
            {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
          | Rbrace =>
            Parser.next(p)
            let loc = mkLoc(startPos, p.prevEndPos)
            let braces = makeBracesAttr(loc)
            {...e, pexp_attributes: list{braces, ...e.pexp_attributes}}
          | _ =>
            let expr = parseExprBlock(~first=e, p)
            Parser.expect(Rbrace, p)
            let loc = mkLoc(startPos, p.prevEndPos)
            let braces = makeBracesAttr(loc)
            {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
          }
        | _ =>
          Parser.leaveBreadcrumb(p, Grammar.ExprBlock)
          let a = parsePrimaryExpr(~operand=Ast_helper.Exp.ident(~loc=pathIdent.loc, pathIdent), p)
          let e = parseBinaryExpr(~a, p, 1)
          let e = parseTernaryExpr(e, p)
          Parser.eatBreadcrumb(p)
          switch p.Parser.token {
          | Semicolon =>
            Parser.next(p)
            let expr = parseExprBlock(~first=e, p)
            Parser.expect(Rbrace, p)
            let loc = mkLoc(startPos, p.prevEndPos)
            let braces = makeBracesAttr(loc)
            {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
          | Rbrace =>
            Parser.next(p)
            let loc = mkLoc(startPos, p.prevEndPos)
            let braces = makeBracesAttr(loc)
            {...e, pexp_attributes: list{braces, ...e.pexp_attributes}}
          | _ =>
            let expr = parseExprBlock(~first=e, p)
            Parser.expect(Rbrace, p)
            let loc = mkLoc(startPos, p.prevEndPos)
            let braces = makeBracesAttr(loc)
            {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
          }
        }
      | _ =>
        Parser.leaveBreadcrumb(p, Grammar.ExprBlock)
        let a = parsePrimaryExpr(~operand=valueOrConstructor, p)
        let e = parseBinaryExpr(~a, p, 1)
        let e = parseTernaryExpr(e, p)
        Parser.eatBreadcrumb(p)
        switch p.Parser.token {
        | Semicolon =>
          Parser.next(p)
          let expr = parseExprBlock(~first=e, p)
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
        | Rbrace =>
          Parser.next(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...e, pexp_attributes: list{braces, ...e.pexp_attributes}}
        | _ =>
          let expr = parseExprBlock(~first=e, p)
          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let braces = makeBracesAttr(loc)
          {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
        }
      }
    | _ =>
      let expr = parseExprBlock(p)
      Parser.expect(Rbrace, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let braces = makeBracesAttr(loc)
      {...expr, pexp_attributes: list{braces, ...expr.pexp_attributes}}
    }
  }

  and parseRecordRowWithStringKey = p =>
    switch p.Parser.token {
    | String(s) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      let field = Location.mkloc(Longident.Lident(s), loc)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let fieldExpr = parseExpr(p)
        Some(field, fieldExpr)
      | _ => Some(field, Ast_helper.Exp.ident(~loc=field.loc, field))
      }
    | _ => None
    }

  and parseRecordRow = p => {
    let () = switch p.Parser.token {
    | Token.DotDotDot =>
      Parser.err(p, Diagnostics.message(ErrorMessages.recordExprSpread))
      Parser.next(p)
    | _ => ()
    }

    switch p.Parser.token {
    | Lident(_) | Uident(_) | List =>
      let field = parseValuePath(p)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let fieldExpr = parseExpr(p)
        Some(field, fieldExpr)
      | _ => Some(field, Ast_helper.Exp.ident(~loc=field.loc, field))
      }
    | _ => None
    }
  }

  and parseRecordExprWithStringKeys = (~startPos, firstRow, p) => {
    let rows = list{
      firstRow,
      ...parseCommaDelimitedRegion(
        ~grammar=Grammar.RecordRowsStringKey,
        ~closing=Rbrace,
        ~f=parseRecordRowWithStringKey,
        p,
      ),
    }
    let loc = mkLoc(startPos, p.endPos)
    let recordStrExpr = Ast_helper.Str.eval(~loc, Ast_helper.Exp.record(~loc, rows, None))
    Ast_helper.Exp.extension(
      ~loc,
      (Location.mkloc("obj", loc), Parsetree.PStr(list{recordStrExpr})),
    )
  }

  and parseRecordExpr = (~startPos, ~spread=None, rows, p) => {
    let exprs = parseCommaDelimitedRegion(
      ~grammar=Grammar.RecordRows,
      ~closing=Rbrace,
      ~f=parseRecordRow,
      p,
    )

    let rows = List.concat(list{rows, exprs})
    let () = switch rows {
    | list{} =>
      let msg = "Record spread needs at least one field that's updated"
      Parser.err(p, Diagnostics.message(msg))
    | _rows => ()
    }

    let loc = mkLoc(startPos, p.endPos)
    Ast_helper.Exp.record(~loc, rows, spread)
  }

  and parseExprBlockItem = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Module =>
      Parser.next(p)
      switch p.token {
      | Lparen => parseFirstClassModuleExpr(~startPos, p)
      | _ =>
        let name = switch p.Parser.token {
        | Uident(ident) =>
          let loc = mkLoc(p.startPos, p.endPos)
          Parser.next(p)
          Location.mkloc(ident, loc)
        | t =>
          Parser.err(p, Diagnostics.uident(t))
          Location.mknoloc("_")
        }

        let body = parseModuleBindingBody(p)
        Parser.optional(p, Semicolon) |> ignore
        let expr = parseExprBlock(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Exp.letmodule(~loc, name, body, expr)
      }
    | Exception =>
      let extensionConstructor = parseExceptionDef(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let blockExpr = parseExprBlock(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.letexception(~loc, extensionConstructor, blockExpr)
    | Open =>
      let od = parseOpenDescription(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let blockExpr = parseExprBlock(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.open_(~loc, od.popen_override, od.popen_lid, blockExpr)
    | Let =>
      let (recFlag, letBindings) = parseLetBindings(~attrs, p)
      let next = switch p.Parser.token {
      | Semicolon =>
        Parser.next(p)
        if Grammar.isBlockExprStart(p.Parser.token) {
          parseExprBlock(p)
        } else {
          let loc = mkLoc(p.startPos, p.endPos)
          Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None)
        }
      | token when Grammar.isBlockExprStart(token) => parseExprBlock(p)
      | _ =>
        let loc = mkLoc(p.startPos, p.endPos)
        Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None)
      }

      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.let_(~loc, recFlag, letBindings, next)
    | _ =>
      let e1 = {
        let expr = parseExpr(p)
        {...expr, pexp_attributes: List.concat(list{attrs, expr.pexp_attributes})}
      }

      ignore(Parser.optional(p, Semicolon))
      if Grammar.isBlockExprStart(p.Parser.token) {
        let e2 = parseExprBlock(p)
        let loc = {...e1.pexp_loc, loc_end: e2.pexp_loc.loc_end}
        Ast_helper.Exp.sequence(~loc, e1, e2)
      } else {
        e1
      }
    }
  }

  /* blockExpr ::= expr
   *            |  expr          ;
   *            |  expr          ; blockExpr
   *            |  module    ... ; blockExpr
   *            |  open      ... ; blockExpr
   *            |  exception ... ; blockExpr
   *            |  let       ...
   *            |  let       ... ;
   *            |  let       ... ; blockExpr
   *
   *  note: semi should be made optional
   *  a block of expression is always
   */
  and parseExprBlock = (~first=?, p) => {
    Parser.leaveBreadcrumb(p, Grammar.ExprBlock)
    let item = switch first {
    | Some(e) => e
    | None => parseExprBlockItem(p)
    }

    let blockExpr = switch p.Parser.token {
    | Semicolon =>
      Parser.next(p)
      if Grammar.isBlockExprStart(p.Parser.token) {
        let next = parseExprBlockItem(p)
        ignore(Parser.optional(p, Semicolon))
        let loc = {...item.pexp_loc, loc_end: next.pexp_loc.loc_end}
        Ast_helper.Exp.sequence(~loc, item, next)
      } else {
        item
      }
    | token when Grammar.isBlockExprStart(token) =>
      let next = parseExprBlockItem(p)
      ignore(Parser.optional(p, Semicolon))
      let loc = {...item.pexp_loc, loc_end: next.pexp_loc.loc_end}
      Ast_helper.Exp.sequence(~loc, item, next)
    | _ => item
    }

    Parser.eatBreadcrumb(p)
    overParseConstrainedOrCoercedOrArrowExpression(p, blockExpr)
  }

  and parseTryExpression = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Try, p)
    let expr = parseExpr(~context=WhenExpr, p)
    Parser.expect(Catch, p)
    Parser.expect(Lbrace, p)
    let cases = parsePatternMatching(p)
    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.try_(~loc, expr, cases)
  }

  and parseIfExpression = p => {
    Parser.beginRegion(p)
    Parser.leaveBreadcrumb(p, Grammar.ExprIf)
    let startPos = p.Parser.startPos
    Parser.expect(If, p)
    Parser.leaveBreadcrumb(p, Grammar.IfCondition)
    /* doesn't make sense to try es6 arrow here? */
    let conditionExpr = parseExpr(~context=WhenExpr, p)
    Parser.eatBreadcrumb(p)
    Parser.leaveBreadcrumb(p, IfBranch)
    Parser.expect(Lbrace, p)
    let thenExpr = parseExprBlock(p)
    Parser.expect(Rbrace, p)
    Parser.eatBreadcrumb(p)
    let elseExpr = switch p.Parser.token {
    | Else =>
      Parser.endRegion(p)
      Parser.leaveBreadcrumb(p, Grammar.ElseBranch)
      Parser.next(p)
      Parser.beginRegion(p)
      let elseExpr = switch p.token {
      | If => parseIfExpression(p)
      | _ =>
        Parser.expect(Lbrace, p)
        let blockExpr = parseExprBlock(p)
        Parser.expect(Rbrace, p)
        blockExpr
      }

      Parser.eatBreadcrumb(p)
      Parser.endRegion(p)
      Some(elseExpr)
    | _ =>
      Parser.endRegion(p)
      None
    }

    let loc = mkLoc(startPos, p.prevEndPos)
    Parser.eatBreadcrumb(p)
    Ast_helper.Exp.ifthenelse(~loc, conditionExpr, thenExpr, elseExpr)
  }

  and parseForRest = (hasOpeningParen, pattern, startPos, p) => {
    Parser.expect(In, p)
    let e1 = parseExpr(p)
    let direction = switch p.Parser.token {
    | To => Asttypes.Upto
    | Downto => Asttypes.Downto
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Asttypes.Upto
    }

    Parser.next(p)
    let e2 = parseExpr(~context=WhenExpr, p)
    if hasOpeningParen {
      Parser.expect(Rparen, p)
    }
    Parser.expect(Lbrace, p)
    let bodyExpr = parseExprBlock(p)
    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.for_(~loc, pattern, e1, e2, direction, bodyExpr)
  }

  and parseForExpression = p => {
    let startPos = p.Parser.startPos
    Parser.expect(For, p)
    switch p.token {
    | Lparen =>
      let lparen = p.startPos
      Parser.next(p)
      switch p.token {
      | Rparen =>
        Parser.next(p)
        let unitPattern = {
          let loc = mkLoc(lparen, p.prevEndPos)
          let lid = Location.mkloc(Longident.Lident("()"), loc)
          Ast_helper.Pat.construct(lid, None)
        }

        parseForRest(false, parseAliasPattern(~attrs=list{}, unitPattern, p), startPos, p)
      | _ =>
        let pat = parsePattern(p)
        switch p.token {
        | Comma =>
          Parser.next(p)
          let tuplePattern = parseTuplePattern(~attrs=list{}, ~startPos=lparen, ~first=pat, p)

          let pattern = parseAliasPattern(~attrs=list{}, tuplePattern, p)
          parseForRest(false, pattern, startPos, p)
        | _ => parseForRest(true, pat, startPos, p)
        }
      }
    | _ => parseForRest(false, parsePattern(p), startPos, p)
    }
  }

  and parseWhileExpression = p => {
    let startPos = p.Parser.startPos
    Parser.expect(While, p)
    let expr1 = parseExpr(~context=WhenExpr, p)
    Parser.expect(Lbrace, p)
    let expr2 = parseExprBlock(p)
    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.while_(~loc, expr1, expr2)
  }

  and parsePatternMatchCase = p => {
    Parser.beginRegion(p)
    Parser.leaveBreadcrumb(p, Grammar.PatternMatchCase)
    switch p.Parser.token {
    | Token.Bar =>
      Parser.next(p)
      let lhs = parsePattern(p)
      let guard = switch p.Parser.token {
      | When =>
        Parser.next(p)
        Some(parseExpr(~context=WhenExpr, p))
      | _ => None
      }

      let () = switch p.token {
      | EqualGreater => Parser.next(p)
      | _ => Recover.recoverEqualGreater(p)
      }

      let rhs = parseExprBlock(p)
      Parser.endRegion(p)
      Parser.eatBreadcrumb(p)
      Some(Ast_helper.Exp.case(lhs, ~guard?, rhs))
    | _ =>
      Parser.endRegion(p)
      None
    }
  }

  and parsePatternMatching = p => {
    Parser.leaveBreadcrumb(p, Grammar.PatternMatching)
    let cases = parseDelimitedRegion(
      ~grammar=Grammar.PatternMatching,
      ~closing=Rbrace,
      ~f=parsePatternMatchCase,
      p,
    )

    let () = switch cases {
    | list{} =>
      Parser.err(
        ~startPos=p.prevEndPos,
        p,
        Diagnostics.message("Pattern matching needs at least one case"),
      )
    | _ => ()
    }

    cases
  }

  and parseSwitchExpression = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Switch, p)
    let switchExpr = parseExpr(~context=WhenExpr, p)
    Parser.expect(Lbrace, p)
    let cases = parsePatternMatching(p)
    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Exp.match_(~loc, switchExpr, cases)
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
   *   | ~ label-name = ? expr : type
   *
   *  uncurried_argument ::=
   *   | . argument
   */
  and parseArgument = p =>
    if (
      p.Parser.token == Token.Tilde ||
        (p.token == Dot ||
        (p.token == Underscore || Grammar.isExprStart(p.token)))
    ) {
      switch p.Parser.token {
      | Dot =>
        let uncurried = true
        let startPos = p.Parser.startPos
        Parser.next(p)
        switch p.token {
        /* apply(.) */
        | Rparen =>
          let loc = mkLoc(startPos, p.prevEndPos)
          let unitExpr = Ast_helper.Exp.construct(
            ~loc,
            Location.mkloc(Longident.Lident("()"), loc),
            None,
          )

          Some(uncurried, Asttypes.Nolabel, unitExpr)
        | _ => parseArgument2(p, ~uncurried)
        }
      | _ => parseArgument2(p, ~uncurried=false)
      }
    } else {
      None
    }

  and parseArgument2 = (p, ~uncurried) =>
    switch p.Parser.token {
    /* foo(_), do not confuse with foo(_ => x), TODO: performance */
    | Underscore when !isEs6ArrowExpression(~inTernary=false, p) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      let exp = Ast_helper.Exp.ident(~loc, Location.mkloc(Longident.Lident("_"), loc))
      Some(uncurried, Asttypes.Nolabel, exp)
    | Tilde =>
      Parser.next(p)
      /* TODO: nesting of pattern matches not intuitive for error recovery */
      switch p.Parser.token {
      | Lident(ident) =>
        let startPos = p.startPos
        Parser.next(p)
        let endPos = p.prevEndPos
        let loc = mkLoc(startPos, endPos)
        let propLocAttr = (Location.mkloc("res.namedArgLoc", loc), Parsetree.PStr(list{}))
        let identExpr = Ast_helper.Exp.ident(
          ~attrs=list{propLocAttr},
          ~loc,
          Location.mkloc(Longident.Lident(ident), loc),
        )
        switch p.Parser.token {
        | Question =>
          Parser.next(p)
          Some(uncurried, Asttypes.Optional(ident), identExpr)
        | Equal =>
          Parser.next(p)
          let label = switch p.Parser.token {
          | Question =>
            Parser.next(p)
            Asttypes.Optional(ident)
          | _ => Labelled(ident)
          }

          let expr = switch p.Parser.token {
          | Underscore when !isEs6ArrowExpression(~inTernary=false, p) =>
            let loc = mkLoc(p.startPos, p.endPos)
            Parser.next(p)
            Ast_helper.Exp.ident(~loc, Location.mkloc(Longident.Lident("_"), loc))
          | _ =>
            let expr = parseConstrainedOrCoercedExpr(p)
            {...expr, pexp_attributes: list{propLocAttr, ...expr.pexp_attributes}}
          }

          Some(uncurried, label, expr)
        | Colon =>
          Parser.next(p)
          let typ = parseTypExpr(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let expr = Ast_helper.Exp.constraint_(~attrs=list{propLocAttr}, ~loc, identExpr, typ)
          Some(uncurried, Labelled(ident), expr)
        | _ => Some(uncurried, Labelled(ident), identExpr)
        }
      | t =>
        Parser.err(p, Diagnostics.lident(t))
        Some(uncurried, Nolabel, Recover.defaultExpr())
      }
    | _ => Some(uncurried, Nolabel, parseConstrainedOrCoercedExpr(p))
    }

  and parseCallExpr = (p, funExpr) => {
    Parser.expect(Lparen, p)
    let startPos = p.Parser.startPos
    Parser.leaveBreadcrumb(p, Grammar.ExprCall)
    let args = parseCommaDelimitedRegion(
      ~grammar=Grammar.ArgumentList,
      ~closing=Rparen,
      ~f=parseArgument,
      p,
    )

    Parser.expect(Rparen, p)
    let args = switch args {
    | list{} =>
      let loc = mkLoc(startPos, p.prevEndPos)
      /* No args -> unit sugar: `foo()` */
      list{
        (
          false,
          Asttypes.Nolabel,
          Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None),
        ),
      }
    | args => args
    }

    let loc = {...funExpr.pexp_loc, loc_end: p.prevEndPos}
    let args = switch args {
    | list{(u, lbl, expr), ...args} =>
      let group = ((grp, acc), (uncurried, lbl, expr)) => {
        let (_u, grp) = grp
        if uncurried === true {
          ((true, list{(lbl, expr)}), list{(_u, List.rev(grp)), ...acc})
        } else {
          ((_u, list{(lbl, expr), ...grp}), acc)
        }
      }

      let ((_u, grp), acc) = List.fold_left(group, ((u, list{(lbl, expr)}), list{}), args)
      List.rev(list{(_u, List.rev(grp)), ...acc})
    | list{} => list{}
    }

    let apply = List.fold_left((callBody, group) => {
      let (uncurried, args) = group
      let (args, wrap) = processUnderscoreApplication(args)
      let exp = if uncurried {
        let attrs = list{uncurryAttr}
        Ast_helper.Exp.apply(~loc, ~attrs, callBody, args)
      } else {
        Ast_helper.Exp.apply(~loc, callBody, args)
      }

      wrap(exp)
    }, funExpr, args)

    Parser.eatBreadcrumb(p)
    apply
  }

  and parseValueOrConstructor = p => {
    let startPos = p.Parser.startPos
    let rec aux = (p, acc) =>
      switch p.Parser.token {
      | Uident(ident) =>
        let endPosLident = p.endPos
        Parser.next(p)
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          aux(p, list{ident, ...acc})
        | Lparen when p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
          let lparen = p.startPos
          let args = parseConstructorArgs(p)
          let rparen = p.prevEndPos
          let lident = buildLongident(list{ident, ...acc})
          let tail = switch args {
          | list{} => None
          | list{{Parsetree.pexp_desc: Pexp_tuple(_)} as arg} as args =>
            let loc = mkLoc(lparen, rparen)
            if p.mode == ParseForTypeChecker {
              /* Some(1, 2) for type-checker */
              Some(arg)
            } else {
              /* Some((1, 2)) for printer */
              Some(Ast_helper.Exp.tuple(~loc, args))
            }
          | list{arg} => Some(arg)
          | args =>
            let loc = mkLoc(lparen, rparen)
            Some(Ast_helper.Exp.tuple(~loc, args))
          }

          let loc = mkLoc(startPos, p.prevEndPos)
          let identLoc = mkLoc(startPos, endPosLident)
          Ast_helper.Exp.construct(~loc, Location.mkloc(lident, identLoc), tail)
        | _ =>
          let loc = mkLoc(startPos, p.prevEndPos)
          let lident = buildLongident(list{ident, ...acc})
          Ast_helper.Exp.construct(~loc, Location.mkloc(lident, loc), None)
        }
      | Lident(ident) =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let lident = buildLongident(list{ident, ...acc})
        Ast_helper.Exp.ident(~loc, Location.mkloc(lident, loc))
      | List =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let lident = buildLongident(list{"list", ...acc})
        Ast_helper.Exp.ident(~loc, Location.mkloc(lident, loc))
      | token =>
        Parser.next(p)
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        Recover.defaultExpr()
      }

    aux(p, list{})
  }

  and parsePolyVariantExpr = p => {
    let startPos = p.startPos
    let (ident, _loc) = parseHashIdent(~startPos, p)
    switch p.Parser.token {
    | Lparen when p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
      let lparen = p.startPos
      let args = parseConstructorArgs(p)
      let rparen = p.prevEndPos
      let loc_paren = mkLoc(lparen, rparen)
      let tail = switch args {
      | list{} => None
      | list{{Parsetree.pexp_desc: Pexp_tuple(_)} as expr} as args =>
        if p.mode == ParseForTypeChecker {
          /* #a(1, 2) for type-checker */
          Some(expr)
        } else {
          /* #a((1, 2)) for type-checker */
          Some(Ast_helper.Exp.tuple(~loc=loc_paren, args))
        }
      | list{arg} => Some(arg)
      | args =>
        /* #a((1, 2)) for printer */
        Some(Ast_helper.Exp.tuple(~loc=loc_paren, args))
      }

      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.variant(~loc, ident, tail)
    | _ =>
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Exp.variant(~loc, ident, None)
    }
  }

  and parseConstructorArgs = p => {
    let lparen = p.Parser.startPos
    Parser.expect(Lparen, p)
    let args = parseCommaDelimitedRegion(
      ~grammar=Grammar.ExprList,
      ~f=parseConstrainedExprRegion,
      ~closing=Rparen,
      p,
    )

    Parser.expect(Rparen, p)
    switch args {
    | list{} =>
      let loc = mkLoc(lparen, p.prevEndPos)
      list{Ast_helper.Exp.construct(~loc, Location.mkloc(Longident.Lident("()"), loc), None)}
    | args => args
    }
  }

  and parseTupleExpr = (~first, ~startPos, p) => {
    let exprs = parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.ExprList,
      ~closing=Rparen,
      ~f=parseConstrainedExprRegion,
    )

    Parser.expect(Rparen, p)
    Ast_helper.Exp.tuple(~loc=mkLoc(startPos, p.prevEndPos), list{first, ...exprs})
  }

  and parseSpreadExprRegion = p =>
    switch p.Parser.token {
    | DotDotDot =>
      Parser.next(p)
      let expr = parseConstrainedOrCoercedExpr(p)
      Some(true, expr)
    | token when Grammar.isExprStart(token) => Some(false, parseConstrainedOrCoercedExpr(p))
    | _ => None
    }

  and parseListExpr = (~startPos, p) => {
    Parser.expect(Lbracket, p)
    let listExprs = parseCommaDelimitedReversedList(
      p,
      ~grammar=Grammar.ListExpr,
      ~closing=Rbracket,
      ~f=parseSpreadExprRegion,
    )

    Parser.expect(Rbracket, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    switch listExprs {
    | list{(true, expr), ...exprs} =>
      let exprs = exprs |> List.map(snd) |> List.rev
      makeListExpression(loc, exprs, Some(expr))
    | exprs =>
      let exprs = exprs |> List.map(((spread, expr)) => {
        if spread {
          Parser.err(p, Diagnostics.message(ErrorMessages.listExprSpread))
        }
        expr
      }) |> List.rev

      makeListExpression(loc, exprs, None)
    }
  }

  /* Overparse ... and give a nice error message */
  and parseNonSpreadExp = (~msg, p) => {
    let () = switch p.Parser.token {
    | DotDotDot =>
      Parser.err(p, Diagnostics.message(msg))
      Parser.next(p)
    | _ => ()
    }

    switch p.Parser.token {
    | token when Grammar.isExprStart(token) =>
      let expr = parseExpr(p)
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(expr.pexp_loc.loc_start, typ.ptyp_loc.loc_end)
        Some(Ast_helper.Exp.constraint_(~loc, expr, typ))
      | _ => Some(expr)
      }
    | _ => None
    }
  }

  and parseArrayExp = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lbracket, p)
    let exprs = parseCommaDelimitedRegion(
      p,
      ~grammar=Grammar.ExprList,
      ~closing=Rbracket,
      ~f=parseNonSpreadExp(~msg=ErrorMessages.arrayExprSpread),
    )

    Parser.expect(Rbracket, p)
    Ast_helper.Exp.array(~loc=mkLoc(startPos, p.prevEndPos), exprs)
  }

  /* TODO: check attributes in the case of poly type vars,
   * might be context dependend: parseFieldDeclaration (see ocaml) */
  and parsePolyTypeExpr = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | SingleQuote =>
      let vars = parseTypeVarList(p)
      switch vars {
      | list{_v1, _v2, ..._} =>
        Parser.expect(Dot, p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Typ.poly(~loc, vars, typ)
      | list{var} =>
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          let typ = parseTypExpr(p)
          let loc = mkLoc(startPos, p.prevEndPos)
          Ast_helper.Typ.poly(~loc, vars, typ)
        | EqualGreater =>
          Parser.next(p)
          let typ = Ast_helper.Typ.var(~loc=var.loc, var.txt)
          let returnType = parseTypExpr(~alias=false, p)
          let loc = mkLoc(typ.Parsetree.ptyp_loc.loc_start, p.prevEndPos)
          Ast_helper.Typ.arrow(~loc, Asttypes.Nolabel, typ, returnType)
        | _ => Ast_helper.Typ.var(~loc=var.loc, var.txt)
        }
      | _ => assert false
      }
    | _ => parseTypExpr(p)
    }
  }

  /* 'a 'b 'c */
  and parseTypeVarList = p => {
    let rec loop = (p, vars) =>
      switch p.Parser.token {
      | SingleQuote =>
        Parser.next(p)
        let (lident, loc) = parseLident(p)
        let var = Location.mkloc(lident, loc)
        loop(p, list{var, ...vars})
      | _ => List.rev(vars)
      }

    loop(p, list{})
  }

  and parseLidentList = p => {
    let rec loop = (p, ls) =>
      switch p.Parser.token {
      | Lident(lident) =>
        let loc = mkLoc(p.startPos, p.endPos)
        Parser.next(p)
        loop(p, list{Location.mkloc(lident, loc), ...ls})
      | _ => List.rev(ls)
      }

    loop(p, list{})
  }

  and parseAtomicTypExpr = (~attrs, p) => {
    Parser.leaveBreadcrumb(p, Grammar.AtomicTypExpr)
    let startPos = p.Parser.startPos
    let typ = switch p.Parser.token {
    | SingleQuote =>
      Parser.next(p)
      let (ident, loc) = parseLident(p)
      Ast_helper.Typ.var(~loc, ~attrs, ident)
    | Underscore =>
      let endPos = p.endPos
      Parser.next(p)
      Ast_helper.Typ.any(~loc=mkLoc(startPos, endPos), ~attrs, ())
    | Lparen =>
      Parser.next(p)
      switch p.Parser.token {
      | Rparen =>
        Parser.next(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let unitConstr = Location.mkloc(Longident.Lident("unit"), loc)
        Ast_helper.Typ.constr(~attrs, unitConstr, list{})
      | _ =>
        let t = parseTypExpr(p)
        switch p.token {
        | Comma =>
          Parser.next(p)
          parseTupleType(~attrs, ~first=t, ~startPos, p)
        | _ =>
          Parser.expect(Rparen, p)
          {
            ...t,
            ptyp_loc: mkLoc(startPos, p.prevEndPos),
            ptyp_attributes: List.concat(list{attrs, t.ptyp_attributes}),
          }
        }
      }
    | Lbracket => parsePolymorphicVariantType(~attrs, p)
    | Uident(_) | Lident(_) | List =>
      let constr = parseValuePath(p)
      let args = parseTypeConstructorArgs(~constrName=constr, p)
      Ast_helper.Typ.constr(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, constr, args)
    | Module =>
      Parser.next(p)
      Parser.expect(Lparen, p)
      let packageType = parsePackageType(~startPos, ~attrs, p)
      Parser.expect(Rparen, p)
      {...packageType, ptyp_loc: mkLoc(startPos, p.prevEndPos)}
    | Percent =>
      let extension = parseExtension(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.extension(~attrs, ~loc, extension)
    | Lbrace => parseBsObjectType(~attrs, p)
    | token =>
      switch skipTokensAndMaybeRetry(p, ~isStartOfGrammar=Grammar.isAtomicTypExprStart) {
      | Some() => parseAtomicTypExpr(~attrs, p)
      | None =>
        Parser.err(~startPos=p.prevEndPos, p, Diagnostics.unexpected(token, p.breadcrumbs))
        Recover.defaultType()
      }
    }

    Parser.eatBreadcrumb(p)
    typ
  }

  /* package-type	::=
      | modtype-path
      ∣ modtype-path with package-constraint  { and package-constraint }
 */
  and parsePackageType = (~startPos, ~attrs, p) => {
    let modTypePath = parseModuleLongIdent(~lowercase=true, p)
    switch p.Parser.token {
    | With =>
      Parser.next(p)
      let constraints = parsePackageConstraints(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.package(~loc, ~attrs, modTypePath, constraints)
    | _ =>
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.package(~loc, ~attrs, modTypePath, list{})
    }
  }

  /* package-constraint  { and package-constraint } */
  and parsePackageConstraints = p => {
    let first = {
      Parser.expect(Typ, p)
      let typeConstr = parseValuePath(p)
      Parser.expect(Equal, p)
      let typ = parseTypExpr(p)
      (typeConstr, typ)
    }

    let rest = parseRegion(~grammar=Grammar.PackageConstraint, ~f=parsePackageConstraint, p)

    list{first, ...rest}
  }

  /* and type typeconstr = typexpr */
  and parsePackageConstraint = p =>
    switch p.Parser.token {
    | And =>
      Parser.next(p)
      Parser.expect(Typ, p)
      let typeConstr = parseValuePath(p)
      Parser.expect(Equal, p)
      let typ = parseTypExpr(p)
      Some(typeConstr, typ)
    | _ => None
    }

  and parseBsObjectType = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Lbrace, p)
    let closedFlag = switch p.token {
    | DotDot =>
      Parser.next(p)
      Asttypes.Open
    | Dot =>
      Parser.next(p)
      Asttypes.Closed
    | _ => Asttypes.Closed
    }

    let fields = parseCommaDelimitedRegion(
      ~grammar=Grammar.StringFieldDeclarations,
      ~closing=Rbrace,
      ~f=parseStringFieldDeclaration,
      p,
    )

    Parser.expect(Rbrace, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    makeBsObjType(~attrs, ~loc, ~closed=closedFlag, fields)
  }

  /* TODO: check associativity in combination with attributes */
  and parseTypeAlias = (p, typ) =>
    switch p.Parser.token {
    | As =>
      Parser.next(p)
      Parser.expect(SingleQuote, p)
      let (ident, _loc) = parseLident(p)
      /* TODO: how do we parse attributes here? */
      Ast_helper.Typ.alias(~loc=mkLoc(typ.Parsetree.ptyp_loc.loc_start, p.prevEndPos), typ, ident)
    | _ => typ
    }

  /* type_parameter ::=
   *  | type_expr
   *  | ~ident: type_expr
   *  | ~ident: type_expr=?
   *
   * note:
   *  | attrs ~ident: type_expr    -> attrs are on the arrow
   *  | attrs type_expr            -> attrs are here part of the type_expr
   *
   * uncurried_type_parameter ::=
   *  | . type_parameter
   */
  and parseTypeParameter = p =>
    if p.Parser.token == Token.Tilde || (p.token == Dot || Grammar.isTypExprStart(p.token)) {
      let startPos = p.Parser.startPos
      let uncurried = Parser.optional(p, Dot)
      let attrs = parseAttributes(p)
      switch p.Parser.token {
      | Tilde =>
        Parser.next(p)
        let (name, _loc) = parseLident(p)
        Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
        let typ = parseTypExpr(p)
        switch p.Parser.token {
        | Equal =>
          Parser.next(p)
          Parser.expect(Question, p)
          Some(uncurried, attrs, Asttypes.Optional(name), typ, startPos)
        | _ => Some(uncurried, attrs, Asttypes.Labelled(name), typ, startPos)
        }
      | Lident(_) | List =>
        let (name, loc) = parseLident(p)
        switch p.token {
        | Colon =>
          let () = {
            let error = Diagnostics.message("Parameter names start with a `~`, like: ~" ++ name)

            Parser.err(~startPos=loc.loc_start, ~endPos=loc.loc_end, p, error)
          }

          Parser.next(p)
          let typ = parseTypExpr(p)
          switch p.Parser.token {
          | Equal =>
            Parser.next(p)
            Parser.expect(Question, p)
            Some(uncurried, attrs, Asttypes.Optional(name), typ, startPos)
          | _ => Some(uncurried, attrs, Asttypes.Labelled(name), typ, startPos)
          }
        | _ =>
          let constr = Location.mkloc(Longident.Lident(name), loc)
          let args = parseTypeConstructorArgs(~constrName=constr, p)
          let typ = Ast_helper.Typ.constr(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, constr, args)

          let typ = parseArrowTypeRest(~es6Arrow=true, ~startPos, typ, p)
          let typ = parseTypeAlias(p, typ)
          Some(uncurried, list{}, Asttypes.Nolabel, typ, startPos)
        }
      | _ =>
        let typ = parseTypExpr(p)
        let typWithAttributes = {
          ...typ,
          ptyp_attributes: List.concat(list{attrs, typ.ptyp_attributes}),
        }
        Some(uncurried, list{}, Asttypes.Nolabel, typWithAttributes, startPos)
      }
    } else {
      None
    }

  /* (int, ~x:string, float) */
  and parseTypeParameters = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lparen, p)
    switch p.Parser.token {
    | Rparen =>
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let unitConstr = Location.mkloc(Longident.Lident("unit"), loc)
      let typ = Ast_helper.Typ.constr(unitConstr, list{})
      list{(false, list{}, Asttypes.Nolabel, typ, startPos)}
    | _ =>
      let params = parseCommaDelimitedRegion(
        ~grammar=Grammar.TypeParameters,
        ~closing=Rparen,
        ~f=parseTypeParameter,
        p,
      )

      Parser.expect(Rparen, p)
      params
    }
  }

  and parseEs6ArrowType = (~attrs, p) => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Tilde =>
      Parser.next(p)
      let (name, _loc) = parseLident(p)
      Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
      let typ = parseTypExpr(~alias=false, ~es6Arrow=false, p)
      let arg = switch p.Parser.token {
      | Equal =>
        Parser.next(p)
        Parser.expect(Question, p)
        Asttypes.Optional(name)
      | _ => Asttypes.Labelled(name)
      }

      Parser.expect(EqualGreater, p)
      let returnType = parseTypExpr(~alias=false, p)
      Ast_helper.Typ.arrow(~attrs, arg, typ, returnType)
    | _ =>
      let parameters = parseTypeParameters(p)
      Parser.expect(EqualGreater, p)
      let returnType = parseTypExpr(~alias=false, p)
      let endPos = p.prevEndPos
      let typ = List.fold_right(((uncurried, attrs, argLbl, typ, startPos), t) => {
        let attrs = if uncurried {
          list{uncurryAttr, ...attrs}
        } else {
          attrs
        }
        Ast_helper.Typ.arrow(~loc=mkLoc(startPos, endPos), ~attrs, argLbl, typ, t)
      }, parameters, returnType)

      {
        ...typ,
        ptyp_attributes: List.concat(list{typ.ptyp_attributes, attrs}),
        ptyp_loc: mkLoc(startPos, p.prevEndPos),
      }
    }
  }

  /*
   * typexpr ::=
   *  | 'ident
   *  | _
   *  | (typexpr)
   *  | typexpr => typexpr            --> es6 arrow
   *  | (typexpr, typexpr) => typexpr --> es6 arrow
   *  | /typexpr, typexpr, typexpr/  --> tuple
   *  | typeconstr
   *  | typeconstr<typexpr>
   *  | typeconstr<typexpr, typexpr,>
   *  | typexpr as 'ident
   *  | %attr-id                      --> extension
   *  | %attr-id(payload)             --> extension
   *
   * typeconstr ::=
   *  | lident
   *  | uident.lident
   *  | uident.uident.lident     --> long module path
   */
  and parseTypExpr = (~attrs=?, ~es6Arrow=true, ~alias=true, p) => {
    /* Parser.leaveBreadcrumb p Grammar.TypeExpression; */
    let startPos = p.Parser.startPos
    let attrs = switch attrs {
    | Some(attrs) => attrs
    | None => parseAttributes(p)
    }
    let typ = if es6Arrow && isEs6ArrowType(p) {
      parseEs6ArrowType(~attrs, p)
    } else {
      let typ = parseAtomicTypExpr(~attrs, p)
      parseArrowTypeRest(~es6Arrow, ~startPos, typ, p)
    }

    let typ = if alias {
      parseTypeAlias(p, typ)
    } else {
      typ
    }

    /* Parser.eatBreadcrumb p; */
    typ
  }

  and parseArrowTypeRest = (~es6Arrow, ~startPos, typ, p) =>
    switch p.Parser.token {
    | (EqualGreater | MinusGreater) as token when es6Arrow === true =>
      /* error recovery */
      if token == MinusGreater {
        Parser.expect(EqualGreater, p)
      }
      Parser.next(p)
      let returnType = parseTypExpr(~alias=false, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Typ.arrow(~loc, Asttypes.Nolabel, typ, returnType)
    | _ => typ
    }

  and parseTypExprRegion = p =>
    if Grammar.isTypExprStart(p.Parser.token) {
      Some(parseTypExpr(p))
    } else {
      None
    }

  and parseTupleType = (~attrs, ~first, ~startPos, p) => {
    let typexprs = parseCommaDelimitedRegion(
      ~grammar=Grammar.TypExprList,
      ~closing=Rparen,
      ~f=parseTypExprRegion,
      p,
    )

    Parser.expect(Rparen, p)
    let tupleLoc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Typ.tuple(~attrs, ~loc=tupleLoc, list{first, ...typexprs})
  }

  and parseTypeConstructorArgRegion = p =>
    if Grammar.isTypExprStart(p.Parser.token) {
      Some(parseTypExpr(p))
    } else if p.token == LessThan {
      Parser.next(p)
      parseTypeConstructorArgRegion(p)
    } else {
      None
    }

  /* Js.Nullable.value<'a> */
  and parseTypeConstructorArgs = (~constrName, p) => {
    let opening = p.Parser.token
    let openingStartPos = p.startPos
    switch opening {
    | LessThan | Lparen =>
      Scanner.setDiamondMode(p.scanner)
      Parser.next(p)
      let typeArgs = /* TODO: change Grammar.TypExprList to TypArgList!!! Why did I wrote this? */
      parseCommaDelimitedRegion(
        ~grammar=Grammar.TypExprList,
        ~closing=GreaterThan,
        ~f=parseTypeConstructorArgRegion,
        p,
      )

      let () = switch p.token {
      | Rparen when opening == Token.Lparen =>
        let typ = Ast_helper.Typ.constr(constrName, typeArgs)
        let msg =
          Doc.breakableGroup(
            ~forceBreak=true,
            Doc.concat(list{
              Doc.text("Type parameters require angle brackets:"),
              Doc.indent(Doc.concat(list{Doc.line, Printer.printTypExpr(typ, CommentTable.empty)})),
            }),
          ) |> Doc.toString(~width=80)

        Parser.err(~startPos=openingStartPos, p, Diagnostics.message(msg))
        Parser.next(p)
      | _ => Parser.expect(GreaterThan, p)
      }

      Scanner.popMode(p.scanner, Diamond)
      typeArgs
    | _ => list{}
    }
  }

  /* string-field-decl ::=
   *  | string: poly-typexpr
   *  | attributes string-field-decl */
  and parseStringFieldDeclaration = p => {
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | String(name) =>
      let nameStartPos = p.startPos
      let nameEndPos = p.endPos
      Parser.next(p)
      let fieldName = Location.mkloc(name, mkLoc(nameStartPos, nameEndPos))
      Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
      let typ = parsePolyTypeExpr(p)
      Some(Parsetree.Otag(fieldName, attrs, typ))
    | _token => None
    }
  }

  /* field-decl	::=
   *  | [mutable] field-name : poly-typexpr
   *  | attributes field-decl */
  and parseFieldDeclaration = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    let mut = if Parser.optional(p, Token.Mutable) {
      Asttypes.Mutable
    } else {
      Asttypes.Immutable
    }

    let (lident, loc) = switch p.token {
    | List =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      ("list", loc)
    | _ => parseLident(p)
    }

    let name = Location.mkloc(lident, loc)
    let typ = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      parsePolyTypeExpr(p)
    | _ => Ast_helper.Typ.constr(~loc=name.loc, {...name, txt: Lident(name.txt)}, list{})
    }

    let loc = mkLoc(startPos, typ.ptyp_loc.loc_end)
    Ast_helper.Type.field(~attrs, ~loc, ~mut, name, typ)
  }

  and parseFieldDeclarationRegion = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    let mut = if Parser.optional(p, Token.Mutable) {
      Asttypes.Mutable
    } else {
      Asttypes.Immutable
    }

    switch p.token {
    | Lident(_) | List =>
      let (lident, loc) = switch p.token {
      | List =>
        let loc = mkLoc(p.startPos, p.endPos)
        Parser.next(p)
        ("list", loc)
      | _ => parseLident(p)
      }

      let name = Location.mkloc(lident, loc)
      let typ = switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        parsePolyTypeExpr(p)
      | _ => Ast_helper.Typ.constr(~loc=name.loc, {...name, txt: Lident(name.txt)}, list{})
      }

      let loc = mkLoc(startPos, typ.ptyp_loc.loc_end)
      Some(Ast_helper.Type.field(~attrs, ~loc, ~mut, name, typ))
    | _ => None
    }
  }

  /* record-decl ::=
   *  | { field-decl }
   *  | { field-decl, field-decl }
   *  | { field-decl, field-decl, field-decl, }
   */
  and parseRecordDeclaration = p => {
    Parser.leaveBreadcrumb(p, Grammar.RecordDecl)
    Parser.expect(Lbrace, p)
    let rows = parseCommaDelimitedRegion(
      ~grammar=Grammar.RecordDecl,
      ~closing=Rbrace,
      ~f=parseFieldDeclarationRegion,
      p,
    )

    Parser.expect(Rbrace, p)
    Parser.eatBreadcrumb(p)
    rows
  }

  /* constr-args ::=
   *  | (typexpr)
   *  | (typexpr, typexpr)
   *  | (typexpr, typexpr, typexpr,)
   *  | (record-decl)
   *
   * TODO: should we overparse inline-records in every position?
   * Give a good error message afterwards?
   */
  and parseConstrDeclArgs = p => {
    let constrArgs = switch p.Parser.token {
    | Lparen =>
      Parser.next(p)
      /* TODO: this could use some cleanup/stratification */
      switch p.Parser.token {
      | Lbrace =>
        let lbrace = p.startPos
        Parser.next(p)
        let startPos = p.Parser.startPos
        switch p.Parser.token {
        | DotDot | Dot =>
          let closedFlag = switch p.token {
          | DotDot =>
            Parser.next(p)
            Asttypes.Open
          | Dot =>
            Parser.next(p)
            Asttypes.Closed
          | _ => Asttypes.Closed
          }

          let fields = parseCommaDelimitedRegion(
            ~grammar=Grammar.StringFieldDeclarations,
            ~closing=Rbrace,
            ~f=parseStringFieldDeclaration,
            p,
          )

          Parser.expect(Rbrace, p)
          let loc = mkLoc(startPos, p.prevEndPos)
          let typ = makeBsObjType(~attrs=list{}, ~loc, ~closed=closedFlag, fields)
          Parser.optional(p, Comma) |> ignore
          let moreArgs = parseCommaDelimitedRegion(
            ~grammar=Grammar.TypExprList,
            ~closing=Rparen,
            ~f=parseTypExprRegion,
            p,
          )

          Parser.expect(Rparen, p)
          Parsetree.Pcstr_tuple(list{typ, ...moreArgs})
        | _ =>
          let attrs = parseAttributes(p)
          switch p.Parser.token {
          | String(_) =>
            let closedFlag = Asttypes.Closed
            let fields = switch attrs {
            | list{} =>
              parseCommaDelimitedRegion(
                ~grammar=Grammar.StringFieldDeclarations,
                ~closing=Rbrace,
                ~f=parseStringFieldDeclaration,
                p,
              )
            | attrs =>
              let first = {
                Parser.leaveBreadcrumb(p, Grammar.StringFieldDeclarations)
                let field = switch parseStringFieldDeclaration(p) {
                | Some(field) => field
                | None => assert false
                }

                /* parse comma after first */
                let () = switch p.Parser.token {
                | Rbrace | Eof => ()
                | Comma => Parser.next(p)
                | _ => Parser.expect(Comma, p)
                }

                Parser.eatBreadcrumb(p)
                switch field {
                | Parsetree.Otag(label, _, ct) => Parsetree.Otag(label, attrs, ct)
                | Oinherit(ct) => Oinherit(ct)
                }
              }

              list{
                first,
                ...parseCommaDelimitedRegion(
                  ~grammar=Grammar.StringFieldDeclarations,
                  ~closing=Rbrace,
                  ~f=parseStringFieldDeclaration,
                  p,
                ),
              }
            }
            Parser.expect(Rbrace, p)
            let loc = mkLoc(startPos, p.prevEndPos)
            let typ = makeBsObjType(~attrs=list{}, ~loc, ~closed=closedFlag, fields)
            Parser.optional(p, Comma) |> ignore
            let moreArgs = parseCommaDelimitedRegion(
              ~grammar=Grammar.TypExprList,
              ~closing=Rparen,
              ~f=parseTypExprRegion,
              p,
            )

            Parser.expect(Rparen, p)
            Parsetree.Pcstr_tuple(list{typ, ...moreArgs})
          | _ =>
            let fields = switch attrs {
            | list{} =>
              parseCommaDelimitedRegion(
                ~grammar=Grammar.FieldDeclarations,
                ~closing=Rbrace,
                ~f=parseFieldDeclarationRegion,
                p,
              )
            | attrs =>
              let first = {
                let field = parseFieldDeclaration(p)
                Parser.expect(Comma, p)
                {...field, Parsetree.pld_attributes: attrs}
              }

              list{
                first,
                ...parseCommaDelimitedRegion(
                  ~grammar=Grammar.FieldDeclarations,
                  ~closing=Rbrace,
                  ~f=parseFieldDeclarationRegion,
                  p,
                ),
              }
            }

            let () = switch fields {
            | list{} =>
              Parser.err(
                ~startPos=lbrace,
                p,
                Diagnostics.message("An inline record declaration needs at least one field"),
              )
            | _ => ()
            }

            Parser.expect(Rbrace, p)
            Parser.optional(p, Comma) |> ignore
            Parser.expect(Rparen, p)
            Parsetree.Pcstr_record(fields)
          }
        }
      | _ =>
        let args = parseCommaDelimitedRegion(
          ~grammar=Grammar.TypExprList,
          ~closing=Rparen,
          ~f=parseTypExprRegion,
          p,
        )

        Parser.expect(Rparen, p)
        Parsetree.Pcstr_tuple(args)
      }
    | _ => Pcstr_tuple(list{})
    }

    let res = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      Some(parseTypExpr(p))
    | _ => None
    }

    (constrArgs, res)
  }

  /* constr-decl ::=
   *  | constr-name
   *  | attrs constr-name
   *  | constr-name const-args
   *  | attrs constr-name const-args */
  and parseTypeConstructorDeclarationWithBar = p =>
    switch p.Parser.token {
    | Bar =>
      let startPos = p.Parser.startPos
      Parser.next(p)
      Some(parseTypeConstructorDeclaration(~startPos, p))
    | _ => None
    }

  and parseTypeConstructorDeclaration = (~startPos, p) => {
    Parser.leaveBreadcrumb(p, Grammar.ConstructorDeclaration)
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Uident(uident) =>
      let uidentLoc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      let (args, res) = parseConstrDeclArgs(p)
      Parser.eatBreadcrumb(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Type.constructor(~loc, ~attrs, ~res?, ~args, Location.mkloc(uident, uidentLoc))
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Ast_helper.Type.constructor(Location.mknoloc("_"))
    }
  }

  /* [|] constr-decl  { | constr-decl } */
  and parseTypeConstructorDeclarations = (~first=?, p) => {
    let firstConstrDecl = switch first {
    | None =>
      let startPos = p.Parser.startPos
      ignore(Parser.optional(p, Token.Bar))
      parseTypeConstructorDeclaration(~startPos, p)
    | Some(firstConstrDecl) => firstConstrDecl
    }

    list{
      firstConstrDecl,
      ...parseRegion(
        ~grammar=Grammar.ConstructorDeclaration,
        ~f=parseTypeConstructorDeclarationWithBar,
        p,
      ),
    }
  }

  /*
   * type-representation ::=
   *  ∣	 = [ | ] constr-decl  { | constr-decl }
   *  ∣	 = private [ | ] constr-decl  { | constr-decl }
   *  |  = |
   *  ∣	 = private |
   *  ∣	 = record-decl
   *  ∣	 = private record-decl
   *  |  = ..
   */
  and parseTypeRepresentation = p => {
    Parser.leaveBreadcrumb(p, Grammar.TypeRepresentation)
    /* = consumed */
    let privateFlag = if Parser.optional(p, Token.Private) {
      Asttypes.Private
    } else {
      Asttypes.Public
    }

    let kind = switch p.Parser.token {
    | Bar | Uident(_) => Parsetree.Ptype_variant(parseTypeConstructorDeclarations(p))
    | Lbrace => Parsetree.Ptype_record(parseRecordDeclaration(p))
    | DotDot =>
      Parser.next(p)
      Ptype_open
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      /* TODO: I have no idea if this is even remotely a good idea */
      Parsetree.Ptype_variant(list{})
    }

    Parser.eatBreadcrumb(p)
    (privateFlag, kind)
  }

  /* type-param	::=
   *  | variance 'lident
   *  | variance _
   *
   * variance ::=
   *   | +
   *   | -
   *   | (* empty *)
   */
  and parseTypeParam = p => {
    let variance = switch p.Parser.token {
    | Plus =>
      Parser.next(p)
      Asttypes.Covariant
    | Minus =>
      Parser.next(p)
      Contravariant
    | _ => Invariant
    }

    switch p.Parser.token {
    | SingleQuote =>
      Parser.next(p)
      let (ident, loc) = parseLident(p)
      Some(Ast_helper.Typ.var(~loc, ident), variance)
    | Underscore =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Some(Ast_helper.Typ.any(~loc, ()), variance)
    /* TODO: should we try parsing lident as 'ident ? */
    | _token => None
    }
  }

  /* type-params	::=
   *  | <type-param>
   *  ∣	<type-param, type-param>
   *  ∣	<type-param, type-param, type-param>
   *  ∣	<type-param, type-param, type-param,>
   *
   *  TODO: when we have pretty-printer show an error
   *  with the actual code corrected. */
  and parseTypeParams = (~parent, p) => {
    let opening = p.Parser.token
    switch opening {
    | LessThan | Lparen when p.startPos.pos_lnum === p.prevEndPos.pos_lnum =>
      Scanner.setDiamondMode(p.scanner)
      let openingStartPos = p.startPos
      Parser.leaveBreadcrumb(p, Grammar.TypeParams)
      Parser.next(p)
      let params = parseCommaDelimitedRegion(
        ~grammar=Grammar.TypeParams,
        ~closing=GreaterThan,
        ~f=parseTypeParam,
        p,
      )

      let () = switch p.token {
      | Rparen when opening == Token.Lparen =>
        let msg =
          Doc.breakableGroup(
            ~forceBreak=true,
            Doc.concat(list{
              Doc.text("Type parameters require angle brackets:"),
              Doc.indent(
                Doc.concat(list{
                  Doc.line,
                  Doc.concat(list{
                    Printer.printLongident(parent.Location.txt),
                    Printer.printTypeParams(params, CommentTable.empty),
                  }),
                }),
              ),
            }),
          ) |> Doc.toString(~width=80)

        Parser.err(~startPos=openingStartPos, p, Diagnostics.message(msg))
        Parser.next(p)
      | _ => Parser.expect(GreaterThan, p)
      }

      Scanner.popMode(p.scanner, Diamond)
      Parser.eatBreadcrumb(p)
      params
    | _ => list{}
    }
  }

  /* type-constraint	::=	constraint ' ident =  typexpr */
  and parseTypeConstraint = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Token.Constraint =>
      Parser.next(p)
      Parser.expect(SingleQuote, p)
      switch p.Parser.token {
      | Lident(ident) =>
        let identLoc = mkLoc(startPos, p.endPos)
        Parser.next(p)
        Parser.expect(Equal, p)
        let typ = parseTypExpr(p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Typ.var(~loc=identLoc, ident), typ, loc)
      | t =>
        Parser.err(p, Diagnostics.lident(t))
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Typ.any(), parseTypExpr(p), loc)
      }
    | _ => None
    }
  }

  /* type-constraints ::=
   *  | (* empty *)
   *  | type-constraint
   *  | type-constraint type-constraint
   *  | type-constraint type-constraint type-constraint (* 0 or more *)
   */
  and parseTypeConstraints = p =>
    parseRegion(~grammar=Grammar.TypeConstraint, ~f=parseTypeConstraint, p)

  and parseTypeEquationOrConstrDecl = p => {
    let uidentStartPos = p.Parser.startPos
    switch p.Parser.token {
    | Uident(uident) =>
      Parser.next(p)
      switch p.Parser.token {
      | Dot =>
        Parser.next(p)
        let typeConstr = parseValuePathTail(p, uidentStartPos, Longident.Lident(uident))

        let loc = mkLoc(uidentStartPos, p.prevEndPos)
        let typ = parseTypeAlias(
          p,
          Ast_helper.Typ.constr(
            ~loc,
            typeConstr,
            parseTypeConstructorArgs(~constrName=typeConstr, p),
          ),
        )
        switch p.token {
        | Equal =>
          Parser.next(p)
          let (priv, kind) = parseTypeRepresentation(p)
          (Some(typ), priv, kind)
        | EqualGreater =>
          Parser.next(p)
          let returnType = parseTypExpr(~alias=false, p)
          let loc = mkLoc(uidentStartPos, p.prevEndPos)
          let arrowType = Ast_helper.Typ.arrow(~loc, Asttypes.Nolabel, typ, returnType)
          let typ = parseTypeAlias(p, arrowType)
          (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
        | _ => (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
        }
      | _ =>
        let uidentEndPos = p.endPos
        let (args, res) = parseConstrDeclArgs(p)
        let first = Some({
          let uidentLoc = mkLoc(uidentStartPos, uidentEndPos)
          Ast_helper.Type.constructor(
            ~loc=mkLoc(uidentStartPos, p.prevEndPos),
            ~res?,
            ~args,
            Location.mkloc(uident, uidentLoc),
          )
        })
        (
          None,
          Asttypes.Public,
          Parsetree.Ptype_variant(parseTypeConstructorDeclarations(p, ~first?)),
        )
      }
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      /* TODO: is this a good idea? */
      (None, Asttypes.Public, Parsetree.Ptype_abstract)
    }
  }

  and parseRecordOrBsObjectDecl = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lbrace, p)
    switch p.Parser.token {
    | DotDot | Dot =>
      let closedFlag = switch p.token {
      | DotDot =>
        Parser.next(p)
        Asttypes.Open
      | Dot =>
        Parser.next(p)
        Asttypes.Closed
      | _ => Asttypes.Closed
      }

      let fields = parseCommaDelimitedRegion(
        ~grammar=Grammar.StringFieldDeclarations,
        ~closing=Rbrace,
        ~f=parseStringFieldDeclaration,
        p,
      )

      Parser.expect(Rbrace, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      let typ = makeBsObjType(~attrs=list{}, ~loc, ~closed=closedFlag, fields) |> parseTypeAlias(p)

      let typ = parseArrowTypeRest(~es6Arrow=true, ~startPos, typ, p)
      (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
    | _ =>
      let attrs = parseAttributes(p)
      switch p.Parser.token {
      | String(_) =>
        let closedFlag = Asttypes.Closed
        let fields = switch attrs {
        | list{} =>
          parseCommaDelimitedRegion(
            ~grammar=Grammar.StringFieldDeclarations,
            ~closing=Rbrace,
            ~f=parseStringFieldDeclaration,
            p,
          )
        | attrs =>
          let first = {
            Parser.leaveBreadcrumb(p, Grammar.StringFieldDeclarations)
            let field = switch parseStringFieldDeclaration(p) {
            | Some(field) => field
            | None => assert false
            }

            /* parse comma after first */
            let () = switch p.Parser.token {
            | Rbrace | Eof => ()
            | Comma => Parser.next(p)
            | _ => Parser.expect(Comma, p)
            }

            Parser.eatBreadcrumb(p)
            switch field {
            | Parsetree.Otag(label, _, ct) => Parsetree.Otag(label, attrs, ct)
            | Oinherit(ct) => Oinherit(ct)
            }
          }

          list{
            first,
            ...parseCommaDelimitedRegion(
              ~grammar=Grammar.StringFieldDeclarations,
              ~closing=Rbrace,
              ~f=parseStringFieldDeclaration,
              p,
            ),
          }
        }

        Parser.expect(Rbrace, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let typ =
          makeBsObjType(~attrs=list{}, ~loc, ~closed=closedFlag, fields) |> parseTypeAlias(p)

        let typ = parseArrowTypeRest(~es6Arrow=true, ~startPos, typ, p)
        (Some(typ), Asttypes.Public, Parsetree.Ptype_abstract)
      | _ =>
        Parser.leaveBreadcrumb(p, Grammar.RecordDecl)
        let fields = switch attrs {
        | list{} =>
          parseCommaDelimitedRegion(
            ~grammar=Grammar.FieldDeclarations,
            ~closing=Rbrace,
            ~f=parseFieldDeclarationRegion,
            p,
          )
        | list{attr, ..._} as attrs =>
          let first = {
            let field = parseFieldDeclaration(p)
            Parser.optional(p, Comma) |> ignore
            {
              ...field,
              Parsetree.pld_attributes: attrs,
              pld_loc: {
                ...field.Parsetree.pld_loc,
                loc_start: (attr |> fst).loc.loc_start,
              },
            }
          }

          list{
            first,
            ...parseCommaDelimitedRegion(
              ~grammar=Grammar.FieldDeclarations,
              ~closing=Rbrace,
              ~f=parseFieldDeclarationRegion,
              p,
            ),
          }
        }

        let () = switch fields {
        | list{} =>
          Parser.err(~startPos, p, Diagnostics.message("A record needs at least one field"))
        | _ => ()
        }

        Parser.expect(Rbrace, p)
        Parser.eatBreadcrumb(p)
        (None, Asttypes.Public, Parsetree.Ptype_record(fields))
      }
    }
  }

  and parsePrivateEqOrRepr = p => {
    Parser.expect(Private, p)
    switch p.Parser.token {
    | Lbrace =>
      let (manifest, _, kind) = parseRecordOrBsObjectDecl(p)
      (manifest, Asttypes.Private, kind)
    | Uident(_) =>
      let (manifest, _, kind) = parseTypeEquationOrConstrDecl(p)
      (manifest, Asttypes.Private, kind)
    | Bar | DotDot =>
      let (_, kind) = parseTypeRepresentation(p)
      (None, Asttypes.Private, kind)
    | t when Grammar.isTypExprStart(t) => (
        Some(parseTypExpr(p)),
        Asttypes.Private,
        Parsetree.Ptype_abstract,
      )
    | _ =>
      let (_, kind) = parseTypeRepresentation(p)
      (None, Asttypes.Private, kind)
    }
  }

  /*
    polymorphic-variant-type	::=
                              | [ tag-spec-first  { | tag-spec } ]
                              | [> [ tag-spec ]  { | tag-spec } ]
                              | [< [|] tag-spec-full  { | tag-spec-full }  [ > { `tag-name }+ ] ]

              tag-spec-first	::=	`tag-name  [ of typexpr ]
 	                            |	[ typexpr ] |  tag-spec

                    tag-spec	::=	`tag-name  [ of typexpr ]
 	                            |	typexpr

                tag-spec-full	::=	`tag-name  [ of [&] typexpr  { & typexpr } ]
                               |	typexpr
 */
  and parsePolymorphicVariantType = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Lbracket, p)
    switch p.token {
    | GreaterThan =>
      Parser.next(p)
      let rowFields = switch p.token {
      | Rbracket => list{}
      | Bar => parseTagSpecs(p)
      | _ =>
        let rowField = parseTagSpec(p)
        list{rowField, ...parseTagSpecs(p)}
      }

      let variant = {
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Typ.variant(~attrs, ~loc, rowFields, Open, None)
      }
      Parser.expect(Rbracket, p)
      variant
    | LessThan =>
      Parser.next(p)
      Parser.optional(p, Bar) |> ignore
      let rowField = parseTagSpecFull(p)
      let rowFields = parseTagSpecFulls(p)
      let tagNames = if p.token === GreaterThan {
        Parser.next(p)
        let rec loop = p =>
          switch p.Parser.token {
          | Rbracket => list{}
          | _ =>
            let (ident, _loc) = parseHashIdent(~startPos=p.startPos, p)
            list{ident, ...loop(p)}
          }

        loop(p)
      } else {
        list{}
      }
      let variant = {
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Typ.variant(~attrs, ~loc, list{rowField, ...rowFields}, Closed, Some(tagNames))
      }
      Parser.expect(Rbracket, p)
      variant
    | _ =>
      let rowFields1 = parseTagSpecFirst(p)
      let rowFields2 = parseTagSpecs(p)
      let variant = {
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Typ.variant(~attrs, ~loc, \"@"(rowFields1, rowFields2), Closed, None)
      }
      Parser.expect(Rbracket, p)
      variant
    }
  }

  and parseTagSpecFulls = p =>
    switch p.Parser.token {
    | Rbracket => list{}
    | GreaterThan => list{}
    | Bar =>
      Parser.next(p)
      let rowField = parseTagSpecFull(p)
      list{rowField, ...parseTagSpecFulls(p)}
    | _ => list{}
    }

  and parseTagSpecFull = p => {
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Hash => parsePolymorphicVariantTypeSpecHash(~attrs, ~full=true, p)
    | _ =>
      let typ = parseTypExpr(~attrs, p)
      Parsetree.Rinherit(typ)
    }
  }

  and parseTagSpecs = p =>
    switch p.Parser.token {
    | Bar =>
      Parser.next(p)
      let rowField = parseTagSpec(p)
      list{rowField, ...parseTagSpecs(p)}
    | _ => list{}
    }

  and parseTagSpec = p => {
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Hash => parsePolymorphicVariantTypeSpecHash(~attrs, ~full=false, p)
    | _ =>
      let typ = parseTypExpr(~attrs, p)
      Parsetree.Rinherit(typ)
    }
  }

  and parseTagSpecFirst = p => {
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Bar =>
      Parser.next(p)
      list{parseTagSpec(p)}
    | Hash => list{parsePolymorphicVariantTypeSpecHash(~attrs, ~full=false, p)}
    | _ =>
      let typ = parseTypExpr(~attrs, p)
      Parser.expect(Bar, p)
      list{Parsetree.Rinherit(typ), parseTagSpec(p)}
    }
  }

  and parsePolymorphicVariantTypeSpecHash = (~attrs, ~full, p): Parsetree.row_field => {
    let startPos = p.Parser.startPos
    let (ident, loc) = parseHashIdent(~startPos, p)
    let rec loop = p =>
      switch p.Parser.token {
      | Band when full =>
        Parser.next(p)
        let rowField = parsePolymorphicVariantTypeArgs(p)
        list{rowField, ...loop(p)}
      | _ => list{}
      }

    let (firstTuple, tagContainsAConstantEmptyConstructor) = switch p.Parser.token {
    | Band when full =>
      Parser.next(p)
      (list{parsePolymorphicVariantTypeArgs(p)}, true)
    | Lparen => (list{parsePolymorphicVariantTypeArgs(p)}, false)
    | _ => (list{}, true)
    }

    let tuples = \"@"(firstTuple, loop(p))
    Parsetree.Rtag(Location.mkloc(ident, loc), attrs, tagContainsAConstantEmptyConstructor, tuples)
  }

  and parsePolymorphicVariantTypeArgs = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lparen, p)
    let args = parseCommaDelimitedRegion(
      ~grammar=Grammar.TypExprList,
      ~closing=Rparen,
      ~f=parseTypExprRegion,
      p,
    )

    Parser.expect(Rparen, p)
    let attrs = list{}
    let loc = mkLoc(startPos, p.prevEndPos)
    switch args {
    | list{{ptyp_desc: Ptyp_tuple(_)} as typ} as types =>
      if p.mode == ParseForTypeChecker {
        typ
      } else {
        Ast_helper.Typ.tuple(~loc, ~attrs, types)
      }
    | list{typ} => typ
    | types => Ast_helper.Typ.tuple(~loc, ~attrs, types)
    }
  }

  and parseTypeEquationAndRepresentation = p =>
    switch p.Parser.token {
    | (Equal | Bar) as token =>
      if token == Bar {
        Parser.expect(Equal, p)
      }
      Parser.next(p)
      switch p.Parser.token {
      | Uident(_) => parseTypeEquationOrConstrDecl(p)
      | Lbrace => parseRecordOrBsObjectDecl(p)
      | Private => parsePrivateEqOrRepr(p)
      | Bar | DotDot =>
        let (priv, kind) = parseTypeRepresentation(p)
        (None, priv, kind)
      | _ =>
        let manifest = Some(parseTypExpr(p))
        switch p.Parser.token {
        | Equal =>
          Parser.next(p)
          let (priv, kind) = parseTypeRepresentation(p)
          (manifest, priv, kind)
        | _ => (manifest, Public, Parsetree.Ptype_abstract)
        }
      }
    | _ => (None, Public, Parsetree.Ptype_abstract)
    }

  /* type-definition	::=	type [rec] typedef  { and typedef }
   * typedef	::=	typeconstr-name [type-params] type-information
   * type-information	::=	[type-equation]  [type-representation]  { type-constraint }
   * type-equation	::=	= typexpr */
  and parseTypeDef = (~attrs, ~startPos, p) => {
    Parser.leaveBreadcrumb(p, Grammar.TypeDef)
    /* let attrs = match attrs with | Some attrs -> attrs | None -> parseAttributes p in */
    Parser.leaveBreadcrumb(p, Grammar.TypeConstrName)
    let (name, loc) = parseLident(p)
    let typeConstrName = Location.mkloc(name, loc)
    Parser.eatBreadcrumb(p)
    let params = {
      let constrName = Location.mkloc(Longident.Lident(name), loc)
      parseTypeParams(~parent=constrName, p)
    }
    let typeDef = {
      let (manifest, priv, kind) = parseTypeEquationAndRepresentation(p)
      let cstrs = parseTypeConstraints(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Type.mk(~loc, ~attrs, ~priv, ~kind, ~params, ~cstrs, ~manifest?, typeConstrName)
    }

    Parser.eatBreadcrumb(p)
    typeDef
  }

  and parseTypeExtension = (~params, ~attrs, ~name, p) => {
    Parser.expect(PlusEqual, p)
    let priv = if Parser.optional(p, Token.Private) {
      Asttypes.Private
    } else {
      Asttypes.Public
    }

    let constrStart = p.Parser.startPos
    Parser.optional(p, Bar) |> ignore
    let first = {
      let (attrs, name, kind) = switch p.Parser.token {
      | Bar =>
        Parser.next(p)
        parseConstrDef(~parseAttrs=true, p)
      | _ => parseConstrDef(~parseAttrs=true, p)
      }

      let loc = mkLoc(constrStart, p.prevEndPos)
      Ast_helper.Te.constructor(~loc, ~attrs, name, kind)
    }

    let rec loop = (p, cs) =>
      switch p.Parser.token {
      | Bar =>
        let startPos = p.Parser.startPos
        Parser.next(p)
        let (attrs, name, kind) = parseConstrDef(~parseAttrs=true, p)
        let extConstr = Ast_helper.Te.constructor(
          ~attrs,
          ~loc=mkLoc(startPos, p.prevEndPos),
          name,
          kind,
        )

        loop(p, list{extConstr, ...cs})
      | _ => List.rev(cs)
      }

    let constructors = loop(p, list{first})
    Ast_helper.Te.mk(~attrs, ~params, ~priv, name, constructors)
  }

  and parseTypeDefinitions = (~attrs, ~name, ~params, ~startPos, p) => {
    let typeDef = {
      let (manifest, priv, kind) = parseTypeEquationAndRepresentation(p)
      let cstrs = parseTypeConstraints(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Type.mk(
        ~loc,
        ~attrs,
        ~priv,
        ~kind,
        ~params,
        ~cstrs,
        ~manifest?,
        {...name, txt: lidentOfPath(name.Location.txt)},
      )
    }

    let rec loop = (p, defs) => {
      let startPos = p.Parser.startPos
      let attrs = parseAttributesAndBinding(p)
      switch p.Parser.token {
      | And =>
        Parser.next(p)
        let attrs = switch p.token {
        | Export =>
          let exportLoc = mkLoc(p.startPos, p.endPos)
          Parser.next(p)
          let genTypeAttr = (Location.mkloc("genType", exportLoc), Parsetree.PStr(list{}))
          list{genTypeAttr, ...attrs}
        | _ => attrs
        }

        let typeDef = parseTypeDef(~attrs, ~startPos, p)
        loop(p, list{typeDef, ...defs})
      | _ => List.rev(defs)
      }
    }

    loop(p, list{typeDef})
  }

  /* TODO: decide if we really want type extensions (eg. type x += Blue)
   * It adds quite a bit of complexity that can be avoided,
   * implemented for now. Needed to get a feel for the complexities of
   * this territory of the grammar */
  and parseTypeDefinitionOrExtension = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Token.Typ, p)
    let recFlag = switch p.token {
    | Rec =>
      Parser.next(p)
      Asttypes.Recursive
    | Lident("nonrec") =>
      Parser.next(p)
      Asttypes.Nonrecursive
    | _ => Asttypes.Nonrecursive
    }

    let name = parseValuePath(p)
    let params = parseTypeParams(~parent=name, p)
    switch p.Parser.token {
    | PlusEqual => TypeExt(parseTypeExtension(~params, ~attrs, ~name, p))
    | _ =>
      let typeDefs = parseTypeDefinitions(~attrs, ~name, ~params, ~startPos, p)
      TypeDef({recFlag: recFlag, types: typeDefs})
    }
  }

  and parsePrimitive = p =>
    switch p.Parser.token {
    | String(s) =>
      Parser.next(p)
      Some(s)
    | _ => None
    }

  and parsePrimitives = p =>
    switch parseRegion(~grammar=Grammar.Primitive, ~f=parsePrimitive, p) {
    | list{} =>
      let msg = "An external definition should have at least one primitive. Example: \"setTimeout\""
      Parser.err(p, Diagnostics.message(msg))
      list{}
    | primitives => primitives
    }

  /* external value-name : typexp = external-declaration */
  and parseExternalDef = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.leaveBreadcrumb(p, Grammar.External)
    Parser.expect(Token.External, p)
    let (name, loc) = parseLident(p)
    let name = Location.mkloc(name, loc)
    Parser.expect(~grammar=Grammar.TypeExpression, Colon, p)
    let typExpr = parseTypExpr(p)
    Parser.expect(Equal, p)
    let prim = parsePrimitives(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    let vb = Ast_helper.Val.mk(~loc, ~attrs, ~prim, name, typExpr)
    Parser.eatBreadcrumb(p)
    vb
  }

  /* constr-def ::=
   *  | constr-decl
   *  | constr-name = constr
   *
   *  constr-decl ::= constr-name constr-args
   *  constr-name ::= uident
   *  constr      ::= path-uident */
  and parseConstrDef = (~parseAttrs, p) => {
    let attrs = if parseAttrs {
      parseAttributes(p)
    } else {
      list{}
    }
    let name = switch p.Parser.token {
    | Uident(name) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(name, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }

    let kind = switch p.Parser.token {
    | Lparen =>
      let (args, res) = parseConstrDeclArgs(p)
      Parsetree.Pext_decl(args, res)
    | Equal =>
      Parser.next(p)
      let longident = parseModuleLongIdent(~lowercase=false, p)
      Parsetree.Pext_rebind(longident)
    | _ => Parsetree.Pext_decl(Pcstr_tuple(list{}), None)
    }

    (attrs, name, kind)
  }

  /*
   * exception-definition	::=
   *  | exception constr-decl
   *  ∣	exception constr-name = constr
   *
   *  constr-name ::= uident
   *  constr ::= long_uident */
  and parseExceptionDef = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Token.Exception, p)
    let (_, name, kind) = parseConstrDef(~parseAttrs=false, p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Te.constructor(~loc, ~attrs, name, kind)
  }

  /* module structure on the file level */
  @progress((Parser.next, Parser.expect, Parser.checkProgress))
  and parseImplementation = (p): Parsetree.structure =>
    parseRegion(p, ~grammar=Grammar.Implementation, ~f=parseStructureItemRegion)

  and parseStructureItemRegion = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Open =>
      let openDescription = parseOpenDescription(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.open_(~loc, openDescription))
    | Let =>
      let (recFlag, letBindings) = parseLetBindings(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.value(~loc, recFlag, letBindings))
    | Typ =>
      Parser.beginRegion(p)
      switch parseTypeDefinitionOrExtension(~attrs, p) {
      | TypeDef({recFlag, types}) =>
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Parser.endRegion(p)
        Some(Ast_helper.Str.type_(~loc, recFlag, types))
      | TypeExt(ext) =>
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Parser.endRegion(p)
        Some(Ast_helper.Str.type_extension(~loc, ext))
      }
    | External =>
      let externalDef = parseExternalDef(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.primitive(~loc, externalDef))
    | Import =>
      let importDescr = parseJsImport(~startPos, ~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      let structureItem = JsFfi.toParsetree(importDescr)
      Some({...structureItem, pstr_loc: loc})
    | Exception =>
      let exceptionDef = parseExceptionDef(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.exception_(~loc, exceptionDef))
    | Include =>
      let includeStatement = parseIncludeStatement(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.include_(~loc, includeStatement))
    | Export =>
      let structureItem = parseJsExport(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some({...structureItem, pstr_loc: loc})
    | Module =>
      let structureItem = parseModuleOrModuleTypeImplOrPackExpr(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some({...structureItem, pstr_loc: loc})
    | AtAt =>
      let attr = parseStandaloneAttribute(p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.attribute(~loc, attr))
    | PercentPercent =>
      let extension = parseExtension(~moduleLanguage=true, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Str.extension(~attrs, ~loc, extension))
    | token when Grammar.isExprStart(token) =>
      let prevEndPos = p.Parser.endPos
      let exp = parseExpr(p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Parser.checkProgress(~prevEndPos, ~result=Ast_helper.Str.eval(~loc, ~attrs, exp), p)
    | _ => None
    }
  }

  and parseJsImport = (~startPos, ~attrs, p) => {
    Parser.expect(Token.Import, p)
    let importSpec = switch p.Parser.token {
    | Token.Lident(_) | Token.At =>
      let decl = switch parseJsFfiDeclaration(p) {
      | Some(decl) => decl
      | None => assert false
      }

      JsFfi.Default(decl)
    | _ => JsFfi.Spec(parseJsFfiDeclarations(p))
    }

    let scope = parseJsFfiScope(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    JsFfi.importDescr(~attrs, ~importSpec, ~scope, ~loc)
  }

  and parseJsExport = (~attrs, p) => {
    let exportStart = p.Parser.startPos
    Parser.expect(Token.Export, p)
    let exportLoc = mkLoc(exportStart, p.prevEndPos)
    let genTypeAttr = (Location.mkloc("genType", exportLoc), Parsetree.PStr(list{}))
    let attrs = list{genTypeAttr, ...attrs}
    switch p.Parser.token {
    | Typ =>
      switch parseTypeDefinitionOrExtension(~attrs, p) {
      | TypeDef({recFlag, types}) => Ast_helper.Str.type_(recFlag, types)
      | TypeExt(ext) => Ast_helper.Str.type_extension(ext)
      }
    /* Let */ | _ =>
      let (recFlag, letBindings) = parseLetBindings(~attrs, p)
      Ast_helper.Str.value(recFlag, letBindings)
    }
  }

  and parseJsFfiScope = p =>
    switch p.Parser.token {
    | Token.Lident("from") =>
      Parser.next(p)
      switch p.token {
      | String(s) =>
        Parser.next(p)
        JsFfi.Module(s)
      | Uident(_) | Lident(_) =>
        let value = parseIdentPath(p)
        JsFfi.Scope(value)
      | _ => JsFfi.Global
      }
    | _ => JsFfi.Global
    }

  and parseJsFfiDeclarations = p => {
    Parser.expect(Token.Lbrace, p)
    let decls = parseCommaDelimitedRegion(
      ~grammar=Grammar.JsFfiImport,
      ~closing=Rbrace,
      ~f=parseJsFfiDeclaration,
      p,
    )

    Parser.expect(Rbrace, p)
    decls
  }

  and parseJsFfiDeclaration = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Lident(_) =>
      let (ident, _) = parseLident(p)
      let alias = switch p.token {
      | As =>
        Parser.next(p)
        let (ident, _) = parseLident(p)
        ident
      | _ => ident
      }

      Parser.expect(Token.Colon, p)
      let typ = parseTypExpr(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(JsFfi.decl(~loc, ~alias, ~attrs, ~name=ident, ~typ))
    | _ => None
    }
  }

  /* include-statement ::= include module-expr */
  and parseIncludeStatement = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Token.Include, p)
    let modExpr = parseModuleExpr(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Incl.mk(~loc, ~attrs, modExpr)
  }

  and parseAtomicModuleExpr = p => {
    let startPos = p.Parser.startPos
    switch p.Parser.token {
    | Uident(_ident) =>
      let longident = parseModuleLongIdent(~lowercase=false, p)
      Ast_helper.Mod.ident(~loc=longident.loc, longident)
    | Lbrace =>
      Parser.next(p)
      let structure = Ast_helper.Mod.structure(
        parseDelimitedRegion(
          ~grammar=Grammar.Structure,
          ~closing=Rbrace,
          ~f=parseStructureItemRegion,
          p,
        ),
      )
      Parser.expect(Rbrace, p)
      let endPos = p.prevEndPos
      {...structure, pmod_loc: mkLoc(startPos, endPos)}
    | Lparen =>
      Parser.next(p)
      let modExpr = switch p.token {
      | Rparen => Ast_helper.Mod.structure(~loc=mkLoc(startPos, p.prevEndPos), list{})
      | _ => parseConstrainedModExpr(p)
      }

      Parser.expect(Rparen, p)
      modExpr
    | Lident("unpack") =>
      /* TODO: should this be made a keyword?? */
      Parser.next(p)
      Parser.expect(Lparen, p)
      let expr = parseExpr(p)
      switch p.Parser.token {
      | Colon =>
        let colonStart = p.Parser.startPos
        Parser.next(p)
        let attrs = parseAttributes(p)
        let packageType = parsePackageType(~startPos=colonStart, ~attrs, p)
        Parser.expect(Rparen, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        let constraintExpr = Ast_helper.Exp.constraint_(~loc, expr, packageType)

        Ast_helper.Mod.unpack(~loc, constraintExpr)
      | _ =>
        Parser.expect(Rparen, p)
        let loc = mkLoc(startPos, p.prevEndPos)
        Ast_helper.Mod.unpack(~loc, expr)
      }
    | Percent =>
      let extension = parseExtension(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Mod.extension(~loc, extension)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Recover.defaultModuleExpr()
    }
  }

  and parsePrimaryModExpr = p => {
    let startPos = p.Parser.startPos
    let modExpr = parseAtomicModuleExpr(p)
    let rec loop = (p, modExpr) =>
      switch p.Parser.token {
      | Lparen when p.prevEndPos.pos_lnum === p.startPos.pos_lnum =>
        loop(p, parseModuleApplication(p, modExpr))
      | _ => modExpr
      }

    let modExpr = loop(p, modExpr)
    {...modExpr, pmod_loc: mkLoc(startPos, p.prevEndPos)}
  }

  /*
   * functor-arg ::=
   *  | uident : modtype
   *  | _ : modtype
   *  | modtype           --> "punning" for _ : modtype
   *  | attributes functor-arg
   */
  and parseFunctorArg = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Uident(ident) =>
      Parser.next(p)
      let uidentEndPos = p.prevEndPos
      switch p.Parser.token {
      | Colon =>
        Parser.next(p)
        let moduleType = parseModuleType(p)
        let loc = mkLoc(startPos, uidentEndPos)
        let argName = Location.mkloc(ident, loc)
        Some(attrs, argName, Some(moduleType), startPos)
      | Dot =>
        Parser.next(p)
        let moduleType = {
          let moduleLongIdent = parseModuleLongIdentTail(
            ~lowercase=false,
            p,
            startPos,
            Longident.Lident(ident),
          )
          Ast_helper.Mty.ident(~loc=moduleLongIdent.loc, moduleLongIdent)
        }

        let argName = Location.mknoloc("_")
        Some(attrs, argName, Some(moduleType), startPos)
      | _ =>
        let loc = mkLoc(startPos, uidentEndPos)
        let modIdent = Location.mkloc(Longident.Lident(ident), loc)
        let moduleType = Ast_helper.Mty.ident(~loc, modIdent)
        let argName = Location.mknoloc("_")
        Some(attrs, argName, Some(moduleType), startPos)
      }
    | Underscore =>
      Parser.next(p)
      let argName = Location.mkloc("_", mkLoc(startPos, p.prevEndPos))
      Parser.expect(Colon, p)
      let moduleType = parseModuleType(p)
      Some(attrs, argName, Some(moduleType), startPos)
    | _ => None
    }
  }

  and parseFunctorArgs = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Lparen, p)
    let args = parseCommaDelimitedRegion(
      ~grammar=Grammar.FunctorArgs,
      ~closing=Rparen,
      ~f=parseFunctorArg,
      p,
    )

    Parser.expect(Rparen, p)
    switch args {
    | list{} => list{(list{}, Location.mkloc("*", mkLoc(startPos, p.prevEndPos)), None, startPos)}
    | args => args
    }
  }

  and parseFunctorModuleExpr = p => {
    let startPos = p.Parser.startPos
    let args = parseFunctorArgs(p)
    let returnType = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      Some(parseModuleType(~es6Arrow=false, p))
    | _ => None
    }

    Parser.expect(EqualGreater, p)
    let rhsModuleExpr = {
      let modExpr = parseModuleExpr(p)
      switch returnType {
      | Some(modType) =>
        Ast_helper.Mod.constraint_(
          ~loc=mkLoc(modExpr.pmod_loc.loc_start, modType.Parsetree.pmty_loc.loc_end),
          modExpr,
          modType,
        )
      | None => modExpr
      }
    }

    let endPos = p.prevEndPos
    let modExpr = List.fold_right(
      ((attrs, name, moduleType, startPos), acc) =>
        Ast_helper.Mod.functor_(~loc=mkLoc(startPos, endPos), ~attrs, name, moduleType, acc),
      args,
      rhsModuleExpr,
    )

    {...modExpr, pmod_loc: mkLoc(startPos, endPos)}
  }

  /* module-expr	::=
   *  | module-path
   *  ∣	{ structure-items }
   *  ∣	functorArgs =>  module-expr
   *  ∣	module-expr(module-expr)
   *  ∣	( module-expr )
   *  ∣	( module-expr : module-type )
   *  | extension
   *  | attributes module-expr */
  and parseModuleExpr = p => {
    let attrs = parseAttributes(p)
    let modExpr = if isEs6ArrowFunctor(p) {
      parseFunctorModuleExpr(p)
    } else {
      parsePrimaryModExpr(p)
    }

    {...modExpr, pmod_attributes: List.concat(list{modExpr.pmod_attributes, attrs})}
  }

  and parseConstrainedModExpr = p => {
    let modExpr = parseModuleExpr(p)
    switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      let modType = parseModuleType(p)
      let loc = mkLoc(modExpr.pmod_loc.loc_start, modType.pmty_loc.loc_end)
      Ast_helper.Mod.constraint_(~loc, modExpr, modType)
    | _ => modExpr
    }
  }

  and parseConstrainedModExprRegion = p =>
    if Grammar.isModExprStart(p.Parser.token) {
      Some(parseConstrainedModExpr(p))
    } else {
      None
    }

  and parseModuleApplication = (p, modExpr) => {
    let startPos = p.Parser.startPos
    Parser.expect(Lparen, p)
    let args = parseCommaDelimitedRegion(
      ~grammar=Grammar.ModExprList,
      ~closing=Rparen,
      ~f=parseConstrainedModExprRegion,
      p,
    )

    Parser.expect(Rparen, p)
    let args = switch args {
    | list{} =>
      let loc = mkLoc(startPos, p.prevEndPos)
      list{Ast_helper.Mod.structure(~loc, list{})}
    | args => args
    }

    List.fold_left(
      (modExpr, arg) =>
        Ast_helper.Mod.apply(
          ~loc=mkLoc(modExpr.Parsetree.pmod_loc.loc_start, arg.Parsetree.pmod_loc.loc_end),
          modExpr,
          arg,
        ),
      modExpr,
      args,
    )
  }

  and parseModuleOrModuleTypeImplOrPackExpr = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Module, p)
    switch p.Parser.token {
    | Typ => parseModuleTypeImpl(~attrs, startPos, p)
    | Lparen =>
      let expr = parseFirstClassModuleExpr(~startPos, p)
      Ast_helper.Str.eval(~attrs, expr)
    | _ => parseMaybeRecModuleBinding(~attrs, ~startPos, p)
    }
  }

  and parseModuleTypeImpl = (~attrs, startPos, p) => {
    Parser.expect(Typ, p)
    let nameStart = p.Parser.startPos
    let name = switch p.Parser.token {
    | List =>
      Parser.next(p)
      let loc = mkLoc(nameStart, p.prevEndPos)
      Location.mkloc("list", loc)
    | Lident(ident) =>
      Parser.next(p)
      let loc = mkLoc(nameStart, p.prevEndPos)
      Location.mkloc(ident, loc)
    | Uident(ident) =>
      Parser.next(p)
      let loc = mkLoc(nameStart, p.prevEndPos)
      Location.mkloc(ident, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }

    Parser.expect(Equal, p)
    let moduleType = parseModuleType(p)
    let moduleTypeDeclaration = Ast_helper.Mtd.mk(
      ~attrs,
      ~loc=mkLoc(nameStart, p.prevEndPos),
      ~typ=moduleType,
      name,
    )

    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Str.modtype(~loc, moduleTypeDeclaration)
  }

  /* definition	::=
    ∣	 module rec module-name :  module-type =  module-expr   { and module-name
    :  module-type =  module-expr } */
  and parseMaybeRecModuleBinding = (~attrs, ~startPos, p) =>
    switch p.Parser.token {
    | Token.Rec =>
      Parser.next(p)
      Ast_helper.Str.rec_module(parseModuleBindings(~startPos, ~attrs, p))
    | _ => Ast_helper.Str.module_(parseModuleBinding(~attrs, ~startPos=p.Parser.startPos, p))
    }

  and parseModuleBinding = (~attrs, ~startPos, p) => {
    let name = switch p.Parser.token {
    | Uident(ident) =>
      let startPos = p.Parser.startPos
      Parser.next(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Location.mkloc(ident, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }

    let body = parseModuleBindingBody(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Mb.mk(~attrs, ~loc, name, body)
  }

  and parseModuleBindingBody = p => {
    /* TODO: make required with good error message when rec module binding */
    let returnModType = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      Some(parseModuleType(p))
    | _ => None
    }

    Parser.expect(Equal, p)
    let modExpr = parseModuleExpr(p)
    switch returnModType {
    | Some(modType) =>
      Ast_helper.Mod.constraint_(
        ~loc=mkLoc(modType.pmty_loc.loc_start, modExpr.pmod_loc.loc_end),
        modExpr,
        modType,
      )
    | None => modExpr
    }
  }

  /* module-name :  module-type =  module-expr
   * { and module-name :  module-type =  module-expr } */
  and parseModuleBindings = (~attrs, ~startPos, p) => {
    let rec loop = (p, acc) => {
      let startPos = p.Parser.startPos
      let attrs = parseAttributesAndBinding(p)
      switch p.Parser.token {
      | And =>
        Parser.next(p)
        ignore(Parser.optional(p, Module)) /* over-parse for fault-tolerance */
        let modBinding = parseModuleBinding(~attrs, ~startPos, p)
        loop(p, list{modBinding, ...acc})
      | _ => List.rev(acc)
      }
    }

    let first = parseModuleBinding(~attrs, ~startPos, p)
    loop(p, list{first})
  }

  and parseAtomicModuleType = p => {
    let startPos = p.Parser.startPos
    let moduleType = switch p.Parser.token {
    | Uident(_) | Lident(_) | List =>
      /* Ocaml allows module types to end with lowercase: module Foo : bar = { ... }
       * lets go with uppercase terminal for now */
      let moduleLongIdent = parseModuleLongIdent(~lowercase=true, p)
      Ast_helper.Mty.ident(~loc=moduleLongIdent.loc, moduleLongIdent)
    | Lparen =>
      Parser.next(p)
      let mty = parseModuleType(p)
      Parser.expect(Rparen, p)
      {...mty, pmty_loc: mkLoc(startPos, p.prevEndPos)}
    | Lbrace =>
      Parser.next(p)
      let spec = parseDelimitedRegion(
        ~grammar=Grammar.Signature,
        ~closing=Rbrace,
        ~f=parseSignatureItemRegion,
        p,
      )

      Parser.expect(Rbrace, p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Mty.signature(~loc, spec)
    | Module =>
      /* TODO: check if this is still atomic when implementing first class modules */
      parseModuleTypeOf(p)
    | Percent =>
      let extension = parseExtension(p)
      let loc = mkLoc(startPos, p.prevEndPos)
      Ast_helper.Mty.extension(~loc, extension)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Recover.defaultModuleType()
    }

    let moduleTypeLoc = mkLoc(startPos, p.prevEndPos)
    {...moduleType, pmty_loc: moduleTypeLoc}
  }

  and parseFunctorModuleType = p => {
    let startPos = p.Parser.startPos
    let args = parseFunctorArgs(p)
    Parser.expect(EqualGreater, p)
    let rhs = parseModuleType(p)
    let endPos = p.prevEndPos
    let modType = List.fold_right(
      ((attrs, name, moduleType, startPos), acc) =>
        Ast_helper.Mty.functor_(~loc=mkLoc(startPos, endPos), ~attrs, name, moduleType, acc),
      args,
      rhs,
    )

    {...modType, pmty_loc: mkLoc(startPos, endPos)}
  }

  /* Module types are the module-level equivalent of type expressions: they
   * specify the general shape and type properties of modules.
   *
   * module-type ::=
   *  | modtype-path
   *  | { signature }
   *  | ( module-type )               --> parenthesized module-type
   *  | functor-args => module-type   --> functor
   *  | module-type => module-type    --> functor
   *  | module type of module-expr
   *  | attributes module-type
   *  | module-type with-mod-constraints
   *  | extension
   */
  and parseModuleType = (~es6Arrow=true, ~with_=true, p) => {
    let attrs = parseAttributes(p)
    let modty = if es6Arrow && isEs6ArrowFunctor(p) {
      parseFunctorModuleType(p)
    } else {
      let modty = parseAtomicModuleType(p)
      switch p.Parser.token {
      | EqualGreater when es6Arrow === true =>
        Parser.next(p)
        let rhs = parseModuleType(~with_=false, p)
        let str = Location.mknoloc("_")
        let loc = mkLoc(modty.pmty_loc.loc_start, p.prevEndPos)
        Ast_helper.Mty.functor_(~loc, str, Some(modty), rhs)
      | _ => modty
      }
    }

    let moduleType = {
      ...modty,
      pmty_attributes: List.concat(list{modty.pmty_attributes, attrs}),
    }
    if with_ {
      parseWithConstraints(moduleType, p)
    } else {
      moduleType
    }
  }

  and parseWithConstraints = (moduleType, p) =>
    switch p.Parser.token {
    | With =>
      Parser.next(p)
      let first = parseWithConstraint(p)
      let rec loop = (p, acc) =>
        switch p.Parser.token {
        | And =>
          Parser.next(p)
          loop(p, list{parseWithConstraint(p), ...acc})
        | _ => List.rev(acc)
        }

      let constraints = loop(p, list{first})
      let loc = mkLoc(moduleType.pmty_loc.loc_start, p.prevEndPos)
      Ast_helper.Mty.with_(~loc, moduleType, constraints)
    | _ => moduleType
    }

  /* mod-constraint	::=
   *  |  type typeconstr<type-params> type-equation type-constraints?
   *  ∣	 type typeconstr-name<type-params> := typexpr
   *  ∣	 module module-path = extended-module-path
   *  ∣	 module module-path :=  extended-module-path
   *
   *  TODO: split this up into multiple functions, better errors */
  and parseWithConstraint = p =>
    switch p.Parser.token {
    | Module =>
      Parser.next(p)
      let modulePath = parseModuleLongIdent(~lowercase=false, p)
      switch p.Parser.token {
      | ColonEqual =>
        Parser.next(p)
        let lident = parseModuleLongIdent(~lowercase=false, p)
        Parsetree.Pwith_modsubst(modulePath, lident)
      | Equal =>
        Parser.next(p)
        let lident = parseModuleLongIdent(~lowercase=false, p)
        Parsetree.Pwith_module(modulePath, lident)
      | token =>
        /* TODO: revisit */
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        let lident = parseModuleLongIdent(~lowercase=false, p)
        Parsetree.Pwith_modsubst(modulePath, lident)
      }
    | Typ =>
      Parser.next(p)
      let typeConstr = parseValuePath(p)
      let params = parseTypeParams(~parent=typeConstr, p)
      switch p.Parser.token {
      | ColonEqual =>
        Parser.next(p)
        let typExpr = parseTypExpr(p)
        Parsetree.Pwith_typesubst(
          typeConstr,
          Ast_helper.Type.mk(
            ~loc=typeConstr.loc,
            ~params,
            ~manifest=typExpr,
            Location.mkloc(Longident.last(typeConstr.txt), typeConstr.loc),
          ),
        )
      | Equal =>
        Parser.next(p)
        let typExpr = parseTypExpr(p)
        let typeConstraints = parseTypeConstraints(p)
        Parsetree.Pwith_type(
          typeConstr,
          Ast_helper.Type.mk(
            ~loc=typeConstr.loc,
            ~params,
            ~manifest=typExpr,
            ~cstrs=typeConstraints,
            Location.mkloc(Longident.last(typeConstr.txt), typeConstr.loc),
          ),
        )
      | token =>
        /* TODO: revisit */
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        let typExpr = parseTypExpr(p)
        let typeConstraints = parseTypeConstraints(p)
        Parsetree.Pwith_type(
          typeConstr,
          Ast_helper.Type.mk(
            ~loc=typeConstr.loc,
            ~params,
            ~manifest=typExpr,
            ~cstrs=typeConstraints,
            Location.mkloc(Longident.last(typeConstr.txt), typeConstr.loc),
          ),
        )
      }
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      exit(-1)
    } /* TODO: handle this case */

  and parseModuleTypeOf = p => {
    let startPos = p.Parser.startPos
    Parser.expect(Module, p)
    Parser.expect(Typ, p)
    Parser.expect(Of, p)
    let moduleExpr = parseModuleExpr(p)
    Ast_helper.Mty.typeof_(~loc=mkLoc(startPos, p.prevEndPos), moduleExpr)
  }

  /* module signature on the file level */
  @progress((Parser.next, Parser.expect, Parser.checkProgress))
  and parseSpecification = p =>
    parseRegion(~grammar=Grammar.Specification, ~f=parseSignatureItemRegion, p)

  and parseSignatureItemRegion = p => {
    let startPos = p.Parser.startPos
    let attrs = parseAttributes(p)
    switch p.Parser.token {
    | Let =>
      Parser.beginRegion(p)
      let valueDesc = parseSignLetDesc(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Parser.endRegion(p)
      Some(Ast_helper.Sig.value(~loc, valueDesc))
    | Typ =>
      Parser.beginRegion(p)
      switch parseTypeDefinitionOrExtension(~attrs, p) {
      | TypeDef({recFlag, types}) =>
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Parser.endRegion(p)
        Some(Ast_helper.Sig.type_(~loc, recFlag, types))
      | TypeExt(ext) =>
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Parser.endRegion(p)
        Some(Ast_helper.Sig.type_extension(~loc, ext))
      }
    | External =>
      let externalDef = parseExternalDef(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.value(~loc, externalDef))
    | Exception =>
      let exceptionDef = parseExceptionDef(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.exception_(~loc, exceptionDef))
    | Open =>
      let openDescription = parseOpenDescription(~attrs, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.open_(~loc, openDescription))
    | Include =>
      Parser.next(p)
      let moduleType = parseModuleType(p)
      let includeDescription = Ast_helper.Incl.mk(
        ~loc=mkLoc(startPos, p.prevEndPos),
        ~attrs,
        moduleType,
      )

      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.include_(~loc, includeDescription))
    | Module =>
      Parser.next(p)
      switch p.Parser.token {
      | Uident(_) =>
        let modDecl = parseModuleDeclarationOrAlias(~attrs, p)
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Sig.module_(~loc, modDecl))
      | Rec =>
        let recModule = parseRecModuleSpec(~attrs, ~startPos, p)
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Sig.rec_module(~loc, recModule))
      | Typ => Some(parseModuleTypeDeclaration(~attrs, ~startPos, p))
      | _t =>
        let modDecl = parseModuleDeclarationOrAlias(~attrs, p)
        Parser.optional(p, Semicolon) |> ignore
        let loc = mkLoc(startPos, p.prevEndPos)
        Some(Ast_helper.Sig.module_(~loc, modDecl))
      }
    | AtAt =>
      let attr = parseStandaloneAttribute(p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.attribute(~loc, attr))
    | PercentPercent =>
      let extension = parseExtension(~moduleLanguage=true, p)
      Parser.optional(p, Semicolon) |> ignore
      let loc = mkLoc(startPos, p.prevEndPos)
      Some(Ast_helper.Sig.extension(~attrs, ~loc, extension))
    | Import =>
      Parser.next(p)
      parseSignatureItemRegion(p)
    | _ => None
    }
  }

  /* module rec module-name :  module-type  { and module-name:  module-type } */
  and parseRecModuleSpec = (~attrs, ~startPos, p) => {
    Parser.expect(Rec, p)
    let rec loop = (p, spec) => {
      let startPos = p.Parser.startPos
      let attrs = parseAttributesAndBinding(p)
      switch p.Parser.token {
      | And =>
        /* TODO: give a good error message when with constraint, no parens
         * and ASet: (Set.S with type elt = A.t)
         * and BTree: (Btree.S with type elt = A.t)
         * Without parens, the `and` signals the start of another
         * `with-constraint`
         */
        Parser.expect(And, p)
        let decl = parseRecModuleDeclaration(~attrs, ~startPos, p)
        loop(p, list{decl, ...spec})
      | _ => List.rev(spec)
      }
    }

    let first = parseRecModuleDeclaration(~attrs, ~startPos, p)
    loop(p, list{first})
  }

  /* module-name : module-type */
  and parseRecModuleDeclaration = (~attrs, ~startPos, p) => {
    let name = switch p.Parser.token {
    | Uident(modName) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(modName, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }

    Parser.expect(Colon, p)
    let modType = parseModuleType(p)
    Ast_helper.Md.mk(~loc=mkLoc(startPos, p.prevEndPos), ~attrs, name, modType)
  }

  and parseModuleDeclarationOrAlias = (~attrs, p) => {
    let startPos = p.Parser.startPos
    let moduleName = switch p.Parser.token {
    | Uident(ident) =>
      let loc = mkLoc(p.Parser.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(ident, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }

    let body = switch p.Parser.token {
    | Colon =>
      Parser.next(p)
      parseModuleType(p)
    | Equal =>
      Parser.next(p)
      let lident = parseModuleLongIdent(~lowercase=false, p)
      Ast_helper.Mty.alias(lident)
    | token =>
      Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
      Recover.defaultModuleType()
    }

    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Md.mk(~loc, ~attrs, moduleName, body)
  }

  and parseModuleTypeDeclaration = (~attrs, ~startPos, p) => {
    Parser.expect(Typ, p)
    let moduleName = switch p.Parser.token {
    | Uident(ident) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(ident, loc)
    | Lident(ident) =>
      let loc = mkLoc(p.startPos, p.endPos)
      Parser.next(p)
      Location.mkloc(ident, loc)
    | t =>
      Parser.err(p, Diagnostics.uident(t))
      Location.mknoloc("_")
    }

    let typ = switch p.Parser.token {
    | Equal =>
      Parser.next(p)
      Some(parseModuleType(p))
    | _ => None
    }

    let moduleDecl = Ast_helper.Mtd.mk(~attrs, ~typ?, moduleName)
    Ast_helper.Sig.modtype(~loc=mkLoc(startPos, p.prevEndPos), moduleDecl)
  }

  and parseSignLetDesc = (~attrs, p) => {
    let startPos = p.Parser.startPos
    Parser.expect(Let, p)
    let (name, loc) = parseLident(p)
    let name = Location.mkloc(name, loc)
    Parser.expect(Colon, p)
    let typExpr = parsePolyTypeExpr(p)
    let loc = mkLoc(startPos, p.prevEndPos)
    Ast_helper.Val.mk(~loc, ~attrs, name, typExpr)
  }

  /* attr-id	::=	lowercase-ident
 	∣	  capitalized-ident
 	∣	  attr-id .  attr-id */
  and parseAttributeId = p => {
    let startPos = p.Parser.startPos
    let rec loop = (p, acc) =>
      switch p.Parser.token {
      | Lident(ident) | Uident(ident) =>
        Parser.next(p)
        let id = acc ++ ident
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          loop(p, id ++ ".")
        | _ => id
        }
      | token when Token.isKeyword(token) =>
        Parser.next(p)
        let id = acc ++ Token.toString(token)
        switch p.Parser.token {
        | Dot =>
          Parser.next(p)
          loop(p, id ++ ".")
        | _ => id
        }
      | token =>
        Parser.err(p, Diagnostics.unexpected(token, p.breadcrumbs))
        acc
      }

    let id = loop(p, "")
    let endPos = p.prevEndPos
    Location.mkloc(id, mkLoc(startPos, endPos))
  }

  /*
   * payload ::=  empty
   *          |  ( structure-item )
   *
   * TODO: what about multiple structure items?
   * @attr({let x = 1; let x = 2})
   *
   * Also what about type-expressions and specifications?
   * @attr(:myType) ???
   */
  and parsePayload = p =>
    switch p.Parser.token {
    | Lparen when p.startPos.pos_cnum == p.prevEndPos.pos_cnum =>
      Parser.next(p)
      switch p.token {
      | Colon =>
        Parser.next(p)
        let typ = parseTypExpr(p)
        Parser.expect(Rparen, p)
        Parsetree.PTyp(typ)
      | _ =>
        let items = parseDelimitedRegion(
          ~grammar=Grammar.Structure,
          ~closing=Rparen,
          ~f=parseStructureItemRegion,
          p,
        )

        Parser.expect(Rparen, p)
        Parsetree.PStr(items)
      }
    | _ => Parsetree.PStr(list{})
    }

  /* type attribute = string loc * payload */
  and parseAttribute = p =>
    switch p.Parser.token {
    | At =>
      Parser.next(p)
      let attrId = parseAttributeId(p)
      let payload = parsePayload(p)
      Some(attrId, payload)
    | _ => None
    }

  and parseAttributes = p => parseRegion(p, ~grammar=Grammar.Attribute, ~f=parseAttribute)

  /*
   * standalone-attribute ::=
   *  | @@ atribute-id
   *  | @@ attribute-id ( structure-item )
   */
  and parseStandaloneAttribute = p => {
    Parser.expect(AtAt, p)
    let attrId = parseAttributeId(p)
    let payload = parsePayload(p)
    (attrId, payload)
  }

  /* extension	::=	% attr-id  attr-payload
   *              | %% attr-id(
   *  expr	::=	 ...
   *    ∣	 extension
   *
   *  typexpr	::=	 ...
   *    ∣	 extension
   *
   *  pattern	::=	 ...
   *    ∣	 extension
   *
   *  module-expr	::=	 ...
   *    ∣	 extension
   *
   *  module-type	::=	 ...
   *    ∣	 extension
   *
   *  class-expr	::=	 ...
   *    ∣	 extension
   *
   *  class-type	::=	 ...
   *    ∣	 extension
   *
   *
   * item extension nodes usable in structures and signature
   *
   * item-extension ::= %% attr-id
   *                  | %% attr-id(structure-item)
   *
   *  attr-payload ::= structure-item
   *
   *  ~moduleLanguage represents whether we're on the module level or not
   */
  and parseExtension = (~moduleLanguage=false, p) => {
    if moduleLanguage {
      Parser.expect(PercentPercent, p)
    } else {
      Parser.expect(Percent, p)
    }
    let attrId = parseAttributeId(p)
    let payload = parsePayload(p)
    (attrId, payload)
  }
}

module OutcomePrinter: {
  open Format
  open Outcometree

  @live let out_value: ref<(formatter, out_value) => unit>
  @live let out_type: ref<(formatter, out_type) => unit>
  @live let out_class_type: ref<(formatter, out_class_type) => unit>
  @live let out_module_type: ref<(formatter, out_module_type) => unit>
  @live let out_sig_item: ref<(formatter, out_sig_item) => unit>
  @live let out_signature: ref<(formatter, list<out_sig_item>) => unit>
  @live let out_type_extension: ref<(formatter, out_type_extension) => unit>
  @live let out_phrase: ref<(formatter, out_phrase) => unit>

  @live let parenthesized_ident: string => bool
} = {
  /* Napkin doesn't have parenthesized identifiers.
   * We don't support custom operators. */
  let parenthesized_ident = _name => true

  /* TODO: better allocation strategy for the buffer */
  let escapeStringContents = s => {
    let len = String.length(s)
    let b = Buffer.create(len)
    for i in 0 to len - 1 {
      let c = (@doesNotRaise String.get)(s, i)
      if c == '\b' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 'b')
      } else if c == '\t' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 't')
      } else if c == '\n' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 'n')
      } else if c == '\r' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 'r')
      } else if c == '"' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, '"')
      } else if c == '\\' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, '\\')
      } else {
        Buffer.add_char(b, c)
      }
    }
    Buffer.contents(b)
  }

  /* let rec print_ident fmt ident = match ident with
    | Outcometree.Oide_ident s -> Format.pp_print_string fmt s
    | Oide_dot (id, s) ->
      print_ident fmt id;
      Format.pp_print_char fmt '.';
      Format.pp_print_string fmt s
    | Oide_apply (id1, id2) ->
      print_ident fmt id1;
      Format.pp_print_char fmt '(';
      print_ident fmt id2;
      Format.pp_print_char fmt ')' */

  let rec printOutIdentDoc = (ident: Outcometree.out_ident) =>
    switch ident {
    | Oide_ident(s) => Doc.text(s)
    | Oide_dot(ident, s) => Doc.concat(list{printOutIdentDoc(ident), Doc.dot, Doc.text(s)})
    | Oide_apply(call, arg) =>
      Doc.concat(list{printOutIdentDoc(call), Doc.lparen, printOutIdentDoc(arg), Doc.rparen})
    }

  let printOutAttributeDoc = (outAttribute: Outcometree.out_attribute) =>
    Doc.concat(list{Doc.text("@"), Doc.text(outAttribute.oattr_name)})

  let printOutAttributesDoc = (attrs: list<Outcometree.out_attribute>) =>
    switch attrs {
    | list{} => Doc.nil
    | attrs =>
      Doc.concat(list{
        Doc.group(Doc.join(~sep=Doc.line, List.map(printOutAttributeDoc, attrs))),
        Doc.line,
      })
    }

  let rec collectArrowArgs = (outType: Outcometree.out_type, args) =>
    switch outType {
    | Otyp_arrow(label, argType, returnType) =>
      let arg = (label, argType)
      collectArrowArgs(returnType, list{arg, ...args})
    | _ as returnType => (List.rev(args), returnType)
    }

  let rec collectFunctorArgs = (outModuleType: Outcometree.out_module_type, args) =>
    switch outModuleType {
    | Omty_functor(lbl, optModType, returnModType) =>
      let arg = (lbl, optModType)
      collectFunctorArgs(returnModType, list{arg, ...args})
    | _ => (List.rev(args), outModuleType)
    }

  let rec printOutTypeDoc = (outType: Outcometree.out_type) =>
    switch outType {
    | Otyp_abstract | Otyp_variant(_) | Otyp_open => Doc.nil
    | Otyp_alias(typ, aliasTxt) =>
      Doc.concat(list{printOutTypeDoc(typ), Doc.text(" as '"), Doc.text(aliasTxt)})
    | Otyp_constr(outIdent, list{}) => printOutIdentDoc(outIdent)
    | Otyp_manifest(typ1, typ2) =>
      Doc.concat(list{printOutTypeDoc(typ1), Doc.text(" = "), printOutTypeDoc(typ2)})
    | Otyp_record(record) => printRecordDeclarationDoc(~inline=true, record)
    | Otyp_stuff(txt) => Doc.text(txt)
    | Otyp_var(ng, s) =>
      Doc.concat(list{
        Doc.text(
          "'" ++ if ng {
            "_"
          } else {
            ""
          },
        ),
        Doc.text(s),
      })
    | Otyp_object(fields, rest) => printObjectFields(fields, rest)
    | Otyp_class(_) => Doc.nil
    | Otyp_attribute(typ, attribute) =>
      Doc.group(Doc.concat(list{printOutAttributeDoc(attribute), Doc.line, printOutTypeDoc(typ)}))
    /* example: Red | Blue | Green | CustomColour(float, float, float) */
    | Otyp_sum(constructors) => printOutConstructorsDoc(constructors)

    /* example: {"name": string, "age": int} */
    | Otyp_constr(Oide_dot(Oide_ident("Js"), "t"), list{Otyp_object(fields, rest)}) =>
      printObjectFields(fields, rest)

    /* example: node<root, 'value> */
    | Otyp_constr(outIdent, args) =>
      let argsDoc = switch args {
      | list{} => Doc.nil
      | args =>
        Doc.concat(list{
          Doc.lessThan,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(printOutTypeDoc, args)),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.greaterThan,
        })
      }

      Doc.group(Doc.concat(list{printOutIdentDoc(outIdent), argsDoc}))
    | Otyp_tuple(tupleArgs) =>
      Doc.group(
        Doc.concat(list{
          Doc.lparen,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                List.map(printOutTypeDoc, tupleArgs),
              ),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        }),
      )
    | Otyp_poly(vars, outType) =>
      Doc.group(
        Doc.concat(list{
          Doc.join(~sep=Doc.space, List.map(var => Doc.text("'" ++ var), vars)),
          printOutTypeDoc(outType),
        }),
      )
    | Otyp_arrow(_) as typ =>
      let (typArgs, typ) = collectArrowArgs(typ, list{})
      let args = Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(((lbl, typ)) =>
          if lbl == "" {
            printOutTypeDoc(typ)
          } else {
            Doc.group(Doc.concat(list{Doc.text("~" ++ (lbl ++ ": ")), printOutTypeDoc(typ)}))
          }
        , typArgs))
      let argsDoc = {
        let needsParens = switch typArgs {
        | list{(_, Otyp_tuple(_) | Otyp_arrow(_))} => true
        /* single argument should not be wrapped */
        | list{("", _)} => false
        | _ => true
        }

        if needsParens {
          Doc.group(
            Doc.concat(list{
              Doc.lparen,
              Doc.indent(Doc.concat(list{Doc.softLine, args})),
              Doc.trailingComma,
              Doc.softLine,
              Doc.rparen,
            }),
          )
        } else {
          args
        }
      }

      Doc.concat(list{argsDoc, Doc.text(" => "), printOutTypeDoc(typ)})
    | Otyp_module(_modName, _stringList, _outTypes) => Doc.nil
    }

  and printObjectFields = (fields, rest) => {
    let dots = switch rest {
    | Some(non_gen) =>
      Doc.text(
        if non_gen {
          "_"
        } else {
          ""
        } ++ "..",
      )
    | None => Doc.nil
    }

    Doc.group(
      Doc.concat(list{
        Doc.lbrace,
        dots,
        Doc.indent(
          Doc.concat(list{
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list{Doc.comma, Doc.line}),
              List.map(
                ((lbl, outType)) =>
                  Doc.group(
                    Doc.concat(list{Doc.text("\"" ++ (lbl ++ "\": ")), printOutTypeDoc(outType)}),
                  ),
                fields,
              ),
            ),
          }),
        ),
        Doc.softLine,
        Doc.trailingComma,
        Doc.rbrace,
      }),
    )
  }

  and printOutConstructorsDoc = constructors =>
    Doc.group(
      Doc.indent(Doc.concat(list{Doc.line, Doc.join(~sep=Doc.line, List.mapi((i, constructor) =>
              Doc.concat(list{
                if i > 0 {
                  Doc.text("| ")
                } else {
                  Doc.ifBreaks(Doc.text("| "), Doc.nil)
                },
                printOutConstructorDoc(constructor),
              })
            , constructors))})),
    )

  and printOutConstructorDoc = ((name, args, gadt)) => {
    let gadtDoc = switch gadt {
    | Some(outType) => Doc.concat(list{Doc.text(": "), printOutTypeDoc(outType)})
    | None => Doc.nil
    }

    let argsDoc = switch args {
    | list{} => Doc.nil
    | list{Otyp_record(record)} =>
      /* inline records
       *   | Root({
       *      mutable value: 'value,
       *      mutable updatedTime: float,
       *    })
       */
      Doc.concat(list{
        Doc.lparen,
        Doc.indent(printRecordDeclarationDoc(~inline=true, record)),
        Doc.rparen,
      })
    | _types =>
      Doc.indent(
        Doc.concat(list{
          Doc.lparen,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(printOutTypeDoc, args)),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        }),
      )
    }

    Doc.group(Doc.concat(list{Doc.text(name), argsDoc, gadtDoc}))
  }

  and printRecordDeclRowDoc = ((name, mut, arg)) =>
    Doc.group(
      Doc.concat(list{
        if mut {
          Doc.text("mutable ")
        } else {
          Doc.nil
        },
        Doc.text(name),
        Doc.text(": "),
        printOutTypeDoc(arg),
      }),
    )

  and printRecordDeclarationDoc = (~inline, rows) => {
    let content = Doc.concat(list{
      Doc.lbrace,
      Doc.indent(
        Doc.concat(list{
          Doc.softLine,
          Doc.join(
            ~sep=Doc.concat(list{Doc.comma, Doc.line}),
            List.map(printRecordDeclRowDoc, rows),
          ),
        }),
      ),
      Doc.trailingComma,
      Doc.softLine,
      Doc.rbrace,
    })
    if !inline {
      Doc.group(content)
    } else {
      content
    }
  }

  let printOutType = (fmt, outType) =>
    Format.pp_print_string(fmt, Doc.toString(~width=80, printOutTypeDoc(outType)))

  let printTypeParameterDoc = ((typ, (co, cn))) =>
    Doc.concat(list{
      if !cn {
        Doc.text("+")
      } else if !co {
        Doc.text("-")
      } else {
        Doc.nil
      },
      if typ == "_" {
        Doc.text("_")
      } else {
        Doc.text("'" ++ typ)
      },
    })

  let rec printOutSigItemDoc = (outSigItem: Outcometree.out_sig_item) =>
    switch outSigItem {
    | Osig_class(_) | Osig_class_type(_) => Doc.nil
    | Osig_ellipsis => Doc.dotdotdot
    | Osig_value(valueDecl) =>
      Doc.group(
        Doc.concat(list{
          printOutAttributesDoc(valueDecl.oval_attributes),
          Doc.text(
            switch valueDecl.oval_prims {
            | list{} => "let "
            | _ => "external "
            },
          ),
          Doc.text(valueDecl.oval_name),
          Doc.text(":"),
          Doc.space,
          printOutTypeDoc(valueDecl.oval_type),
          switch valueDecl.oval_prims {
          | list{} => Doc.nil
          | primitives =>
            Doc.indent(
              Doc.concat(list{
                Doc.text(" ="),
                Doc.line,
                Doc.group(
                  Doc.join(
                    ~sep=Doc.line,
                    List.map(prim => Doc.text("\"" ++ (prim ++ "\"")), primitives),
                  ),
                ),
              }),
            )
          },
        }),
      )
    | Osig_typext(outExtensionConstructor, _outExtStatus) =>
      printOutExtensionConstructorDoc(outExtensionConstructor)
    | Osig_modtype(modName, Omty_signature(list{})) =>
      Doc.concat(list{Doc.text("module type "), Doc.text(modName)})
    | Osig_modtype(modName, outModuleType) =>
      Doc.group(
        Doc.concat(list{
          Doc.text("module type "),
          Doc.text(modName),
          Doc.text(" = "),
          printOutModuleTypeDoc(outModuleType),
        }),
      )
    | Osig_module(modName, Omty_alias(ident), _) =>
      Doc.group(
        Doc.concat(list{
          Doc.text("module "),
          Doc.text(modName),
          Doc.text(" ="),
          Doc.line,
          printOutIdentDoc(ident),
        }),
      )
    | Osig_module(modName, outModType, outRecStatus) =>
      Doc.group(
        Doc.concat(list{
          Doc.text(
            switch outRecStatus {
            | Orec_not => "module "
            | Orec_first => "module rec "
            | Orec_next => "and"
            },
          ),
          Doc.text(modName),
          Doc.text(" = "),
          printOutModuleTypeDoc(outModType),
        }),
      )
    | Osig_type(outTypeDecl, outRecStatus) =>
      /* TODO: manifest ? */
      let attrs = switch (outTypeDecl.otype_immediate, outTypeDecl.otype_unboxed) {
      | (false, false) => Doc.nil
      | (true, false) => Doc.concat(list{Doc.text("@immediate"), Doc.line})
      | (false, true) => Doc.concat(list{Doc.text("@unboxed"), Doc.line})
      | (true, true) => Doc.concat(list{Doc.text("@immediate @unboxed"), Doc.line})
      }

      let kw = Doc.text(
        switch outRecStatus {
        | Orec_not => "type "
        | Orec_first => "type rec "
        | Orec_next => "and "
        },
      )
      let typeParams = switch outTypeDecl.otype_params {
      | list{} => Doc.nil
      | _params =>
        Doc.group(
          Doc.concat(list{
            Doc.lessThan,
            Doc.indent(
              Doc.concat(list{
                Doc.softLine,
                Doc.join(
                  ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                  List.map(printTypeParameterDoc, outTypeDecl.otype_params),
                ),
              }),
            ),
            Doc.trailingComma,
            Doc.softLine,
            Doc.greaterThan,
          }),
        )
      }

      let privateDoc = switch outTypeDecl.otype_private {
      | Asttypes.Private => Doc.text("private ")
      | Public => Doc.nil
      }

      let kind = switch outTypeDecl.otype_type {
      | Otyp_open => Doc.concat(list{Doc.text(" = "), privateDoc, Doc.text("..")})
      | Otyp_abstract => Doc.nil
      | Otyp_record(record) =>
        Doc.concat(list{
          Doc.text(" = "),
          privateDoc,
          printRecordDeclarationDoc(~inline=false, record),
        })
      | typ => Doc.concat(list{Doc.text(" = "), printOutTypeDoc(typ)})
      }

      let constraints = switch outTypeDecl.otype_cstrs {
      | list{} => Doc.nil
      | _ =>
        Doc.group(
          Doc.concat(list{
            Doc.line,
            Doc.indent(
              Doc.concat(list{
                Doc.hardLine,
                Doc.join(
                  ~sep=Doc.line,
                  List.map(
                    ((typ1, typ2)) =>
                      Doc.group(
                        Doc.concat(list{
                          Doc.text("constraint "),
                          printOutTypeDoc(typ1),
                          Doc.text(" ="),
                          Doc.indent(Doc.concat(list{Doc.line, printOutTypeDoc(typ2)})),
                        }),
                      ),
                    outTypeDecl.otype_cstrs,
                  ),
                ),
              }),
            ),
          }),
        )
      }
      Doc.group(
        Doc.concat(list{
          attrs,
          Doc.group(
            Doc.concat(list{attrs, kw, Doc.text(outTypeDecl.otype_name), typeParams, kind}),
          ),
          constraints,
        }),
      )
    }

  and printOutModuleTypeDoc = (outModType: Outcometree.out_module_type) =>
    switch outModType {
    | Omty_abstract => Doc.nil
    | Omty_ident(ident) => printOutIdentDoc(ident)
    /* example: module Increment = (M: X_int) => X_int */
    | Omty_functor(_) =>
      let (args, returnModType) = collectFunctorArgs(outModType, list{})
      let argsDoc = switch args {
      | list{(_, None)} => Doc.text("()")
      | args =>
        Doc.group(
          Doc.concat(list{
            Doc.lparen,
            Doc.indent(
              Doc.concat(list{
                Doc.softLine,
                Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(((lbl, optModType)) =>
                    Doc.group(
                      Doc.concat(list{
                        Doc.text(lbl),
                        switch optModType {
                        | None => Doc.nil
                        | Some(modType) =>
                          Doc.concat(list{Doc.text(": "), printOutModuleTypeDoc(modType)})
                        },
                      }),
                    )
                  , args)),
              }),
            ),
            Doc.trailingComma,
            Doc.softLine,
            Doc.rparen,
          }),
        )
      }

      Doc.group(Doc.concat(list{argsDoc, Doc.text(" => "), printOutModuleTypeDoc(returnModType)}))
    | Omty_signature(list{}) => Doc.nil
    | Omty_signature(signature) =>
      Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list{
          Doc.lbrace,
          Doc.indent(Doc.concat(list{Doc.line, printOutSignatureDoc(signature)})),
          Doc.softLine,
          Doc.rbrace,
        }),
      )
    | Omty_alias(_ident) => Doc.nil
    }

  and printOutSignatureDoc = (signature: list<Outcometree.out_sig_item>) => {
    let rec loop = (signature, acc) =>
      switch signature {
      | list{} => List.rev(acc)
      | list{Outcometree.Osig_typext(ext, Oext_first), ...items} =>
        /* Gather together the extension constructors */
        let rec gather_extensions = (acc, items) =>
          switch items {
          | list{Outcometree.Osig_typext(ext, Oext_next), ...items} =>
            gather_extensions(
              list{(ext.oext_name, ext.oext_args, ext.oext_ret_type), ...acc},
              items,
            )
          | _ => (List.rev(acc), items)
          }

        let (exts, items) = gather_extensions(
          list{(ext.oext_name, ext.oext_args, ext.oext_ret_type)},
          items,
        )

        let te = {
          Outcometree.otyext_name: ext.oext_type_name,
          otyext_params: ext.oext_type_params,
          otyext_constructors: exts,
          otyext_private: ext.oext_private,
        }

        let doc = printOutTypeExtensionDoc(te)
        loop(items, list{doc, ...acc})
      | list{item, ...items} =>
        let doc = printOutSigItemDoc(item)
        loop(items, list{doc, ...acc})
      }

    switch loop(signature, list{}) {
    | list{doc} => doc
    | docs => Doc.breakableGroup(~forceBreak=true, Doc.join(~sep=Doc.line, docs))
    }
  }

  and printOutExtensionConstructorDoc = (outExt: Outcometree.out_extension_constructor) => {
    let typeParams = switch outExt.oext_type_params {
    | list{} => Doc.nil
    | params =>
      Doc.group(
        Doc.concat(list{
          Doc.lessThan,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(ty =>
                  Doc.text(
                    if ty == "_" {
                      ty
                    } else {
                      "'" ++ ty
                    },
                  )
                , params)),
            }),
          ),
          Doc.softLine,
          Doc.greaterThan,
        }),
      )
    }

    Doc.group(
      Doc.concat(list{
        Doc.text("type "),
        Doc.text(outExt.oext_type_name),
        typeParams,
        Doc.text(" +="),
        Doc.line,
        if outExt.oext_private == Asttypes.Private {
          Doc.text("private ")
        } else {
          Doc.nil
        },
        printOutConstructorDoc((outExt.oext_name, outExt.oext_args, outExt.oext_ret_type)),
      }),
    )
  }

  and printOutTypeExtensionDoc = (typeExtension: Outcometree.out_type_extension) => {
    let typeParams = switch typeExtension.otyext_params {
    | list{} => Doc.nil
    | params =>
      Doc.group(
        Doc.concat(list{
          Doc.lessThan,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(~sep=Doc.concat(list{Doc.comma, Doc.line}), List.map(ty =>
                  Doc.text(
                    if ty == "_" {
                      ty
                    } else {
                      "'" ++ ty
                    },
                  )
                , params)),
            }),
          ),
          Doc.softLine,
          Doc.greaterThan,
        }),
      )
    }

    Doc.group(
      Doc.concat(list{
        Doc.text("type "),
        Doc.text(typeExtension.otyext_name),
        typeParams,
        Doc.text(" +="),
        if typeExtension.otyext_private == Asttypes.Private {
          Doc.text("private ")
        } else {
          Doc.nil
        },
        printOutConstructorsDoc(typeExtension.otyext_constructors),
      }),
    )
  }

  let printOutSigItem = (fmt, outSigItem) =>
    Format.pp_print_string(fmt, Doc.toString(~width=80, printOutSigItemDoc(outSigItem)))

  let printOutSignature = (fmt, signature) =>
    Format.pp_print_string(fmt, Doc.toString(~width=80, printOutSignatureDoc(signature)))

  let validFloatLexeme = s => {
    let l = String.length(s)
    let rec loop = i =>
      if i >= l {
        s ++ "."
      } else {
        switch @doesNotRaise
        String.get(s, i) {
        | '0' .. '9' | '-' => loop(i + 1)
        | _ => s
        }
      }
    loop(0)
  }

  let floatRepres = f =>
    switch classify_float(f) {
    | FP_nan => "nan"
    | FP_infinite =>
      if f < 0.0 {
        "neg_infinity"
      } else {
        "infinity"
      }
    | _ =>
      let float_val = {
        let s1 = Printf.sprintf("%.12g", f)
        if f == (@doesNotRaise float_of_string)(s1) {
          s1
        } else {
          let s2 = Printf.sprintf("%.15g", f)
          if f == (@doesNotRaise float_of_string)(s2) {
            s2
          } else {
            Printf.sprintf("%.18g", f)
          }
        }
      }
      validFloatLexeme(float_val)
    }

  let rec printOutValueDoc = (outValue: Outcometree.out_value) =>
    switch outValue {
    | Oval_array(outValues) =>
      Doc.group(
        Doc.concat(list{
          Doc.lbracket,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                List.map(printOutValueDoc, outValues),
              ),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rbracket,
        }),
      )
    | Oval_char(c) => Doc.text("'" ++ (Char.escaped(c) ++ "'"))
    | Oval_constr(outIdent, outValues) =>
      Doc.group(
        Doc.concat(list{
          printOutIdentDoc(outIdent),
          Doc.lparen,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                List.map(printOutValueDoc, outValues),
              ),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        }),
      )
    | Oval_ellipsis => Doc.text("...")
    | Oval_int(i) => Doc.text(Format.sprintf("%i", i))
    | Oval_int32(i) => Doc.text(Format.sprintf("%lil", i))
    | Oval_int64(i) => Doc.text(Format.sprintf("%LiL", i))
    | Oval_nativeint(i) => Doc.text(Format.sprintf("%nin", i))
    | Oval_float(f) => Doc.text(floatRepres(f))
    | Oval_list(outValues) =>
      Doc.group(
        Doc.concat(list{
          Doc.text("list["),
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                List.map(printOutValueDoc, outValues),
              ),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rbracket,
        }),
      )
    | Oval_printer(fn) =>
      let fmt = Format.str_formatter
      fn(fmt)
      let str = Format.flush_str_formatter()
      Doc.text(str)
    | Oval_record(rows) =>
      Doc.group(
        Doc.concat(list{
          Doc.lparen,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                List.map(
                  ((outIdent, outValue)) =>
                    Doc.group(
                      Doc.concat(list{
                        printOutIdentDoc(outIdent),
                        Doc.text(": "),
                        printOutValueDoc(outValue),
                      }),
                    ),
                  rows,
                ),
              ),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        }),
      )
    | Oval_string(txt, _sizeToPrint, _kind) => Doc.text(escapeStringContents(txt))
    | Oval_stuff(txt) => Doc.text(txt)
    | Oval_tuple(outValues) =>
      Doc.group(
        Doc.concat(list{
          Doc.lparen,
          Doc.indent(
            Doc.concat(list{
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list{Doc.comma, Doc.line}),
                List.map(printOutValueDoc, outValues),
              ),
            }),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        }),
      )
    /* Not supported by NapkinScript */
    | Oval_variant(_) => Doc.nil
    }

  let printOutExceptionDoc = (exc, outValue) =>
    switch exc {
    | Sys.Break => Doc.text("Interrupted.")
    | Out_of_memory => Doc.text("Out of memory during evaluation.")
    | Stack_overflow => Doc.text("Stack overflow during evaluation (looping recursion?).")
    | _ =>
      Doc.group(
        Doc.indent(Doc.concat(list{Doc.text("Exception:"), Doc.line, printOutValueDoc(outValue)})),
      )
    }

  let printOutPhraseSignature = signature => {
    let rec loop = (signature, acc) =>
      switch signature {
      | list{} => List.rev(acc)
      | list{(Outcometree.Osig_typext(ext, Oext_first), None), ...signature} =>
        /* Gather together extension constructors */
        let rec gather_extensions = (acc, items) =>
          switch items {
          | list{(Outcometree.Osig_typext(ext, Oext_next), None), ...items} =>
            gather_extensions(
              list{(ext.oext_name, ext.oext_args, ext.oext_ret_type), ...acc},
              items,
            )
          | _ => (List.rev(acc), items)
          }

        let (exts, signature) = gather_extensions(
          list{(ext.oext_name, ext.oext_args, ext.oext_ret_type)},
          signature,
        )

        let te = {
          Outcometree.otyext_name: ext.oext_type_name,
          otyext_params: ext.oext_type_params,
          otyext_constructors: exts,
          otyext_private: ext.oext_private,
        }

        let doc = printOutTypeExtensionDoc(te)
        loop(signature, list{doc, ...acc})
      | list{(sigItem, optOutValue), ...signature} =>
        let doc = switch optOutValue {
        | None => printOutSigItemDoc(sigItem)
        | Some(outValue) =>
          Doc.group(
            Doc.concat(list{
              printOutSigItemDoc(sigItem),
              Doc.text(" = "),
              printOutValueDoc(outValue),
            }),
          )
        }

        loop(signature, list{doc, ...acc})
      }

    Doc.breakableGroup(~forceBreak=true, Doc.join(~sep=Doc.line, loop(signature, list{})))
  }

  let printOutPhraseDoc = (outPhrase: Outcometree.out_phrase) =>
    switch outPhrase {
    | Ophr_eval(outValue, outType) =>
      Doc.group(
        Doc.concat(list{
          Doc.text("- : "),
          printOutTypeDoc(outType),
          Doc.text(" ="),
          Doc.indent(Doc.concat(list{Doc.line, printOutValueDoc(outValue)})),
        }),
      )
    | Ophr_signature(list{}) => Doc.nil
    | Ophr_signature(signature) => printOutPhraseSignature(signature)
    | Ophr_exception(exc, outValue) => printOutExceptionDoc(exc, outValue)
    }

  let printOutPhase = (fmt, outPhrase) =>
    Format.pp_print_string(fmt, Doc.toString(~width=80, printOutPhraseDoc(outPhrase)))

  let printOutModuleType = (fmt, outModuleType) =>
    Format.pp_print_string(fmt, Doc.toString(~width=80, printOutModuleTypeDoc(outModuleType)))

  let printOutTypeExtension = (fmt, typeExtension) =>
    Format.pp_print_string(fmt, Doc.toString(~width=80, printOutTypeExtensionDoc(typeExtension)))

  let printOutValue = (fmt, outValue) =>
    Format.pp_print_string(fmt, Doc.toString(~width=80, printOutValueDoc(outValue)))

  /* Not supported in Napkin */
  let printOutClassType = (_fmt, _) => ()

  let out_value = ref(printOutValue)
  let out_type = ref(printOutType)
  let out_module_type = ref(printOutModuleType)
  let out_sig_item = ref(printOutSigItem)
  let out_signature = ref(printOutSignature)
  let out_type_extension = ref(printOutTypeExtension)
  let out_phrase = @live ref(printOutPhase)
  let out_class_type = ref(printOutClassType)
}

module Repl = {
  let parseToplevelPhrase = filename => {
    let src = IO.readFile(filename)
    let p = Parser.make(src, filename)
    Parsetree.Ptop_def(NapkinScript.parseImplementation(p))
  }

  let typeAndPrintOutcome = filename => {
    Compmisc.init_path(false)
    let env = Compmisc.initial_env()
    try {
      let sstr = switch parseToplevelPhrase(filename) {
      | Parsetree.Ptop_def(sstr) => sstr
      | _ => assert false
      }

      let (_str, signature, _newenv) = Typemod.type_toplevel_phrase(env, sstr)
      let outSigItems = Printtyp.tree_of_signature(signature)
      let fmt = Format.str_formatter
      OutcomePrinter.out_signature.contents(fmt, outSigItems)
      let result = Format.flush_str_formatter()
      print_string(result)
    } catch {
    | Typetexp.Error(_, _, err) =>
      let fmt = Format.str_formatter
      Typetexp.report_error(env, fmt, err)
      let result = Format.flush_str_formatter()
      let () = print_endline(result)
    | _ => print_endline("catch all")
    }
  }
}

/* command line flags */
module Clflags: {
  let recover: ref<bool>
  let print: ref<string>
  let width: ref<int>
  let origin: ref<string>
  let files: ref<list<string>>
  let interface: ref<bool>
  let report: ref<string>

  let parse: unit => unit
  let outcome: ref<bool>
} = {
  let recover = ref(false)
  let width = ref(100)

  let files = ref(list{})
  let addFilename = filename => files := list{filename, ...files.contents}

  let print = ref("")
  let outcome = ref(false)
  let origin = ref("")
  let interface = ref(false)
  let report = ref("pretty")

  let usage = "Usage: napkinscript <options> <file>\nOptions are:"

  let spec = list{
    ("-recover", Arg.Unit(() => recover := true), "Emit partial ast"),
    ("-print", Arg.String(txt => print := txt), "Print either binary, ocaml or ast"),
    ("-parse", Arg.String(txt => origin := txt), "Parse ocaml or napkinscript"),
    ("-outcome", Arg.Bool(printOutcomeTree => outcome := printOutcomeTree), "print outcometree"),
    ("-width", Arg.Int(w => width := w), "Specify the line length that the printer will wrap on"),
    ("-interface", Arg.Unit(() => interface := true), "Parse as interface"),
    (
      "-report",
      Arg.String(txt => report := txt),
      "Stylize errors and messages using color and context. Accepts `Pretty` and `Plain`. Default `Plain`",
    ),
  }

  let parse = () => Arg.parse(spec, addFilename, usage)
}

module Driver: {
  let processFile: (
    ~isInterface: bool,
    ~width: int,
    ~recover: bool,
    ~origin: string,
    ~target: string,
    ~report: string,
    string,
  ) => unit
} = {
  type rec file_kind<'a> =
    | Structure: file_kind<Parsetree.structure>
    | Signature: file_kind<Parsetree.signature>

  let parseNapkin = (type a, kind: file_kind<a>, p): a =>
    switch kind {
    | Structure => NapkinScript.parseImplementation(p)
    | Signature => NapkinScript.parseSpecification(p)
    }

  let extractOcamlStringData = filename => {
    let lexbuf = if String.length(filename) > 0 {
      IO.readFile(filename) |> Lexing.from_string
    } else {
      Lexing.from_channel(stdin)
    }

    let stringLocs = ref(list{})
    let rec next = () => {
      let token = Lexer.token_with_comments(lexbuf)
      switch token {
      | OcamlParser.STRING(_txt, None) =>
        open Location
        let loc = {
          loc_start: lexbuf.lex_start_p,
          loc_end: lexbuf.Lexing.lex_curr_p,
          loc_ghost: false,
        }
        let len = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum
        let txt = Bytes.to_string(
          (@doesNotRaise Bytes.sub)(lexbuf.Lexing.lex_buffer, loc.loc_start.pos_cnum, len),
        )
        stringLocs := list{(txt, loc), ...stringLocs.contents}
        next()
      | OcamlParser.EOF => ()
      | _ => next()
      }
    }

    next()
    List.rev(stringLocs.contents)
  }

  let parseOcaml = (type a, kind: file_kind<a>, filename): a => {
    let lexbuf = if String.length(filename) > 0 {
      IO.readFile(filename) |> Lexing.from_string
    } else {
      Lexing.from_channel(stdin)
    }

    let stringData = extractOcamlStringData(filename)
    switch kind {
    | Structure =>
      Parse.implementation(lexbuf)
      |> ParsetreeCompatibility.replaceStringLiteralStructure(stringData)
      |> ParsetreeCompatibility.structure
    | Signature =>
      Parse.interface(lexbuf)
      |> ParsetreeCompatibility.replaceStringLiteralSignature(stringData)
      |> ParsetreeCompatibility.signature
    }
  }

  let parseNapkinFile = (~destination, kind, filename) => {
    let src = if String.length(filename) > 0 {
      IO.readFile(filename)
    } else {
      IO.readStdin()
    }

    let p = {
      let mode = switch destination {
      | "napkinscript" | "ns" | "sexp" => Parser.Default
      | _ => Parser.ParseForTypeChecker
      }

      Parser.make(~mode, src, filename)
    }
    let ast = parseNapkin(kind, p)
    let report = switch p.diagnostics {
    | list{} => None
    | diagnostics => Some(diagnostics)
    }

    (ast, report, p)
  }

  let parseOcamlFile = (kind, filename) => {
    let ast = parseOcaml(kind, filename)
    let lexbuf2 = if String.length(filename) > 0 {
      IO.readFile(filename) |> Lexing.from_string
    } else {
      Lexing.from_channel(stdin)
    }

    let comments = {
      let rec next = (prevTokEndPos: Lexing.position, comments, lb) => {
        let token = Lexer.token_with_comments(lb)
        switch token {
        | OcamlParser.EOF => comments
        | OcamlParser.COMMENT(txt, loc) =>
          let comment = Comment.fromOcamlComment(~loc, ~prevTokEndPos, ~txt)

          next(loc.Location.loc_end, list{comment, ...comments}, lb)
        | _ => next(lb.Lexing.lex_curr_p, comments, lb)
        }
      }

      let cmts = next(lexbuf2.Lexing.lex_start_p, list{}, lexbuf2)
      cmts
    }

    let p = Parser.make("", filename)
    p.comments = comments
    (ast, None, p)
  }

  let reasonFilename = ref("")
  let commentData = ref(list{})
  let stringData = ref(list{})

  let parseReasonBinaryFromStdin = (type a, kind: file_kind<a>, filename): a => {
    let (chan, close) =
      String.length(filename) === 0
        ? (stdin, _ => ())
        : {
            let file_chan = open_in_bin(filename)
            seek_in(file_chan, 0)
            (file_chan, close_in_noerr)
          }

    let ic = chan
    let magic = switch kind {
    | Structure => Config.ast_impl_magic_number
    | Signature => Config.ast_intf_magic_number
    }

    let buffer = (@doesNotRaise really_input_string)(ic, String.length(magic))
    assert (buffer == magic)
    let filename = input_value(ic)
    reasonFilename := filename
    let ast = input_value(ic)
    close(chan)

    let src = if String.length(filename) > 0 {
      IO.readFile(filename)
    } else {
      IO.readStdin()
    }

    let scanner = Scanner.make(Bytes.of_string(src), filename)

    let rec next = (prevEndPos, scanner) => {
      let (startPos, endPos, token) = Scanner.scan(scanner)
      switch token {
      | Eof => ()
      | Comment(c) =>
        Comment.setPrevTokEndPos(c, prevEndPos)
        commentData := list{c, ...commentData.contents}
        next(endPos, scanner)
      | String(_) =>
        let loc = {Location.loc_start: startPos, loc_end: endPos, loc_ghost: false}
        let len = endPos.pos_cnum - startPos.pos_cnum
        let txt = (@doesNotRaise String.sub)(src, startPos.pos_cnum, len)
        stringData := list{(txt, loc), ...stringData.contents}
        next(endPos, scanner)
      | _ => next(endPos, scanner)
      }
    }

    next(Lexing.dummy_pos, scanner)

    switch kind {
    | Structure =>
      ast
      |> ParsetreeCompatibility.replaceStringLiteralStructure(stringData.contents)
      |> ParsetreeCompatibility.normalizeReasonArityStructure(~forPrinter=true)
      |> ParsetreeCompatibility.structure
    | Signature =>
      ast
      |> ParsetreeCompatibility.replaceStringLiteralSignature(stringData.contents)
      |> ParsetreeCompatibility.normalizeReasonAritySignature(~forPrinter=true)
      |> ParsetreeCompatibility.signature
    }
  }

  let isReasonDocComment = (comment: Comment.t) => {
    let content = Comment.txt(comment)
    let len = String.length(content)
    if len == 0 {
      true
    } else if (
      len >= 2 && (String.unsafe_get(content, 0) == '*' && String.unsafe_get(content, 1) == '*')
    ) {
      false
    } else if len >= 1 && String.unsafe_get(content, 0) == '*' {
      true
    } else {
      false
    }
  }

  let parseReasonBinary = (kind, filename) => {
    let ast = parseReasonBinaryFromStdin(kind, filename)
    let p = Parser.make("", reasonFilename.contents)
    p.comments = List.filter(c => !isReasonDocComment(c), commentData.contents)
    (ast, None, p)
  }

  let parseImplementation = (~origin, ~destination, filename) =>
    switch origin {
    | "ml" | "ocaml" => parseOcamlFile(Structure, filename)
    | "reasonBinary" => parseReasonBinary(Structure, filename)
    | _ => parseNapkinFile(~destination, Structure, filename)
    }

  let parseInterface = (~destination, ~origin, filename) =>
    switch origin {
    | "ml" | "ocaml" => parseOcamlFile(Signature, filename)
    | "reasonBinary" => parseReasonBinary(Signature, filename)
    | _ => parseNapkinFile(~destination, Signature, filename)
    }

  let process = (~reportStyle, parseFn, printFn, recover, filename) => {
    let (ast, report, parserState) = parseFn(filename)
    switch report {
    | Some(report) when recover == true =>
      printFn(ast, parserState)
      prerr_string(
        Diagnostics.stringOfReport(
          ~style=Diagnostics.parseReportStyle(reportStyle),
          report,
          Bytes.to_string(parserState.Parser.scanner.src),
        ),
      )
    | Some(report) =>
      prerr_string(
        Diagnostics.stringOfReport(
          ~style=Diagnostics.parseReportStyle(reportStyle),
          report,
          Bytes.to_string(parserState.Parser.scanner.src),
        ),
      )
      exit(1)
    | None => printFn(ast, parserState)
    }
  }

  type action =
    | ProcessImplementation
    | ProcessInterface

  let printImplementation = (~target, ~width, filename, ast, _parserState) =>
    switch target {
    | "ml" | "ocaml" => Pprintast.structure(Format.std_formatter, ast)
    | "ns" | "napkinscript" =>
      Printer.printImplementation(~width, ast, List.rev(_parserState.Parser.comments))
    | "ast" => Printast.implementation(Format.std_formatter, ast)
    | "sexp" => ast |> SexpAst.implementation |> Sexp.toString |> print_string
    | _ =>
      /* default binary */
      output_string(stdout, Config.ast_impl_magic_number)
      output_value(stdout, filename)
      output_value(stdout, ast)
    }

  let printInterface = (~target, ~width, filename, ast, _parserState) =>
    switch target {
    | "ml" | "ocaml" => Pprintast.signature(Format.std_formatter, ast)
    | "ns" | "napkinscript" =>
      Printer.printInterface(~width, ast, List.rev(_parserState.Parser.comments))
    | "ast" => Printast.interface(Format.std_formatter, ast)
    | "sexp" => ast |> SexpAst.interface |> Sexp.toString |> print_string
    | _ =>
      /* default binary */
      output_string(stdout, Config.ast_intf_magic_number)
      output_value(stdout, filename)
      output_value(stdout, ast)
    }

  let processFile = (~isInterface, ~width, ~recover, ~origin, ~target, ~report, filename) =>
    try {
      let len = String.length(filename)
      let action = if (
        isInterface || (len > 0 && (@doesNotRaise String.get)(filename, len - 1) == 'i')
      ) {
        ProcessInterface
      } else {
        ProcessImplementation
      }

      switch action {
      | ProcessImplementation =>
        process(
          ~reportStyle=report,
          parseImplementation(~origin, ~destination=target),
          printImplementation(~target, ~width, filename),
          recover,
          filename,
        )
      | ProcessInterface =>
        process(
          ~reportStyle=report,
          parseInterface(~origin, ~destination=target),
          printInterface(~target, ~width, filename),
          recover,
          filename,
        )
      }
    } catch {
    | Failure(txt) =>
      prerr_string(txt)
      prerr_newline()
      exit(1)
    | _ => exit(1)
    }
}

let () = {
  Clflags.parse()
  if Clflags.outcome.contents {
    Repl.typeAndPrintOutcome(List.hd(Clflags.files.contents))
  } else {
    let () = switch Clflags.files.contents {
    | list{_file, ..._} as files =>
      List.iter(
        filename =>
          Driver.processFile(
            ~isInterface=Clflags.interface.contents,
            ~width=Clflags.width.contents,
            ~recover=Clflags.recover.contents,
            ~target=Clflags.print.contents,
            ~origin=Clflags.origin.contents,
            ~report=Clflags.report.contents,
            filename,
          ),
        files,
      )
    | list{} =>
      Driver.processFile(
        ~isInterface=Clflags.interface.contents,
        ~width=Clflags.width.contents,
        ~recover=Clflags.recover.contents,
        ~target=Clflags.print.contents,
        ~origin=Clflags.origin.contents,
        ~report=Clflags.report.contents,
        "",
      )
    }

    exit(0)
  }
}
