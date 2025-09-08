module MiniBuffer = Res_minibuffer

type mode = Break | Flat

type lineStyle =
  | Classic /* fits? -> replace with space */
  | Soft /* fits? -> replaced with nothing */
  | Hard /* always included, forces breaks in parents */
  /* always included, forces breaks in parents, but doesn't increase indentation
   use case: template literals, multiline string content */
  | Literal

type rec t =
  | Nil
  | Text(string)
  | Concat(list<t>)
  | Indent(t)
  | IfBreaks({
      yes: t,
      no: t,
      mutable broken: bool,
    }) /* when broken is true, treat as the yes branch */
  | LineSuffix(t)
  | LineBreak(lineStyle)
  | Group({mutable shouldBreak: bool, doc: t})
  | CustomLayout(list<t>)
  | BreakParent

let nil = Nil
let line = LineBreak(Classic)
let hardLine = LineBreak(Hard)
let softLine = LineBreak(Soft)
let literalLine = LineBreak(Literal)
let text = s => Text(s)

/* Optimization. We eagerly collapse and reduce whatever allocation we can */
let rec _concat = (acc, l) =>
  switch l {
  | list{Text(s1), Text(s2), ...rest} => list{Text(s1 ++ s2), ..._concat(acc, rest)}
  | list{Nil, ...rest} => _concat(acc, rest)
  | list{Concat(l2), ...rest} => _concat(_concat(acc, rest), l2) /* notice the order here */
  | list{x, ...rest} =>
    let rest1 = _concat(acc, rest)
    if rest1 === rest {
      l
    } else {
      list{x, ...rest1}
    }
  | list{} => acc
  }

let concat = l => Concat(_concat(list{}, l))

let indent = d => Indent(d)
let ifBreaks = (t, f) => IfBreaks({yes: t, no: f, broken: false})
let lineSuffix = d => LineSuffix(d)
let group = d => Group({shouldBreak: false, doc: d})
let breakableGroup = (~forceBreak, d) => Group({shouldBreak: forceBreak, doc: d})
let customLayout = gs => CustomLayout(gs)
let breakParent = BreakParent

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
let trailingComma = ifBreaks(comma, nil)
let doubleQuote = Text("\"")

let propagateForcedBreaks = doc => {
  let rec walk = doc =>
    switch doc {
    | Text(_) | Nil | LineSuffix(_) => false
    | BreakParent => true
    | LineBreak(Hard | Literal) => true
    | LineBreak(Classic | Soft) => false
    | Indent(children) =>
      let childForcesBreak = walk(children)
      childForcesBreak
    | IfBreaks({yes: trueDoc, no: falseDoc} as ib) =>
      let falseForceBreak = walk(falseDoc)
      if falseForceBreak {
        let _ = walk(trueDoc)
        ib.broken = true
        true
      } else {
        let forceBreak = walk(trueDoc)
        forceBreak
      }
    | Group({shouldBreak: forceBreak, doc: children} as gr) =>
      let childForcesBreak = walk(children)
      let shouldBreak = forceBreak || childForcesBreak
      gr.shouldBreak = shouldBreak
      shouldBreak
    | Concat(children) => List.fold_left((forceBreak, child) => {
        let childForcesBreak = walk(child)
        forceBreak || childForcesBreak
      }, false, children)
    | CustomLayout(children) =>
      /* When using CustomLayout, we don't want to propagate forced breaks
       * from the children up. By definition it picks the first layout that fits
       * otherwise it takes the last of the list.
       * However we do want to propagate forced breaks in the sublayouts. They
       * might need to be broken. We just don't propagate them any higher here */
      let _ = walk(Concat(children))
      false
    }

  let _ = walk(doc)
}

/* See documentation in interface file */
let rec willBreak = doc =>
  switch doc {
  | LineBreak(Hard | Literal) | BreakParent | Group({shouldBreak: true}) => true
  | Group({doc}) | Indent(doc) | CustomLayout(list{doc, ..._}) => willBreak(doc)
  | Concat(docs) => List.exists(willBreak, docs)
  | IfBreaks({yes, no}) => willBreak(yes) || willBreak(no)
  | _ => false
  }

let join = (~sep, docs) => {
  let rec loop = (acc, sep, docs) =>
    switch docs {
    | list{} => List.rev(acc)
    | list{x} => List.rev(list{x, ...acc})
    | list{x, ...xs} => loop(list{sep, x, ...acc}, sep, xs)
    }

  concat(loop(list{}, sep, docs))
}

let fits = (w, stack) => {
  let width = ref(w)
  let result = ref(None)

  let rec calculate = (indent, mode, doc) =>
    switch (mode, doc) {
    | _ if result.contents !== None => ()
    | _ if width.contents < 0 => result := Some(false)
    | (_, Nil)
    | (_, LineSuffix(_))
    | (_, BreakParent) => ()
    | (_, Text(txt)) => width := width.contents - String.length(txt)
    | (_, Indent(doc)) => calculate(indent + 2, mode, doc)
    | (Flat, LineBreak(Hard))
    | (Flat, LineBreak(Literal)) =>
      result := Some(true)
    | (Flat, LineBreak(Classic)) => width := width.contents - 1
    | (Flat, LineBreak(Soft)) => ()
    | (Break, LineBreak(_)) => result := Some(true)
    | (_, Group({shouldBreak: true, doc})) => calculate(indent, Break, doc)
    | (_, Group({doc})) => calculate(indent, mode, doc)
    | (_, IfBreaks({yes: breakDoc, broken: true})) => calculate(indent, mode, breakDoc)
    | (Break, IfBreaks({yes: breakDoc})) => calculate(indent, mode, breakDoc)
    | (Flat, IfBreaks({no: flatDoc})) => calculate(indent, mode, flatDoc)
    | (_, Concat(docs)) => calculateConcat(indent, mode, docs)
    | (_, CustomLayout(list{hd, ..._})) =>
      /* TODO: if we have nested custom layouts, what we should do here? */
      calculate(indent, mode, hd)
    | (_, CustomLayout(list{})) => ()
    }
  and calculateConcat = (indent, mode, docs) =>
    if result.contents === None {
      switch docs {
      | list{} => ()
      | list{doc, ...rest} =>
        calculate(indent, mode, doc)
        calculateConcat(indent, mode, rest)
      }
    }

  let rec calculateAll = stack =>
    switch (result.contents, stack) {
    | (Some(r), _) => r
    | (None, list{}) => width.contents >= 0
    | (None, list{(indent, mode, doc), ...rest}) =>
      calculate(indent, mode, doc)
      calculateAll(rest)
    }

  calculateAll(stack)
}

let toString = (~width, doc) => {
  propagateForcedBreaks(doc)
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
      | IfBreaks({yes: breakDoc, broken: true}) =>
        process(~pos, lineSuffices, list{(ind, mode, breakDoc), ...rest})
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
            if lineStyle == Literal {
              MiniBuffer.add_char(buffer, '\n')
              process(~pos=0, list{}, rest)
            } else {
              MiniBuffer.flush_newline(buffer)
              MiniBuffer.add_string(buffer, @doesNotRaise String.make(ind, ' '))
              process(~pos=ind, list{}, rest)
            }
          | _docs =>
            process(~pos=ind, list{}, List.concat(list{List.rev(lineSuffices), list{cmd, ...rest}}))
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
          | Literal =>
            MiniBuffer.add_char(buffer, '\n')
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
  MiniBuffer.contents(buffer)
}

@live
let debug = t => {
  let rec toDoc = x =>
    switch x {
    | Nil => text("nil")
    | BreakParent => text("breakparent")
    | Text(txt) => text("text(\"" ++ (txt ++ "\")"))
    | LineSuffix(doc) =>
      group(
        concat(list{text("linesuffix("), indent(concat(list{line, toDoc(doc)})), line, text(")")}),
      )
    | Concat(list{}) => text("concat()")
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
    | IfBreaks({yes: trueDoc, broken: true}) => toDoc(trueDoc)
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
      | Literal => "Liteal"
      }

      text("LineBreak(" ++ (breakTxt ++ ")"))
    | Group({shouldBreak, doc}) =>
      group(
        concat(list{
          text("Group("),
          indent(
            concat(list{
              line,
              text("{shouldBreak: " ++ (string_of_bool(shouldBreak) ++ "}")),
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
