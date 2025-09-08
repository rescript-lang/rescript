module Scanner = Res_scanner
module Diagnostics = Res_diagnostics
module Token = Res_token
module Grammar = Res_grammar
module Reporting = Res_reporting

module Comment = Res_comment

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

let err = (~startPos=?, ~endPos=?, p, error) =>
  switch p.regions {
  | list{{contents: Report} as region, ..._} =>
    let d = Diagnostics.make(
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

    p.diagnostics = list{d, ...p.diagnostics}
    region := Silent
  | _ => ()
  }

let beginRegion = p => p.regions = list{ref(Report), ...p.regions}
let endRegion = p =>
  switch p.regions {
  | list{} => ()
  | list{_, ...rest} => p.regions = rest
  }

/* Advance to the next non-comment token and store any encountered comment
 * in the parser's state. Every comment contains the end position of its
 * previous token to facilite comment interleaving */
let rec next = (~prevEndPos=?, p) => {
  if p.token == Eof {
    assert false
  }
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

let nextUnsafe = p =>
  if p.token != Eof {
    next(p)
  }

let nextTemplateLiteralToken = p => {
  let (startPos, endPos, token) = Scanner.scanTemplateLiteralToken(p.scanner)
  p.token = token
  p.prevEndPos = p.endPos
  p.startPos = startPos
  p.endPos = endPos
}

let checkProgress = (~prevEndPos, ~result, p) =>
  if p.endPos === prevEndPos {
    None
  } else {
    Some(result)
  }

let make = (~mode=ParseForTypeChecker, src, filename) => {
  let scanner = Scanner.make(~filename, src)
  let parserState = {
    mode: mode,
    scanner: scanner,
    token: Token.Semicolon,
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
    let diagnostic = Diagnostics.make(~startPos, ~endPos, error)

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

