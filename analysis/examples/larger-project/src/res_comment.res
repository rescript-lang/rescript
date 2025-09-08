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
    let i = ref(0)
    while i.contents < len && String.unsafe_get(s, i.contents) == ' ' {
      incr(i)
    }
    let j = ref(len - 1)
    while j.contents >= i.contents && String.unsafe_get(s, j.contents) == ' ' {
      decr(j)
    }
    if j.contents >= i.contents {
      (@doesNotRaise String.sub)(s, i.contents, j.contents - i.contents + 1)
    } else {
      ""
    }
  } else {
    s
  }
}
