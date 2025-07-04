open P

module Diagnostics = Res_diagnostics
module Token = Res_token
module Comment = Res_comment

type mode = Jsx | Diamond

/* We hide the implementation detail of the scanner reading character. Our char
will also contain the special -1 value to indicate end-of-file. This isn't
ideal; we should clean this up */
let hackyEOFChar = Char.unsafe_chr(-1)
type charEncoding = Char.t

type t = {
  filename: string,
  src: string,
  mutable err: (~startPos: Lexing.position, ~endPos: Lexing.position, Diagnostics.category) => unit,
  mutable ch: charEncoding /* current character */,
  mutable offset: int /* character offset */,
  mutable lineOffset: int /* current line offset */,
  mutable lnum: int /* current line number */,
  mutable mode: list<mode>,
}

let setDiamondMode = scanner => scanner.mode = list{Diamond, ...scanner.mode}

let setJsxMode = scanner => scanner.mode = list{Jsx, ...scanner.mode}

let popMode = (scanner, mode) =>
  switch scanner.mode {
  | list{m, ...ms} if m == mode => scanner.mode = ms
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

/* Small debugging util
❯ echo 'let msg = "hello"' | ./lib/rescript.exe
let msg = "hello"
^-^ let 0-3
let msg = "hello"
    ^-^ msg 4-7
let msg = "hello"
        ^ = 8-9
let msg = "hello"
          ^-----^ string "hello" 10-17
let msg = "hello"
                  ^ eof 18-18
let msg = "hello"
*/
@live
let _printDebug = (~startPos, ~endPos, scanner, token) => {
  open Lexing
  print_string(scanner.src)
  print_string((@doesNotRaise String.make)(startPos.pos_cnum, ' '))
  print_char('^')
  switch endPos.pos_cnum - startPos.pos_cnum {
  | 0 =>
    if token == Token.Eof {
      ()
    } else {
      assert false
    }
  | 1 => ()
  | n =>
    print_string((@doesNotRaise String.make)(n - 2, '-'))
    print_char('^')
  }
  print_char(' ')
  print_string(Res_token.toString(token))
  print_char(' ')
  print_int(startPos.pos_cnum)
  print_char('-')
  print_int(endPos.pos_cnum)
  print_endline("")
}

let next = scanner => {
  let nextOffset = scanner.offset + 1
  switch scanner.ch {
  | '\n' =>
    scanner.lineOffset = nextOffset
    scanner.lnum = scanner.lnum + 1
  /* What about CRLF (\r + \n) on windows?
   * \r\n will always be terminated by a \n
   * -> we can just bump the line count on \n */
  | _ => ()
  }
  if nextOffset < String.length(scanner.src) {
    scanner.offset = nextOffset
    scanner.ch = String.unsafe_get(scanner.src, scanner.offset)
  } else {
    scanner.offset = String.length(scanner.src)
    scanner.ch = hackyEOFChar
  }
}

let next2 = scanner => {
  next(scanner)
  next(scanner)
}

let next3 = scanner => {
  next(scanner)
  next(scanner)
  next(scanner)
}

let peek = scanner =>
  if scanner.offset + 1 < String.length(scanner.src) {
    String.unsafe_get(scanner.src, scanner.offset + 1)
  } else {
    hackyEOFChar
  }

let peek2 = scanner =>
  if scanner.offset + 2 < String.length(scanner.src) {
    String.unsafe_get(scanner.src, scanner.offset + 2)
  } else {
    hackyEOFChar
  }

let make = (~filename, src) => {
  filename: filename,
  src: src,
  err: (~startPos as _, ~endPos as _, _) => (),
  ch: if src == "" {
    hackyEOFChar
  } else {
    String.unsafe_get(src, 0)
  },
  offset: 0,
  lineOffset: 0,
  lnum: 1,
  mode: list{},
}

/* generic helpers */

let isWhitespace = ch =>
  switch ch {
  | ' ' | '\t' | '\n' | '\r' => true
  | _ => false
  }

let rec skipWhitespace = scanner =>
  if isWhitespace(scanner.ch) {
    next(scanner)
    skipWhitespace(scanner)
  }

let digitValue = ch =>
  switch ch {
  | '0' .. '9' => Char.code(ch) - 48
  | 'a' .. 'f' => Char.code(ch) - Char.code('a') + 10
  | 'A' .. 'F' => Char.code(ch) + 32 - Char.code('a') + 10
  | _ => 16
  } /* larger than any legal value */

let rec skipLowerCaseChars = scanner =>
  switch scanner.ch {
  | 'a' .. 'z' =>
    next(scanner)
    skipLowerCaseChars(scanner)
  | _ => ()
  }

/* scanning helpers */

let scanIdentifier = scanner => {
  let startOff = scanner.offset
  let rec skipGoodChars = scanner =>
    switch scanner.ch {
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' =>
      next(scanner)
      skipGoodChars(scanner)
    | _ => ()
    }

  skipGoodChars(scanner)
  let str = (@doesNotRaise String.sub)(scanner.src, startOff, scanner.offset - startOff)
  if '{' === scanner.ch && str == "list" {
    next(scanner)
    /* TODO: this isn't great */
    Token.lookupKeyword("list{")
  } else {
    Token.lookupKeyword(str)
  }
}

let scanDigits = (scanner, ~base) =>
  if base <= 10 {
    let rec loop = scanner =>
      switch scanner.ch {
      | '0' .. '9' | '_' =>
        next(scanner)
        loop(scanner)
      | _ => ()
      }
    loop(scanner)
  } else {
    let rec loop = scanner =>
      switch scanner.ch {
      /* hex */
      | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '_' =>
        next(scanner)
        loop(scanner)
      | _ => ()
      }
    loop(scanner)
  }

/* float: (0…9) { 0…9∣ _ } [. { 0…9∣ _ }] [(e∣ E) [+∣ -] (0…9) { 0…9∣ _ }] */
let scanNumber = scanner => {
  let startOff = scanner.offset

  /* integer part */
  let base = switch scanner.ch {
  | '0' =>
    switch peek(scanner) {
    | 'x' | 'X' =>
      next2(scanner)
      16
    | 'o' | 'O' =>
      next2(scanner)
      8
    | 'b' | 'B' =>
      next2(scanner)
      2
    | _ =>
      next(scanner)
      8
    }
  | _ => 10
  }

  scanDigits(scanner, ~base)

  /*  */
  let isFloat = if '.' === scanner.ch {
    next(scanner)
    scanDigits(scanner, ~base)
    true
  } else {
    false
  }

  /* exponent part */
  let isFloat = switch scanner.ch {
  | 'e' | 'E' | 'p' | 'P' =>
    switch peek(scanner) {
    | '+' | '-' => next2(scanner)
    | _ => next(scanner)
    }
    scanDigits(scanner, ~base)
    true
  | _ => isFloat
  }

  let literal = (@doesNotRaise String.sub)(scanner.src, startOff, scanner.offset - startOff)

  /* suffix */
  let suffix = switch scanner.ch {
  | 'n' =>
    let msg = "Unsupported number type (nativeint). Did you mean `" ++ (literal ++ "`?")

    let pos = position(scanner)
    scanner.err(~startPos=pos, ~endPos=pos, Diagnostics.message(msg))
    next(scanner)
    Some('n')
  | ('g' .. 'z' | 'G' .. 'Z') as ch =>
    next(scanner)
    Some(ch)
  | _ => None
  }

  if isFloat {
    Token.Float({f: literal, suffix: suffix})
  } else {
    Token.Int({i: literal, suffix: suffix})
  }
}

let scanExoticIdentifier = scanner => {
  /* TODO: are we disregarding the current char...? Should be a quote */
  next(scanner)
  let buffer = Buffer.create(20)
  let startPos = position(scanner)

  let rec scan = () =>
    switch scanner.ch {
    | '"' => next(scanner)
    | '\n' | '\r' =>
      /* line break */
      let endPos = position(scanner)
      scanner.err(
        ~startPos,
        ~endPos,
        Diagnostics.message("A quoted identifier can't contain line breaks."),
      )
      next(scanner)
    | ch if ch === hackyEOFChar =>
      let endPos = position(scanner)
      scanner.err(~startPos, ~endPos, Diagnostics.message("Did you forget a \" here?"))
    | ch =>
      Buffer.add_char(buffer, ch)
      next(scanner)
      scan()
    }

  scan()
  /* TODO: do we really need to create a new buffer instead of substring once? */
  Token.Lident(Buffer.contents(buffer))
}

let scanStringEscapeSequence = (~startPos, scanner) => {
  let scan = (~n, ~base, ~max) => {
    let rec loop = (n, x) =>
      if n === 0 {
        x
      } else {
        let d = digitValue(scanner.ch)
        if d >= base {
          let pos = position(scanner)
          let msg = if scanner.ch === hackyEOFChar {
            "unclosed escape sequence"
          } else {
            "unknown escape sequence"
          }

          scanner.err(~startPos, ~endPos=pos, Diagnostics.message(msg))
          -1
        } else {
          let () = next(scanner)
          loop(n - 1, x * base + d)
        }
      }

    let x = loop(n, 0)
    if x > max || (0xD800 <= x && x < 0xE000) {
      let pos = position(scanner)
      let msg = "escape sequence is invalid unicode code point"
      scanner.err(~startPos, ~endPos=pos, Diagnostics.message(msg))
    }
  }

  switch scanner.ch {
  /* \ already consumed */
  | 'n' | 't' | 'b' | 'r' | '\\' | ' ' | '\'' | '"' => next(scanner)
  | '0' .. '9' =>
    /* decimal */
    scan(~n=3, ~base=10, ~max=255)
  | 'o' =>
    /* octal */
    next(scanner)
    scan(~n=3, ~base=8, ~max=255)
  | 'x' =>
    /* hex */
    next(scanner)
    scan(~n=2, ~base=16, ~max=255)
  | 'u' =>
    next(scanner)
    switch scanner.ch {
    | '{' =>
      /* unicode code point escape sequence: '\u{7A}', one or more hex digits */
      next(scanner)
      let x = ref(0)
      while (
        switch scanner.ch {
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' => true
        | _ => false
        }
      ) {
        x := x.contents * 16 + digitValue(scanner.ch)
        next(scanner)
      }
      /* consume '}' in '\u{7A}' */
      switch scanner.ch {
      | '}' => next(scanner)
      | _ => ()
      }
    | _ => scan(~n=4, ~base=16, ~max=Res_utf8.max)
    }
  | _ => /* unknown escape sequence
     * TODO: we should warn the user here. Let's not make it a hard error for now, for reason compat */
    /*
      let pos = position scanner in
      let msg =
        if ch == -1 then "unclosed escape sequence"
        else "unknown escape sequence"
      in
      scanner.err ~startPos ~endPos:pos (Diagnostics.message msg)
 */
    ()
  }
}

let scanString = scanner => {
  /* assumption: we've just matched a quote */

  let startPosWithQuote = position(scanner)
  next(scanner)
  let firstCharOffset = scanner.offset

  let rec scan = () =>
    switch scanner.ch {
    | '"' =>
      let lastCharOffset = scanner.offset
      next(scanner)
      (@doesNotRaise String.sub)(scanner.src, firstCharOffset, lastCharOffset - firstCharOffset)
    | '\\' =>
      let startPos = position(scanner)
      next(scanner)
      scanStringEscapeSequence(~startPos, scanner)
      scan()
    | ch if ch === hackyEOFChar =>
      let endPos = position(scanner)
      scanner.err(~startPos=startPosWithQuote, ~endPos, Diagnostics.unclosedString)
      (@doesNotRaise String.sub)(scanner.src, firstCharOffset, scanner.offset - firstCharOffset)
    | _ =>
      next(scanner)
      scan()
    }

  Token.String(scan())
}

let scanEscape = scanner => {
  /* '\' consumed */
  let offset = scanner.offset - 1
  let convertNumber = (scanner, ~n, ~base) => {
    let x = ref(0)
    for _ in n downto 1 {
      let d = digitValue(scanner.ch)
      x := x.contents * base + d
      next(scanner)
    }
    let c = x.contents
    if Res_utf8.isValidCodePoint(c) {
      Char.unsafe_chr(c)
    } else {
      Char.unsafe_chr(Res_utf8.repl)
    }
  }

  let codepoint = switch scanner.ch {
  | '0' .. '9' => convertNumber(scanner, ~n=3, ~base=10)
  | 'b' =>
    next(scanner)
    '\b'
  | 'n' =>
    next(scanner)
    '\n'
  | 'r' =>
    next(scanner)
    '\r'
  | 't' =>
    next(scanner)
    '\t'
  | 'x' =>
    next(scanner)
    convertNumber(scanner, ~n=2, ~base=16)
  | 'o' =>
    next(scanner)
    convertNumber(scanner, ~n=3, ~base=8)
  | 'u' =>
    next(scanner)
    switch scanner.ch {
    | '{' =>
      /* unicode code point escape sequence: '\u{7A}', one or more hex digits */
      next(scanner)
      let x = ref(0)
      while (
        switch scanner.ch {
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' => true
        | _ => false
        }
      ) {
        x := x.contents * 16 + digitValue(scanner.ch)
        next(scanner)
      }
      /* consume '}' in '\u{7A}' */
      switch scanner.ch {
      | '}' => next(scanner)
      | _ => ()
      }
      let c = x.contents
      if Res_utf8.isValidCodePoint(c) {
        Char.unsafe_chr(c)
      } else {
        Char.unsafe_chr(Res_utf8.repl)
      }
    | _ =>
      /* unicode escape sequence: '\u007A', exactly 4 hex digits */
      convertNumber(scanner, ~n=4, ~base=16)
    }
  | ch =>
    next(scanner)
    ch
  }

  let contents = (@doesNotRaise String.sub)(scanner.src, offset, scanner.offset - offset)
  next(scanner) /* Consume \' */
  /* TODO: do we know it's \' ? */
  Token.Codepoint({c: codepoint, original: contents})
}

let scanSingleLineComment = scanner => {
  let startOff = scanner.offset
  let startPos = position(scanner)
  let rec skip = scanner =>
    switch scanner.ch {
    | '\n' | '\r' => ()
    | ch if ch === hackyEOFChar => ()
    | _ =>
      next(scanner)
      skip(scanner)
    }

  skip(scanner)
  let endPos = position(scanner)
  Token.Comment(
    Comment.makeSingleLineComment(
      ~loc={
        open Location
        {loc_start: startPos, loc_end: endPos, loc_ghost: false}
      },
      (@doesNotRaise String.sub)(scanner.src, startOff, scanner.offset - startOff),
    ),
  )
}

let scanMultiLineComment = scanner => {
  /* assumption: we're only ever using this helper in `scan` after detecting a comment */
  let contentStartOff = scanner.offset + 2
  let startPos = position(scanner)
  let rec scan = (~depth) =>
    /* invariant: depth > 0 right after this match. See assumption */
    switch (scanner.ch, peek(scanner)) {
    | ('/', '*') =>
      next2(scanner)
      scan(~depth=depth + 1)
    | ('*', '/') =>
      next2(scanner)
      if depth > 1 {
        scan(~depth=depth - 1)
      }
    | (ch, _) if ch === hackyEOFChar =>
      let endPos = position(scanner)
      scanner.err(~startPos, ~endPos, Diagnostics.unclosedComment)
    | _ =>
      next(scanner)
      scan(~depth)
    }

  scan(~depth=0)
  let length = scanner.offset - 2 - contentStartOff
  let length = if length < 0 /* in case of EOF */ {
    0
  } else {
    length
  }
  Token.Comment(
    Comment.makeMultiLineComment(
      ~loc={
        open Location
        {loc_start: startPos, loc_end: position(scanner), loc_ghost: false}
      },
      (@doesNotRaise String.sub)(scanner.src, contentStartOff, length),
    ),
  )
}

let scanTemplateLiteralToken = scanner => {
  let startOff = scanner.offset

  /* if starting } here, consume it */
  if scanner.ch === '}' {
    next(scanner)
  }

  let startPos = position(scanner)

  let rec scan = () =>
    switch scanner.ch {
    | '`' =>
      next(scanner)
      Token.TemplateTail(
        (@doesNotRaise String.sub)(scanner.src, startOff, scanner.offset - 1 - startOff),
      )
    | '$' =>
      switch peek(scanner) {
      | '{' =>
        next2(scanner)
        let contents = (@doesNotRaise String.sub)(
          scanner.src,
          startOff,
          scanner.offset - 2 - startOff,
        )

        Token.TemplatePart(contents)
      | _ =>
        next(scanner)
        scan()
      }
    | '\\' =>
      switch peek(scanner) {
      | '`'
      | '\\'
      | '$'
      | '\n'
      | '\r' =>
        /* line break */
        next2(scanner)
        scan()
      | _ =>
        next(scanner)
        scan()
      }
    | ch if ch == hackyEOFChar =>
      let endPos = position(scanner)
      scanner.err(~startPos, ~endPos, Diagnostics.unclosedTemplate)
      Token.TemplateTail(
        (@doesNotRaise String.sub)(scanner.src, startOff, max(scanner.offset - 1 - startOff, 0)),
      )
    | _ =>
      next(scanner)
      scan()
    }

  let token = scan()
  let endPos = position(scanner)
  (startPos, endPos, token)
}

let rec scan = scanner => {
  skipWhitespace(scanner)
  let startPos = position(scanner)

  let token = switch scanner.ch {
  /* peeking 0 char */
  | 'A' .. 'Z' | 'a' .. 'z' => scanIdentifier(scanner)
  | '0' .. '9' => scanNumber(scanner)
  | '`' =>
    next(scanner)
    Token.Backtick
  | '~' =>
    next(scanner)
    Token.Tilde
  | '?' =>
    next(scanner)
    Token.Question
  | ';' =>
    next(scanner)
    Token.Semicolon
  | '(' =>
    next(scanner)
    Token.Lparen
  | ')' =>
    next(scanner)
    Token.Rparen
  | '[' =>
    next(scanner)
    Token.Lbracket
  | ']' =>
    next(scanner)
    Token.Rbracket
  | '{' =>
    next(scanner)
    Token.Lbrace
  | '}' =>
    next(scanner)
    Token.Rbrace
  | ',' =>
    next(scanner)
    Token.Comma
  | '"' => scanString(scanner)

  /* peeking 1 char */
  | '_' =>
    switch peek(scanner) {
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' => scanIdentifier(scanner)
    | _ =>
      next(scanner)
      Token.Underscore
    }
  | '#' =>
    switch peek(scanner) {
    | '=' =>
      next2(scanner)
      Token.HashEqual
    | _ =>
      next(scanner)
      Token.Hash
    }
  | '*' =>
    switch peek(scanner) {
    | '*' =>
      next2(scanner)
      Token.Exponentiation
    | '.' =>
      next2(scanner)
      Token.AsteriskDot
    | _ =>
      next(scanner)
      Token.Asterisk
    }
  | '@' =>
    switch peek(scanner) {
    | '@' =>
      next2(scanner)
      Token.AtAt
    | _ =>
      next(scanner)
      Token.At
    }
  | '%' =>
    switch peek(scanner) {
    | '%' =>
      next2(scanner)
      Token.PercentPercent
    | _ =>
      next(scanner)
      Token.Percent
    }
  | '|' =>
    switch peek(scanner) {
    | '|' =>
      next2(scanner)
      Token.Lor
    | '>' =>
      next2(scanner)
      Token.BarGreater
    | _ =>
      next(scanner)
      Token.Bar
    }
  | '&' =>
    switch peek(scanner) {
    | '&' =>
      next2(scanner)
      Token.Land
    | _ =>
      next(scanner)
      Token.Band
    }
  | ':' =>
    switch peek(scanner) {
    | '=' =>
      next2(scanner)
      Token.ColonEqual
    | '>' =>
      next2(scanner)
      Token.ColonGreaterThan
    | _ =>
      next(scanner)
      Token.Colon
    }
  | '\\' =>
    next(scanner)
    scanExoticIdentifier(scanner)
  | '/' =>
    switch peek(scanner) {
    | '/' =>
      next2(scanner)
      scanSingleLineComment(scanner)
    | '*' => scanMultiLineComment(scanner)
    | '.' =>
      next2(scanner)
      Token.ForwardslashDot
    | _ =>
      next(scanner)
      Token.Forwardslash
    }
  | '-' =>
    switch peek(scanner) {
    | '.' =>
      next2(scanner)
      Token.MinusDot
    | '>' =>
      next2(scanner)
      Token.MinusGreater
    | _ =>
      next(scanner)
      Token.Minus
    }
  | '+' =>
    switch peek(scanner) {
    | '.' =>
      next2(scanner)
      Token.PlusDot
    | '+' =>
      next2(scanner)
      Token.PlusPlus
    | '=' =>
      next2(scanner)
      Token.PlusEqual
    | _ =>
      next(scanner)
      Token.Plus
    }
  | '>' =>
    switch peek(scanner) {
    | '=' if !inDiamondMode(scanner) =>
      next2(scanner)
      Token.GreaterEqual
    | _ =>
      next(scanner)
      Token.GreaterThan
    }
  | '<' if !inJsxMode(scanner) =>
    switch peek(scanner) {
    | '=' =>
      next2(scanner)
      Token.LessEqual
    | _ =>
      next(scanner)
      Token.LessThan
    }
  /* special handling for JSX < */
  | '<' =>
    /* Imagine the following: <div><
     * < indicates the start of a new jsx-element, the parser expects
     * the name of a new element after the <
     * Example: <div> <div
     * But what if we have a / here: example </ in  <div></div>
     * This signals a closing element. To simulate the two-token lookahead,
     * the </ is emitted as a single new token LessThanSlash */
    next(scanner)
    skipWhitespace(scanner)
    switch scanner.ch {
    | '/' =>
      next(scanner)
      Token.LessThanSlash
    | '=' =>
      next(scanner)
      Token.LessEqual
    | _ => Token.LessThan
    }

  /* peeking 2 chars */
  | '.' =>
    switch (peek(scanner), peek2(scanner)) {
    | ('.', '.') =>
      next3(scanner)
      Token.DotDotDot
    | ('.', _) =>
      next2(scanner)
      Token.DotDot
    | _ =>
      next(scanner)
      Token.Dot
    }
  | '\'' =>
    switch (peek(scanner), peek2(scanner)) {
    | ('\\', '"') =>
      /* careful with this one! We're next-ing _once_ (not twice),
       then relying on matching on the quote */
      next(scanner)
      SingleQuote
    | ('\\', _) =>
      next2(scanner)
      scanEscape(scanner)
    | (ch, '\'') =>
      let offset = scanner.offset + 1
      next3(scanner)
      Token.Codepoint({c: ch, original: (@doesNotRaise String.sub)(scanner.src, offset, 1)})
    | (ch, _) =>
      next(scanner)
      let offset = scanner.offset
      let (codepoint, length) = Res_utf8.decodeCodePoint(
        scanner.offset,
        scanner.src,
        String.length(scanner.src),
      )
      for _ in 0 to length - 1 {
        next(scanner)
      }
      if scanner.ch == '\'' {
        let contents = (@doesNotRaise String.sub)(scanner.src, offset, length)
        next(scanner)
        Token.Codepoint({c: Obj.magic(codepoint), original: contents})
      } else {
        scanner.ch = ch
        scanner.offset = offset
        SingleQuote
      }
    }
  | '!' =>
    switch (peek(scanner), peek2(scanner)) {
    | ('=', '=') =>
      next3(scanner)
      Token.BangEqualEqual
    | ('=', _) =>
      next2(scanner)
      Token.BangEqual
    | _ =>
      next(scanner)
      Token.Bang
    }
  | '=' =>
    switch (peek(scanner), peek2(scanner)) {
    | ('=', '=') =>
      next3(scanner)
      Token.EqualEqualEqual
    | ('=', _) =>
      next2(scanner)
      Token.EqualEqual
    | ('>', _) =>
      next2(scanner)
      Token.EqualGreater
    | _ =>
      next(scanner)
      Token.Equal
    }

  /* special cases */
  | ch if ch === hackyEOFChar =>
    next(scanner)
    Token.Eof
  | ch =>
    /* if we arrive here, we're dealing with an unknown character,
     * report the error and continue scanning… */
    next(scanner)
    let endPos = position(scanner)
    scanner.err(~startPos, ~endPos, Diagnostics.unknownUchar(ch))
    let (_, _, token) = scan(scanner)
    token
  }

  let endPos = position(scanner)
  /* _printDebug ~startPos ~endPos scanner token; */
  (startPos, endPos, token)
}

/* misc helpers used elsewhere */

/* Imagine: <div> <Navbar /> <
 * is `<` the start of a jsx-child? <div …
 * or is it the start of a closing tag?  </div>
 * reconsiderLessThan peeks at the next token and
 * determines the correct token to disambiguate */
let reconsiderLessThan = scanner => {
  /* < consumed */
  skipWhitespace(scanner)
  if scanner.ch === '/' {
    let () = next(scanner)
    Token.LessThanSlash
  } else {
    Token.LessThan
  }
}

/* If an operator has whitespace around both sides, it's a binary operator */
/* TODO: this helper seems out of place */
let isBinaryOp = (src, startCnum, endCnum) =>
  if startCnum === 0 {
    false
  } else {
    /* we're gonna put some assertions and invariant checks here because this is
     used outside of the scanner's normal invariant assumptions */
    assert (endCnum >= 0)
    assert (startCnum > 0 && startCnum < String.length(src))
    let leftOk = isWhitespace(String.unsafe_get(src, startCnum - 1))
    /* we need some stronger confidence that endCnum is ok */
    let rightOk = endCnum >= String.length(src) || isWhitespace(String.unsafe_get(src, endCnum))
    leftOk && rightOk
  }

/* Assume `{` consumed, advances the scanner towards the ends of Reason quoted strings. (for conversion)
 * In {| foo bar |} the scanner will be advanced until after the `|}` */
let tryAdvanceQuotedString = scanner => {
  let rec scanContents = tag =>
    switch scanner.ch {
    | '|' =>
      next(scanner)
      switch scanner.ch {
      | 'a' .. 'z' =>
        let startOff = scanner.offset
        skipLowerCaseChars(scanner)
        let suffix = (@doesNotRaise String.sub)(scanner.src, startOff, scanner.offset - startOff)
        if tag == suffix {
          if scanner.ch == '}' {
            next(scanner)
          } else {
            scanContents(tag)
          }
        } else {
          scanContents(tag)
        }
      | '}' => next(scanner)
      | _ => scanContents(tag)
      }
    | ch if ch === hackyEOFChar => /* TODO: why is this place checking EOF and not others? */
      ()
    | _ =>
      next(scanner)
      scanContents(tag)
    }

  switch scanner.ch {
  | 'a' .. 'z' =>
    let startOff = scanner.offset
    skipLowerCaseChars(scanner)
    let tag = (@doesNotRaise String.sub)(scanner.src, startOff, scanner.offset - startOff)
    if scanner.ch == '|' {
      scanContents(tag)
    }
  | '|' => scanContents("")
  | _ => ()
  }
}

