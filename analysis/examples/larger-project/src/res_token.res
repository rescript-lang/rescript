module Comment = Res_comment

type t =
  | Open
  | True
  | False
  | Codepoint({c: char, original: string})
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
  | Assert
  | Lazy
  | Tilde
  | Question
  | If
  | Else
  | For
  | In
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
  | Codepoint({original}) => "codepoint '" ++ (original ++ "'")
  | String(s) => "string \"" ++ (s ++ "\"")
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
  | Comment(c) => "Comment" ++ Comment.toString(c)
  | List => "list{"
  | TemplatePart(text) => text ++ "${"
  | TemplateTail(text) => "TemplateTail(" ++ (text ++ ")")
  | Backtick => "`"
  | BarGreater => "|>"
  | Try => "try"
  | Import => "import"
  | Export => "export"
  }

@raises(Not_found)
let keywordTable = x =>
  switch x {
  | "and" => And
  | "as" => As
  | "assert" => Assert
  | "constraint" => Constraint
  | "else" => Else
  | "exception" => Exception
  | "export" => Export
  | "external" => External
  | "false" => False
  | "for" => For
  | "if" => If
  | "import" => Import
  | "in" => In
  | "include" => Include
  | "lazy" => Lazy
  | "let" => Let
  | "list{" => List
  | "module" => Module
  | "mutable" => Mutable
  | "of" => Of
  | "open" => Open
  | "private" => Private
  | "rec" => Rec
  | "switch" => Switch
  | "true" => True
  | "try" => Try
  | "type" => Typ
  | "when" => When
  | "while" => While
  | _ => throw(Not_found)
  }

let isKeyword = x =>
  switch x {
  | And
  | As
  | Assert
  | Constraint
  | Else
  | Exception
  | Export
  | External
  | False
  | For
  | If
  | Import
  | In
  | Include
  | Land
  | Lazy
  | Let
  | List
  | Lor
  | Module
  | Mutable
  | Of
  | Open
  | Private
  | Rec
  | Switch
  | True
  | Try
  | Typ
  | When
  | While => true
  | _ => false
  }

let lookupKeyword = str =>
  try keywordTable(str) catch {
  | Not_found =>
    switch @doesNotRaise
    String.get(str, 0) {
    | 'A' .. 'Z' => Uident(str)
    | _ => Lident(str)
    }
  }

let isKeywordTxt = str =>
  try {
    let _ = keywordTable(str)
    true
  } catch {
  | Not_found => false
  }

let catch = Lident("catch")
