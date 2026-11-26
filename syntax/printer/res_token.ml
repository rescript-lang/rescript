module Comment = Res_comment

type t =
  | Await
  | Break
  | Continue
  | True
  | False
  | Codepoint of {c: int; original: string}
  | Int of {i: string; suffix: char option}
  | Float of {f: string; suffix: char option}
  | String of string
  | Lident of string
  | Uident of string
  | Dot
  | DotDot
  | DotDotDot
  | Bang
  | Semicolon
  | Let of {unwrap: bool}
  | And
  | Underscore
  | SingleQuote
  | Equal
  | EqualEqual
  | EqualEqualEqual
  | Ampersand
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
  | Backslash [@live]
  | Forwardslash
  | ForwardslashDot
  | Regex of string * string
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
  | Hash
  | HashEqual
  | Assert
  | Tilde
  | Question
  | If
  | Else
  | For
  | In
  | While
  | Switch
  | EqualGreater
  | MinusGreater
  | Typ
  | Include
  | Module
  | Land
  | Lor
  | Bnot (* Bitwise not: ~~~ *)
  | Bor (* Bitwise or: ||| *)
  | Bxor (* Bitwise xor: ^^^ *)
  | Band (* Bitwise and: &&& *)
  | Caret
  | BangEqual
  | BangEqualEqual
  | LessEqual
  | GreaterEqual
  | ColonEqual
  | At
  | AtAt
  | Percent
  | PercentPercent
  | Comment of Comment.t
  | List
  | Dict
  | TemplateTail of string * Lexing.position
  | TemplatePart of string * Lexing.position
  | Backtick
  | Try
  | DocComment of Location.t * string
  | ModuleComment of Location.t * string
  | LeftShift
  | RightShift
  | RightShiftUnsigned

let precedence = function
  | HashEqual | ColonEqual -> 1
  | Lor -> 2
  | Land -> 3
  | Bor -> 4
  | Bxor -> 5
  | Band -> 6
  | Equal | EqualEqual | EqualEqualEqual | LessThan | GreaterThan | BangEqual
  | BangEqualEqual | LessEqual | GreaterEqual ->
    7
  | LeftShift | RightShift | RightShiftUnsigned -> 8
  | Plus | PlusDot | Minus | MinusDot | PlusPlus -> 9
  | Asterisk | AsteriskDot | Forwardslash | ForwardslashDot | Percent -> 10
  | Exponentiation -> 11
  | MinusGreater -> 12
  | Dot -> 13
  | _ -> 0

let to_string = function
  | Await -> "await"
  | Break -> "break"
  | Continue -> "continue"
  | True -> "true"
  | False -> "false"
  | Codepoint {original} -> "codepoint '" ^ original ^ "'"
  | String s -> "string \"" ^ s ^ "\""
  | Lident str -> str
  | Uident str -> str
  | Dot -> "."
  | DotDot -> ".."
  | DotDotDot -> "..."
  | Int {i} -> "int " ^ i
  | Float {f} -> "Float: " ^ f
  | Bang -> "!"
  | Semicolon -> ";"
  | Let {unwrap = true} -> "let?"
  | Let {unwrap = false} -> "let"
  | And -> "and"
  | Underscore -> "_"
  | SingleQuote -> "'"
  | Equal -> "="
  | EqualEqual -> "=="
  | EqualEqualEqual -> "==="
  | Eof -> "eof"
  | Ampersand -> "&"
  | Bar -> "|"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbracket -> "["
  | Rbracket -> "]"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | ColonGreaterThan -> ":>"
  | Colon -> ":"
  | Comma -> ","
  | Minus -> "-"
  | MinusDot -> "-."
  | Plus -> "+"
  | PlusDot -> "+."
  | PlusPlus -> "++"
  | PlusEqual -> "+="
  | Backslash -> "\\"
  | Regex (pattern, flags) -> "regex: /" ^ pattern ^ "/" ^ flags
  | Forwardslash -> "/"
  | ForwardslashDot -> "/."
  | Hash -> "#"
  | HashEqual -> "#="
  | GreaterThan -> ">"
  | LessThan -> "<"
  | Asterisk -> "*"
  | AsteriskDot -> "*."
  | Exponentiation -> "**"
  | Assert -> "assert"
  | Tilde -> "~"
  | Question -> "?"
  | If -> "if"
  | Else -> "else"
  | For -> "for"
  | In -> "in"
  | While -> "while"
  | Switch -> "switch"
  | EqualGreater -> "=>"
  | MinusGreater -> "->"
  | Typ -> "type"
  | Include -> "include"
  | Module -> "module"
  | Lor -> "||"
  | Bnot -> "~~~"
  | Bor -> "|||"
  | Bxor -> "^^^"
  | Band -> "&&&"
  | Caret -> "^"
  | Land -> "&&"
  | BangEqual -> "!="
  | BangEqualEqual -> "!=="
  | GreaterEqual -> ">="
  | LessEqual -> "<="
  | ColonEqual -> ":="
  | At -> "@"
  | AtAt -> "@@"
  | Percent -> "%"
  | PercentPercent -> "%%"
  | Comment c -> "Comment" ^ Comment.to_string c
  | List -> "list{"
  | Dict -> "dict{"
  | TemplatePart (text, _) -> text ^ "${"
  | TemplateTail (text, _) -> "TemplateTail(" ^ text ^ ")"
  | Backtick -> "`"
  | Try -> "try"
  | DocComment (_loc, s) -> "DocComment " ^ s
  | ModuleComment (_loc, s) -> "ModuleComment " ^ s
  | LeftShift -> "<<"
  | RightShift -> ">>"
  | RightShiftUnsigned -> ">>>"

let keyword_table = function
  | "and" -> And
  | "assert" -> Assert
  | "await" -> Await
  | "break" -> Break
  | "continue" -> Continue
  | "else" -> Else
  | "false" -> False
  | "for" -> For
  | "if" -> If
  | "in" -> In
  | "include" -> Include
  | "let?" -> Let {unwrap = true}
  | "let" -> Let {unwrap = false}
  | "list{" -> List
  | "dict{" -> Dict
  | "module" -> Module
  | "switch" -> Switch
  | "true" -> True
  | "try" -> Try
  | "type" -> Typ
  | "while" -> While
  | _ -> raise Not_found
[@@throws Not_found]

let is_keyword = function
  | Await | Break | Continue | And | Assert | Else | False | For | If | In
  | Include | Land | Let _ | List | Lor | Module | Switch | True | Try | Typ
  | While | Dict ->
    true
  | _ -> false

(* Contextual keywords are identifiers, recognised as keywords by the parser
   only where the grammar uses them (see [Res_core]): [as], [constraint],
   [exception], [external], [mutable], [of], [open], [private], [rec] and
   [when]. Like [async], [catch] or [nonrec], each can name a variable, a field
   or a label elsewhere. *)

(* The text of a reserved word, for the positions where the grammar expects a
   name that no variable is bound to: a record field, an argument label, a JSX
   prop, a polymorphic variant tag or a lowercase JSX tag name. [let?] is a
   single token; the name it stands for is [let], followed by [?]. *)
let keyword_name = function
  | And -> Some "and"
  | Assert -> Some "assert"
  | Await -> Some "await"
  | Break -> Some "break"
  | Continue -> Some "continue"
  | Else -> Some "else"
  | False -> Some "false"
  | For -> Some "for"
  | If -> Some "if"
  | In -> Some "in"
  | Include -> Some "include"
  | Let _ -> Some "let"
  | Module -> Some "module"
  | Switch -> Some "switch"
  | True -> Some "true"
  | Try -> Some "try"
  | Typ -> Some "type"
  | While -> Some "while"
  | _ -> None

let lookup_keyword str =
  try keyword_table str
  with Not_found -> (
    match str.[0] [@doesNotThrow] with
    | 'A' .. 'Z' -> Uident str
    | _ -> Lident str)

let is_keyword_txt str =
  try
    let _ = keyword_table str in
    true
  with Not_found -> false

(* Whether [str] is a reserved word: a keyword spelled like an identifier,
   which [keyword_name] accepts as a name. [let?], [list{] and [dict{] are
   keywords but not words. *)
let is_keyword_name_txt str =
  match keyword_table str with
  | token -> keyword_name token = Some str
  | exception Not_found -> false

let catch = Lident "catch"
let finally = Lident "finally"
