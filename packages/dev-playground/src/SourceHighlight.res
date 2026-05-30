type tokenKind =
  | TokenPlain
  | TokenKeyword
  | TokenBuiltin
  | TokenConstructor
  | TokenString
  | TokenNumber
  | TokenComment
  | TokenAttribute
  | TokenOperator

type token = {
  kind: tokenKind,
  text: string,
}

let syntaxKeywords = [
  "and",
  "as",
  "async",
  "await",
  "catch",
  "constraint",
  "else",
  "exception",
  "external",
  "false",
  "for",
  "if",
  "in",
  "include",
  "let",
  "module",
  "mutable",
  "open",
  "private",
  "rec",
  "switch",
  "to",
  "true",
  "try",
  "type",
  "when",
  "while",
]

let syntaxBuiltins = [
  "array",
  "bigint",
  "bool",
  "dict",
  "float",
  "int",
  "list",
  "option",
  "promise",
  "result",
  "string",
  "unit",
]

let charAt = (source, index) => source->String.charAt(index)

let isLower = char => char >= "a" && char <= "z"
let isUpper = char => char >= "A" && char <= "Z"
let isDigit = char => char >= "0" && char <= "9"
let isAlpha = char => isLower(char) || isUpper(char)
let isIdentStart = char => isAlpha(char) || char === "_"
let isIdentPart = char => isIdentStart(char) || isDigit(char) || char === "'"

let isOperatorChar = char =>
  switch char {
  | "="
  | ">"
  | "<"
  | "+"
  | "-"
  | "*"
  | "/"
  | "|"
  | "&"
  | "!"
  | "?"
  | ":"
  | "."
  | "~"
  | "^"
  | "%"
  | "#" => true
  | _ => false
  }

let startsWithAt = (source, index, prefix) =>
  source->String.slice(~start=index, ~end=index + String.length(prefix)) === prefix

let rec findLineEnd = (source, index, length) =>
  if index >= length || charAt(source, index) === "\n" {
    index
  } else {
    findLineEnd(source, index + 1, length)
  }

let rec findBlockEnd = (source, index, length, closing) =>
  if index >= length {
    length
  } else if startsWithAt(source, index, closing) {
    index + String.length(closing)
  } else {
    findBlockEnd(source, index + 1, length, closing)
  }

let rec findStringEnd = (source, index, length, delimiter, escaped) =>
  if index >= length {
    length
  } else {
    let char = charAt(source, index)
    if escaped {
      findStringEnd(source, index + 1, length, delimiter, false)
    } else if char === "\\" {
      findStringEnd(source, index + 1, length, delimiter, true)
    } else if char === delimiter {
      index + 1
    } else {
      findStringEnd(source, index + 1, length, delimiter, false)
    }
  }

let rec findIdentEnd = (source, index, length) =>
  if index < length && isIdentPart(charAt(source, index)) {
    findIdentEnd(source, index + 1, length)
  } else {
    index
  }

let rec findAttributeEnd = (source, index, length) =>
  if index < length {
    let char = charAt(source, index)
    if isIdentPart(char) || char === "." || char === "@" {
      findAttributeEnd(source, index + 1, length)
    } else {
      index
    }
  } else {
    index
  }

let rec findNumberEnd = (source, index, length) =>
  if index < length {
    let char = charAt(source, index)
    if isDigit(char) || isAlpha(char) || char === "_" || char === "." {
      findNumberEnd(source, index + 1, length)
    } else {
      index
    }
  } else {
    index
  }

let rec findOperatorEnd = (source, index, length) =>
  if index < length && isOperatorChar(charAt(source, index)) {
    findOperatorEnd(source, index + 1, length)
  } else {
    index
  }

let tokenKindForIdent = word =>
  if syntaxKeywords->Array.includes(word) {
    TokenKeyword
  } else if syntaxBuiltins->Array.includes(word) {
    TokenBuiltin
  } else if String.length(word) > 0 && isUpper(charAt(word, 0)) {
    TokenConstructor
  } else {
    TokenPlain
  }

let tokenize = source => {
  let tokens: array<token> = []
  let length = String.length(source)
  let index = ref(0)

  while index.contents < length {
    let start = index.contents
    let char = charAt(source, start)
    let (next, kind) = if startsWithAt(source, start, "//") {
      (findLineEnd(source, start, length), TokenComment)
    } else if startsWithAt(source, start, "/*") {
      (findBlockEnd(source, start + 2, length, "*/"), TokenComment)
    } else if char === "\"" || char === "`" {
      (findStringEnd(source, start + 1, length, char, false), TokenString)
    } else if char === "@" {
      (findAttributeEnd(source, start + 1, length), TokenAttribute)
    } else if isDigit(char) {
      (findNumberEnd(source, start + 1, length), TokenNumber)
    } else if isIdentStart(char) {
      let next = findIdentEnd(source, start + 1, length)
      let word = source->String.slice(~start, ~end=next)
      (next, tokenKindForIdent(word))
    } else if isOperatorChar(char) {
      (findOperatorEnd(source, start + 1, length), TokenOperator)
    } else {
      (start + 1, TokenPlain)
    }

    tokens->Array.push({
      kind,
      text: source->String.slice(~start, ~end=next),
    })
    index := next
  }

  tokens
}

let tokenClass = kind =>
  switch kind {
  | TokenPlain => "syntax-token"
  | TokenKeyword => "syntax-token syntax-keyword"
  | TokenBuiltin => "syntax-token syntax-builtin"
  | TokenConstructor => "syntax-token syntax-constructor"
  | TokenString => "syntax-token syntax-string"
  | TokenNumber => "syntax-token syntax-number"
  | TokenComment => "syntax-token syntax-comment"
  | TokenAttribute => "syntax-token syntax-attribute"
  | TokenOperator => "syntax-token syntax-operator"
  }

let render = source =>
  tokenize(source)->Array.map(token =>
    <span class={tokenClass(token.kind)}> {Node.text(token.text)} </span>
  )
