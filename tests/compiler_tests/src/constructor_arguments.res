type unary = Unary((int, int))
type binary = Binary(int, int)

let unary = Unary((1, 2))
let binary = Binary(1, 2)

let unaryUnparenthesized = Unary(1, 2)
let binaryParenthesized = Binary((1, 2))
let optionUnparenthesized = Some(1, 2)

let readUnary = value =>
  switch value {
  | Unary((x, y)) => x + y
  }

let readBinary = value =>
  switch value {
  | Binary(x, y) => x + y
  }

let readUnaryUnparenthesized = value =>
  switch value {
  | Unary(x, y) => x + y
  }

let readBinaryParenthesized = value =>
  switch value {
  | Binary((x, y)) => x + y
  }

let readOptionUnparenthesized = value =>
  switch value {
  | Some(x, y) => x + y
  | None => 0
  }

type poly = [#UnaryTuple((int, int)) | #BinaryArgs(int, int)]

let polyUnary: poly = #UnaryTuple((1, 2))
let polyBinary: poly = #BinaryArgs(1, 2)

let readPoly = value =>
  switch value {
  | #UnaryTuple((x, y)) | #BinaryArgs(x, y) => x + y
  }
