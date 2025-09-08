type t = float

@inline let less = -1.
@inline let equal = 0.
@inline let greater = 1.

let isLess = ord => ord < equal
let isEqual = ord => ord == equal
let isGreater = ord => ord > equal

let invert = ord => -.ord

let fromInt = n => n < 0 ? less : n > 0 ? greater : equal

/**
  `ignore(ordering)` ignores the provided ordering and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"
