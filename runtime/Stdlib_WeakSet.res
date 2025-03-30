type t<'a>

@new external make: unit => t<'a> = "WeakSet"

@send external add: (t<'a>, 'a) => t<'a> = "add"
@send external delete: (t<'a>, 'a) => bool = "delete"
@send external has: (t<'a>, 'a) => bool = "has"

/**
  `ignore(weakSet)` ignores the provided weakSet and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t<'a> => unit = "%ignore"
