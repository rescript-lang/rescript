type t<'k, 'v>

@new external make: unit => t<'k, 'v> = "WeakMap"

@send external get: (t<'k, 'v>, 'k) => option<'v> = "get"
@send external has: (t<'k, 'v>, 'k) => bool = "has"
@send external set: (t<'k, 'v>, 'k, 'v) => t<'k, 'v> = "set"
@send external delete: (t<'k, 'v>, 'k) => bool = "delete"

/**
  `ignore(weakMap)` ignores the provided weakMap and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t<'k, 'v> => unit = "%ignore"
