type t<'k, 'v>

@new external make: unit => t<'k, 'v> = "WeakMap"

@send external get: (t<'k, 'v>, 'k) => option<'v> = "get"
@send external has: (t<'k, 'v>, 'k) => bool = "has"
@send external set: (t<'k, 'v>, 'k, 'v) => t<'k, 'v> = "set"
@send external delete: (t<'k, 'v>, 'k) => bool = "delete"
