type t<'a>

@new external make: unit => t<'a> = "WeakSet"

@send external add: (t<'a>, 'a) => t<'a> = "add"
@send external delete: (t<'a>, 'a) => bool = "delete"
@send external has: (t<'a>, 'a) => bool = "has"
