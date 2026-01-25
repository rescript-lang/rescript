/*** ES6 WeakSet API */

@deprecated({
  reason: "Use `WeakSet.t` instead.",
  migrate: %replace.type(: WeakSet.t),
})
type t<'a> = Stdlib_WeakSet.t<'a>
