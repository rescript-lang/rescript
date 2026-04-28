/*** ES6 WeakMap API */

@deprecated({
  reason: "Use `WeakMap.t` instead.",
  migrate: %replace.type(: WeakMap.t),
})
type t<'k, 'v> = Stdlib_WeakMap.t<'k, 'v>
