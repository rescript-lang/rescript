/*** ES6 Map API */

@deprecated({
  reason: "Use `Map.t` instead.",
  migrate: %replace.type(: Map.t),
})
type t<'k, 'v> = Stdlib_Map.t<'k, 'v>
