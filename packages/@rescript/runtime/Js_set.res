/*** ES6 Set API */

@deprecated({
  reason: "Use `Set.t` instead.",
  migrate: %replace.type(: Set.t),
})
type t<'a> = Stdlib_Set.t<'a>
