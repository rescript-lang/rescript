let getOrThrow: (list<'a>, int) => 'a

@deprecated({
  reason: "Use `getOrThrow` instead",
  migrate: Legacy.getOrThrow(),
})
let getExn: (list<'a>, int) => 'a
