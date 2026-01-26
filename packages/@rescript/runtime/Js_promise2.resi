@deprecated({
  reason: "Use `promise` directly instead.",
  migrate: %replace.type(: promise),
})
type t<+'a> = promise<'a>
type error

/** Type-safe t-first then */
@deprecated({
  reason: "Use `Promise.then` instead.",
  migrate: Promise.then(),
})
let then: (promise<'a>, 'a => promise<'b>) => promise<'b>

/** Type-safe t-first catch */
@deprecated({
  reason: "Use `Promise.catch` instead.",
  migrate: Promise.catch(),
})
let catch: (promise<'a>, error => promise<'a>) => promise<'a>

@deprecated({
  reason: "Use `Promise.make` instead.",
  migrate: Promise.make(
    @apply.transforms(["labelledToUnlabelledArgumentsInFnDefinition"])
    %insert.unlabelledArgument(0),
  ),
})
@new
external make: ((~resolve: 'a => unit, ~reject: exn => unit) => unit) => promise<'a> = "Promise"

@deprecated({
  reason: "Use `Promise.resolve` instead.",
  migrate: Promise.resolve(),
})
@val
@scope("Promise")
external resolve: 'a => promise<'a> = "resolve"
@deprecated({
  reason: "Use `Promise.reject` instead.",
  migrate: Promise.reject(),
})
@val
@scope("Promise")
external reject: exn => promise<'a> = "reject"

@deprecated({
  reason: "Use `Promise.all` instead.",
  migrate: Promise.all(),
})
@val
@scope("Promise")
external all: array<promise<'a>> => promise<array<'a>> = "all"

@deprecated({
  reason: "Use `Promise.all2` instead.",
  migrate: Promise.all2(),
})
@val
@scope("Promise")
external all2: ((promise<'a0>, promise<'a1>)) => promise<('a0, 'a1)> = "all"

@deprecated({
  reason: "Use `Promise.all3` instead.",
  migrate: Promise.all3(),
})
@val
@scope("Promise")
external all3: ((promise<'a0>, promise<'a1>, promise<'a2>)) => promise<('a0, 'a1, 'a2)> = "all"

@deprecated({
  reason: "Use `Promise.all4` instead.",
  migrate: Promise.all4(),
})
@val
@scope("Promise")
external all4: ((promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>)) => promise<(
  'a0,
  'a1,
  'a2,
  'a3,
)> = "all"

@deprecated({
  reason: "Use `Promise.all5` instead.",
  migrate: Promise.all5(),
})
@val
@scope("Promise")
external all5: ((promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>, promise<'a4>)) => promise<(
  'a0,
  'a1,
  'a2,
  'a3,
  'a4,
)> = "all"

@deprecated({
  reason: "Use `Promise.all6` instead.",
  migrate: Promise.all6(),
})
@val
@scope("Promise")
external all6: (
  (promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>, promise<'a4>, promise<'a5>)
) => promise<('a0, 'a1, 'a2, 'a3, 'a4, 'a5)> = "all"

@deprecated({
  reason: "Use `Promise.race` instead.",
  migrate: Promise.race(),
})
@val
@scope("Promise")
external race: array<promise<'a>> => promise<'a> = "race"

external unsafe_async: 'a => promise<'a> = "%identity"
external unsafe_await: promise<'a> => 'a = "%await"
