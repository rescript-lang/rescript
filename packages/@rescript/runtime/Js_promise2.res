type t<+'a> = promise<'a>
type error

/** Type-safe t-first then */
let then: (promise<'a>, 'a => promise<'b>) => promise<'b> = %raw(`
  function(p, cont) {
    return Promise.resolve(p).then(cont)
  }
  `)

/** Type-safe t-first catch */
let catch: (promise<'a>, error => promise<'a>) => promise<'a> = %raw(`
    function(p, cont) {
      return Promise.resolve(p).catch(cont)
    }
    `)

@new
external make: ((~resolve: 'a => unit, ~reject: exn => unit) => unit) => promise<'a> = "Promise"

@val @scope("Promise")
external resolve: 'a => promise<'a> = "resolve"

@val @scope("Promise")
external reject: exn => promise<'a> = "reject"

@val @scope("Promise")
external all: array<promise<'a>> => promise<array<'a>> = "all"

@val @scope("Promise")
external all2: ((promise<'a0>, promise<'a1>)) => promise<('a0, 'a1)> = "all"

@val @scope("Promise")
external all3: ((promise<'a0>, promise<'a1>, promise<'a2>)) => promise<('a0, 'a1, 'a2)> = "all"

@val @scope("Promise")
external all4: ((promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>)) => promise<(
  'a0,
  'a1,
  'a2,
  'a3,
)> = "all"

@val @scope("Promise")
external all5: ((promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>, promise<'a4>)) => promise<(
  'a0,
  'a1,
  'a2,
  'a3,
  'a4,
)> = "all"

@val @scope("Promise")
external all6: (
  (promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>, promise<'a4>, promise<'a5>)
) => promise<('a0, 'a1, 'a2, 'a3, 'a4, 'a5)> = "all"

@val @scope("Promise")
external race: array<promise<'a>> => promise<'a> = "race"

external unsafe_async: 'a => promise<'a> = "%identity"
external unsafe_await: promise<'a> => 'a = "%await"
