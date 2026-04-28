type t<+'a> = promise<'a>

@new
external make: (('a => unit, 'e => unit) => unit) => t<'a> = "Promise"

type promiseAndResolvers<'a> = {
  promise: t<'a>,
  resolve: 'a => unit,
  reject: exn => unit,
}

@scope("Promise") @val
external withResolvers: unit => promiseAndResolvers<_> = "withResolvers"

@scope("Promise") @val
external resolve: 'a => t<'a> = "resolve"

@send external then: (t<'a>, 'a => t<'b>) => t<'b> = "then"

@send
external thenResolve: (t<'a>, 'a => 'b) => t<'b> = "then"

@send external finally: (t<'a>, unit => unit) => t<'a> = "finally"

@scope("Promise") @val
external reject: exn => t<_> = "reject"

@scope("Promise") @val
external all: array<t<'a>> => t<array<'a>> = "all"

@scope("Promise") @val
external all2: ((t<'a>, t<'b>)) => t<('a, 'b)> = "all"

@scope("Promise") @val
external all3: ((t<'a>, t<'b>, t<'c>)) => t<('a, 'b, 'c)> = "all"

@scope("Promise") @val
external all4: ((t<'a>, t<'b>, t<'c>, t<'d>)) => t<('a, 'b, 'c, 'd)> = "all"

@scope("Promise") @val
external all5: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>)) => t<('a, 'b, 'c, 'd, 'e)> = "all"

@scope("Promise") @val
external all6: ((t<'a>, t<'b>, t<'c>, t<'d>, t<'e>, t<'f>)) => t<('a, 'b, 'c, 'd, 'e, 'f)> = "all"

@tag("status")
type settledResult<+'a> =
  | @as("fulfilled") Fulfilled({value: 'a}) | @as("rejected") Rejected({reason: exn})

@scope("Promise") @val
external allSettled: array<promise<'a>> => promise<array<settledResult<'a>>> = "allSettled"

@scope("Promise") @val
external allSettled2: ((promise<'a0>, promise<'a1>)) => promise<(
  settledResult<'a0>,
  settledResult<'a1>,
)> = "allSettled"

@scope("Promise") @val
external allSettled3: ((promise<'a0>, promise<'a1>, promise<'a2>)) => promise<(
  settledResult<'a0>,
  settledResult<'a1>,
  settledResult<'a2>,
)> = "allSettled"

@scope("Promise") @val
external allSettled4: ((promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>)) => promise<(
  settledResult<'a0>,
  settledResult<'a1>,
  settledResult<'a2>,
  settledResult<'a3>,
)> = "allSettled"

@scope("Promise") @val
external allSettled5: (
  (promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>, promise<'a4>)
) => promise<(
  settledResult<'a0>,
  settledResult<'a1>,
  settledResult<'a2>,
  settledResult<'a3>,
  settledResult<'a4>,
)> = "allSettled"

@scope("Promise") @val
external allSettled6: (
  (promise<'a0>, promise<'a1>, promise<'a2>, promise<'a3>, promise<'a4>, promise<'a5>)
) => promise<(
  settledResult<'a0>,
  settledResult<'a1>,
  settledResult<'a2>,
  settledResult<'a3>,
  settledResult<'a4>,
  settledResult<'a5>,
)> = "allSettled"

@send
external _catch: (t<'a>, exn => t<'a>) => t<'a> = "catch"

let catch = (promise: promise<'a>, callback: exn => promise<'a>): promise<'a> => {
  _catch(promise, err => {
    callback(Stdlib_Exn.anyToExnInternal(err))
  })
}

@scope("Promise") @val
external race: array<t<'a>> => t<'a> = "race"

@scope("Promise") @val
external any: array<t<'a>> => t<'a> = "any"

external done: promise<'a> => unit = "%ignore"

external ignore: promise<'a> => unit = "%ignore"
