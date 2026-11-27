open Mocha
open Test_utils

// Iterator-like types are covariant in their yield and return types and
// contravariant in their resume type, so these coercions type-check.
let widenIterator = (i: Iterator.t<[#A], [#R], [#X | #Y]>) =>
  (i :> Iterator.t<[#A | #B], [#R | #S], [#X]>)

let widenAsyncIterator = (i: AsyncIterator.t<[#A], [#R], [#X | #Y]>) =>
  (i :> AsyncIterator.t<[#A | #B], [#R | #S], [#X]>)

let widenRawResult = (r: Iterator.rawResult<[#A], [#R]>) =>
  (r :> Iterator.rawResult<[#A | #B], [#R | #S]>)

let widenAsyncRawResult = (r: AsyncIterator.rawResult<[#A], [#R]>) =>
  (r :> AsyncIterator.rawResult<[#A | #B], [#R | #S]>)

let widenGenerator = (g: Generator.t<[#A], [#R], [#X | #Y]>) =>
  (g :> Generator.t<[#A | #B], [#R | #S], [#X]>)

let widenAsyncGenerator = (g: AsyncGenerator.t<[#A], [#R], [#X | #Y]>) =>
  (g :> AsyncGenerator.t<[#A | #B], [#R | #S], [#X]>)

let widenIterableIterator = (i: IterableIterator.t<[#A], [#R], [#X | #Y]>) =>
  (i :> IterableIterator.t<[#A | #B], [#R | #S], [#X]>)

let widenAsyncIterableIterator = (i: AsyncIterableIterator.t<[#A], [#R], [#X | #Y]>) =>
  (i :> AsyncIterableIterator.t<[#A | #B], [#R | #S], [#X]>)

let widenIteratorObject = (i: IteratorObject.t<[#A], [#R], [#X | #Y]>) =>
  (i :> IteratorObject.t<[#A | #B], [#R | #S], [#X]>)

let widenArrayLike = (a: Array.arrayLike<[#A]>) => (a :> Array.arrayLike<[#A | #B]>)

// A tag is contravariant in its parameters and covariant in its output.
let widenTaggedTemplate = (t: taggedTemplate<[#A | #B], [#R]>) =>
  (t :> taggedTemplate<[#A], [#R | #S]>)

// A library type defined as an alias of `Generator.t` stays polymorphic under
// the relaxed value restriction, so one value can be used at several types.
module Effect: {
  type step<+'e>
  type t<'a, 'e> = Generator.t<step<'e>, 'a, unknown>
  let fail: 'e => t<'a, 'e>
  let map: (t<'a, 'e>, 'a => 'b) => t<'b, 'e>
} = {
  type step<+'e>
  type t<'a, 'e> = Generator.t<step<'e>, 'a, unknown>
  let fail = _ => %raw(`(function* () {})()`)
  let map = (m, _) => Obj.magic(m)
}

let failing = Effect.fail(#A)->Effect.map(x => x + 1)
let useWithB: Effect.t<int, [#A | #B]> = failing
let useWithC: Effect.t<int, [#A | #C]> = failing

let letters: unit => Generator.t<[#A], unit, unit> = %raw(`function* () {
  yield "A"
}`)

let joinTag: taggedTemplate<[#x | #y], string> = TaggedTemplate.make((
  strings,
  params: array<[#x | #y]>,
) => strings->Array.join("|") ++ ":" ++ params->Array.map(p => (p :> string))->Array.join(","))

describe(__MODULE__, () => {
  test("a widened generator yields the same values", () => {
    let widened = (letters() :> Generator.t<[#A | #B], unit, unit>)
    let seen = []
    for x of widened {
      seen->Array.push(x)
    }
    eq(__LOC__, [#A], seen)
  })

  test("a tag accepting more parameters is used where fewer are expected", () => {
    let onlyX = (joinTag :> taggedTemplate<[#x], string>)
    eq(__LOC__, "a|b:x", onlyX`a${#x}b`)
  })
})
