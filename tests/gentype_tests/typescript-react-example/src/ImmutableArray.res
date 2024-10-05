type t<+'a>
module Array = {
  open Belt
  type array2<'a> = (array<'a>, array<'a>)
  external fromT: t<'a> => array<'a> = "%identity"
  external fromTp: t<('a, 'b)> => array<('a, 'b)> = "%identity"
  external fromTT: t<t<'a>> => array<array<'a>> = "%identity"
  external toT: array<'a> => t<'a> = "%identity"
  external toTp: array<('a, 'b)> => t<('a, 'b)> = "%identity"
  external toT2: array2<'a> => (t<'a>, t<'a>) = "%identity"

  /* Conversions involve a copy */

  let fromArray = a => Array.copy(a)->toT

  let toArray = a => Array.copy(a->fromT)

  /* Type-cast immutable functions from Belt.Array. */

  let length = a => Array.length(a->fromT)

  let size = a => Array.size(a->fromT)

  let get = (a, x) => (a->fromT)[x]

  let getExn = (a, x) => Array.getExn(a->fromT, x)

  let getUnsafe = (a, x) => Array.getUnsafe(a->fromT, x)

  let getUndefined = (a, x) => Array.getUndefined(a->fromT, x)

  let shuffle = x => Array.shuffle(x->fromT)->toT

  let reverse = x => Array.reverse(x->fromT)->toT

  let makeUninitialized = x => Array.makeUninitialized(x)->toT

  let makeUninitializedUnsafe = x => Array.makeUninitializedUnsafe(x)->toT

  let make = (x, y) => Array.make(x, y)->toT

  let range = (x, y) => Array.range(x, y)->toT

  let rangeBy = (x, y, ~step) => Array.rangeBy(x, y, ~step)->toT

  let makeByU = (c, f) => Array.makeByU(c, f)->toT
  let makeBy = (c, f) => Array.makeBy(c, x => f(x))->toT

  let makeByAndShuffleU = (c, f) => Array.makeByAndShuffleU(c, f)->toT
  let makeByAndShuffle = (c, f) => Array.makeByAndShuffle(c, x => f(x))->toT

  let zip = (a1, a2) => Array.zip(fromT(a1), fromT(a2))->toTp

  let zipByU = (a1, a2, f) => Array.zipByU(fromT(a1), fromT(a2), f)->toT
  let zipBy = (a1, a2, f) => Array.zipBy(fromT(a1), fromT(a2), (x, y) => f(x, y))->toT

  let unzip = a => Array.unzip(a->fromTp)->toT2

  let concat = (a1, a2) => Array.concat(a1->fromT, a2->fromT)->toT

  let concatMany = (a: t<t<_>>) => Array.concatMany(a->fromTT)->toT

  let slice = (a, ~offset, ~len) => Array.slice(a->fromT, ~offset, ~len)->toT

  let sliceToEnd = (a, b) => Array.sliceToEnd(a->fromT, b)->toT

  let copy = a => Array.copy(a->fromT)->toT

  let forEachU = (a, f) => Array.forEachU(a->fromT, f)
  let forEach = (a, f) => Array.forEach(a->fromT, x => f(x))

  let mapU = (a, f) => Array.mapU(a->fromT, f)->toT
  let map = (a, f) => Array.map(a->fromT, x => f(x))->toT

  let keepWithIndexU = (a, f) => Array.keepWithIndexU(a->fromT, f)->toT
  let keepWithIndex = (a, f) => Array.keepWithIndex(a->fromT, (x, y) => f(x, y))->toT

  let keepMapU = (a, f) => Array.keepMapU(a->fromT, f)->toT
  let keepMap = (a, f) => Array.keepMap(a->fromT, x => f(x))->toT

  let forEachWithIndexU = (a, f) => Array.forEachWithIndexU(a->fromT, f)
  let forEachWithIndex = (a, f) => Array.forEachWithIndex(a->fromT, (x, y) => f(x, y))

  let mapWithIndexU = (a, f) => Array.mapWithIndexU(a->fromT, f)->toT
  let mapWithIndex = (a, f) => Array.mapWithIndex(a->fromT, (x, y) => f(x, y))->toT

  let partitionU = (a, f) => Array.partitionU(a->fromT, f)->toT2
  let partition = (a, f) => Array.partition(a->fromT, x => f(x))->toT2

  let reduceU = (a, b, f) => Array.reduceU(a->fromT, b, f)
  let reduce = (a, b, f) => Array.reduce(a->fromT, b, (x, y) => f(x, y))

  let reduceReverseU = (a, b, f) => Array.reduceReverseU(a->fromT, b, f)
  let reduceReverse = (a, b, f) => Array.reduceReverse(a->fromT, b, (x, y) => f(x, y))

  let reduceReverse2U = (a1, a2, c, f) => Array.reduceReverse2U(fromT(a1), fromT(a2), c, f)
  let reduceReverse2 = (a1, a2, c, f) =>
    Array.reduceReverse2(fromT(a1), fromT(a2), c, (x, y, z) => f(x, y, z))

  let someU = (a, f) => Array.someU(a->fromT, f)
  let some = (a, f) => Array.some(a->fromT, x => f(x))

  let everyU = (a, f) => Array.everyU(a->fromT, f)
  let every = (a, f) => Array.every(a->fromT, x => f(x))

  let every2U = (a1, a2, f) => Array.every2U(fromT(a1), fromT(a2), f)
  let every2 = (a1, a2, f) => Array.every2(fromT(a1), fromT(a2), (x, y) => f(x, y))

  let some2U = (a1, a2, f) => Array.some2U(fromT(a1), fromT(a2), f)
  let some2 = (a1, a2, f) => Array.some2(fromT(a1), fromT(a2), (x, y) => f(x, y))

  let cmpU = (a1, a2, f) => Array.cmpU(fromT(a1), fromT(a2), f)
  let cmp = (a1, a2, f) => Array.cmp(fromT(a1), fromT(a2), (x, y) => f(x, y))

  let eqU = (a1, a2, f) => Array.eqU(fromT(a1), fromT(a2), f)
  let eq = (a1, a2, f) => Array.eq(fromT(a1), fromT(a2), (x, y) => f(x, y))
}

include Array
