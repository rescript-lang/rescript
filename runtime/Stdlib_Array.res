type t<'a> = array<'a>
type arrayLike<'a>

@new external makeUninitializedUnsafe: int => array<'a> = "Array"
@set external truncateToLengthUnsafe: (array<'a>, int) => unit = "length"
external getUnsafe: (array<'a>, int) => 'a = "%array_unsafe_get"
external setUnsafe: (array<'a>, int, 'a) => unit = "%array_unsafe_set"

external unsafe_get: (array<'a>, int) => 'a = "%array_unsafe_get"

@val external fromIterator: Stdlib_Iterator.t<'a> => array<'a> = "Array.from"
@val external fromArrayLike: arrayLike<'a> => array<'a> = "Array.from"
@val
external fromArrayLikeWithMap: (arrayLike<'a>, 'a => 'b) => array<'b> = "Array.from"

@send external fillAll: (array<'a>, 'a) => unit = "fill"

@send external fillToEnd: (array<'a>, 'a, ~start: int) => unit = "fill"

@send external fill: (array<'a>, 'a, ~start: int, ~end: int) => unit = "fill"

let make = (~length, x) =>
  if length <= 0 {
    []
  } else {
    let arr = makeUninitializedUnsafe(length)
    arr->fillAll(x)
    arr
  }

let fromInitializer = (~length, f) =>
  if length <= 0 {
    []
  } else {
    let arr = makeUninitializedUnsafe(length)
    for i in 0 to length - 1 {
      arr->setUnsafe(i, f(i))
    }
    arr
  }

@val external isArray: 'a => bool = "Array.isArray"

@get external length: array<'a> => int = "length"

let rec equalFromIndex = (a, b, i, eq, len) =>
  if i === len {
    true
  } else if eq(a->getUnsafe(i), b->getUnsafe(i)) {
    equalFromIndex(a, b, i + 1, eq, len)
  } else {
    false
  }

let equal = (a, b, eq) => {
  let len = a->length
  if len === b->length {
    equalFromIndex(a, b, 0, eq, len)
  } else {
    false
  }
}

let rec compareFromIndex = (a, b, i, cmp, len) =>
  if i === len {
    Stdlib_Ordering.equal
  } else {
    let c = cmp(a->getUnsafe(i), b->getUnsafe(i))
    if c == Stdlib_Ordering.equal {
      compareFromIndex(a, b, i + 1, cmp, len)
    } else {
      c
    }
  }

let compare = (a, b, cmp) => {
  let lenA = a->length
  let lenB = b->length
  lenA < lenB
    ? Stdlib_Ordering.less
    : lenA > lenB
    ? Stdlib_Ordering.greater
    : compareFromIndex(a, b, 0, cmp, lenA)
}

@send external copyAllWithin: (array<'a>, ~target: int) => array<'a> = "copyWithin"

@send
external copyWithinToEnd: (array<'a>, ~target: int, ~start: int) => array<'a> = "copyWithin"

@send
external copyWithin: (array<'a>, ~target: int, ~start: int, ~end: int) => array<'a> = "copyWithin"

@send external pop: array<'a> => option<'a> = "pop"

@send external push: (array<'a>, 'a) => unit = "push"

@variadic @send external pushMany: (array<'a>, array<'a>) => unit = "push"

@send external reverse: array<'a> => unit = "reverse"
@send external toReversed: array<'a> => array<'a> = "toReversed"

@send external shift: array<'a> => option<'a> = "shift"

@variadic @send
external splice: (array<'a>, ~start: int, ~remove: int, ~insert: array<'a>) => unit = "splice"
@variadic @send
external toSpliced: (array<'a>, ~start: int, ~remove: int, ~insert: array<'a>) => array<'a> =
  "toSpliced"

@send
external removeInPlace: (array<'a>, int, @as(1) _) => unit = "splice"

@send external with: (array<'a>, int, 'a) => array<'a> = "with"

@send external unshift: (array<'a>, 'a) => unit = "unshift"

@variadic @send external unshiftMany: (array<'a>, array<'a>) => unit = "unshift"

@send external concat: (array<'a>, array<'a>) => array<'a> = "concat"
@variadic @send external concatMany: (array<'a>, array<array<'a>>) => array<'a> = "concat"

@send external flat: array<array<'a>> => array<'a> = "flat"

@send external includes: (array<'a>, 'a) => bool = "includes"

@send external indexOf: (array<'a>, 'a) => int = "indexOf"
let indexOfOpt = (arr, item) =>
  switch arr->indexOf(item) {
  | -1 => None
  | index => Some(index)
  }
@send external indexOfFrom: (array<'a>, 'a, int) => int = "indexOf"

@send external join: (array<string>, string) => string = "join"

@deprecated("Use `join` instead") @send
external joinWith: (array<string>, string) => string = "join"

@send external joinUnsafe: (array<'a>, string) => string = "join"

@deprecated("Use `joinUnsafe` instead") @send
external joinWithUnsafe: (array<'a>, string) => string = "join"

@send external lastIndexOf: (array<'a>, 'a) => int = "lastIndexOf"
let lastIndexOfOpt = (arr, item) =>
  switch arr->lastIndexOf(item) {
  | -1 => None
  | index => Some(index)
  }
@send external lastIndexOfFrom: (array<'a>, 'a, int) => int = "lastIndexOf"

@send external slice: (array<'a>, ~start: int, ~end: int) => array<'a> = "slice"
@send external sliceToEnd: (array<'a>, ~start: int) => array<'a> = "slice"
@send external copy: array<'a> => array<'a> = "slice"

@send external sort: (array<'a>, ('a, 'a) => Stdlib_Ordering.t) => unit = "sort"
@send external toSorted: (array<'a>, ('a, 'a) => Stdlib_Ordering.t) => array<'a> = "toSorted"

@send external toString: array<'a> => string = "toString"
@send external toLocaleString: array<'a> => string = "toLocaleString"

@send external every: (array<'a>, 'a => bool) => bool = "every"
@send external everyWithIndex: (array<'a>, ('a, int) => bool) => bool = "every"

@send external filter: (array<'a>, 'a => bool) => array<'a> = "filter"
@send external filterWithIndex: (array<'a>, ('a, int) => bool) => array<'a> = "filter"

@send external find: (array<'a>, 'a => bool) => option<'a> = "find"
@send external findWithIndex: (array<'a>, ('a, int) => bool) => option<'a> = "find"

@send external findIndex: (array<'a>, 'a => bool) => int = "findIndex"
@send external findIndexWithIndex: (array<'a>, ('a, int) => bool) => int = "findIndex"

@send external forEach: (array<'a>, 'a => unit) => unit = "forEach"
@send external forEachWithIndex: (array<'a>, ('a, int) => unit) => unit = "forEach"

@send external map: (array<'a>, 'a => 'b) => array<'b> = "map"
@send external mapWithIndex: (array<'a>, ('a, int) => 'b) => array<'b> = "map"

@send external reduce: (array<'b>, ('a, 'b) => 'a, 'a) => 'a = "reduce"
let reduce = (arr, init, f) => reduce(arr, f, init)
@send external reduceWithIndex: (array<'b>, ('a, 'b, int) => 'a, 'a) => 'a = "reduce"
let reduceWithIndex = (arr, init, f) => reduceWithIndex(arr, f, init)
@send
external reduceRight: (array<'b>, ('a, 'b) => 'a, 'a) => 'a = "reduceRight"
let reduceRight = (arr, init, f) => reduceRight(arr, f, init)
@send
external reduceRightWithIndex: (array<'b>, ('a, 'b, int) => 'a, 'a) => 'a = "reduceRight"
let reduceRightWithIndex = (arr, init, f) => reduceRightWithIndex(arr, f, init)

@send external some: (array<'a>, 'a => bool) => bool = "some"
@send external someWithIndex: (array<'a>, ('a, int) => bool) => bool = "some"

@get_index external get: (array<'a>, int) => option<'a> = ""
@set_index external set: (array<'a>, int, 'a) => unit = ""

@get_index external getSymbol: (array<'a>, Stdlib_Symbol.t) => option<'b> = ""
@get_index external getSymbolUnsafe: (array<'a>, Stdlib_Symbol.t) => 'b = ""
@set_index external setSymbol: (array<'a>, Stdlib_Symbol.t, 'b) => unit = ""

let findIndexOpt = (array: array<'a>, finder: 'a => bool): option<int> =>
  switch findIndex(array, finder) {
  | -1 => None
  | index => Some(index)
  }

let swapUnsafe = (xs, i, j) => {
  let tmp = getUnsafe(xs, i)
  setUnsafe(xs, i, getUnsafe(xs, j))
  setUnsafe(xs, j, tmp)
}

module M = {
  @val external floor: float => float = "Math.floor"
  @val external random: unit => float = "Math.random"
  external fromFloat: float => int = "%intoffloat"
  external toFloat: int => float = "%identity"

  let random_int: (int, int) => int = (min, max) =>
    floor(random() *. toFloat(max - min))->fromFloat + min
}

let shuffle = xs => {
  let len = length(xs)
  for i in 0 to len - 1 {
    swapUnsafe(xs, i, M.random_int(i, len)) /* [i,len) */
  }
}

let toShuffled = xs => {
  let result = copy(xs)
  shuffle(result)
  result
}

let filterMap = (a, f) => {
  let l = length(a)
  let r = makeUninitializedUnsafe(l)
  let j = ref(0)
  for i in 0 to l - 1 {
    let v = getUnsafe(a, i)
    switch f(v) {
    | None => ()
    | Some(v) =>
      setUnsafe(r, j.contents, v)
      j.contents = j.contents + 1
    }
  }
  truncateToLengthUnsafe(r, j.contents)
  r
}

let keepSome = filterMap(_, x => x)

@send external flatMap: (array<'a>, 'a => array<'b>) => array<'b> = "flatMap"
@send external flatMapWithIndex: (array<'a>, ('a, int) => array<'b>) => array<'b> = "flatMap"

let findMap = (arr, f) => {
  let rec loop = i =>
    if i == arr->length {
      None
    } else {
      switch f(getUnsafe(arr, i)) {
      | None => loop(i + 1)
      | Some(_) as r => r
      }
    }

  loop(0)
}

@send external at: (array<'a>, int) => option<'a> = "at"

let last = a => a->get(a->length - 1)

external ignore: array<'a> => unit = "%ignore"
