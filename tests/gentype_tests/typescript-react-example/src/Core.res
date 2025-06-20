@genType
let null0 = (x: Js.null<int>) => x

@genType
let null1 = (x: Null.t<int>) => x

@genType
let nullable0 = (x: Js.nullable<int>) => x

@genType
let nullable1 = (x: Nullable.t<int>) => x

@genType
let undefined0 = (x: Js.undefined<int>) => x

@genType
let undefined1 = (x: Undefined.t<int>) => x

@genType
let dict0 = (x: dict<string>) => x

@genType
let dict1 = (x: dict<string>) => x

@genType
let promise0 = (x: promise<string>) => x

@genType
let promise1 = (x: Promise.t<string>) => x

@genType
let date0 = (x: Js.Date.t) => x

@genType
let date1 = (x: Date.t) => x

@genType
let bigint0 = (x: bigint) => x

@genType
let regexp0 = (x: Js.Re.t) => x

@genType
let regexp1 = (x: RegExp.t) => x

module Map = Map_
module Set = Set_

@genType
let map1 = (x: Map.t<string, int>) => x

@genType
let weakmap1 = (x: WeakMap.t<array<int>, int>) => x

@genType
let set1 = (x: Set.t<string>) => x

@genType
let weakset1 = (x: WeakSet.t<array<int>>) => x

type variant = A | B(string)

@genType
let option0 = (x: option<string>) => x

@genType
let option1 = (x: option<variant>) => x

@genType
type t1 = {x?: string}

@genType
type t2 = {x: Js.undefined<string>}

@genType.import("./CoreTS")
external someFunWithNullThenOptionalArgs: (
  Null.t<string> /* Cannot be Nullable.t or option */,
  option<string> /* Cannot be Null.t or Nullable.t */,
) => string = "someFunWithNullThenOptionalArgs"

@genType.import("./CoreTS")
external someFunWithNullUndefinedArg: (
  Nullable.t<string> /* Can also be Null.t or option as they are subtypes */,
  int,
) => string = "someFunWithNullUndefinedArg"

@genType
let jsonEncodeString1 = JSON.Encode.string("hello")

@genType
let jsonEncodeString2: JSON.t = JSON.Encode.string("hello")
