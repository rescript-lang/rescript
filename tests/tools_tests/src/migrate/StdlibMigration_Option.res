let someCall = Js.Option.some(3)
let somePiped = 3->Js.Option.some

let isSome1 = Some(1)->Js.Option.isSome
let isSome2 = Js.Option.isSome(None)

let isNone1 = None->Js.Option.isNone
let isNone2 = Js.Option.isNone(Some(2))

let eq = (a: int, b: int) => a == b
// let isSomeValue1 = Js.Option.isSomeValue(eq, 2, Some(2))

let getExn1 = Js.Option.getExn(Some(3))
let getExn2 = Some(3)->Js.Option.getExn

let equal1 = Js.Option.equal(eq, Some(2), Some(2))

let f = (x: int) => x > 0 ? Some(x + 1) : None
let andThen1 = Js.Option.andThen(f, Some(2))

let map1 = Js.Option.map(x => x * 2, Some(2))

let getWithDefault1 = Js.Option.getWithDefault(0, Some(2))

let default1 = Js.Option.default(0, Some(2))

let filter1 = Js.Option.filter(x => x > 0, Some(1))

let firstSome1 = Js.Option.firstSome(Some(1), None)
let firstSome2 = Some(1)->Js.Option.firstSome(None)

// Type alias migration
let optT: Js.Option.t<int> = Some(1)
