let d = Js.Dict.empty()

let get1 = d->Js.Dict.get("k")
let get2 = Js.Dict.get(d, "k")

let unsafeGet1 = d->Js.Dict.unsafeGet("k")
let unsafeGet2 = Js.Dict.unsafeGet(d, "k")

let set1 = d->Js.Dict.set("k", 1)
let set2 = Js.Dict.set(d, "k", 1)

let keys1 = d->Js.Dict.keys
let keys2 = Js.Dict.keys(d)

let values1 = d->Js.Dict.values
let values2 = Js.Dict.values(d)

let entries1 = d->Js.Dict.entries
let entries2 = Js.Dict.entries(d)

let dStr: Js.Dict.t<string> = Js.Dict.empty()
let del1 = dStr->Js.Dict.unsafeDeleteKey("k")
let del2 = Js.Dict.unsafeDeleteKey(dStr, "k")

let empty1: Js.Dict.t<int> = Js.Dict.empty()

let fromArray1 = [("a", 1), ("b", 2)]->Js.Dict.fromArray
let fromArray2 = Js.Dict.fromArray([("a", 1), ("b", 2)])

let fromList1 = list{("a", 1), ("b", 2)}->Js.Dict.fromList
let fromList2 = Js.Dict.fromList(list{("a", 1), ("b", 2)})

let map2 = Js.Dict.map(x => x + 1, d)
