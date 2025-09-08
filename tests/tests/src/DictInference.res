let dict = Js.Dict.empty()
dict->Js.Dict.set("someKey1", 1)
dict->Js.Dict.set("someKey2", 2)
let asArray = dict->Js.Dict.values

let _: dict<int> = dict
