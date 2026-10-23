let dict = Dict.make()
dict->Dict.set("someKey1", 1)
dict->Dict.set("someKey2", 2)
let asArray = dict->Dict.valuesToArray

let _: dict<int> = dict
