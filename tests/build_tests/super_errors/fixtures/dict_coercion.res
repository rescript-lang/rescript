let dict = Dict.make()
dict->Dict.set("someKey1", 1)
dict->Dict.set("someKey2", 2)

type fakeDict<'t> = {dictValuesType?: 't}

let d = (dict :> fakeDict<int>)
