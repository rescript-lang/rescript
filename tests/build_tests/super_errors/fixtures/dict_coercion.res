let dict = Js.Dict.empty()
dict->Js.Dict.set("someKey1", 1)
dict->Js.Dict.set("someKey2", 2)

type fakeDict<'t> = {dictValuesType?: 't}

let d = (dict :> fakeDict<int>)
