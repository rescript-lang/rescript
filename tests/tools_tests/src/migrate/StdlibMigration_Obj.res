let empty1 = Js.Obj.empty()

let assign1 = Js.Obj.empty()->Js.Obj.assign({"a": 1})
let assign2 = Js.Obj.assign(Js.Obj.empty(), {"a": 1})

let keys1 = {"a": 1, "b": 2}->Js.Obj.keys
let keys2 = Js.Obj.keys({"a": 1, "b": 2})
