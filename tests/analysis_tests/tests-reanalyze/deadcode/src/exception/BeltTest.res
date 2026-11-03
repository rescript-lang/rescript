open Belt.List

@throws(Not_found)
let lstHead1 = l => l->Belt.List.headExn

@throws(Not_found)
let lstHead2 = l => l->Belt_List.headExn

@throws(Not_found)
let mapGetExn1 = (s, k) => s->Belt.Map.Int.getExn(k)

@throws(Not_found)
let mapGetExn2 = (s, k) => s->Belt_Map.Int.getExn(k)

@throws(Not_found)
let mapGetExn3 = (s, k) => s->Belt_MapInt.getExn(k)

@throws(Not_found)
let mapGetExn4 = (s, k) => s->Belt.Map.String.getExn(k)

@throws(Not_found)
let mapGetExn5 = (s, k) => s->Belt_Map.String.getExn(k)

@throws(Not_found)
let mapGetExn6 = (s, k) => s->Belt_MapString.getExn(k)

@throws(Not_found)
let mapGetExn7 = (s, k) => s->Belt.Map.getExn(k)

@throws(Not_found)
let mapGetExn8 = (s, k) => s->Belt_Map.getExn(k)

@throws(Not_found)
let stackPopOrThrow = s => Belt.MutableStack.popOrThrow(s)

@throws(Not_found)
let directStackPopOrThrow = s => Belt_MutableStack.popOrThrow(s)

@throws(Not_found)
let mapDictGetOrThrow = (m, k, ~cmp) => Belt.Map.Dict.getOrThrow(m, k, ~cmp)

@throws(Not_found)
let mapDictGetExn = (m, k, ~cmp) => Belt.Map.Dict.getExn(m, k, ~cmp)

@throws(Not_found)
let directMapDictGetOrThrow = (m, k, ~cmp) => Belt_MapDict.getOrThrow(m, k, ~cmp)

@throws(Not_found)
let setDictGetOrThrow = (s, v, ~cmp) => Belt.Set.Dict.getOrThrow(s, v, ~cmp)

@throws(Not_found)
let directSetDictGetOrThrow = (s, v, ~cmp) => Belt_SetDict.getOrThrow(s, v, ~cmp)

@throws(Not_found)
let directMutableSetGetOrThrow = (s, v) => Belt_MutableSet.getOrThrow(s, v)

@throws(Not_found)
let directMutableSetIntGetOrThrow = (s, v) => Belt_MutableSet.Int.getOrThrow(s, v)

@throws(Assert_failure)
let arrayGetExn = (a, i) => Belt.Array.getExn(a, i)

@throws(Not_found)
let queuePopOrThrow = q => Belt.MutableQueue.popOrThrow(q)
