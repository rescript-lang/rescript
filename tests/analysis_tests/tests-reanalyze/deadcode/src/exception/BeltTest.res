open Belt.List

@raises(Not_found)
let lstHead1 = l => l->Belt.List.headExn

@raises(Not_found)
let lstHead2 = l => l->Belt_List.headExn

@raises(Not_found)
let mapGetExn1 = (s, k) => s->Belt.Map.Int.getExn(k)

@raises(Not_found)
let mapGetExn2 = (s, k) => s->Belt_Map.Int.getExn(k)

@raises(Not_found)
let mapGetExn3 = (s, k) => s->Belt_MapInt.getExn(k)

@raises(Not_found)
let mapGetExn4 = (s, k) => s->Belt.Map.String.getExn(k)

@raises(Not_found)
let mapGetExn5 = (s, k) => s->Belt_Map.String.getExn(k)

@raises(Not_found)
let mapGetExn6 = (s, k) => s->Belt_MapString.getExn(k)

@raises(Not_found)
let mapGetExn7 = (s, k) => s->Belt.Map.getExn(k)

@raises(Not_found)
let mapGetExn8 = (s, k) => s->Belt_Map.getExn(k)
