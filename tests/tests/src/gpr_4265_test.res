let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

open Belt
let mockMap = MutableMap.Int.make()
let add = id => {
  mockMap->MutableMap.Int.set(id, id)
  id
}
let remove = id => mockMap->MutableMap.Int.remove(id)
let _ = add(1726)
let n = add(6667)
let _ = add(486)
let _ = remove(1726)
let n1 = mockMap->MutableMap.Int.getExn(6667)

eq(__LOC__, n, n1)

Mt.from_pair_suites(__FILE__, suites.contents)
