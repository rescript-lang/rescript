// A cache of a lookup that may find nothing: the cached value is option<int>,
// so a cache read is option<option<int>>.
let find = (xs: array<int>, k: int) => xs->Array.find(x => x == k)

let cached = (cache: dict<option<int>>, xs, k) =>
  cache->Dict.get(Int.toString(k))->Option.getOr(find(xs, k))
