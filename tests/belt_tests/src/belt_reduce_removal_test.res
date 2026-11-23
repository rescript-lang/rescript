open Mocha
open Test_utils

/***
`reduce` reads the rest of the container after running its callback, like
`forEach`, so it does not visit entries that the callback has just removed.
*/

module HashMap = Belt.HashMap.Int
module HashSet = Belt.HashSet.Int
module MutableSet = Belt.MutableSet.Int

// With a hint size of 10, the first non-empty bucket holds two of the keys.
let keys = Belt.Array.range(0, 25)

let makeHashMap = () => {
  let m = HashMap.make(~hintSize=10)
  keys->Belt.Array.forEach(k => m->HashMap.set(k, k))
  m
}

let makeHashSet = () => {
  let s = HashSet.make(~hintSize=10)
  keys->Belt.Array.forEach(k => s->HashSet.add(k))
  s
}

let hashMapReduce = () => {
  let m = makeHashMap()
  m->HashMap.reduce([], (visited, key, _) => {
    keys->Belt.Array.forEach(k => k != key ? m->HashMap.remove(k) : ())
    [...visited, key]
  })
}

let hashMapForEach = () => {
  let m = makeHashMap()
  let visited = ref([])
  m->HashMap.forEach((key, _) => {
    keys->Belt.Array.forEach(k => k != key ? m->HashMap.remove(k) : ())
    visited := [...visited.contents, key]
  })
  visited.contents
}

let hashSetReduce = () => {
  let s = makeHashSet()
  s->HashSet.reduce([], (visited, key) => {
    keys->Belt.Array.forEach(k => k != key ? s->HashSet.remove(k) : ())
    [...visited, key]
  })
}

let hashSetForEach = () => {
  let s = makeHashSet()
  let visited = ref([])
  s->HashSet.forEach(key => {
    keys->Belt.Array.forEach(k => k != key ? s->HashSet.remove(k) : ())
    visited := [...visited.contents, key]
  })
  visited.contents
}

let mutableSetReduce = () => {
  let s = MutableSet.fromArray(Belt.Array.range(0, 6))
  s->MutableSet.reduce([], (visited, key) => {
    if key == 3 {
      s->MutableSet.removeMany([4, 5, 6])
    }
    [...visited, key]
  })
}

let mutableSetForEach = () => {
  let s = MutableSet.fromArray(Belt.Array.range(0, 6))
  let visited = ref([])
  s->MutableSet.forEach(key => {
    if key == 3 {
      s->MutableSet.removeMany([4, 5, 6])
    }
    visited := [...visited.contents, key]
  })
  visited.contents
}

describe(__MODULE__, () => {
  test("HashMap.reduce with a callback removing entries", () => {
    eq(__LOC__, hashMapReduce(), hashMapForEach())
  })

  test("HashSet.reduce with a callback removing entries", () => {
    eq(__LOC__, hashSetReduce(), hashSetForEach())
  })

  test("MutableSet.reduce with a callback removing entries", () => {
    eq(__LOC__, mutableSetReduce(), [0, 1, 2, 3])
    eq(__LOC__, mutableSetReduce(), mutableSetForEach())
  })
})
