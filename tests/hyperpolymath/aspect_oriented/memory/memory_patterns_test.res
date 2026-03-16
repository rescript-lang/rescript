// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Aspect-oriented test: Memory patterns
// Cross-cutting concern: closure captures, reference cells, mutable state,
// and data structure creation/cleanup patterns.

let eq = (a, b) => a == b

// ─── Closure capture correctness ────────────────────────────────────
// Each closure must capture its own copy of the variable
let closureCapture = {
  let fns = []
  for i in 0 to 4 {
    let captured = i
    ignore(fns->Array.push(() => captured))
  }
  fns->Array.map(f => f())
}
Test.run(__POS_OF__("memory: closure capture"), closureCapture, eq, [0, 1, 2, 3, 4])

// Closure over mutable ref
let closureRef = {
  let r = ref(0)
  let get = () => r.contents
  let set = v => r := v
  set(42)
  get()
}
Test.run(__POS_OF__("memory: closure ref"), closureRef, eq, 42)

// Nested closures
let nestedClosure = {
  let outer = 10
  let mid = () => {
    let middle = 20
    let inner = () => outer + middle
    inner()
  }
  mid()
}
Test.run(__POS_OF__("memory: nested closure"), nestedClosure, eq, 30)

// ─── Reference cell patterns ────────────────────────────────────────
let refCell = ref(0)
Test.run(__POS_OF__("memory: ref initial"), refCell.contents, eq, 0)

refCell := 10
Test.run(__POS_OF__("memory: ref assign"), refCell.contents, eq, 10)

refCell.contents = 20
Test.run(__POS_OF__("memory: ref contents"), refCell.contents, eq, 20)

// Ref in loop
let loopRef = ref(0)
for i in 1 to 10 {
  loopRef := loopRef.contents + i
}
Test.run(__POS_OF__("memory: ref in loop"), loopRef.contents, eq, 55)

// ─── Mutable record field patterns ──────────────────────────────────
type counter = {mutable count: int, label: string}

let c = {count: 0, label: "clicks"}
c.count = c.count + 1
c.count = c.count + 1
c.count = c.count + 1
Test.run(__POS_OF__("memory: mutable record"), c.count, eq, 3)
Test.run(__POS_OF__("memory: immutable preserved"), c.label, eq, "clicks")

// ─── Large data structure creation ──────────────────────────────────
// Create and consume a large array
let largeArray = Array.fromInitializer(~length=10000, i => i)
let largeSum = largeArray->Array.reduce(0, (a, b) => a + b)
Test.run(__POS_OF__("memory: large array sum"), largeSum, eq, 49995000)

// Create nested structure
let nestedArrays = Array.fromInitializer(~length=100, i =>
  Array.fromInitializer(~length=100, j => i * 100 + j)
)
Test.run(__POS_OF__("memory: nested array access"), nestedArrays[50][50], eq, 5050)
Test.run(
  __POS_OF__("memory: nested array length"),
  Array.length(nestedArrays) * Array.length(nestedArrays[0]),
  eq,
  10000,
)

// ─── Buffer accumulation patterns ───────────────────────────────────
let accumulate = n => {
  let parts = []
  for i in 0 to n - 1 {
    ignore(parts->Array.push(Int.toString(i)))
  }
  parts->Array.join(",")
}
let accumulated = accumulate(100)
Test.run(__POS_OF__("memory: accumulation start"), String.startsWith(accumulated, "0,1,2"), eq, true)
Test.run(
  __POS_OF__("memory: accumulation end"),
  String.endsWith(accumulated, "98,99"),
  eq,
  true,
)

// ─── Dict memory patterns ──────────────────────────────────────────
// Create, populate, and iterate a large dict
let d = Dict.make()
for i in 0 to 999 {
  Dict.set(d, Int.toString(i), i * i)
}
Test.run(__POS_OF__("memory: dict size"), Array.length(Dict.keysToArray(d)), eq, 1000)
Test.run(__POS_OF__("memory: dict lookup"), Dict.get(d, "500"), eq, Some(250000))

// ─── Linked list patterns (functional) ──────────────────────────────
type rec flist<'a> = Nil | Cons('a, flist<'a>)

let rec fromArray = arr =>
  switch Array.length(arr) {
  | 0 => Nil
  | _ =>
    let rest = arr->Array.slice(~start=1, ~end=Array.length(arr))
    Cons(arr[0], fromArray(rest))
  }

let rec toArray = lst =>
  switch lst {
  | Nil => []
  | Cons(x, rest) => Array.concat([x], toArray(rest))
  }

let rec fmap = (lst, f) =>
  switch lst {
  | Nil => Nil
  | Cons(x, rest) => Cons(f(x), fmap(rest, f))
  }

let flist = fromArray([1, 2, 3, 4, 5])
Test.run(__POS_OF__("memory: flist roundtrip"), toArray(flist), eq, [1, 2, 3, 4, 5])
Test.run(
  __POS_OF__("memory: flist map"),
  toArray(fmap(flist, x => x * 2)),
  eq,
  [2, 4, 6, 8, 10],
)

// ─── Tree memory patterns ──────────────────────────────────────────
type rec tree<'a> = Leaf | Branch(tree<'a>, 'a, tree<'a>)

let rec insertBST = (t, v) =>
  switch t {
  | Leaf => Branch(Leaf, v, Leaf)
  | Branch(l, x, r) =>
    if v < x {
      Branch(insertBST(l, v), x, r)
    } else if v > x {
      Branch(l, x, insertBST(r, v))
    } else {
      t
    }
  }

let rec treeSize = t =>
  switch t {
  | Leaf => 0
  | Branch(l, _, r) => 1 + treeSize(l) + treeSize(r)
  }

// Build a tree with many nodes
let bigTree = {
  let t = ref(Leaf)
  // Insert in a pattern that creates a somewhat balanced tree
  let values = [50, 25, 75, 12, 37, 62, 87, 6, 18, 31, 43, 56, 68, 81, 93]
  values->Array.forEach(v => t := insertBST(t.contents, v))
  t.contents
}
Test.run(__POS_OF__("memory: tree size"), treeSize(bigTree), eq, 15)

// ─── Promise resolution chain ──────────────────────────────────────
// Chain of promises should resolve correctly
let _ = Promise.resolve(1)
  ->Promise.then(v => Promise.resolve(v + 1))
  ->Promise.then(v => Promise.resolve(v * 2))
  ->Promise.then(v => {
    Test.run(__POS_OF__("memory: promise chain"), v, eq, 4)
    Promise.resolve()
  })

// ─── Recursive data construction ────────────────────────────────────
// Build a recursive structure and traverse it
type rec nestedObj = {value: int, children: array<nestedObj>}

let makeTree = (depth, value) =>
  if depth <= 0 {
    {value, children: []}
  } else {
    {
      value,
      children: [
        makeTree(depth - 1, value * 2),
        makeTree(depth - 1, value * 2 + 1),
      ],
    }
  }

let rec countNodes = obj =>
  1 + obj.children->Array.reduce(0, (acc, child) => acc + countNodes(child))

let tree4 = makeTree(4, 1)
// Full binary tree of depth 4: 2^5 - 1 = 31 nodes
Test.run(__POS_OF__("memory: recursive tree"), countNodes(tree4), eq, 31)
