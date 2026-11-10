open Mocha
open Test_utils

// A local ref that does not escape compiles to a JS variable that is assigned
// with `=`. A value read from it and stored in a tuple, record or variant, or
// passed to a local function that gets inlined, keeps the value it had then:
// the generated JS binds it to a new variable instead of reading the ref's
// variable again. The same holds for a JS global read through a @val
// external, which JS code may assign. An element or argument that is a
// variable no assignment changes, such as `y`, `label` or `step` below, is
// still read in place, and the checked-in JS shows it without a new binding.

type point = {x: int, label: string}

type pair = Pair(int, string) | Empty

type config = {name: string, version: string, debug: bool}

type restConfig = {version: string, debug: bool}

%%raw(`globalThis.assignedVariableReadCounter = 0`)

@val external globalCounter: int = "assignedVariableReadCounter"
let setGlobalCounter: int => unit = %raw(`n => { globalThis.assignedVariableReadCounter = n }`)

let tuple = y => {
  let x = ref(1)
  let t = (x.contents, y)
  x := 2
  let (a, b) = t
  (a, b, x.contents)
}

let record = label => {
  let x = ref(1)
  let p = {x: x.contents, label}
  x := 2
  (p.x, p.label, x.contents)
}

let variant = () => {
  let x = ref(1)
  let v = Pair(x.contents, "b")
  x := 2
  switch v {
  | Pair(a, _) => (a, x.contents)
  | Empty => (0, x.contents)
  }
}

let elementThenCall = () => {
  let x = ref(1)
  let bump = () => {
    x := x.contents + 1
    x.contents * 10
  }
  let t = (x.contents, bump())
  let (a, b) = t
  (a, b, x.contents)
}

let closuresInLoop = () => {
  let i = ref(0)
  let out = []
  while i.contents < 3 {
    let pair = (i.contents, "x")
    out->Array.push(() => {
      let (n, _) = pair
      n
    })
    i := i.contents + 1
  }
  out->Array.map(get => get())
}

let inlinedCall = step => {
  let count = ref(0)
  let bump = (before, by) => {
    count := count.contents + by
    (before, count.contents)
  }
  bump(count.contents, step)
}

let inlinedCallThroughFunction = () => {
  let count = ref(0)
  let incr = () => count := count.contents + 1
  let report = before => {
    incr()
    (before, count.contents)
  }
  report(count.contents)
}

let argumentThenCall = () => {
  let count = ref(0)
  let bumpAndGet = () => {
    count := count.contents + 1
    count.contents * 10
  }
  let report = (before, after) => (before, after, count.contents)
  report(count.contents, bumpAndGet())
}

let inlinedRestPattern = () => {
  let c = ref({name: "a", version: "1", debug: true})
  let replace = ({name: _, ...restConfig as rest}: config) => {
    c := {name: "b", version: "2", debug: false}
    (rest.version, c.contents.name)
  }
  replace(c.contents)
}

let globalTuple = () => {
  setGlobalCounter(1)
  let t = (globalCounter, "b")
  setGlobalCounter(2)
  let (a, _) = t
  (a, globalCounter)
}

let globalInlinedCall = () => {
  setGlobalCounter(0)
  let report = before => {
    setGlobalCounter(globalCounter + 1)
    (before, globalCounter)
  }
  report(globalCounter)
}

%%private(let topLevelRef = ref(1))
%%private(let topLevelPair = (topLevelRef.contents, "b"))
let () = topLevelRef := 2

let topLevel = () => {
  let (a, _) = topLevelPair
  (a, topLevelRef.contents)
}

// A JS global named like the variable that the element of [t] gets, [t_0].
// The binding must not capture the global it reads.
@val external tZero: int = "t_0"
let setTZero: int => unit = %raw(`n => { globalThis.t_0 = n }`)
let () = setTZero(1)
%%private(let t = (tZero, "b"))
let () = setTZero(2)

let globalNamedLikeElement = () => {
  let (a, _) = t
  (a, tZero)
}

describe(__MODULE__, () => {
  test("a tuple element read from a ref", () => {
    eq(__LOC__, (1, 5, 2), tuple(5))
  })

  test("a record field read from a ref", () => {
    eq(__LOC__, (1, "p", 2), record("p"))
  })

  test("a constructor payload read from a ref", () => {
    eq(__LOC__, (1, 2), variant())
  })

  test("a tuple element read from a ref before a call that assigns the ref", () => {
    eq(__LOC__, (1, 20, 2), elementThenCall())
  })

  test("tuple elements read from a ref by closures created in a loop", () => {
    eq(__LOC__, [0, 1, 2], closuresInLoop())
  })

  test("an argument read from a ref that the inlined body assigns", () => {
    eq(__LOC__, (0, 3), inlinedCall(3))
  })

  test("an argument read from a ref that a function called in the inlined body assigns", () => {
    eq(__LOC__, (0, 1), inlinedCallThroughFunction())
  })

  test("an argument read from a ref before a later argument's call assigns it", () => {
    eq(__LOC__, (0, 10, 1), argumentThenCall())
  })

  test("a field of a rest pattern parameter whose argument is read from a ref", () => {
    eq(__LOC__, ("1", "b"), inlinedRestPattern())
  })

  test("a tuple element read from a JS global", () => {
    eq(__LOC__, (1, 2), globalTuple())
  })

  test("an argument read from a JS global that the inlined body assigns", () => {
    eq(__LOC__, (0, 1), globalInlinedCall())
  })

  test("a tuple element read from a top-level ref", () => {
    eq(__LOC__, (1, 2), topLevel())
  })

  test("a tuple element read from a global named like its variable", () => {
    eq(__LOC__, (1, 2), globalNamedLikeElement())
  })
})
