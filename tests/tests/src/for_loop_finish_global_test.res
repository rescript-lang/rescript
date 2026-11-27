open Mocha
open Test_utils

/***
The temporaries a `for` loop holds its start and its end in are named after
the loop variable. A JS name the loop refers to may be the same; the temporary
must not capture it, as `let i_finish = i_finish()` would read the temporary
before it is initialized. Nor may it rely on reading the name through
`globalThis`: a function declared in this module's `%%raw` is no property of
it.
*/

%%raw(`
function End_finish() { return 1 }
function i_finish() { return 3 }
function j_start() { return 1 }
`)

@val external endFinish: unit => int = "End_finish"
@val external iFinish: unit => int = "i_finish"
@val external jStart: unit => int = "j_start"

let uppercaseExotic = () => {
  let fs = []
  for \"End" in 0 to endFinish() {
    fs->Array.push(() => \"End")
  }
  fs->Array.map(f => f())
}

let lowercase = () => {
  let fs = []
  for i in 2 to iFinish() {
    fs->Array.push(() => i)
  }
  fs->Array.map(f => f())
}

// The start runs before the end's statements, so it is held in a temporary.
let startBeforeStatements = g => {
  let fs = []
  for j in jStart() to {
    try g() catch {
    | _ => 0
    }
  } {
    fs->Array.push(() => j)
  }
  fs->Array.map(f => f())
}

describe(__MODULE__, () => {
  test("uppercase exotic loop variable", () => {
    eq(__LOC__, uppercaseExotic(), [0, 1])
  })
  test("lowercase loop variable", () => {
    eq(__LOC__, lowercase(), [2, 3])
  })
  test("start calling a raw function named like its temporary", () => {
    eq(__LOC__, startBeforeStatements(() => 3), [1, 2, 3])
  })
})
