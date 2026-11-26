open Mocha
open Test_utils

/***
The temporary a `for` loop binds its end to in the loop header is named after
the loop variable. A JS global the end refers to may have the same name; the
temporary must not capture it, as `let i_finish = i_finish()` would read the
temporary before it is initialized.
*/

%%raw(`
function End_finish() { return 1 }
function i_finish() { return 3 }
`)

@val external endFinish: unit => int = "End_finish"
@val external iFinish: unit => int = "i_finish"

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

describe(__MODULE__, () => {
  test("uppercase exotic loop variable", () => {
    eq(__LOC__, uppercaseExotic(), [0, 1])
  })
  test("lowercase loop variable", () => {
    eq(__LOC__, lowercase(), [2, 3])
  })
})
