open Mocha
open Test_utils

/***
Closures created in a `for` loop each capture their own iteration's variable,
also when the start isn't safe to delay past a bound that needs statements.
*/

@inline(never) let id = x => x

let closures = () => {
  let fs = []
  for i in id(0) to try {
    id(2)
  } catch {
  | _ => 0
  } {
    fs->Array.push(() => i)
  }
  fs->Array.map(f => f())
}

let closuresDownto = () => {
  let fs = []
  for i in id(2) downto switch id(Some(0)) {
  | Some(n) => n
  | None => 1
  } {
    fs->Array.push(() => i)
  }
  fs->Array.map(f => f())
}

let emptyRange = () => {
  let fs = []
  for i in id(3) to try {
    id(1)
  } catch {
  | _ => 0
  } {
    fs->Array.push(() => i)
  }
  fs->Array.length
}

let startBeforeEnd = () => {
  let order = []
  let note = x => {
    order->Array.push(x)
    x
  }
  let fs = []
  for i in note(1) to try {
    note(3)
  } catch {
  | _ => 0
  } {
    fs->Array.push(() => i)
  }
  (order, fs->Array.map(f => f()))
}

let uppercaseExotic = () => {
  let fs = []
  for \"Start" in id(0) to try {
    id(2)
  } catch {
  | _ => 0
  } {
    fs->Array.push(() => \"Start")
  }
  for \"End" in 3 to id(4) {
    fs->Array.push(() => \"End")
  }
  fs->Array.map(f => f())
}

describe(__MODULE__, () => {
  test("closures capture each iteration", () => {
    eq(__LOC__, closures(), [0, 1, 2])
  })
  test("closures capture each iteration with downto", () => {
    eq(__LOC__, closuresDownto(), [2, 1, 0])
  })
  test("empty range runs no iteration", () => {
    eq(__LOC__, emptyRange(), 0)
  })
  test("the start runs before the end's statements", () => {
    eq(__LOC__, startBeforeEnd(), ([1, 3], [1, 2, 3]))
  })
  test("uppercase exotic loop variables", () => {
    eq(__LOC__, uppercaseExotic(), [0, 1, 2, 3, 4])
  })
})
