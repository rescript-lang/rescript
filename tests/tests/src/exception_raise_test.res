open Mocha
open Test_utils

@@warning("-22")

exception Local
exception B(list<int>)
exception C(int, int)
exception D((int, int))
let appf = (g, x) => {
  module U = {
    exception A(int)
  }
  try g(x) catch {
  | Local => 3
  | Not_found => 2
  | U.A(32) => 3
  | U.A(_) => 3
  | B(list{_, _, x, ..._}) => x
  | C(x, _)
  | D(x, _) => x
  | _ => 4
  }
}

exception A(int)

let f = try %raw(` function () {throw (new Error ("x"))} ()`) catch {
| A(x) => x
| _ => 2
}

let ff = try %raw(` function () {throw 3} ()`) catch {
| A(x) => x
| _ => 2
}

let fff = try %raw(` function () {throw 2} ()`) catch {
| A(x) => x
| _ => 2
}

let a0 = try %raw(` function (){throw 2} () `) catch {
| A(x) => x
| Js.Exn.Error(v) => Obj.magic(v)
| _ => assert(false)
}

let a1: exn = try %raw(` function (){throw 2} () `) catch {
| e => e
}

let a2: exn = try %raw(` function (){throw (new Error("x"))} () `) catch {
| e => e
}

let fff0 = (x, g) =>
  switch x() {
  | exception _ => 1
  | _ => g()
  }
type in_channel
@val external input_line: in_channel => string = "input_line"
let rec input_lines = (ic, acc) =>
  switch input_line(ic) {
  | exception _ => Belt.List.reverse(acc)
  | line => input_lines(ic, list{line, ...acc})
  }

describe(__MODULE__, () => {
  test("exception values", () => {
    eq(__LOC__, (f, ff, fff, a0), (2, 2, 2, 2))
  })

  test("Js.Exn.Error conversion", () => {
    switch a1 {
    | Js.Exn.Error(v) => eq(__LOC__, Obj.magic(v), 2)
    | _ => assert(false)
    }
  })

  test("Js.Exn.asJsExn with raw throw", () => {
    let testValue = try %raw(`()=>{throw 2}`)() catch {
    | e => Js.Exn.asJsExn(e) != None
    }
    eq(__LOC__, testValue, true)
  })

  test("raw function call", () => {
    eq(__LOC__, (%raw(`(a,b,c,_) => a + b + c `): (_, _, _, _) => _)(1, 2, 3, 4), 6)
  })
})
