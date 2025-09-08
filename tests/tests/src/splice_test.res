open Belt

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

module Caml_splice_call = {}
@variadic @val external f: (int, array<int>) => int = "Math.max"

f(1, [])->ignore

@variadic @send external send: (int, array<int>) => int = "send"

let f00 = (a, b) => a->send([b])

@send @variadic external push: (array<int>, int, array<int>) => unit = "push"

/* This is only test, the binding maybe wrong
  since in OCaml array'length is not mutable
*/
let () = {
  let a = []
  a->push(1, [2, 3, 4])

  eq(__LOC__, a, [1, 2, 3, 4])
}

let dynamic = arr => {
  let a = []
  a->push(1, arr)
  eq(__LOC__, a, Array.concatMany([[1], arr]))
}

dynamic([2, 3, 4])
dynamic([])
dynamic([1, 1, 3])

/* Array constructor with a single parameter `x`
   just makes an array with its length set to `x`,
   so at least two parameters are needed
*/
@variadic @new external newArr: (int, int, array<int>) => array<int> = "Array"

let () = {
  let a = newArr(1, 2, [3, 4])
  eq(__LOC__, a, [1, 2, 3, 4])
}

let dynamicNew = arr => {
  let a = newArr(1, 2, arr)
  eq(__LOC__, a, Array.concatMany([[1, 2], arr]))
}

dynamicNew([3, 4])
dynamicNew([])
dynamicNew([1, 3])

%%raw(`
class Foo {
  constructor(...names) {
    this.names = names;
  }
}
`)

type foo

@variadic @new external newFoo: array<string> => foo = "Foo"
@get external fooNames: foo => array<string> = "names"

let () = {
  let f = newFoo(["a", "b", "c"])
  eq(__LOC__, fooNames(f), ["a", "b", "c"])
}

let dynamicFoo = arr => {
  let f = newFoo(arr)
  eq(__LOC__, fooNames(f), arr)
}

dynamicFoo([])
dynamicFoo(["a"])
dynamicFoo(["a", "b", "c"])

module Pipe = {
  @send @variadic external push: (array<int>, int, array<int>) => unit = "push"

  /* This is only test, the binding maybe wrong
     since in OCaml array'length is not mutable
 */
  let () = {
    let a = []
    a->push(1, [2, 3, 4])

    eq(__LOC__, a, [1, 2, 3, 4])
  }

  let dynamic = arr => {
    let a = []
    a->push(1, arr)
    eq(__LOC__, a, Array.concatMany([[1], arr]))
  }

  dynamic([2, 3, 4])
  dynamic([])
  dynamic([1, 1, 3])
}

let f1 = (c: array<int>) => f(1, c)

eq(__LOC__, f1([2, 3]), 3)
eq(__LOC__, f1([]), 1)
eq(__LOC__, f1([1, 2, 3, 4, 5, 2, 3]), 5)

Mt.from_pair_suites(__FILE__, suites.contents)
