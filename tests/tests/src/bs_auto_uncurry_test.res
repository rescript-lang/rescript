open Mocha
open Test_utils

@send external map: (array<'a>, 'a => 'b) => array<'b> = "map"

%%raw(`
function hi (cb){
    cb ();
    return 0;
}
`)

@val external hi: (unit => unit) => unit = "hi"

describe(__MODULE__, () => {
  test("callback_test", () => {
    let xs = ref(list{})
    hi((() as x) => xs := list{x, ...xs.contents})
    hi((() as x) => xs := list{x, ...xs.contents})
    eq(__LOC__, xs.contents, list{(), ()})
  })

  test("array_operations_test", () => {
    eq(__LOC__, [1, 2, 3]->map(x => x + 1), [2, 3, 4])
    eq(__LOC__, [1, 2, 3]->Js.Array2.map(x => x + 1), [2, 3, 4])
    eq(__LOC__, [1, 2, 3]->Js.Array2.reduce((x, y) => x + y, 0), 6)
    eq(__LOC__, [1, 2, 3]->Js.Array2.reducei((x, y, i) => x + y + i, 0), 9)
    eq(__LOC__, [1, 2, 3]->Js.Array2.some(x => x < 1), false)
    eq(__LOC__, [1, 2, 3]->Js.Array2.every(x => x > 0), true)
  })
})
