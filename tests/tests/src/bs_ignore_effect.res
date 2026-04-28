open Mocha
open Test_utils

%%raw(`
function add(x,y){
  return x + y
}
`)
type rec kind<_> =
  | Float: kind<float>
  | String: kind<string>
@val external add: (@ignore kind<'a>, 'a, 'a) => 'a = "add"

let v = ref(0)

@obj external config: (~hi: int, ~lo: int, unit) => _ = ""

let h = config(~hi=2, ~lo=0, ignore(incr(v)))
let z = add(
  {
    incr(v)
    Float
  },
  3.0,
  2.0,
)

describe(__MODULE__, () => {
  test("ignore effect 1", () => eq(__LOC__, v.contents, 2))
  test("ignore effect 2", () => eq(__LOC__, z, 5.0))
})
