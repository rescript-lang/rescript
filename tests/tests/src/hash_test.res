open Belt
open Mocha
open Test_utils

let test_strings = Array.init(32, i => Js.String2.fromCodePoint(i)->Js.String2.repeat(i))

let test_strings_hash_results = [
  0,
  904391063,
  889600889,
  929588010,
  596566298,
  365199070,
  448044845,
  311625091,
  681445541,
  634941451,
  82108334,
  17482990,
  491949228,
  696194769,
  711728152,
  594966620,
  820561748,
  958901713,
  102794744,
  378848504,
  349314368,
  114167579,
  71240932,
  110067399,
  280623927,
  323523937,
  310683234,
  178511779,
  585018975,
  544388424,
  1043872806,
  831138595,
]

let normalize = x => land(x, 0x3FFFFFFF)
let caml_hash = x => normalize(Hash_utils.hash(x))

describe(__MODULE__, () => {
  test("test strings hash results", () => {
    eq(__LOC__, test_strings->Array.map(caml_hash), test_strings_hash_results)
  })

  test("hash 0", () => {
    eq(__LOC__, normalize(Hash_utils.hash(0)), 129913994)
  })

  test("hash x", () => {
    eq(__LOC__, normalize(Hash_utils.hash("x")), 780510073)
  })

  test("hash xy", () => {
    eq(__LOC__, normalize(Hash_utils.hash("xy")), 194127723)
  })
})
