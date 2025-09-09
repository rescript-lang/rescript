open Mocha
open Test_utils

@@warning("-107")

let max_int = 2147483647 // 0x80000000
let min_int = -2147483648 // 0x7FFFFFFF

let hash_variant = s => {
  let accu = ref(0)
  for i in 0 to String.length(s) - 1 {
    accu := land(223 * accu.contents + String.codePointAt(s, i)->Option.getUnsafe, lsl(1, 31) - 1)
    /* Here accu is 31 bits, times 223 will not be than 53 bits..
       TODO: we can use `Sys.backend_type` for patching
 */
  }

  /* reduce to 31 bits */
  /* accu := !accu land (1 lsl 31 - 1); */
  /* make it signed for 64 bits architectures */
  if accu.contents > 0x3FFFFFFF {
    lor(accu.contents - lsl(1, 31), 0)
  } else {
    accu.contents
  }
}

let hash_variant2 = s => {
  let accu = ref(0)
  for i in 0 to String.length(s) - 1 {
    accu := 223 * accu.contents + String.codePointAt(s, i)->Option.getUnsafe
  }
  /* reduce to 31 bits */
  accu := land(accu.contents, lsl(1, 31) - 1)

  /* make it signed for 64 bits architectures */
  if accu.contents > 0x3FFFFFFF {
    accu.contents - lsl(1, 31)
  } else {
    accu.contents
  }
}

let rec fib = x =>
  switch x {
  | 0 | 1 => 1
  | n => fib(n - 1l) + fib(n - 2)
  }

describe(__MODULE__, () => {
  test("plus_overflow", () => eq(__LOC__, true, max_int + 1 == min_int))
  test("minus_overflow", () => eq(__LOC__, true, min_int - 1 == max_int))
  test("flow_again1", () => eq(__LOC__, 2147483646, max_int + max_int + min_int))
  test("flow_again2", () => eq(__LOC__, -2, max_int + max_int))
  test("hash_test", () => eq(__LOC__, hash_variant("xxyyzzuuxxzzyy00112233"), 544087776))
  test("hash_test2", () => eq(__LOC__, hash_variant("xxyyzxzzyy"), -449896130))
  test("hash_variant_test1", () => eq(__LOC__, hash_variant2("xxyyzzuuxxzzyy00112233"), 544087776))
  test("hash_variant_test2", () => eq(__LOC__, hash_variant2("xxyyzxzzyy"), -449896130))
  test("int_literal_flow", () => eq(__LOC__, -1, 0xffffffff))
  test("int_literal_flow2", () => eq(__LOC__, -1, -1))
  test("float_conversion_test1", () => eq(__LOC__, int_of_float(Js.Float.fromString("3")), 3))
  test("float_conversion_test2", () => eq(__LOC__, int_of_float(Js.Float.fromString("3.2")), 3))
})
