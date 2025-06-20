let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

module rec Int32: {
  type t
  type buffer
  @get external buffer: t => buffer = "buffer"
  @get_index external get: (t, int) => int = ""
  @set_index external set: (t, int, int) => unit = ""
  @new external create: array<int> => t = "Int32Array"
  @new external of_buffer: buffer => t = "Int32Array"
} = Int32 /* Int32 is compiled away in 4.06 */

module Xx: {
  let f: (int, int) => int
} = {
  external f: (int, int) => int = "hfiehi"
}

let uuu = Xx.f

module rec Int3: {
  let u: int => int
} = Int3

module A = {
  module rec Inta: {
    let a: Lazy.t<int>
  } = {
    let a = Lazy.make(() => Lazy.get(Intb.a) + 1)
  }
  and Intb: {
    let a: Lazy.t<int>
  } = {
    let a = Lazy.make(() => 2)
  }
}

eq(__LOC__, Lazy.get(A.Inta.a), 3)
/* expect raise Undefined_recursive_module */
eq(
  __LOC__,
  4,
  try {
    ignore(Int3.u(3))
    3
  } catch {
  | Undefined_recursive_module(_) => 4
  },
)

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
