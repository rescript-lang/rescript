let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}
let add = suite => suites := list{suite, ...suites.contents}

module rec Int3: {
  let u: int => int
} = Int3

module Fact = {
  module type S = {
    let fact: int => int
  }
  module rec M: S = {
    let fact = n =>
      if n <= 1 {
        1
      } else {
        n * M.fact(n - 1)
      }
  }
  include M
}

let () = eq(__LOC__, 120, Fact.fact(5))

let () = add((__LOC__, _ => Mt.ThrowAny(_ => ignore(Int3.u(3)))))

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
