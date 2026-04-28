open Mocha
open Test_utils

let generic_basename = (is_dir_sep, current_dir_name, name) => {
  let rec find_end = n =>
    if n < 0 {
      Js.String2.substrAtMost(name, ~from=0, ~length=1)
    } else if is_dir_sep(name, n) {
      find_end(n - 1)
    } else {
      find_beg(n, n + 1)
    }
  and find_beg = (n, p) =>
    if n < 0 {
      Js.String2.substrAtMost(name, ~from=0, ~length=p)
    } else if is_dir_sep(name, n) {
      Js.String2.substrAtMost(name, ~from=n + 1, ~length=p - n - 1)
    } else {
      find_beg(n - 1, p)
    }

  if name == "" {
    current_dir_name
  } else {
    find_end(Js.String2.length(name) - 1)
  }
}

let basename = generic_basename((s, i) => String.getUnsafe(s, i) == "/", "", ...)

describe(__MODULE__, () => {
  test("basename", () => {
    eq(__LOC__, basename("b/c/a.b"), "a.b")
  })
})
