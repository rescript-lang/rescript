open Mocha
open Test_utils

let generic_basename = (is_dir_sep, current_dir_name, name) => {
  let rec find_end = n =>
    if n < 0 {
      String.substring(name, ~start=0, ~end=1)
    } else if is_dir_sep(name, n) {
      find_end(n - 1)
    } else {
      find_beg(n, n + 1)
    }
  and find_beg = (n, p) =>
    if n < 0 {
      String.substring(name, ~start=0, ~end=p)
    } else if is_dir_sep(name, n) {
      String.substring(name, ~start=n + 1, ~end=p)
    } else {
      find_beg(n - 1, p)
    }

  if name == "" {
    current_dir_name
  } else {
    find_end(String.length(name) - 1)
  }
}

let basename = generic_basename((s, i) => String.getUnsafe(s, i) == "/", "", ...)

describe(__MODULE__, () => {
  test("basename", () => {
    eq(__LOC__, basename("b/c/a.b"), "a.b")
  })
})
