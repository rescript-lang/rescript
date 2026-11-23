@@config({flags: ["-bs-cross-module-opt"]})

// Inlined by inline_static_exit_cross_module_test.res. Only closed functions
// are inlined in other modules, so the handlers call no function of this one.
@inline
let pick = (x: int, y: int, flag: bool) =>
  switch (x, y) {
  | (0, _) | (_, 0) =>
    if flag {
      "t"
    } else {
      "a" ++ Int.toString(x) ++ Int.toString(x + y)
    }
  | _ => "other"
  }

module type S = {
  let v: int
  let w: int
  let flag: bool
}

module Make = (X: S) => {
  let r = switch (X.v, X.w) {
  | (0, _) | (_, 0) =>
    if X.flag {
      "t"
    } else {
      "a" ++ Int.toString(X.v) ++ Int.toString(X.v + X.w)
    }
  | _ => "other"
  }
}
