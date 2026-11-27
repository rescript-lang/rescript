@@config({flags: ["-bs-cross-module-opt"]})

// Inlined by inline_assign_cross_module_test.res
@inline
let bumpTwice = (x: int) => {
  let r = ref(x)
  r := r.contents + 1
  if x > 3 {
    r := r.contents * 2
  }
  r.contents
}

// Inlined by inline_assign_cross_module_test.res: [f] reads [r] before the
// assignment, and the closure it returns must see that value, not the new one.
@inline
let readThenAssign = (x: int, f: int => unit => int) => {
  let r = ref(x)
  let g = f(r.contents)
  r := 10
  g() + r.contents
}

// Inlined by inline_assign_cross_module_test.res: [s] keeps the value [r] had
// before the assignment.
@inline
let copyThenAssign = (x: int) => {
  let r = ref(x)
  let s = r.contents
  r := s + 1
  s + r.contents
}
