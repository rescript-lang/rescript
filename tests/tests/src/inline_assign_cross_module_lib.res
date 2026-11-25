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
