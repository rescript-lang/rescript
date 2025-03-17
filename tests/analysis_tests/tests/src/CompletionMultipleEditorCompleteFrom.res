@@warning("-26")
@@warning("-27")
@@warning("-110")

module A = {
  @editor.completeFrom(B) @editor.completeFrom(C)
  type a
}

module B = {
  let b = (a: A.a) => 1
}

module C = {
  open A
  let c = (a: a) => {'c'}
}

let a : A.a = %todo
// a.
//   ^com
// B.b and C.c should be completed