let foo = (~x) => {
  let _ = ()
  (~y) => x + y
}
// This looks too far into the return type
let d = foo(~y=3)
