let foo = 1

module M = {
  let foo = 2
}

let _ = foo
open M
let _ = foo
