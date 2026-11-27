module A = {
  module B = {
    module C = {
      module D = {
        let aaaa = 1
      }
    }
  }
}

let asd = A.B.C.D.aaa

/* there's another, unrelated test, that we're just gonna paste here. We won't
run this, because CI and macOS give different results because of the way path
sensitivity is handled */

/* let asd = JS.toOption */
