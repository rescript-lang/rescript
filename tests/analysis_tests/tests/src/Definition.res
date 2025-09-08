let xx = 10

let y = xx
//      ^def

module Inner = {
  type tInner = int
  let vInner = 34
}

type typeInner = Inner.tInner
//                     ^def

// open Belt
let m1 = List.map
//            ^hov

open ShadowedBelt
let m2 = List.map
//            ^hov

let uncurried = (. x, y) => x + y

uncurried(. 3, 12)->ignore
// ^hov

uncurried(. 3, 12)->ignore
// ^def