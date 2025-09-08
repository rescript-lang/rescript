type someVariant = One | Two | Three(option<bool>)
type somePolyVariant = [#one | #two | #three(option<bool>) | #"exotic ident" | #"switch"]

let withSomeVariant = One
let withSomePoly: somePolyVariant = #one
let someBool = true
let someOpt = Some(true)

// switch withSomeVarian
//                      ^com

// switch withSomePol
//                   ^com

// switch someBoo
//               ^com

// switch someOp
//              ^com

type rcrd = {someVariant: someVariant}

let getV = r => r.someVariant

let x: rcrd = {
  someVariant: One,
}

let vvv = Some(x->getV)

// switch x->getV
//           ^xfm

// x->getV
// ^xfm  ^

// vvv
//  ^xfm

// ^ve+ 11.1
// switch withSomeVarian
//                      ^com
// ^ve-
