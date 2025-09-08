type kind = First | Second | Third | Fourth(int)
type r = {name: string, age: int}

let ret = _ => assert(false)
let kind = assert(false)

if kind == First {
  // ^xfm
  ret("First")
} else {
  ret("Not First")
}

#kind("First", {name: "abc", age: 3}) != kind ? ret("Not First") : ret("First")
//             ^xfm

let name = "hello"
//   ^xfm

let annotated: int = 34
//   ^xfm

module T = {
  type r = {a: int, x: string}
}

let foo = x =>
  //      ^xfm
  switch x {
  | None => 33
  | Some(q) => q.T.a + 1
  //     ^xfm
  }

let withAs = (~x as name) => name + 1
//                   ^xfm

@react.component
let make = (~name) => React.string(name)
//   ^xfm

let _ = (~x) => x + 1
//       ^xfm

//
// Add braces to the body of a function
//

let noBraces = () => name
//                   ^xfm

let nested = () => {
  let _noBraces = (_x, _y, _z) => "someNewFunc"
  //                              ^xfm
}

let bar = () => {
  module Inner = {
    let foo = (_x, y, _z) =>
      switch y {
      | #some => 3
      | #stuff => 4
      }
    //^xfm
  }
  Inner.foo(1, ...)
}

module ExtractableModule = {
  /** Doc comment. */
  type t = int
  // A comment here
  let doStuff = a => a + 1
  // ^xfm
}

let variant = First

let _x = switch variant {
| First => "first"
| _ => "other"
//  ^xfm
}

let _x = switch variant {
| First | Second => "first"
| _ => "other"
//  ^xfm
}

let _x = switch variant {
| First if 1 > 2 => "first"
| Second => "second"
| _ => "other"
//  ^xfm
}

let polyvariant: [#first | #second | #"illegal identifier" | #third(int)] = #first

let _y = switch polyvariant {
| #first => "first"
| _ => "other"
//  ^xfm
}

let _y = switch polyvariant {
| #first | #second => "first"
| _ => "other"
//  ^xfm
}

let variantOpt = Some(variant)

let _x = switch variantOpt {
| Some(First) => "first"
| _ => "other"
//  ^xfm
}

let _x = switch variantOpt {
| Some(First) | Some(Second) => "first"
| _ => "other"
//  ^xfm
}

let _x = switch variantOpt {
| Some(First | Second) => "first"
| _ => "other"
//  ^xfm
}

let polyvariantOpt = Some(polyvariant)

let _x = switch polyvariantOpt {
| Some(#first) => "first"
| None => "nothing"
| _ => "other"
//  ^xfm
}

let _x = switch polyvariantOpt {
| Some(#first | #second) => "first"
| _ => "other"
//  ^xfm
}
