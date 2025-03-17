type someVariant = One | Two | Three

/** Does stuff. */
let someFunc = (one: int, ~two: option<string>=?, ~three: unit => unit, ~four: someVariant, ()) => {
  ignore(one)
  ignore(two)
  ignore(three())
  ignore(four)
}

let otherFunc = (first: string, second: int, third: float) => {
  ignore(first)
  ignore(second)
  ignore(third)
}

// let _ = someFunc(
//                  ^she

// let _ = someFunc(1
//                   ^she

// let _ = someFunc(123, ~two
//                           ^she

// let _ = someFunc(123, ~two="123"
//                               ^she

// let _ = someFunc(123, ~two="123", ~four
//                                    ^she

// let _ = someFunc(123, ~two="123", ~four=O
//                                        ^she

// let _ = otherFunc(
//                   ^she

// let _ = otherFunc("123"
//                      ^she

// let _ = otherFunc("123", 123, 123.0)
//                                 ^she

// let _ = Completion.Lib.foo(~age
//                               ^she

let iAmSoSpecial = (iJustHaveOneArg: string) => {
  ignore(iJustHaveOneArg)
}

// let _ = iAmSoSpecial(
//                      ^she

// let _ = "hello"->otherFunc(1
//                             ^she

let fn = (age: int, name: string, year: int) => {
  ignore(age)
  ignore(name)
  ignore(year)
}

// let _ = fn(22, )
//               ^she

// let _ = fn(22, , 2023)
//               ^she

// let _ = fn(12, "hello", )
//                        ^she

// let _ = fn({ iAmSoSpecial() })
//                           ^she

// let _ = fn({ iAmSoSpecial({ someFunc() }) })
//                                      ^she

/** This is my own special thing. */
type mySpecialThing = string

type t =
  | /** One is cool. */ One({miss?: bool, hit?: bool, stuff?: string})
  | /** Two is fun! */ Two(mySpecialThing)
  | /** Three is... three */ Three(mySpecialThing, array<option<string>>)

let _one = One({})
//              ^she

let _one = One({miss: true})
//                ^she

let _one = One({hit: true, miss: true})
//                     ^she

let two = Two("true")
//             ^she

let three = Three("", [])
//                 ^she

let three2 = Three("", [])
//                      ^she

let _deepestTakesPrecedence = [12]->Array.map(v =>
  if v > 0 {
    One({})
    //   ^she
  } else {
    Two("")
  }
)

/** Main docstring here. */
let _usesCorrectTypeInfo = [12]->Array.map(v => v)
//                                        ^she

/** Type x... */
type x = {
  age?: int,
  name?: string,
}

/** Type tt! */
type tt = One

/** Some stuff */
let stuffers = (x: x, y: tt) => {
  ignore(x)
  ignore(y)
  "hello"
}

let _ = stuffers({}, One)
//                ^she

let _ = stuffers({}, One)
//                    ^she

let _ = switch _one {
| One({hit: _hit}) => ""
//      ^she
| One(_a) => ""
//     ^she
| Two(_ms) => ""
//     ^she
| Three(_a, []) => ""
//       ^she
| Three(_, _b) => ""
//          ^she
}

let _bb = Ok(true)
//            ^she

let _bbb = Error("err")
//                 ^she

let _cc = Some(true)
//              ^she
