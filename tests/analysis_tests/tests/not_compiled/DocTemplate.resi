type a = {a: int}
//	^xfm

type rec t = A | B
// ^xfm
and e = C
@unboxed type name = Name(string)
//             ^xfm
let a: int
//  ^xfm
let inc: int => int
//  ^xfm
module T: {
  //   ^xfm
  let b: int
  //  ^xfm
}
@module("path")
external dirname: string => string = "dirname"
//^xfm
