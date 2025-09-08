type a = {a: int}
//	^xfm

type rec t = A | B
// ^xfm
and e = C
@unboxed type name = Name(string)
//             ^xfm
let a = 1
//  ^xfm
let inc = x => x + 1
//  ^xfm
module T = {
  //   ^xfm
  let b = 1
  //  ^xfm
}
@module("path")
external dirname: string => string = "dirname"
//^xfm
