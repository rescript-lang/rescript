// The same for an include of a functor parameter.
module F = (X: IncludeDeclarationsSig.S) => {
  include X
  let u0 = 0
  let u1 = 1
  let u2 = 2
  let u3 = 3
  let u4 = 4
  let u5 = 5
  let u6 = 6
  let u7 = 7
  let u8 = 8
  let u9 = 9
  let k = Ja
//        ^ref
  let k = Ja
  let k = Ja
  let k = Ja
//        ^ren Jx
  let s = sv
  let s = sv
  let fld = {ga: 1, gb: 2}
//           ^ref
}
