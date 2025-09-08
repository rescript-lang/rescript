let x = 12
//  ^ref

let a = x

let b = a

let c = x

let foo = (~xx) => xx + 1
//                 ^ref

module M: {
  let aa: int
} = {
  let aa = 10
}

let bb = M.aa
let cc = bb
let dd = M.aa
//          ^ref

let _ = <ComponentInner/>
//             ^ref