type t = Three(string, array<int>) | Unary(string)

let a = Three("", [])
//               ^she
let b = Three("", [])
//                ^she
let c = Three("", [])
//                 ^she
let d = Three( "", [])
//            ^she
let e = Unary( "")
//            ^she

let f = Three("",    [])
//                  ^she
let g = Three("" , [])
//               ^she
let h = Three(
  "",
  [],
//^she
)

let i = Three("", [])
//         ^she

let read = value => switch value {
| Three(a, []) => a
//        ^she
| Three(a, _) => a
//         ^she
| Unary( a) => a
//      ^she
}
