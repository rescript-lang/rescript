type attr = ..
type attr += Str(string) | Num(int)

module Sub = {
  type attr += Flag | Toggle(bool)
  // Inside the declaring module the constructor is not qualified
  // let inside: attr = Fl
  //                      ^com
}

// let a: attr = Nu
//                 ^com

let value: attr = Str("s")
//                 ^hov

// switch value { | S
//                   ^com

// switch valu
//            ^com

// Open types of the same name are told apart
module A = {
  type t = ..
  type t += FromA
}
module B = {
  type t = ..
  type t += FromB
}
type A.t += MoreA

let a: A.t = A.FromA

// switch a { | M
//               ^com

// switch a { | F
//               ^com

// switch value { | Fl
//                    ^com

// An extension declared later is not in scope yet
// switch value { | La
//                    ^com

type attr += Later

// Payload of a qualified extension constructor
// let toggled: attr = Sub.Toggle(t
//                                 ^com

// Expanding the catch-all of an open type
let _x = switch value {
| Str(_) => "str"
| _ => "other"
//  ^xfm
}

let optValue = Some(value)
let _y = switch optValue {
| Some(Str(_)) => "str"
| _ => "other"
//  ^xfm
}

// An exhaustive switch of an open type
// value
// ^xfm^
