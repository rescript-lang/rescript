type attr = ..
type attr += Str(string) | Num(int)

module Sub = {
  type attr += Flag
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
