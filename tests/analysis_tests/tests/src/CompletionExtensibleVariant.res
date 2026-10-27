type attr = ..
type attr += Str(string) | Num(int)

module Sub = {
  type attr += Flag
}

// let a: attr = Nu
//                 ^com

let value: attr = Str("s")

// switch value { | S
//                   ^com

// switch valu
//            ^com
