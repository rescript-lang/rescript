type t = Pair(int, string)

type unary = Unary((int, string))

let unary = Unary(1, "test")
//                    ^she

let readUnary = value => switch value {
| Unary(first, second) => second
//               ^she
}

let value = Pair((1, "test"))
//                     ^she

let read = value => switch value {
| Pair((first, second)) => second
//             ^she
}
