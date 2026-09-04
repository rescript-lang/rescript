type t = Pair(int, string)

let value = Pair((1, "test"))
//                     ^she

let read = value => switch value {
| Pair((first, second)) => second
//             ^she
}
