let pair = Pair /* payload */ (1, 2)

let read = value => switch value {
| Module.Pair /* payload */ (a, b) => (a, b)
}
