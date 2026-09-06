let pair = #Pair /* payload */ (1, 2)

let read = value => switch value {
| #"quoted label" /* payload */ (a, b) => (a, b)
}
