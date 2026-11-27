let xs = [1]

// The error points at the spread, not at the whole literal
let a: array<string> = [...xs, 2]
