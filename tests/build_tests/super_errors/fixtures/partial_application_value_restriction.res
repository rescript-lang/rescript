let first = (x, _y) => x

// `[]` is evaluated once, so every call of `h` returns the same array
let h = first([], ...)
let ints: array<int> = h(0)
let strings: array<string> = h(0)
