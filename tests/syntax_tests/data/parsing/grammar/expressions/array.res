let x = [1, 2, 3]

// trailing comma
let x = [1, 2, 3,]

// with constrained expressions
let x = [1 :int, (2: int), 3 : int]

// spread
let x = [4, 5, ...y]

// spread anywhere
let x = [4, 5, ...y, 7, ...y]

// spread constrained expressions
let x = [4, 5, ...y: array<int>]

// spread with other variable
let x = [4, 5, k, ...y]

// the only spread
let x = [...y]
