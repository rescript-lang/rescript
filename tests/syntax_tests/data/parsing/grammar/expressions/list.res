// empty list
let x = list{}

let x = list{1, 2, 3}

// trailing comma
let x = list{1, 2, 3,}

// with constrained expressions
let x = list{1: int, (2: int), 3: int}

// spread
let x = list{4, 5, ...y}

// spread anywhere
let x = list{1, ...x, 2, 3, ...x}

// spread constrained expression
let x = list{1, 2, ...y: list<int>}
