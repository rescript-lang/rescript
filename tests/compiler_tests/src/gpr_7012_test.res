type t = {a: int, b: int}

let f1 = () => () => Some({a: 1, b: 2})

let f2 = () => Some(Some(Some({a: 1, b: 2})))
