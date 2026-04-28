// This should not show coercion suggestion since just the inner types are coercable, not the full type + expression (dict<float> -> dict<JSON.t>)
let x: dict<JSON.t> = dict{"1": 1.}
