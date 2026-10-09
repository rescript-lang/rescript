let ints = dict{"a": 1}

// The error points at the spread dict
let strings: dict<string> = dict{"b": "x", ...ints}
