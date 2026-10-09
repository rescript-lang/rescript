// Dict literals get an inferred-type hint
let ints = dict{"a": 1}
let spread = dict{...ints, "b": 2}

//^hin
