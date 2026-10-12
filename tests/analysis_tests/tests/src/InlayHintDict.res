// Dict literals get an inferred-type hint
let ints = dict{"a": 1}
let spread = dict{...ints, "b": 2}

// So do variables bound by a dict pattern
let dict{"a": ?first} = ints

//^hin
