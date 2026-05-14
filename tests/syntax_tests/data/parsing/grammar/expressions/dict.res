// empty dict
let x = dict{}

// one value
let x = dict{"foo": "bar"}

// two values
let x = dict{"foo": "bar", "bar": "baz"}

let baz = "foo"
let x = dict{"foo": "bar", "bar": "baz", "baz": baz}

let foo = dict{"a": 1}
let qux = dict{"c": 3}
let x = dict{...foo, "bar": 2, ...qux}
let x = dict{"before": 1, ...foo, "after": 2}
let x = dict{...foo}
