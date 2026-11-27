type aa = option<string>
type bb = option<int>
type a = option<aa>
type b = option<bb>
let a: a = (Some(Some(5)): b)
