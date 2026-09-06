type nested<'a>= array<option<array<'a>>>
let make = (): nested<int>=> [Some([1])]
let shifts = (a, b) => (a >> b, a >>> b, a << b, a >= b)
let template = `value ${a >> 1}`
