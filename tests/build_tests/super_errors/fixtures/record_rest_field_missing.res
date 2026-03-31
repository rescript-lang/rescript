type source = {a: int, b: string, c: bool, d: float}
type sub = {b: string}
let {a, ...sub as rest} = ({a: 1, b: "x", c: true, d: 1.0}: source)
