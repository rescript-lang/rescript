type source = {a: int, b: string, c: bool}
type sub = {b: string}
let {a, ...sub as rest} = ({a: 1, b: "x", c: true}: source)
