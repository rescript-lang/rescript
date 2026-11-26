type source = {a: int, x: int}
type sub = {a: int, b: string}
let {x, ...sub as rest} = ({a: 1, x: 2}: source)
