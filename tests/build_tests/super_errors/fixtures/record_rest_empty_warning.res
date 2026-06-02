type source = {a: int, b?: string}
type sub = {b?: string}
let {a, ?b, ...sub as rest} = ({a: 1}: source)
