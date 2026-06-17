type source = {a?: int, b?: string, c: bool}
type sub = {a?: int, b?: string}
let {a, b, ...sub as rest}: source = {c: true}
