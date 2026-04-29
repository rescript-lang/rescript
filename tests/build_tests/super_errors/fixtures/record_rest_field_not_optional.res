type source = {a?: int, b?: string, c: bool}
type sub = {a?: int, b?: string}
let {a, ...sub as rest}: source = {c: true}
