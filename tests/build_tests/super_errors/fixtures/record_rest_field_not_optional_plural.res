type source = {a: int, b: string}
type sub = {a: int, b: string}
let {a, b, ...sub as rest}: source = {a: 1, b: "x"}
