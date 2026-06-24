type source = {a?: int, b: string}
type sub = {a?: int, b: string}
let getRest = ({a: ?_, ...sub as rest}: source) => rest
