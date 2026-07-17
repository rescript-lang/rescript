type source = {a: int, b: string}
type wrong = {b: int}

let {a, ...wrong as rest} = ({a: 1, b: "x"}: source)
