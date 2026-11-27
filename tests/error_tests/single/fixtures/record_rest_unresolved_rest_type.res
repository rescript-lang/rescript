type source = {a: int, b: string}
type rest
let {a, ...rest as value} = ({a: 1, b: "x"}: source)
