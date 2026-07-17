type source = {a: int, b: string}
type notRecord = One | Two
let {a, ...notRecord as rest} = ({a: 1, b: "x"}: source)
