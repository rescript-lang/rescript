type source = {a: int, b: string}
let {a, ...'a as rest} = ({a: 1, b: "x"}: source)
