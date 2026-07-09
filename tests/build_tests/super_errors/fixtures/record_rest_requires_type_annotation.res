type source = {a: int, b: string}
let {a, ...theRest} = ({a: 1, b: "x"}: source)
