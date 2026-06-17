@unboxed type source = {value: int}
type empty = {}

let {value: _, ...empty as rest} = ({value: 1}: source)
