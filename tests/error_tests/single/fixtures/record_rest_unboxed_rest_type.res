type source = {name: string, value: int}
@unboxed type value = {value: int}

let {name: _, ...value as rest} = ({name: "x", value: 1}: source)
