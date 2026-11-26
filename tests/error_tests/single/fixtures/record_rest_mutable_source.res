type source = {mutable name: string, version: string}
type rest = {version: string}

let {name: _, ...rest as rest} = ({name: "x", version: "1"}: source)
