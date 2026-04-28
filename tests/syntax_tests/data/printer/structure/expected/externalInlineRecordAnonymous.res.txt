external f1: {x: int} => unit = "f1"
external f2: ({x: int}, string) => unit = "f2"
external f3: (option<{x: int}>, {y: string}) => option<{done: bool}> = "f3"
external f4: (~named: {x: int}, {y: string}) => unit = "f4"

type f = string
external f: {z: int} => unit = "f"
