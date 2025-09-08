@genType
type item = {id: int}

@genType
type items = TypeParams1.ocaml_array<item>

@genType
type items2 = array<item>

let exportSomething = 10

