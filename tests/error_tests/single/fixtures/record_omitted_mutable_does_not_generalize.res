type cell<'a> = {tag: int, mutable value?: 'a}

let cell = {tag: 0}
cell.value = Some(1)
let s: option<string> = cell.value
