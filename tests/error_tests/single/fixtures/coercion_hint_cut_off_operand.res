type a = {x: int, y: int}
type b = {x: int}
let v: a = {x: 1, y: 2}
let w: b = (true ? v : v)->(r => r)
