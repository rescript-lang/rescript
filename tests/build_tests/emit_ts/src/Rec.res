type point = {x: int, y: int, label: string}

let make = (x: int, y: int) => {x, y, label: "p"}
let norm1 = (p: point) => Math.Int.abs(p.x) + Math.Int.abs(p.y)
let moveX = (p: point, dx: int) => {...p, x: p.x + dx}
