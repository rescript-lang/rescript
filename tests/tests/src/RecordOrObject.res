type rx = {x: int}
type ry = {y: string}
type rxi = {...rx, i?: int}
type rxy = {...rx, ...ry} // this is a record

let vxy: rxy = {x: 10, y: "abc"}
let xxi: rxi = {x: 10}

type ox = {"x": int}
type oy = {"y": int}
type oxz = {...ox, "z": int}
type oxy = {...ox, ...oy} // this starts as a record but type checking infers that it is an object
