let a = {"x": 3, "y": [1]}

let b = {"x": 3, "y": [1], "z": 3, "u": (x, y) => x + y}

let f = obj => obj["x"] + Belt.Array.length(obj["y"])

let u = f(a)

let v = f(b)
