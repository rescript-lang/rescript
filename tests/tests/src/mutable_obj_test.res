type u = {@set "height": int}

let f = (x: u) => {
  x["height"] = 3
  x["height"] * 2
}

let f = (x: {@set({no_get: no_get}) "height": int}) => x["height"] = 3

type v = {@set "dec": int => {"x": int, "y": float}}

let f = (x: v) => x["dec"] = x => {"x": x, "y": float_of_int(x)}
