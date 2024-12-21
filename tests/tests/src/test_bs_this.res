let uux_this: @this ({"length": int}, int, int) => int = @this (o, x, y) => o["length"] + x + y

let even = @this (o, x) => x + o

let bark = () =>
  @this
  (o: 'self, x, y) => {
    Js.log((o["length"], o["x"], o["y"], x, y))
    x + y
  }

let js_obj: 'self = {
  "bark": @this
  (o: 'self, x, y) => {
    Js.log(o)
    x + y
  },
}

let f = x => {
  x["onload"] = @this o => Js.log(o)
  x["addEventListener"]("onload", @this o => Js.log(o["response"]))
}

let u = @this (_: int, x: int) => x
