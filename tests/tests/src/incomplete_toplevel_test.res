let f = () => {
  Js.log("no inline")
  (1, 2, 3)
}

let (a, b, c) = f()
