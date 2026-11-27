let f = () => {
  Console.log("no inline")
  (1, 2, 3)
}

let (a, b, c) = f()
