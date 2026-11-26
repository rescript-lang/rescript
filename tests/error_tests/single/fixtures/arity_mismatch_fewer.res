let f = (cb: (int, int) => int): string => {
  ignore(cb)
  "hello"
}

let x = f(_a => "hello")
