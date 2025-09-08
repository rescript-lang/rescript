type record = {
  x: int,
  y: string,
}

let r = {x: 3, y: "hello"}

module type MT = {
  let x: int
}

type firstClassModule = module(MT)
