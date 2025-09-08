@genType
type record = {
  x: int,
  y: string,
}

let r: record

@genType
module type MT = {
  let x: int
}

@genType
type firstClassModule = module(MT)

