module M: {
  type t = private Wrap(int)
  let make: int => t
} = {
  type t = Wrap(int)
  let make = v => Wrap(v)
}

let v = M.Wrap(1)
