module M = (): {
  type t
  let v: t
} => {
  type t = int
  let v = 3
}

module V = M()
