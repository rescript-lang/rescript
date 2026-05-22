module M: {
  type t = private {mutable x: int}
  let make: int => t
} = {
  type t = {mutable x: int}
  let make = v => {x: v}
}

let r = M.make(1)
let () = r.x = 2
