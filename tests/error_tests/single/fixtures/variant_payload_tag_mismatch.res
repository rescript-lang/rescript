// Payload cases swapped by @as across a signature (#8956)
module M: {
  type t = A({x: int}) | C({x: int})
  let a: t
} = {
  type t = | @as("C") A({x: int}) | @as("A") C({x: int})
  let a = A({x: 1})
}

Console.log(
  switch M.a {
  | M.A(_) => "A"
  | M.C(_) => "C"
  },
)
