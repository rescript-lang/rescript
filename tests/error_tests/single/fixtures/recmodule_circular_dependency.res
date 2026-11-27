module rec A: {
  let v: int
} = {
  let v = B.v
}
and B: {
  let v: int
} = {
  let v = A.v
}
