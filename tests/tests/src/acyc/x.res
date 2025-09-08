module A0_a1 = {
  let v = 3
}
module A1_a2: {
  let v: int
} = {
  let v = A0_a1.v
}
module A2_a3 = {
  let v = A1_a2.v
}
module A3_a4 = {
  include A2_a3
}
module A4_a5 = {
  include A3_a4

  Js.log(v)
}
