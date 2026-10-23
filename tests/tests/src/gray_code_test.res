let gray_encode = b => Int.bitwiseXor(b, Int.shiftRightUnsigned(b, 1))

let gray_decode = n => {
  let rec aux = (p, n) =>
    if n == 0 {
      p
    } else {
      aux(Int.bitwiseXor(p, n), Int.shiftRightUnsigned(n, 1))
    }

  aux(n, Int.shiftRightUnsigned(n, 1))
}

let next_power = v => {
  let v = v - 1
  let v = Int.bitwiseOr(Int.shiftRightUnsigned(v, 1), v)
  let v = Int.bitwiseOr(Int.shiftRightUnsigned(v, 2), v)
  let v = Int.bitwiseOr(Int.shiftRightUnsigned(v, 4), v)
  let v = Int.bitwiseOr(Int.shiftRightUnsigned(v, 8), v)
  let v = Int.bitwiseOr(Int.shiftRightUnsigned(v, 16), v)
  v + 1
}
