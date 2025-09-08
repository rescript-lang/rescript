let gray_encode = b => lxor(b, lsr(b, 1))

let gray_decode = n => {
  let rec aux = (p, n) =>
    if n == 0 {
      p
    } else {
      aux(lxor(p, n), lsr(n, 1))
    }

  aux(n, lsr(n, 1))
}

let next_power = v => {
  let v = v - 1
  let v = lor(lsr(v, 1), v)
  let v = lor(lsr(v, 2), v)
  let v = lor(lsr(v, 4), v)
  let v = lor(lsr(v, 8), v)
  let v = lor(lsr(v, 16), v)
  v + 1
}
