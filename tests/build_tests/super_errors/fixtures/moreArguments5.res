module Sub = {
  type a = {a: int}
}
let x = (a, b, c, d) => {Sub.a: 2}
let y = x(2).Sub.a
