module N = {
  type t<'a> = {
    a: int,
    b: int,
    c: int,
    d: 'a,
  }
}

let f = (e: N.t<_>) => {
  let {a, b, c} = e
  a + b + c
}

type e = N.t<int => int>

let f1 = (e: e) => {
  let {a, b, c, d} = e
  a + b + c + d(c)
}
