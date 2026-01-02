module X = {
  type y = {d: int}
  type x = {
    a: int,
    b: int,
    c: option<y>,
  }

  let x = {a: 1, b: 2, c: Some({d: 3})}
}

let f = X.x.c.d
