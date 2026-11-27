type rec u = A(int) | B(u) | C((int, int)) | F(u, int) | G | H

let f = x =>
  switch x {
  | A(_) => 0
  | B(_) => 1
  | C(_) => 2
  | F(_) => 3
  | G => 4
  | H => 5
  }

type result<'a, 'b> =
  | Left('a)
  | Error('b)

let bind = (x, f) =>
  switch x {
  | Left(x) => Left(f(x))
  | Error(_) as y => y
  }
