@genType type u0 = unit => string

@genType type u1 = int => string

@genType type u2 = (int, string) => string

@genType type u3 = (int, string, int) => string

@genType let uncurried0 = () => ""

@genType let uncurried1 = x => x->Belt.Int.toString

@genType let uncurried2 = (x, y) => x->Belt.Int.toString ++ y

@genType let uncurried3 = (x, y, z) => x->Belt.Int.toString ++ (y ++ z->Belt.Int.toString)

@genType let curried3 = (x, y, z) => x->Belt.Int.toString ++ (y ++ z->Belt.Int.toString)

@genType let callback = cb => cb()->Belt.Int.toString

type auth = {login: unit => string}
type authU = {loginU: unit => string}

@genType let callback2 = auth => auth.login()

@genType let callback2U = auth => auth.loginU()

@genType let sumU = (n, m) => Console.log4("sumU 2nd arg", m, "result", n + m)

@genType let sumU2 = n => m => Console.log4("sumU2 2nd arg", m, "result", n + m)

@genType
let sumCurried = n => {
  Console.log2("sumCurried 1st arg", n)
  m => Console.log4("sumCurried 2nd arg", m, "result", n + m)
}

@genType
let sumLblCurried = (s: string, ~n) => {
  Console.log3(s, "sumLblCurried 1st arg", n)
  (~m) => Console.log4("sumLblCurried 2nd arg", m, "result", n + m)
}
