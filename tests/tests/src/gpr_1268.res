let f = (a, b, x, y) => b(y) + a(x)

@val external add: (int, int) => int = "add"

let f_add2 = (a, b, x, y) => add(b(y), a(x))

let f = (a, b, x, y) => a(x) + b(y)

let f1 = (a, b, x, y) => add(a(x), b(y))

let f2 = x => Console.log(x)

let f3 = x => Console.log(x)

let f4 = (x, y) => add(y, x)
