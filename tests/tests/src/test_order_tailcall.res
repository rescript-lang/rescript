let rec f = (x, y) => f(y, x)

let rec f1 = (x, y, z) => {
  Console.log(z)
  f1(y, z, x)
}

let rec f2 = (x, y) => f2(x, y + 10)

let rec f3 = (x, y) => f3(y, x + 10)

let rec f4 = (x, y) => f4(x + 10, y + x)

let rec f5 = (x, y, z) => f5(y + 10, z + 20, z)

/** tail call as well */
let rec f6 = b => b && f6(b)

let rec f7 = b => b || f7(b)

let rec f8 = (x, y) =>
  if x > 10 {
    f8(x, y + 1)
  } else if x < 5 {
    f8(x - 1, y)
  } else if x > 6 {
    f8(x - 2, y)
  } else {
    f8(x, y + 1) + f8(x - 1, y)
  }
