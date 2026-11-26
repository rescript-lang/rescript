let expectCurried = f => f(1) + 2

let z1 = expectCurried((x, y) => x + y)
