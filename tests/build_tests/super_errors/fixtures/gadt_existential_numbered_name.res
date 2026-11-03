type rec a2 = A2('a): a2
type rec a1 = A1('a1): a1
type rec a3 = A3('a): a3
let f = (A2(y), A1(x), A3(z)) => z == x
