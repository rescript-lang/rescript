type rec t = Any(_): t | Any1(_): t
let f = (Any(x), Any(y), Any1(z)) => z == y
