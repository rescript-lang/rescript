let \"|>" = (x, f) => f(x)

let hello1 = (y, f) => f(y)

let hello2 = (y, f) => f(y)

let hello3 = (y, f) => f(y)

let hello4 = (y, f) => f(y)

let hello5 = (y, f) => hello1(y, f)

let rec f = x => f(x + 1)

let rec ff = (x, y) => ff(y, x + 1)

let rec fff = (x, y) => fff(y, x)
