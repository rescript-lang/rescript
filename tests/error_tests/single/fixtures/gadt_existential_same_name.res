type rec any = First('a): any | Second('a): any

let same = (First(x), Second(y)) => x == y
