foo(_ => bla, blaz)
foo((_) => bla, blaz)
foo((. _) => bla, blaz)
foo(_ => bla, _ => blaz)

List.map(x => x + 1, myList)
List.reduce((acc, curr) => acc + curr, 0, myList)

let unitUncurried = apply(.)

call(~a: int)

call_partial(3, ...)
