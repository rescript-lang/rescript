foo(_ => bla, blaz)
foo((_) => bla, blaz)
foo((. _) => bla, blaz)
foo(_ => bla, _ => blaz)

List.map(x => x + 1, myList)
List.reduce((acc, curr) => acc + curr, 0, myList)

let unitUncurried = apply(.)

call(~a: int)

// pass the result of bitwise NOT expression
call((~a))
call(~a=a ^ ~a)
call(~a=~a)

call_partial(3, ...)
