let rec fib = (n, k) =>
  switch n {
  | 0 | 1 => k(1)
  | _ =>
    Suspend(
      () =>
        fib(n - 1, (v0) =>
          fib(n - 2, (v1) =>
            k(v0 + v1)
            /* comment */
          )
        ),
    )
  }

let first = call(x => x
  /* first callback */
, value)

let last = call(value, x => x
  // last callback
)

let inline = call(value, x => x // inline callback
)

let firstInline = call(x => x // first inline callback
, value)

let nested = call(value, x => call(value, y => y
  /* nested callback */
))

let blocks = call(value, x => x /* inline block */)
