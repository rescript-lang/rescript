let () = {
  let delayed = ref(() => ())
  for i in 1 to 2 {
    let rec f = (n, x) =>
      switch x {
      | 0 => assert(i == n)
      | j =>
        delayed := {
          let prev = delayed.contents
          () => {
            prev()
            f(n + 1 + i - i, j - 1)
          }
        }
      }
    f(0, i)
  }
  delayed.contents()
}
