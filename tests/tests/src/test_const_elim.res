include (
  {
    let f = x => {
      let u = (1, 2)
      let v = (x, x)
      (Pair.first(u), Pair.second(v))
    }
  }: {}
)
