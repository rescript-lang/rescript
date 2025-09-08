include (
  {
    let f = x => {
      let u = (1, 2)
      let v = (x, x)
      (fst(u), snd(v))
    }
  }: {}
)
