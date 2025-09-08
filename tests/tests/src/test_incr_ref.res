include (
  {
    let u = ref(0)
    let v = incr(u)
  }: {
    let v: unit
  }
)
