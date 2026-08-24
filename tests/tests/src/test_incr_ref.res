include (
  {
    let u = ref(0)
    let v = Int.Ref.increment(u)
  }: {
    let v: unit
  }
)
