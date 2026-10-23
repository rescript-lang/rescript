include (
  {
    let u = ref(0)
    let v = u.contents = u.contents + 1
  }: {
    let v: unit
  }
)
