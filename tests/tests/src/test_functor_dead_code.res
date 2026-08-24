include (
  {
    module M = Map
    let v = M.isEmpty(M.make())
  }: {
    let v: bool
  }
)
