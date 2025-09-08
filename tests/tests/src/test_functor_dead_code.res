include (
  {
    module M = Belt.Map.String
    let v = M.isEmpty(M.empty)
  }: {
    let v: bool
  }
)
