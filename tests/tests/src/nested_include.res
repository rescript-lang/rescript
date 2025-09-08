include {
  type t
  include (
    {
      let f = x => x
    }: {
      let f: t => t
    }
  )
}
