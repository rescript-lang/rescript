@unboxed
type variant = One(string) | Two

let do = (x: variant) => {
  (x :> string)
}

let _ = do(#One("test"))
