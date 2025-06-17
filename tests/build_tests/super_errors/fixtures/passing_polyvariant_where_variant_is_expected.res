type variant = One | Two

let do = (x: variant) => {
  (x :> string)
}

let _ = do(#One)
