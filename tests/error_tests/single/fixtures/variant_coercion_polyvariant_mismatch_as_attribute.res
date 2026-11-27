type poly = [#One | #Two]

type variant = One | @as("two") Two

let p: poly = #One

let v: variant = (p :> variant)
