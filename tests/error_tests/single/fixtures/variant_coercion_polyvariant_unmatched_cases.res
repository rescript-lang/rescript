type poly = [#One | #Two]

type variant = One

let p: poly = #One

let v: variant = (p :> variant)
