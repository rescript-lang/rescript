type poly = [#One | #Two]

type variant = One | @as(2) Two

let p: poly = #One

let v: variant = (p :> variant)
