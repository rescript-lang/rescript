type poly = [#One | #Two(string)]

type variant = One | Two

let p: poly = #One

let v: variant = (p :> variant)
