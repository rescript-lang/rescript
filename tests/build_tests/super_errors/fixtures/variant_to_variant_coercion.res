type x = One(bool) | Two
type y = One(string) | Two

let x: x = One(true)

let y = (x :> y)
