@tag("kind") type x = One(bool) | Two(string)
type y = One(bool) | Two(string)

let x: x = One(true)

let y = (x :> y)
