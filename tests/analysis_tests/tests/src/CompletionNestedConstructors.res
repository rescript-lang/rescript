type inner = Inner(bool)
type outer = Outer(inner, inner) | Unary(inner)

let first = Outer(Inner(true), Inner(false))
//                       ^com

let second = Outer(Inner(false), Inner(true))
//                                      ^com

let unary = Unary(Inner(true))
//                       ^com

let tuple = Outer((Inner(false), Inner(true)))
//                                      ^com
