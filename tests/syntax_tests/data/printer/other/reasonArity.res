let _ = Constructor(a, b)
let _ = Constructor((a, b))
let _ = #Constructor(a, b)
let _ = #Constructor(a, b)

switch a {
| C(c, d) => 1
| C((c, d)) => 1
}

switch a {
| #C(c, d) => 1
| #C(c, d) => 1
}
