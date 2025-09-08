module M = (
  U: {
    let f: (int, string) => string
  },
) => {
  let v = U.f(100, "x")
}

let f = () => 3

let u = f()

let \"+>" = (a, h: _ => int) => h(a)

let u = h => \"+>"(3, h)
