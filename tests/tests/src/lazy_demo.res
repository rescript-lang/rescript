let lazy1 = Lazy.make(() => {
  "Hello, lazy"->Js.log
  1
})

let lazy2 = Lazy.make(() => 3)

Js.log2(lazy1, lazy2)

// can't destructure lazy values
let (la, lb) = (Lazy.get(lazy1), Lazy.get(lazy2))

Js.log2(la, lb)
