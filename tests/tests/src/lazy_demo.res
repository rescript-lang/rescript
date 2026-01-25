let lazy1 = Lazy.make(() => {
  "Hello, lazy"->Console.log
  1
})

let lazy2 = Lazy.make(() => 3)

Console.log2(lazy1, lazy2)

// can't destructure lazy values
let (la, lb) = (Lazy.get(lazy1), Lazy.get(lazy2))

Console.log2(la, lb)
