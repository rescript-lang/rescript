let f = (a, b, _) => a + b

let f2 = a => f(a, 1, ...)

let (a, b, c) = (
  Js.Int.toString(f(1, 2, 3)),
  {
    let f3 = f2(100)
    f3(2)
  },
  {
    let arr = Belt.Array.init(2, _ => 0)
    for i in 0 to 1 {
      let f3 = f2(i)
      arr[i] = f3(2)
    }
    arr
  },
)

let () = Console.log((a, b, c))
