let f = () => {
  let n = ref(0)
  while {
    let rec fib = x =>
      switch x {
      | 0 | 1 => 1
      | n => fib(n - 1) + fib(n - 2)
      }
    fib(n.contents) > 10
  } {
    n.contents->Js.Int.toString->Console.log
    incr(n)
  }
}

let ff = () =>
  while {
    let a = 3
    let b = a * a
    a + b > 10
  } {
    ()
  }
