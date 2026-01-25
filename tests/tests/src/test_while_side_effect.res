let v = ref(0)

while {
  v.contents->Js.Int.toString->Console.log
  incr(v)
  v.contents < 10
} {
  ignore()
}
let rec fib = x =>
  switch x {
  | 0 | 1 => 1
  | n => fib(n - 1) + fib(n - 2)
  }

let x = ref(3)

while {
  let y = ref(3)
  x.contents->Js.Int.toString->Console.log
  incr(y)
  incr(x)
  fib(x.contents) + fib(x.contents) < 20
} {
  3->Js.Int.toString->Console.log
}
