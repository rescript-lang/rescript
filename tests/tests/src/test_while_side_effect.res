let v = ref(0)

while {
  v.contents->Int.toString->Console.log
  Int.Ref.increment(v)
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
  x.contents->Int.toString->Console.log
  Int.Ref.increment(y)
  Int.Ref.increment(x)
  fib(x.contents) + fib(x.contents) < 20
} {
  3->Int.toString->Console.log
}
