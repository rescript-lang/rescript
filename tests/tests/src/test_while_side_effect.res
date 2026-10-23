let v = ref(0)

while {
  v.contents->Int.toString->Console.log
  v.contents = v.contents + 1
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
  y.contents = y.contents + 1
  x.contents = x.contents + 1
  fib(x.contents) + fib(x.contents) < 20
} {
  3->Int.toString->Console.log
}
