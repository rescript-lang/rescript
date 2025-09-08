let rec fib = x =>
  switch x {
  | 0 | 1 => 1
  | n => fib(n - 1) + fib(n - 2)
  }

let fib2 = n => {
  let rec aux = (a, b, i) =>
    if n == i {
      a
    } else {
      aux(b, a + b, i + 1)
    }
  aux(1, 1, 0)
}

let fib3 = n => {
  let a = ref(1)
  let b = ref(1)
  for i in 1 to n {
    let tmp = a.contents
    a := b.contents
    b := b.contents + tmp
  }
  a.contents
}
