let test4 = n => n + 5

let rec test3 = n =>
  if n == 0 {
    test4(n) + 4
  } else {
    test3(n - 1)
  }

let rec test2 = n =>
  if n == 0 {
    test3(n) + 3
  } else {
    test2(n - 1)
  }

let rec test1 = n =>
  if n == 0 {
    test2(n) + 2
  } else {
    test1(n - 1)
  }

let rec test0 = n =>
  if n == 0 {
    test1(n)
  } else {
    test0(n - 1)
  }

let v = test0(10)

let u = test0(10) + 2
