let rec f = (b, x, n) =>
  if n > 100000 {
    false
  } else {
    b && f(b, x, n + 1)
  }

let rec or_f = (b, x, n) =>
  if n > 100000 {
    false
  } else {
    b || or_f(b, x, n + 1)
  }

let suites = {
  open Mt
  list{
    /* becareful inlining will defeat the test purpose here */
    ("and_tail", _ => Eq(false, f(true, 1, 0))),
    ("or_tail", _ => Eq(false, or_f(false, 1, 0))),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
