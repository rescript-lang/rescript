let f = () => {
  let v = ref(0)
  let acc = ref(0)

  let rec loop = (n): int =>
    if v.contents > n {
      acc.contents
    } else {
      acc := acc.contents + v.contents
      incr(v)
      loop(n)
    }
  loop(10)
}

let suites = {
  open Mt
  list{("sum", _ => Eq(55, f()))}
}

Mt.from_pair_suites(__MODULE__, suites)
