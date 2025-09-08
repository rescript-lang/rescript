let v = %raw(`Number.EPSILON?Number.EPSILON:2.220446049250313e-16`)

let suites = {
  open Mt
  list{("epsilon", _ => Eq(epsilon_float, v)), ("raw_epsilon", _ => Eq(2.220446049250313e-16, v))}
}

Mt.from_pair_suites(__MODULE__, suites)
