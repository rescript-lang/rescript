let v = Not_found

let u = Not_found
let s = End_of_file
let suites = {
  open Mt
  list{("not_found_equal", _ => Eq(u, v)), ("not_found_not_equal_end_of_file", _ => Neq(u, s))}
}

Mt.from_pair_suites(__MODULE__, suites)
