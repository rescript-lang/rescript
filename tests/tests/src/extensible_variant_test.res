type attr = ..

type attr += Str(string)

module N = {
  type attr += Int(int, int)
}

type attr += Int(int, int)

let to_int = (x: attr) =>
  switch x {
  | Str(_) => -1
  | N.Int(a, _) => a
  | Int(_, b) => b
  | _ => assert(false)
  }

let suites = {
  open Mt
  list{
    ("test_int", _ => Eq(3, to_int(N.Int(3, 0)))),
    ("test_int2", _ => Eq(0, to_int(Int(3, 0)))),
    ("test_string", _ => Eq(-1, to_int(Str("x")))),
  }
}

Mt.from_pair_suites(__MODULE__, suites)
