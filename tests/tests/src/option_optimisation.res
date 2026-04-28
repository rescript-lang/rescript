let boolean = (~val1: bool, ~val2: bool) => {
  let a = Some(val1)
  let b = Some(val2)

  switch (a, b) {
  | (_, Some(true))
  | (Some(true), _) => "a"
  | _ => "b"
  }
}

let null = (~val1: Nullable.t<int>, ~val2: Nullable.t<int>) => {
  let a = Some(val1)
  let b = Some(val2)

  switch (a, b) {
  | (_, Some(Value(_)))
  | (Some(Value(_)), _) => "a"
  | _ => "b"
  }
}

let constant = () => {
  let opt = Some(42)
  switch opt {
  | Some(x) => Console.log(x)
  | None => ()
  }
}

let param = (opt: int) => {
  switch Some(opt) {
  | Some(x) => Console.log(x)
  | None => ()
  }
}
