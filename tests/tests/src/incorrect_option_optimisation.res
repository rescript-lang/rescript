let bla = (~val1: bool, ~val2: bool) => {
  let a = Some(val1)
  let b = Some(val2)

  switch (a, b) {
  | (_, Some(true))
  | (Some(true), _) => "a"
  | _ => "b"
  }
}
