try {
  let x = 1
  let y = 2
  dangerousCall()
} catch {
| Foo => Console.log()
| Exit => Console.log()
}

try myDangerousFn() catch {
| Foo => Console.log()
}

let x = {
  let y = 1
  try {
    apply()
  } catch {
  | _ => 2
  }
}

@attr @attr2
try myDangerousFn() catch {
| Foo => Console.log()
}


let () =
  @attr @attr2
  try myDangerousFn() catch {
  | Foo => Console.log()
  }
