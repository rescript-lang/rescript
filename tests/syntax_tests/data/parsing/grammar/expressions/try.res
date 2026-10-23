try {
  let x = 1
  let y = 2
  dangerousCall(x + y)
} catch {
| Foo => Console.log("catched Foo")
| Exit => Console.log("catched exit")
}

@attr
try myDangerousFn() catch {
| Foo => Console.log("catched Foo")
}

let x = {
  let y = 1
  try {
    apply(y)
  } catch {
  | _ => 2
  }
}
