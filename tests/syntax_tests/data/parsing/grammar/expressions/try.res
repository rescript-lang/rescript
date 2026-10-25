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

try {
  dangerousCall()
} finally {
  cleanup()
}

try dangerousCall() catch {
| Foo => Console.log("catched Foo")
} finally {
  cleanup()
}

let x = try 1 finally {()}

// `finally` stays an identifier unless a block follows it
try dangerousCall() catch {
| _ => ()
}
finally(x)
