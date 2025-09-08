try {
  let x = 1
  let y = 2
  dangerousCall(x + y)
} catch {
| Foo => Js.log("catched Foo")
| Exit => Js.log("catched exit")
}

@attr
try myDangerousFn() catch {
| Foo => Js.log("catched Foo")
}

let x = {
  let y = 1
  try {
    apply(y)
  } catch {
  | _ => 2
  }
}
