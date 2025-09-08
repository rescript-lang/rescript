let foo = dict =>
  switch dict {
  | dict{"one": 1, "two": "hello"} => Js.log("one")
  | _ => Js.log("not one")
  }
