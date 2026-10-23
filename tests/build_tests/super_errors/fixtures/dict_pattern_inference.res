let foo = dict =>
  switch dict {
  | dict{"one": 1, "two": "hello"} => Console.log("one")
  | _ => Console.log("not one")
  }
