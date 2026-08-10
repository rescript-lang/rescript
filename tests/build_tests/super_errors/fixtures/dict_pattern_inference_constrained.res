let foo = dict =>
  switch dict {
  | dict{"one": 1} =>
    let _: dict<string> = dict
    Console.log("one")
  | _ => Console.log("not one")
  }
