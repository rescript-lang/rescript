type x = {one: int}

let constrainedAsDict = (dict: x) =>
  switch dict {
  | dict{"one": "one"} => Console.log("one")
  | _ => Console.log("not one")
  }
