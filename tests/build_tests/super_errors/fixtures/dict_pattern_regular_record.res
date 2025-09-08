type x = {one: int}

let constrainedAsDict = (dict: x) =>
  switch dict {
  | dict{"one": "one"} => Js.log("one")
  | _ => Js.log("not one")
  }
