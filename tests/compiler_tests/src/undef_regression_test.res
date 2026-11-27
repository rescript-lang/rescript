@get external size_of_t: Obj.t => option<'a> = "length"

let f = obj =>
  if typeof(obj) == #function {
    ()
  } else {
    let size = size_of_t(obj)
    switch size {
    | None => ()
    | Some(s) => Console.log(s)
    }
  } /* TODO: This case should be peepwholed .. */
