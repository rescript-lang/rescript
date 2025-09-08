@get external size_of_t: Obj.t => Js.undefined<'a> = "length"

let f = obj =>
  if Js.typeof(obj) == "function" {
    ()
  } else {
    let size = size_of_t(obj)
    switch Js.Undefined.toOption(size) {
    | None => ()
    | Some(s) => Js.log(s)
    }
  } /* TODO: This case should be peepwholed .. */
