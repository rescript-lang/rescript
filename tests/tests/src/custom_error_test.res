let test_js_error = () =>
  switch Js.Json.parseExn(` {"x" : }`) {
  | exception Js.Exn.Error(err) =>
    Js.log(Js.Exn.stack(err))
    None
  | e => Some(e)
  }

let test_js_error2 = () =>
  try Js.Json.parseExn(` {"x" : }`) catch {
  | Js.Exn.Error(err) as e =>
    Js.log(Js.Exn.stack(err))
    throw(e)
  }

let example1 = () =>
  switch Js.Json.parseExn(` {"x"  }`) {
  | exception Js.Exn.Error(err) =>
    Js.log(Js.Exn.stack(err))
    None
  | v => Some(v)
  }

let example2 = () =>
  try Some(Js.Json.parseExn(` {"x"}`)) catch {
  | Js.Exn.Error(_) => None
  }

/* let () = 
  Js.log @@ test_js_error () 
*/
