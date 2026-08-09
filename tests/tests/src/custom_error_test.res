let test_js_error = () =>
  switch JSON.parseOrThrow(` {"x" : }`) {
  | exception JsExn(err) =>
    Console.log(JsExn.stack(err))
    None
  | e => Some(e)
  }

let test_js_error2 = () =>
  try JSON.parseOrThrow(` {"x" : }`) catch {
  | JsExn(err) as e =>
    Console.log(JsExn.stack(err))
    throw(e)
  }

let example1 = () =>
  switch JSON.parseOrThrow(` {"x"  }`) {
  | exception JsExn(err) =>
    Console.log(JsExn.stack(err))
    None
  | v => Some(v)
  }

let example2 = () =>
  try Some(JSON.parseOrThrow(` {"x"}`)) catch {
  | JsExn(_) => None
  }

/* let () = 
  Console.log @@ test_js_error () 
*/
