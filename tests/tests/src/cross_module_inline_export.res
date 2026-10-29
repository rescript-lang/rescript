@inline(crossModule)
let apply = (value, f) => f(value)

let ordinary = (value, f) => f(value)

@inline(crossModule)
let catchMessage = f =>
  try f() catch {
  | JsExn(e) => JsExn.message(e)
  | _ => None
  }
