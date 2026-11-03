@throws(JsExn)
external parse: string => int = "parseThrowing"

module Inner = {
  @throws(Not_found)
  external find: int => int = "findThrowing"
}

module type S = {
  @throws(Not_found)
  let get: int => int
}

module Make = (X: {}): S => {
  let get = x => x
}

@throws(Not_found)
let viaLet = x => x > 0 ? x : throw(Not_found)
