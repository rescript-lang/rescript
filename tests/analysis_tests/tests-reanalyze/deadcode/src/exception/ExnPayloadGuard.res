exception Type_error(string, int)

module Util = {
  @throws(Type_error)
  let member = (s, j) => s == "" ? throw(Type_error(s, j)) : j
}

let bar = (str, json) =>
  switch {
    open Util
    member(str, json)
  } {
  | j => j
  | exception Type_error("a", d) if d == json => json
  }
