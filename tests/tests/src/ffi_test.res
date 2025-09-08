@val external f: int => int = "xx"

let u = () => f(3)
let v = Js.Null.empty

let (a, b, c, d) = (true, false, Js.Null.empty, Js.Undefined.empty)

module Textarea = {
  type t
  @new external create: unit => t = "TextArea"
  /* TODO: */
  @set external set_minHeight: (t, int) => unit = "minHeight"
  @get external get_minHeight: t => int = "minHeight"
  @send external draw: (t, string) => unit = "string"
}

/*
external never_used : Textarea.t ->  int -> int  = "minHeight" [@@get]

let v = never_used (Textarea.create ()) 3 
*/
module Int32Array = {
  type t
  @new external create: int => t = "Int32Array"
  @get_index external get: (t, int) => int = ""
  @set_index external set: (t, int, int) => unit = ""
}

let v = () => {
  let u = Textarea.create()
  Textarea.set_minHeight(u, 3)
  Textarea.get_minHeight(u)
}
/* Textarea.set_minHeight_x */

let f = () => {
  module Array = Int32Array
  let v = Array.create(32)

  Int32Array.set(v, 0, 3)
  Int32Array.get(v, 0)
}
