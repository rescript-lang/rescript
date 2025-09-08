@raises(Yojson.Json_error)
let foo = x => Yojson.Basic.from_string(x)

let bar = (str, json) =>
  switch {
    open Yojson.Basic.Util
    json |> member(str)
  } {
  | j => j
  | exception Yojson.Basic.Util.Type_error("a", d) if d == json => json
  }

@raises(Yojson.Basic.Util.Type_error)
let toString = x => Yojson.Basic.Util.to_string(x)

@raises(Yojson.Basic.Util.Type_error)
let toInt = x => Yojson.Basic.Util.to_int(x)
