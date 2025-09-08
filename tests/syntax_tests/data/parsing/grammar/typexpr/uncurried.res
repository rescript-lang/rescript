type t = {
  mutable field: (. float, int, bool) => unit
}

type t = (. float, int, bool) => unit

type t = (. @attr float, @attr2 int, . @attr3 bool, @attr4 string) => unit
type t = @attr (. float) => @attr2 int => @attr3 (. bool) => @attr4 string => unit
type t = (. (@attr float), (@attr2 int), . (@attr3 bool), (@attr4 string)) => unit

@val
external setTimeout : ((. ()) => unit, int) => timerId = "setTimeout"
// totally different meaning
external setTimeout : (. unit => unit, int) => timerId = "setTimeout"
