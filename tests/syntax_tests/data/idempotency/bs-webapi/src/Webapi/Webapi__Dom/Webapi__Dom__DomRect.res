type t = Dom.domRect

@new
external make: (~x: float, ~y: float, ~width: float, ~height: float) => t =
  "DOMRect" /* experimental */

@get external top: t => float = ""
@get external bottom: t => float = ""
@get external left: t => float = ""
@get external right: t => float = ""
@get external height: t => float = ""
@get external width: t => float = ""
@get external x: t => float = ""
@get external y: t => float = ""
