type t = Dom.htmlCollection

@val @scope(("Array", "prototype", "slice")) external toArray: t => array<Dom.element> = "call"

@get external length: t => int = ""
