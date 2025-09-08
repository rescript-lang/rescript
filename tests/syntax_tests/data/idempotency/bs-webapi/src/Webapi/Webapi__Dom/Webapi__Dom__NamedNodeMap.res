type t = Dom.namedNodeMap

@get external length: t => int = ""

@val @scope(("Array", "prototype", "slice")) external toArray: t => array<Dom.attr> = "call"
