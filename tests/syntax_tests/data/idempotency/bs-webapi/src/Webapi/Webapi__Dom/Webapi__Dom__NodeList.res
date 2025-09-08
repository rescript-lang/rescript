type t = Dom.nodeList

@val external toArray: t => array<Dom.node> = "Array.prototype.slice.call"

@get external length: t => int = ""
