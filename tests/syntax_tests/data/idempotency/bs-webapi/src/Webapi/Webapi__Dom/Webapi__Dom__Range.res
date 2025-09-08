type t = Dom.range

@new external make: unit => t = "Range" /* experimental */

@get external collapsed: t => bool = ""
@get external commonAncestorContainer: t => Dom.node = ""
@get external endContainer: t => Dom.node = ""
@get external endOffset: t => int = ""
@get external startContainer: t => Dom.node = ""
@get external startOffset: t => int = ""

let comparePoint: (Dom.node_like<'a>, int, t) => Webapi__Dom__Types.compareResult = (
  node,
  offset,
  self,
) => Webapi__Dom__Types.decodeCompareResult(comparePoint(node, offset, self))
