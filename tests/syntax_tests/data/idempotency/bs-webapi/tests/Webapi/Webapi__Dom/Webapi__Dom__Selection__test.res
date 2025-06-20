open Webapi.Dom
open Selection

let node = document->Document.createElement("strong")
let sel =
  document
  ->Document.asHtmlDocument
  ->TestHelpers.unsafelyUnwrapOption
  ->HtmlDocument.getSelection

let range = Range.make()

let _ = anchorNode(sel)
let _ = anchorOffset(sel)
let _ = focusNode(sel)
let _ = focusOffset(sel)
let _ = isCollapsed(sel)
let _ = rangeCount(sel)
let _ = getRangeAt(0, sel)

collapse(node, 0, sel)
extend(node, 0, sel)
collapseToStart(sel)
collapseToEnd(sel)
selectAllChildren(node, sel)
addRange(range, sel)
removeRange(range, sel)
removeAllRanges(sel)
deleteFromDocument(sel)
setBaseAndExtent(node, 0, node, 0, sel)
let _ = toString(sel)
let _ = containsNode(node, sel)
let _ = containsNodePartly(node, sel)
