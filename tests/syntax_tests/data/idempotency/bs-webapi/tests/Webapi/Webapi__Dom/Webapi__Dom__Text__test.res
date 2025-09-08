open Webapi.Dom

let node = document->Document.createTextNode("text")->Text.asNode

let text = Text.ofNode(node)
