module Impl = (
  T: {
    type t
  },
) => {
  external asNode: T.t => Dom.node = "%identity"

  /* baseURI */
  @get external childNodes: T.t => Dom.nodeList = ""
  @get @return(nullable) external firstChild: T.t => option<Dom.node> = ""
  @get external innerText: T.t => string = ""
  @set external setInnerText: (T.t, string) => unit = "innerText"
  @get @return(nullable) external lastChild: T.t => option<Dom.node> = ""
  @get @return(nullable) external nextSibling: T.t => option<Dom.node> = ""
  @get external nodeName: T.t => string = "" /* nodePrincipal */
  @get external nodeType: T.t => int /* nodeType enum */ = ""
  let nodeType: T.t => Webapi__Dom__Types.nodeType = self =>
    Webapi__Dom__Types.decodeNodeType(nodeType(self))
  @get @return(nullable) external nodeValue: T.t => option<string> = ""
  @set external setNodeValue: (T.t, Js.null<string>) => unit = "nodeValue"
  /* let setNodeValue : T.t => option string => unit = fun self value => setNodeValue self (Js.Null.fromOption value); */ /* temporarily removed to reduce codegen size */
  /* Not supported yet
  external setNodeValue : T.t => string => unit = "nodeValue" [@@set];
  external clearNodeValue : T.t => _ [@as {json|null|json}] => unit = "nodeValue" [@@set];
 */
  /* outerText */
  @get external ownerDocument: T.t => Dom.document = ""
  @get @return(nullable) external parentElement: T.t => option<Dom.element> = ""
  @get @return(nullable) external parentNode: T.t => option<Dom.node> = ""
  @get @return(nullable) external previousSibling: T.t => option<Dom.node> = ""
  @get external rootNode: T.t => Dom.node = ""
  @get external textContent: T.t => string = ""
  @set external setTextContent: (T.t, string) => unit = "textContent"

}

type t = Dom.node

include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Impl({
  type t = t
})
