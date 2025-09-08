/* internal, moved out of Impl to reduce unnecessary code duplication */
let ofNode = (node: Dom.node): option<'a> =>
  Webapi__Dom__Node.nodeType(node) == Element ? Some(Obj.magic(node)) : None

module Impl = (
  T: {
    type t
  },
) => {
  let asHtmlElement: T.t => Js.null<Dom.htmlElement> = %raw(`
    function (element) {
      // BEWARE: Assumes "contentEditable" uniquely identifies an HTMLELement
      return element.contentEditable !== undefined ?  element : null;
    }
  `)
  @deprecated("asHtmlElement uses a weak heuristic, consider using unsafeAsHtmlElement instead")
  let asHtmlElement: T.t => option<Dom.htmlElement> = self => Js.Null.toOption(asHtmlElement(self))

  external unsafeAsHtmlElement: T.t => Dom.htmlElement = "%identity"
  let ofNode: Dom.node => option<T.t> = ofNode

  @get external attributes: T.t => Dom.namedNodeMap = ""
  @get external classList: T.t => Dom.domTokenList = ""
  @get external className: T.t => string = ""
  @set external setClassName: (T.t, string) => unit = "className"
  @get external clientHeight: T.t => int = "" /* experimental */
  @get external clientLeft: T.t => int = "" /* experimental */
  @get external clientTop: T.t => int = "" /* experimental */
  @get external clientWidth: T.t => int = "" /* experimental */
  @get external id: T.t => string = ""
  @set external setId: (T.t, string) => unit = "id"
  @get external innerHTML: T.t => string = ""
  @set external setInnerHTML: (T.t, string) => unit = "innerHTML"
  @get external localName: T.t => string = ""
  @get @return(nullable) external namespaceURI: T.t => option<string> = ""
  @get external outerHTML: T.t => string = "" /* experimental, but widely supported */
  @set
  external setOuterHTML: (T.t, string) => unit =
    "outerHTML" /* experimental, but widely supported */
  @get @return(nullable) external prefix: T.t => option<string> = ""
  @get external scrollHeight: T.t => int = "" /* experimental, but widely supported */
  @get external scrollLeft: T.t => float = "" /* experimental */
  @set external setScrollLeft: (T.t, float) => unit = "scrollLeft" /* experimental */
  @get external scrollTop: T.t => float = "" /* experimental, but widely supported */
  @set
  external setScrollTop: (T.t, float) => unit = "scrollTop" /* experimental, but widely supported */
  @get external scrollWidth: T.t => int = "" /* experimental */
  @get external shadowRoot: T.t => Dom.element = "" /* experimental */
  @get external slot: T.t => string = "" /* experimental */
  @set external setSlot: (T.t, string) => unit = "slot" /* experimental */
  @get external tagName: T.t => string = ""

  let insertAdjacentElement: (
    Webapi__Dom__Types.insertPosition,
    Dom.element_like<'a>,
    T.t,
  ) => unit = (position, element, self) =>
    insertAdjacentElement(Webapi__Dom__Types.encodeInsertPosition(position), element, self)
  let insertAdjacentHTML: (Webapi__Dom__Types.insertPosition, string, T.t) => unit = (
    position,
    text,
    self,
  ) => insertAdjacentHTML(Webapi__Dom__Types.encodeInsertPosition(position), text, self)
  let insertAdjacentText: (Webapi__Dom__Types.insertPosition, string, T.t) => unit = (
    position,
    text,
    self,
  ) => insertAdjacentText(Webapi__Dom__Types.encodeInsertPosition(position), text, self)
  /* GlobalEventHandlers interface */
  /* Not sure this should be exposed, since EventTarget seems like a better API */

  @set external setOnClick: (T.t, Dom.mouseEvent => unit) => unit = "onclick"
}

/* TODO: This doesn't work. Why?
module Tree (T: { type t; }) => {
  include NodeRe.Impl { type t = Type };
  include EventTargetRe.Impl { type t = Type };
  include Impl { type t = Type };
};

include Tree { type t = Dom.element };
*/

type t = Dom.element

include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Webapi__Dom__GlobalEventHandlers.Impl({
  type t = t
})
include Webapi__Dom__ParentNode.Impl({
  type t = t
})
include Webapi__Dom__NonDocumentTypeChildNode.Impl({
  type t = t
})
include Webapi__Dom__ChildNode.Impl({
  type t = t
})
include Webapi__Dom__Slotable.Impl({
  type t = t
})
include Impl({
  type t = t
})
