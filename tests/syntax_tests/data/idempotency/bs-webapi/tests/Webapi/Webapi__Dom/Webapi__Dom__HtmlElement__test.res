open Webapi.Dom
open HtmlElement

let el =
  document
  ->Document.createElement("strong")
  ->Element.asHtmlElement
  ->TestHelpers.unsafelyUnwrapOption
let el2 =
  document
  ->Document.createElement("small")
  ->Element.asHtmlElement
  ->TestHelpers.unsafelyUnwrapOption
let event = document->Document.createEvent("my-event")

let _ = accessKey(el)
let _ = setAccessKey(el, "")
let _ = accessKeyLabel(el)
let _ = contentEditable(el)
let _ = setContentEditable(el, Inherit)
let _ = isContentEditable(el)
let _ = contextMenu(el)
let _ = setContextMenu(el, el2)
let _ = dataset(el)
let _ = dir(el)
let _ = setDir(el, Rtl)
let _ = draggable(el)
let _ = setDraggable(el, true)
let _ = dropzone(el)
let _ = hidden(el)
let _ = setHidden(el, true)
let _ = itemScope(el)
let _ = setItemScope(el, true)
let _ = itemType(el)
let _ = itemId(el)
let _ = setItemId(el, "my-id")
let _ = itemRef(el)
let _ = itemProp(el)
let _ = itemValue(el)
let _ = setItemValue(el, %raw("{}"))
let _ = lang(el)
let _ = setLang(el, "en")
let _ = offsetHeight(el)
let _ = offsetLeft(el)
let _ = offsetParent(el)
let _ = offsetTop(el)
let _ = offsetWidth(el)
let _ = spellcheck(el)
let _ = setSpellcheck(el, true)
let _ = style(el)
/* let _ = setStyle el CSSStyleDeclaration.t; /* TODO: No way to make a CSSStyleDeclaration at the moment */ */
let _ = tabIndex(el)
let _ = setTabIndex(el, 42)
let _ = title(el)
let _ = setTitle(el, "hovertext!")
let _ = translate(el)
let _ = setTranslate(el, true)

blur(el)
click(el)
focus(el)
forceSpellCheck(el)
