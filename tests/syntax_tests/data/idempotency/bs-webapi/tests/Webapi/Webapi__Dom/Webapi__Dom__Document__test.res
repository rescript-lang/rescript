open Webapi.Dom
open Document

let el = document->createElement("strong")

let _ = characterSet(document)
let _ = compatMode(document)
let _ = doctype(document)
let _ = documentElement(document)
let _ = documentURI(document)
let _ = hidden(document)
let _ = implementation(document)
let _ = lastStyleSheetSet(document)
let _ = pointerLockElement(document)
let _ = preferredStyleSheetSet(document)
let _ = scrollingElement(document)
let _ = selectedStyleSheetSet(document)
let _ = setSelectedStyleSheetSet(document, "muh-stylesheet")
let _ = styleSheets(document)
let _ = styleSheetSets(document)
let _ = visibilityState(document)

let _ = adoptNode(el, document)
let _ = createAttribute("data-foo", document)
let _ = createAttributeNS("http://...", "foo", document)
let _ = createComment("witty comment", document)
let _ = createDocumentFragment(document)
let _ = createElement("div", document)
let _ = createElementWithOptions(
  "div",
  %raw("{}"),
  document,
) /* I've no idea what this options object is supposed to be, even the spec doesn't seem to bother explaining it */
let _ = createElementNS("http://...", "foo", document)
let _ = createElementNSWithOptions(
  "http://...",
  "div",
  %raw("{}"),
  document,
) /* I've no idea what this options object is supposed to be, even the spec doesn't seem to bother explaining it */
let _ = createEvent("MyCustomEvent", document)
let _ = createNodeIterator(el, document)
let _ = createNodeIteratorWithWhatToShow(el, WhatToShow._All, document)
let _ = createNodeIteratorWithWhatToShowFilter(
  el,
  {
    open WhatToShow
    many(list{_Element, _Attribute})
  },
  NodeFilter.make(_ => 0),
  document,
)
let _ = createRange(document)
let _ = createTextNode("Very reasonable!", document)
let _ = createTreeWalker(el, document)
let _ = createTreeWalkerWithWhatToShow(el, WhatToShow._All, document)
let _ = createTreeWalkerWithWhatToShowFilter(
  el,
  {
    open WhatToShow
    many(list{_Element, _Attribute})
  },
  NodeFilter.make(_ => 0),
  document,
)
let _ = elementFromPoint(0, 0, document)
let _ = elementsFromPoint(0, 0, document)
let _ = enableStyleSheetsForSet("my-stylesheet-set", document)
let _ = exitPointerLock(document)
let _ = getAnimations(document)
let _ = getElementsByClassName("lstlisting", document)
let _ = getElementsByTagName("code", document)
let _ = getElementsByTagNameNS("http://...", "foo", document)
let _ = importNode(el, document)
let _ = importNodeDeep(el, document)
/* TODO: These get dead code eliminated
let _ = registerElement(document, "my-component");
let _ = registerElementWithOptions(document, "my-component", [%raw "{}"]);
*/
let _ = getElementById("root", document)
let _ = querySelector(".lstlisting", document)
let _ = querySelectorAll(".lstlisting", document)
