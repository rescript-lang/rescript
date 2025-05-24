let el = Webapi.Dom.document->Webapi.Dom.Document.createElement("strong")

let handler = entries => {
  let entry = entries[0]
  let _: Dom.domRect = Webapi.ResizeObserver.ResizeObserverEntry.contentRect(entry)
  let _: Dom.element = Webapi.ResizeObserver.ResizeObserverEntry.target(entry)
}

let observer = Webapi.ResizeObserver.make(handler)

Webapi.ResizeObserver.observe(observer, el)
Webapi.ResizeObserver.unobserve(observer, el)
Webapi.ResizeObserver.disconnect(observer)
