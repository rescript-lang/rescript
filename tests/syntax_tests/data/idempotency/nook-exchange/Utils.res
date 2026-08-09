@val @scope("Object")
external objectAssign: (dict<'a>, dict<'a>) => unit = "assign"

@val @scope("Object") @variadic
external objectAssignMany: array<dict<'a>> => unit = "assign"

let cloneJsDict = dict => {
  let clone = Dict.make()
  objectAssign(clone, dict)
  clone
}

let combineJsDict = (dictA, dictB) => {
  let combined = Dict.make()
  objectAssignMany([combined, dictA, dictB])
  combined
}

type any
let _internalDeleteJsDictKey: (any, string) => unit = %raw(
  "function(dict, key) { delete dict[key]; }"
)
external convertToAny: dict<'a> => any = "%identity"

let deleteJsDictKey = (dict, key) => _internalDeleteJsDictKey(convertToAny(dict), key)

let getElementForDomRef = domRef =>
  domRef->React.Ref.current->Nullable.toOption->Belt.Option.getExn

let capitalizeFirstLetter = input =>
  String.toUpperCase(String.charAt(input, 0)) ++ (input->String.slice(~start=1))

let throttle = (fn, ms) => {
  let timeoutRef = ref(None)
  _ => {
    switch timeoutRef.contents {
    | Some(timeout) => clearTimeout(timeout)
    | None => ()
    }
    timeoutRef := Some(setTimeout(() => {
          timeoutRef := None
          fn()
        }, ms))
  }
}

let useViewportWidth = () => {
  let (viewportWidth, setViewportWidth) = React.useState(() => {
    open Webapi.Dom
    window->Window.innerWidth
  })
  React.useEffect0(() => {
    open Webapi.Dom
    let onResize = _ => setViewportWidth(_ => window->Window.innerWidth)
    let onResize = throttle(onResize, 300)
    window->Window.addEventListener("resize", onResize)
    Some(() => window->Window.removeEventListener("resize", onResize))
  })
  viewportWidth
}

@get
external mediaQueryListMatches: Webapi.Dom.Window.mediaQueryList => bool = "matches"
let browserSupportsHover = {
  open Webapi.Dom
  window->Window.matchMedia("(hover: hover)")
}->mediaQueryListMatches

let getPath = (~url: ReasonReactRouter.url) =>
  "/" ++ (Belt.List.toArray(url.path)->Array.joinUnsafe("/"))

let getPathWithSearch = (~url: ReasonReactRouter.url) =>
  "/" ++
  ((Belt.List.toArray(url.path)->Array.joinUnsafe("/")) ++
  switch url.search {
  | "" => ""
  | search => "?" ++ search
  })

let getItemDetailUrl = (~itemId, ~variant) => {
  let url = ReasonReactRouter.dangerouslyGetInitialUrl()
  "/" ++
  (Array.joinUnsafe(Belt.List.toArray(url.path), "/") ++
  (switch url.search {
  | "" => ""
  | search => "?" ++ search
  } ++
  ("#i" ++
  (string_of_int(itemId) ++
  switch variant {
  | Some(variant) => ":" ++ string_of_int(variant)
  | None => ""
  }))))
}
