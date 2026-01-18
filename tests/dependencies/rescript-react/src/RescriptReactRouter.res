@scope("globalThis")
external window: option<Dom.window> = "window"

@scope("globalThis")
external history: option<Dom.history> = "history"

@get external location: Dom.window => Dom.location = "location"

/* actually the cb is Dom.event => unit, but let's restrict the access for now */
@send
external addEventListener: (Dom.window, string, unit => unit) => unit = "addEventListener"

@send
external removeEventListener: (Dom.window, string, unit => unit) => unit = "removeEventListener"

@send
external dispatchEvent: (Dom.window, Dom.event) => unit = "dispatchEvent"

@get external pathname: Dom.location => string = "pathname"

@get external hash: Dom.location => string = "hash"

@get external search: Dom.location => string = "search"

@send
external pushState: (Dom.history, @as(json`null`) _, @as("") _, ~href: string) => unit = "pushState"

@send
external replaceState: (Dom.history, @as(json`null`) _, @as("") _, ~href: string) => unit =
  "replaceState"

@val external event: 'a = "Event"

@new external makeEventIE11Compatible: string => Dom.event = "Event"

@val @scope("document")
external createEventNonIEBrowsers: string => Dom.event = "createEvent"

@send
external initEventNonIEBrowsers: (Dom.event, string, bool, bool) => unit = "initEvent"

let safeMakeEvent = eventName =>
  if Js.typeof(event) == "function" {
    makeEventIE11Compatible(eventName)
  } else {
    let event = createEventNonIEBrowsers("Event")
    initEventNonIEBrowsers(event, eventName, true, true)
    event
  }

/* This is copied from array.ml. We want to cut dependencies for rescript-react so
 that it's friendlier to use in size-constrained codebases */
let arrayToList = a => {
  let rec tolist = (i, res) =>
    if i < 0 {
      res
    } else {
      tolist(i - 1, list{a->Js.Array2.unsafe_get(i), ...res})
    }
  tolist(a->Js.Array2.length - 1, list{})
}
/* if we ever roll our own parser in the future, make sure you test all url combinations
   e.g. foo.com/?#bar
 */
/* sigh URLSearchParams doesn't work on IE11, edge16, etc. */
/* actually you know what, not gonna provide search for now. It's a mess.
 We'll let users roll their own solution/data structure for now */
let pathParse = str =>
  switch str {
  | ""
  | "/" =>
    list{}
  | raw =>
    /* remove the preceeding /, which every pathname seems to have */
    let raw = raw->Js.String2.sliceToEnd(~from=1)
    /* remove the trailing /, which some pathnames might have. Ugh */
    let raw = switch raw->Js.String2.get(raw->Js.String2.length - 1) {
    | "/" => raw->Js.String2.slice(~from=0, ~to_=-1)
    | _ => raw
    }
    /* remove search portion if present in string */
    let raw = switch raw->Js.String2.splitAtMost("?", ~limit=2) {
    | [path, _] => path
    | _ => raw
    }

    raw->Js.String2.split("/")->Js.Array2.filter(item => item->Js.String2.length != 0)->arrayToList
  }
let path = (~serverUrlString=?, ()) =>
  switch (serverUrlString, window) {
  | (None, None) => list{}
  | (Some(serverUrlString), _) => pathParse(serverUrlString)
  | (_, Some(window: Dom.window)) => pathParse(window->location->pathname)
  }
let hash = () =>
  switch window {
  | None => ""
  | Some(window: Dom.window) =>
    switch window->location->hash {
    | ""
    | "#" => ""
    | raw =>
      /* remove the preceeding #, which every hash seems to have.
       Why is this even included in location.hash?? */
      raw->Js.String2.sliceToEnd(~from=1)
    }
  }
let searchParse = str =>
  switch str {
  | ""
  | "?" => ""
  | raw =>
    switch raw->Js.String2.splitAtMost("?", ~limit=2) {
    | [_, search] => search
    | _ => ""
    }
  }

let search = (~serverUrlString=?, ()) =>
  switch (serverUrlString, window) {
  | (None, None) => ""
  | (Some(serverUrlString), _) => searchParse(serverUrlString)
  | (_, Some(window: Dom.window)) => searchParse(window->location->search)
  }

let push = path =>
  switch (history, window) {
  | (None, _)
  | (_, None) => ()
  | (Some(history: Dom.history), Some(window: Dom.window)) =>
    pushState(history, ~href=path)
    dispatchEvent(window, safeMakeEvent("popstate"))
  }

let replace = path =>
  switch (history, window) {
  | (None, _)
  | (_, None) => ()
  | (Some(history: Dom.history), Some(window: Dom.window)) =>
    replaceState(history, ~href=path)
    dispatchEvent(window, safeMakeEvent("popstate"))
  }

type url = {
  path: list<string>,
  hash: string,
  search: string,
}

let urlNotEqual = (a, b) => {
  let rec listNotEqual = (aList, bList) =>
    switch (aList, bList) {
    | (list{}, list{}) => false
    | (list{}, list{_, ..._})
    | (list{_, ..._}, list{}) => true
    | (list{aHead, ...aRest}, list{bHead, ...bRest}) =>
      if aHead !== bHead {
        true
      } else {
        listNotEqual(aRest, bRest)
      }
    }
  a.hash !== b.hash || (a.search !== b.search || listNotEqual(a.path, b.path))
}

type watcherID = unit => unit

let url = (~serverUrlString=?, ()) => {
  path: path(~serverUrlString?, ()),
  hash: hash(),
  search: search(~serverUrlString?, ()),
}

/* alias exposed publicly */
let dangerouslyGetInitialUrl = url

let watchUrl = callback =>
  switch window {
  | None => () => ()
  | Some(window: Dom.window) =>
    let watcherID = () => callback(url())
    addEventListener(window, "popstate", watcherID)
    watcherID
  }

let unwatchUrl = watcherID =>
  switch window {
  | None => ()
  | Some(window: Dom.window) => removeEventListener(window, "popstate", watcherID)
  }

let useUrl = (~serverUrl=?, ()) => {
  let (url, setUrl) = React.useState(() =>
    switch serverUrl {
    | Some(url) => url
    | None => dangerouslyGetInitialUrl()
    }
  )

  React.useEffect(() => {
    let watcherId = watchUrl(url => setUrl(_ => url))

    // check for updates that may have occured between the initial state and
    // the subscribe above
    let newUrl = dangerouslyGetInitialUrl()
    if urlNotEqual(newUrl, url) {
      setUrl(_ => newUrl)
    }

    Some(() => unwatchUrl(watcherId))
  }, [])

  url
}
