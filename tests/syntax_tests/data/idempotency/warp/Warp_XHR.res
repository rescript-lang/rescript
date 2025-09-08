type t

type readyState =
  | Unsent
  | Opened
  | HeadersReceived
  | Loading
  | Done
  | Unknown

let decodeReadyState = readyState =>
  switch readyState {
  | 0 => Unsent
  | 1 => Opened
  | 2 => HeadersReceived
  | 3 => Loading
  | 4 => Done
  | _ => Unknown
  }

@new external make: unit => t = "XMLHttpRequest"

@get external readyStateExternal: t => int = "readyState"

let readyState = (xhr: t) => decodeReadyState(readyStateExternal(xhr))

@get
external responseArrayBuffer: t => Js.Nullable.t<Js.Typed_array.array_buffer> = "response"

@get
external responseDocument: t => Js.Nullable.t<Dom.document> = "response"

@get external responseJson: t => Js.Nullable.t<Js.Json.t> = "response"

@get external responseText: t => Js.Nullable.t<string> = "responseText"

@get external responseType: t => string = "responseType"

@get external responseUrl: t => Js.Nullable.t<string> = "responseUrl"

@get
external responseXml: t => Js.Nullable.t<Dom.xmlDocument> = "responseXml"

@set
external setResponseType: (
  t,
  @string
  [
    | @as("arraybuffer") #arrayBuffer
    | #document
    | #json
    | #text
  ],
) => string = "responseType"

@get external status: t => int = "status"

@get external statusText: t => string = "statusText"

@get external timeout: t => int = "timeout"

@set external setTimeout: (t, int) => int = "timeout"

@get external withCredentials: t => bool = "withCredentials"

@set external setWithCredentials: (t, bool) => bool = "withCredentials"

@send external abort: t => unit = "abort"

@send
external getAllResponseHeaders: t => Js.Nullable.t<string> = "getAllResponseHeaders"

@send
external getResponseHeader: (t, string) => Js.Nullable.t<string> = "getResponseHeader"

@send
external open_: (
  t,
  ~method: string,
  ~url: string,
  ~async: bool=?,
  ~user: string=?,
  ~password: string=?,
  unit,
) => unit = "open"

@send external overrideMimeType: (t, string) => unit = "overrideMimeType"

@send external send: t => unit = "send"

@send
external sendArrayBuffer: (t, Js.Typed_array.array_buffer) => unit = "send"

@send external sendDocument: (t, Dom.document) => unit = "send"

@send external sendString: (t, string) => unit = "send"

@send
external setRequestHeader: (t, string, string) => unit = "setRequestHeader"

@set
external onReadyStateChange: (t, Dom.event => unit) => unit = "onreadystatechange"

@set external onAbort: (t, Dom.progressEvent => unit) => unit = "onabort"

@set external onError: (t, Dom.progressEvent => unit) => unit = "onerror"

@set external onLoad: (t, Dom.progressEvent => unit) => unit = "onload"

@set
external onLoadEnd: (t, Dom.progressEvent => unit) => unit = "onloadend"

@set
external onLoadStart: (t, Dom.progressEvent => unit) => unit = "onloadstart"

@set
external onProgress: (t, Dom.progressEvent => unit) => unit = "onprogress"

@set
external onTimeout: (t, Dom.progressEvent => unit) => unit = "ontimeout"

@send
external addEventListener: (
  t,
  @string
  [
    | #abort(Dom.progressEvent => unit)
    | #error(Dom.progressEvent => unit)
    | #load(Dom.progressEvent => unit)
    | @as("loadend") #loadEnd(Dom.progressEvent => unit)
    | @as("loadstart") #loadStart(Dom.progressEvent => unit)
    | #progress(Dom.progressEvent => unit)
    | @as("readystatechange") #readyStateChange(Dom.event => unit)
    | #timeout(Dom.progressEvent => unit)
  ],
) => unit = "addEventListener"

@send
external addEventListenerWithOptions: (
  t,
  @string
  [
    | #abort(Dom.progressEvent => unit)
    | #error(Dom.progressEvent => unit)
    | #load(Dom.progressEvent => unit)
    | @as("loadend") #loadEnd(Dom.progressEvent => unit)
    | @as("loadstart") #loadStart(Dom.progressEvent => unit)
    | #progress(Dom.progressEvent => unit)
    | @as("readystatechange") #readyStateChange(Dom.event => unit)
    | #timeout(Dom.progressEvent => unit)
  ],
  {"capture": bool, "once": bool, "passive": bool},
) => unit = "addEventListener"

@send
external removeEventListener: (
  t,
  @string
  [
    | #abort(Dom.progressEvent => unit)
    | #error(Dom.progressEvent => unit)
    | #load(Dom.progressEvent => unit)
    | @as("loadend") #loadEnd(Dom.progressEvent => unit)
    | @as("loadstart") #loadStart(Dom.progressEvent => unit)
    | #progress(Dom.progressEvent => unit)
    | @as("readystatechange") #readyStateChange(Dom.event => unit)
    | #timeout(Dom.progressEvent => unit)
  ],
) => unit = "removeEventListener"

@send
external removeEventListenerWithOptions: (
  t,
  @string
  [
    | #abort(Dom.progressEvent => unit)
    | #error(Dom.progressEvent => unit)
    | #load(Dom.progressEvent => unit)
    | @as("loadend") #loadEnd(Dom.progressEvent => unit)
    | @as("loadstart") #loadStart(Dom.progressEvent => unit)
    | #progress(Dom.progressEvent => unit)
    | @as("readystatechange") #readyStateChange(Dom.event => unit)
    | #timeout(Dom.progressEvent => unit)
  ],
  {"capture": bool, "passive": bool},
) => unit = "removeEventListener"
