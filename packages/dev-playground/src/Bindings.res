type compilerApi
type compilerInstance
type rescriptCompiler
type compilerConfig
type compileResult
type diagnostic

module Env = {
  @val
  external viteDefaultCompilerVersion: option<string> =
    "import.meta.env.VITE_DEFAULT_COMPILER_VERSION"
  @val external viteCompilerVersions: option<string> = "import.meta.env.VITE_COMPILER_VERSIONS"
  @val external viteBaseUrl: option<string> = "import.meta.env.BASE_URL"
}

module DynamicProperty = {
  @get_index external get: ('value, string) => option<unknown> = ""
}

module Api = {
  @val external global: option<compilerApi> = "globalThis.rescript_compiler"
  @send external makeCompiler: compilerApi => compilerInstance = "make"
  @get external apiVersion: compilerApi => option<string> = "api_version"
}

module Instance = {
  @send external setModuleSystem: (compilerInstance, string) => unit = "setModuleSystem"
  @send external setWarnFlags: (compilerInstance, string) => unit = "setWarnFlags"
  @send external setFilename: (compilerInstance, string) => unit = "setFilename"
  @send external setJsxPreserveMode: (compilerInstance, bool) => unit = "setJsxPreserveMode"
  @send
  external setExperimentalFeatures: (compilerInstance, array<string>) => unit =
    "setExperimentalFeatures"
  @get external rescript: compilerInstance => rescriptCompiler = "rescript"
  @send external getConfig: compilerInstance => compilerConfig = "getConfig"
  @get external version: compilerInstance => option<string> = "version"
}

module Rescript = {
  @get external version: rescriptCompiler => option<string> = "version"
  @send external compile: (rescriptCompiler, string) => compileResult = "compile"
  @send external compileWithDebug: (rescriptCompiler, string) => compileResult = "compileWithDebug"
  @send external format: (rescriptCompiler, string) => compileResult = "format"
}

module Config = {
  @get external moduleSystem: compilerConfig => option<string> = "module_system"
  @get external warnFlags: compilerConfig => option<string> = "warn_flags"
  @get external jsxPreserveMode: compilerConfig => option<bool> = "jsx_preserve_mode"
  @get
  external experimentalFeatures: compilerConfig => option<array<string>> = "experimental_features"
}

module Diagnostic = {
  @get external row: diagnostic => option<int> = "row"
  @get external column: diagnostic => option<int> = "column"
  @get external warnNumber: diagnostic => option<int> = "warnNumber"
  @get external isError: diagnostic => option<bool> = "isError"
  @get external shortMsg: diagnostic => option<string> = "shortMsg"
  @get external fullMsg: diagnostic => option<string> = "fullMsg"
}

module CompileResult = {
  @get external type_: compileResult => option<string> = "type"
  @get external code: compileResult => option<string> = "code"
  @get external jsCode: compileResult => option<string> = "js_code"
  @get external parsetree: compileResult => option<string> = "parsetree"
  @get external typedtree: compileResult => option<string> = "typedtree"
  @get external lambda: compileResult => option<string> = "lambda"
  @get external lam: compileResult => option<string> = "lam"
  @get external errors: compileResult => option<array<diagnostic>> = "errors"
  @get external warnings: compileResult => option<array<diagnostic>> = "warnings"
  @get external msg: compileResult => option<string> = "msg"
  @get external shortMsg: compileResult => option<string> = "shortMsg"
  @get external fullMsg: compileResult => option<string> = "fullMsg"
}

module Window = {
  @val external setTimeout: (unit => unit, int) => int = "setTimeout"
  @val external clearTimeout: int => unit = "clearTimeout"
  @val external requestAnimationFrame: (unit => unit) => unit = "window.requestAnimationFrame"
  @val external isSecureContext: bool = "window.isSecureContext"
}

module Url = {
  type t

  @new external make: (string, string) => t = "URL"
  @get external href: t => string = "href"
  @get external pathname: t => string = "pathname"
}

module Event = {
  @get external target: Dom.event => {..} = "target"
  @get external key: Dom.event => string = "key"
  @send external preventDefault: Dom.event => unit = "preventDefault"

  let value = (event: Dom.event): string => (event->target)["value"]

  let checked = (event: Dom.event): bool => (event->target)["checked"]

  let selectionStart = (event: Dom.event): int => (event->target)["selectionStart"]

  let scrollTop = (event: Dom.event): int => {
    let scrollTop: float = (event->target)["scrollTop"]
    scrollTop->Math.round->Float.toInt
  }

  let scrollLeft = (event: Dom.event): int => {
    let scrollLeft: float = (event->target)["scrollLeft"]
    scrollLeft->Math.round->Float.toInt
  }
}

module EventTarget = {
  let value = (target: {..}): string => target["value"]
  let setValue = (target: {..}, value: string) => target["value"] = value
  let selectionStart = (target: {..}): int => target["selectionStart"]
  let selectionEnd = (target: {..}): int => target["selectionEnd"]
  let setSelectionRange = (target: {..}, start, end_) => {
    let setSelectionRange: (int, int) => unit = target["setSelectionRange"]
    setSelectionRange(start, end_)
  }
}

module CssStyle = {
  type t

  @set external setPosition: (t, string) => unit = "position"
  @set external setTop: (t, string) => unit = "top"
  @set external setLeft: (t, string) => unit = "left"
}

module Element = {
  @send external setAttribute: (Dom.element, string, string) => unit = "setAttribute"
  @send
  external addEventListener: (Dom.element, string, Dom.event => unit) => unit = "addEventListener"
  @send
  external removeEventListener: (Dom.element, string, Dom.event => unit) => unit =
    "removeEventListener"
  @send external appendChild: (Dom.element, Dom.element) => unit = "appendChild"
  @send external removeChild: (Dom.element, Dom.element) => unit = "removeChild"
  @get external style: Dom.element => CssStyle.t = "style"
  @get @return(nullable)
  external getScrollHandler: Dom.element => option<Dom.event => unit> =
    "__devPlaygroundScrollHandler"
  @set
  external setScrollHandler: (Dom.element, Dom.event => unit) => unit =
    "__devPlaygroundScrollHandler"
}

module ScriptElement = {
  @set external setSrc: (Dom.element, string) => unit = "src"
  @set external setAsync: (Dom.element, bool) => unit = "async"
  @set external setOnLoad: (Dom.element, unknown => unit) => unit = "onload"
  @set external setOnError: (Dom.element, unknown => unit) => unit = "onerror"
}

module TextAreaElement = {
  @set external setValue: (Dom.element, string) => unit = "value"
  @send external select: Dom.element => unit = "select"
}

module Document = {
  @val external current: {..} = "document"
  @get external head: {..} => Dom.element = "head"
  @get external body: {..} => Dom.element = "body"
  @send external createScriptElement: ({..}, @as("script") _) => Dom.element = "createElement"
  @send external createTextAreaElement: ({..}, @as("textarea") _) => Dom.element = "createElement"
  @send @return(nullable)
  external getElementById: ({..}, string) => option<Dom.element> = "getElementById"
  @send external execCommand: ({..}, string) => bool = "execCommand"
}

module UrlSearchParams = {
  type t

  @new external make: string => t = "URLSearchParams"
  @send @return(nullable) external get: (t, string) => option<string> = "get"
  @send external set: (t, string, string) => unit = "set"
  @send external delete: (t, string) => unit = "delete"
  @send external toString: t => string = "toString"
}

module Location = {
  @val external search: string = "window.location.search"
  @val external pathname: string = "window.location.pathname"
  @val external hash: string = "window.location.hash"
  @val external href: string = "window.location.href"
  @val external origin: string = "window.location.origin"
}

module History = {
  @val @scope(("window", "history"))
  external replaceState: (@as(json`null`) _, @as("") _, string) => unit = "replaceState"
}

module Performance = {
  @val @scope("performance") external now: unit => float = "now"
}

module Base64 = {
  @val external encode: string => string = "btoa"
  @val external decode: string => string = "atob"
}

module WebTextEncoder = {
  type t

  @new external make: unit => t = "TextEncoder"
  @send external encode: (t, string) => Uint8Array.t = "encode"
}

module WebTextDecoder = {
  type t

  @new external make: unit => t = "TextDecoder"
  @send external decode: (t, Uint8Array.t) => string = "decode"
}

module WebDecompressionStream = {
  type t

  @val external supported: option<unknown> = "globalThis.DecompressionStream"
  @new external make: string => t = "DecompressionStream"
}

module ReadableStream = {
  type t

  @send external pipeThrough: (t, WebDecompressionStream.t) => t = "pipeThrough"
}

module WebBlob = {
  type t

  @new external make: array<Uint8Array.t> => t = "Blob"
  @send external stream: t => ReadableStream.t = "stream"
}

module WebResponse = {
  type t

  @new external make: ReadableStream.t => t = "Response"
  @send external arrayBuffer: t => promise<ArrayBuffer.t> = "arrayBuffer"
}

module SharedCode = {
  let bytesToBinary = bytes => {
    let chunkSize = 0x8000
    let length = bytes->TypedArray.length
    let chunks: array<string> = []

    let rec collect = start =>
      if start < length {
        let end_ = Math.Int.min(start + chunkSize, length)
        let chunk = bytes->TypedArray.subarray(~start, ~end=end_)
        let chars = Array.fromInitializer(~length=end_ - start, index =>
          chunk->TypedArray.get(index)->Option.getOr(0)
        )
        chunks->Array.push(chars->String.fromCharCodeMany)
        collect(end_)
      }

    collect(0)
    chunks->Array.join("")
  }

  let base64UrlToBytes = value => {
    let base64 = value->String.replaceAll("-", "+")->String.replaceAll("_", "/")
    let remainder = mod(base64->String.length, 4)
    let padded = switch remainder {
    | 0 => base64
    | remainder => base64->String.padEnd(base64->String.length + 4 - remainder, "=")
    }
    let binary = padded->Base64.decode
    let length = binary->String.length
    let bytes = Uint8Array.fromLength(length)

    for index in 0 to length - 1 {
      bytes->TypedArray.set(index, binary->String.charCodeAtUnsafe(index))
    }

    bytes
  }

  let encode = async source => {
    let bytes = WebTextEncoder.make()->WebTextEncoder.encode(source)
    "b:" ++
    bytes
    ->bytesToBinary
    ->Base64.encode
    ->String.replaceAllRegExp(/\+/g, "-")
    ->String.replaceAllRegExp(/\//g, "_")
    ->String.replaceAllRegExp(/=+$/g, "")
  }

  let decode = async encoded =>
    if encoded->String.startsWith("z:") {
      switch WebDecompressionStream.supported {
      | None =>
        JsError.throwWithMessage(
          "Compressed shared links require browser DecompressionStream support",
        )
      | Some(_) =>
        let compressedBytes = encoded->String.slice(~start=2)->base64UrlToBytes
        let stream =
          WebBlob.make([compressedBytes])
          ->WebBlob.stream
          ->ReadableStream.pipeThrough(WebDecompressionStream.make("gzip"))
        let buffer = await WebResponse.make(stream)->WebResponse.arrayBuffer
        WebTextDecoder.make()->WebTextDecoder.decode(Uint8Array.fromBuffer(buffer))
      }
    } else if encoded->String.startsWith("b:") {
      WebTextDecoder.make()->WebTextDecoder.decode(
        encoded->String.slice(~start=2)->base64UrlToBytes,
      )
    } else {
      encoded
    }
}

module NavigatorClipboard = {
  type t

  @val external current: option<t> = "navigator.clipboard"
  @get @return(nullable) external writeTextMethod: t => option<unknown> = "writeText"
  @send external writeText: (t, string) => promise<unit> = "writeText"

  let canWriteText = clipboard =>
    switch clipboard->writeTextMethod {
    | Some(writeText) => writeText->Type.typeof === #function
    | None => false
    }
}

module Clipboard = {
  let writeWithFallback = value => {
    let document = Document.current
    let textarea = document->Document.createTextAreaElement
    textarea->TextAreaElement.setValue(value)
    textarea->Element.setAttribute("readonly", "")

    let style = textarea->Element.style
    style->CssStyle.setPosition("fixed")
    style->CssStyle.setTop("-9999px")
    style->CssStyle.setLeft("-9999px")

    let body = document->Document.body
    body->Element.appendChild(textarea)
    textarea->TextAreaElement.select

    let copied = switch document->Document.execCommand("copy") {
    | copied => Ok(copied)
    | exception _ => Error()
    }

    body->Element.removeChild(textarea)

    switch copied {
    | Ok(true) => ()
    | Ok(false) | Error() => JsError.throwWithMessage("Copy command failed")
    }
  }

  let writeText = async value =>
    switch NavigatorClipboard.current {
    | Some(clipboard) if Window.isSecureContext && clipboard->NavigatorClipboard.canWriteText =>
      await clipboard->NavigatorClipboard.writeText(value)
    | _ => writeWithFallback(value)
    }
}
