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

module Element = {
  @send external setAttribute: (Dom.element, string, string) => unit = "setAttribute"
  @send
  external addEventListener: (Dom.element, string, Dom.event => unit) => unit = "addEventListener"
  @send
  external removeEventListener: (Dom.element, string, Dom.event => unit) => unit =
    "removeEventListener"
  @send external appendChild: (Dom.element, Dom.element) => unit = "appendChild"
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

module Document = {
  @val external current: {..} = "document"
  @get external head: {..} => Dom.element = "head"
  @send external createScriptElement: ({..}, @as("script") _) => Dom.element = "createElement"
  @send @return(nullable)
  external getElementById: ({..}, string) => option<Dom.element> = "getElementById"
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

module SharedCode = {
  let encode = async source => {
    ignore(source)
    let encoded: string = %raw(`
      (() => {
        const bytes = new TextEncoder().encode(source);
        let binary = "";
        const chunkSize = 0x8000;
        for (let index = 0; index < bytes.length; index += chunkSize) {
          const chunk = bytes.subarray(index, index + chunkSize);
          binary += String.fromCharCode(...chunk);
        }
        return "b:" + btoa(binary)
          .replace(/\+/g, "-")
          .replace(/\//g, "_")
          .replace(/=+$/g, "");
      })()
    `)
    encoded
  }

  let decode = encoded => {
    ignore(encoded)
    let decoded: promise<string> = %raw(`
      (async () => {
        const base64UrlToBytes = value => {
          const base64 = value.replace(/-/g, "+").replace(/_/g, "/");
          const padded = base64.padEnd(Math.ceil(base64.length / 4) * 4, "=");
          const binary = atob(padded);
          const bytes = new Uint8Array(binary.length);
          for (let index = 0; index < binary.length; index += 1) {
            bytes[index] = binary.charCodeAt(index);
          }
          return bytes;
        };

        if (encoded.startsWith("z:")) {
          if (typeof DecompressionStream === "undefined") {
            throw new Error(
              "Compressed shared links require browser DecompressionStream support",
            );
          }

          const compressedBytes = base64UrlToBytes(encoded.slice(2));
          const stream = new Blob([compressedBytes])
            .stream()
            .pipeThrough(new DecompressionStream("gzip"));
          return new TextDecoder().decode(
            new Uint8Array(await new Response(stream).arrayBuffer()),
          );
        }

        if (encoded.startsWith("b:")) {
          return new TextDecoder().decode(base64UrlToBytes(encoded.slice(2)));
        }

        return encoded;
      })()
    `)
    decoded
  }
}

module Clipboard = {
  let writeText = value => {
    ignore(value)
    let promise: promise<unit> = %raw(`
      (async () => {
        if (navigator.clipboard?.writeText != null && window.isSecureContext) {
          await navigator.clipboard.writeText(value);
          return;
        }

        const textarea = document.createElement("textarea");
        textarea.value = value;
        textarea.setAttribute("readonly", "");
        textarea.style.position = "fixed";
        textarea.style.top = "-9999px";
        textarea.style.left = "-9999px";
        document.body.appendChild(textarea);
        textarea.select();

        try {
          if (!document.execCommand("copy")) {
            throw new Error("Copy command failed");
          }
        } finally {
          document.body.removeChild(textarea);
        }
      })()
    `)
    promise
  }
}
