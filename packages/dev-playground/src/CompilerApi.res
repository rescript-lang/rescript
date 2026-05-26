open PlaygroundTypes

type info = {
  bundleId: string,
  version: string,
  apiVersion: string,
  moduleSystem: PlaygroundTypes.moduleSystem,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<PlaygroundTypes.experimentalFeature>,
  libraries: array<string>,
}

type config = {
  compilerVersion: string,
  moduleSystem: PlaygroundTypes.moduleSystem,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<PlaygroundTypes.experimentalFeature>,
}

type compilerVersion = {
  id: string,
  label: string,
}

type successResult = {
  jsCode: string,
  parsetree: option<string>,
  typedtree: option<string>,
  @as("lambda")
  lambda_: option<string>,
  lam: option<string>,
  warnings: array<string>,
  time: float,
}

type failureResult = {
  errors: array<string>,
  warnings: array<string>,
  message: string,
  time: float,
}

type result =
  | Success(successResult)
  | Failure(failureResult)

type normalizedConfig = {
  moduleSystem: PlaygroundTypes.moduleSystem,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<PlaygroundTypes.experimentalFeature>,
}

type jsUrl
type jsDocument
type jsElement
type jsScriptElement
type compilerApi
type compilerInstance
type rescriptCompiler
type compilerConfig
type compileResult
type diagnostic

module Env = {
  @val
  external viteDefaultCompilerVersion: Nullable.t<string> =
    "import.meta.env.VITE_DEFAULT_COMPILER_VERSION"
  @val external viteCompilerVersions: Nullable.t<string> = "import.meta.env.VITE_COMPILER_VERSIONS"
  @val external viteBaseUrl: Nullable.t<string> = "import.meta.env.BASE_URL"
  @val external locationOrigin: string = "globalThis.location.origin"
}

module Url = {
  @new external make: (string, string) => jsUrl = "URL"
  @get external href: jsUrl => string = "href"
  @get external pathname: jsUrl => string = "pathname"
}

module DynamicProperty = {
  @get_index external get: ('value, string) => Nullable.t<unknown> = ""
}

module Dom = {
  @val external document: jsDocument = "document"
  @get external head: jsDocument => jsElement = "head"
  @send external createElement: (jsDocument, string) => jsScriptElement = "createElement"
  @set external setSrc: (jsScriptElement, string) => unit = "src"
  @set external setAsync: (jsScriptElement, bool) => unit = "async"
  @set external setOnLoad: (jsScriptElement, unknown => unit) => unit = "onload"
  @set external setOnError: (jsScriptElement, unknown => unit) => unit = "onerror"
  @send external appendChild: (jsElement, jsScriptElement) => unit = "appendChild"
}

module Api = {
  @val external global: Nullable.t<compilerApi> = "globalThis.rescript_compiler"
  @send external makeCompiler: compilerApi => compilerInstance = "make"
  @get external apiVersion: compilerApi => Nullable.t<string> = "api_version"
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
  @get external version: compilerInstance => Nullable.t<string> = "version"
}

module Rescript = {
  @get external version: rescriptCompiler => Nullable.t<string> = "version"
  @send external compile: (rescriptCompiler, string) => compileResult = "compile"
  @send external compileWithDebug: (rescriptCompiler, string) => compileResult = "compileWithDebug"
}

module Config = {
  @get external moduleSystem: compilerConfig => Nullable.t<string> = "module_system"
  @get external warnFlags: compilerConfig => Nullable.t<string> = "warn_flags"
  @get external jsxPreserveMode: compilerConfig => Nullable.t<bool> = "jsx_preserve_mode"
  @get
  external experimentalFeatures: compilerConfig => Nullable.t<array<string>> =
    "experimental_features"
}

module Diagnostic = {
  @get external row: diagnostic => Nullable.t<int> = "row"
  @get external column: diagnostic => Nullable.t<int> = "column"
  @get external warnNumber: diagnostic => Nullable.t<int> = "warnNumber"
  @get external isError: diagnostic => Nullable.t<bool> = "isError"
  @get external shortMsg: diagnostic => Nullable.t<string> = "shortMsg"
  @get external fullMsg: diagnostic => Nullable.t<string> = "fullMsg"
}

module CompileResult = {
  @get external type_: compileResult => Nullable.t<string> = "type"
  @get external jsCode: compileResult => Nullable.t<string> = "js_code"
  @get external parsetree: compileResult => Nullable.t<string> = "parsetree"
  @get external typedtree: compileResult => Nullable.t<string> = "typedtree"
  @get external lambda_: compileResult => Nullable.t<string> = "lambda"
  @get external lam: compileResult => Nullable.t<string> = "lam"
  @get external errors: compileResult => Nullable.t<array<diagnostic>> = "errors"
  @get external warnings: compileResult => Nullable.t<array<diagnostic>> = "warnings"
  @get external msg: compileResult => Nullable.t<string> = "msg"
  @get external shortMsg: compileResult => Nullable.t<string> = "shortMsg"
  @get external fullMsg: compileResult => Nullable.t<string> = "fullMsg"
}

module Performance = {
  @val @scope("performance") external now: unit => float = "now"
}

let defaultWarnFlags = "+a-4-9-20-40-41-42-50-61-102-109"

let defaultCompilerVersion = Env.viteDefaultCompilerVersion->Nullable.getOr("local")

let pathFromBase = relativePath => {
  let baseUrl = switch Env.viteBaseUrl->Nullable.toOption {
  | Some("") | None => "/"
  | Some(baseUrl) => baseUrl
  }
  let pathname =
    Url.make(relativePath, Url.make(baseUrl, Env.locationOrigin)->Url.href)->Url.pathname
  if pathname->String.endsWith("/") {
    pathname->String.slice(~start=0, ~end=pathname->String.length - 1)
  } else {
    pathname
  }
}

let compilerVersionFromJson = json =>
  switch json {
  | JSON.Object(item) =>
    switch (item->Dict.get("id"), item->Dict.get("label")) {
    | (Some(JSON.String(id)), Some(JSON.String(label))) => Some({id, label})
    | _ => None
    }
  | _ => None
  }

let parseCompilerVersions = defaultVersion => {
  let fallback = [{id: defaultVersion, label: defaultVersion}]
  switch Env.viteCompilerVersions->Nullable.toOption {
  | None | Some("") => fallback
  | Some(versionJson) =>
    try {
      switch JSON.parseOrThrow(versionJson) {
      | JSON.Array(items) =>
        let versions = items->Array.filterMap(compilerVersionFromJson)
        versions->Array.length === items->Array.length ? versions : fallback
      | _ => fallback
      }
    } catch {
    | _ => fallback
    }
  }
}

let availableCompilerVersions = parseCompilerVersions(defaultCompilerVersion)
let compilerRoot = pathFromBase("playground-bundles")
let loadedScripts: Map.t<string, promise<unit>> = Map.make()
let compilerApis: Map.t<string, compilerApi> = Map.make()
let compilers: Map.t<string, compilerInstance> = Map.make()
let loadedLibrariesByVersion: Map.t<string, array<string>> = Map.make()
let activeLibraryVersion = ref(None)

let hasFunction = (value, name) =>
  switch value->DynamicProperty.get(name)->Nullable.toOption {
  | Some(value) => value->Type.typeof === #function
  | None => false
  }

let versionOrDefault = version => version === "" ? defaultCompilerVersion : version

let createScriptLoadPromise = src =>
  Promise.make((resolve, reject) => {
    let script = Dom.document->Dom.createElement("script")
    script->Dom.setSrc(src)
    script->Dom.setAsync(true)
    script->Dom.setOnLoad(_ => resolve())
    script->Dom.setOnError(_ => reject(JsError.make(`Could not load ${src}`)))
    Dom.document->Dom.head->Dom.appendChild(script)
  })

let loadScript = (src, ~cache=true) =>
  if cache {
    switch loadedScripts->Map.get(src) {
    | Some(promise) => promise
    | None =>
      let promise = createScriptLoadPromise(src)
      loadedScripts->Map.set(src, promise)
      promise
    }
  } else {
    createScriptLoadPromise(src)
  }

let versionRoot = version => `${compilerRoot}/${versionOrDefault(version)}`

let applyConfig = (
  instance,
  ~moduleSystem: moduleSystem,
  ~warnFlags,
  ~jsxPreserveMode,
  ~experimentalFeatures: array<experimentalFeature>,
) => {
  if hasFunction(instance, "setModuleSystem") {
    instance->Instance.setModuleSystem((moduleSystem :> string))
  }
  if hasFunction(instance, "setWarnFlags") {
    instance->Instance.setWarnFlags(warnFlags === "" ? defaultWarnFlags : warnFlags)
  }
  if hasFunction(instance, "setFilename") {
    instance->Instance.setFilename("Playground.res")
  }
  if hasFunction(instance, "setJsxPreserveMode") {
    instance->Instance.setJsxPreserveMode(jsxPreserveMode)
  }
  if hasFunction(instance, "setExperimentalFeatures") {
    instance->Instance.setExperimentalFeatures(
      experimentalFeatures->Array.map(feature => (feature :> string)),
    )
  }
}

let moduleSystemFromConfig = configValue =>
  switch configValue->Config.moduleSystem->Nullable.toOption {
  | Some(moduleSystem) =>
    switch moduleSystem->PlaygroundTypes.parseModuleSystem {
    | Some(moduleSystem) => moduleSystem
    | None => Esmodule
    }
  | None => Esmodule
  }

let experimentalFeaturesFromConfig = configValue =>
  configValue
  ->Config.experimentalFeatures
  ->Nullable.getOr([])
  ->Array.filterMap(PlaygroundTypes.parseExperimentalFeature)

let normalizeConfig = (configValue: Nullable.t<compilerConfig>): normalizedConfig =>
  switch configValue->Nullable.toOption {
  | None => {
      moduleSystem: Esmodule,
      warnFlags: defaultWarnFlags,
      jsxPreserveMode: false,
      experimentalFeatures: [],
    }
  | Some(configValue) => {
      moduleSystem: configValue->moduleSystemFromConfig,
      warnFlags: configValue->Config.warnFlags->Nullable.getOr(defaultWarnFlags),
      jsxPreserveMode: configValue->Config.jsxPreserveMode->Nullable.getOr(false),
      experimentalFeatures: configValue->experimentalFeaturesFromConfig,
    }
  }

let getConfigIfAvailable = (instance: compilerInstance): Nullable.t<compilerConfig> =>
  if hasFunction(instance, "getConfig") {
    instance->Instance.getConfig->Nullable.make
  } else {
    Nullable.undefined
  }

let diagnosticMessage = (item, fallback) =>
  switch item->Diagnostic.shortMsg->Nullable.toOption {
  | Some(message) => message
  | None => item->Diagnostic.fullMsg->Nullable.getOr(fallback)
  }

let formatLocation = item => {
  let row = item->Diagnostic.row->Nullable.getOr(0)
  let column = item->Diagnostic.column->Nullable.getOr(0)
  row > 0 ? `Line ${row->Int.toString}, ${column->Int.toString}` : "Compiler"
}

let warningToText = item => {
  let prefix = item->Diagnostic.isError->Nullable.getOr(false) ? "error" : "warning"
  let warnNumber = switch item->Diagnostic.warnNumber->Nullable.toOption {
  | Some(warnNumber) => ` ${warnNumber->Int.toString}`
  | None => ""
  }
  let message = diagnosticMessage(item, "Unknown warning")
  `${formatLocation(item)}: ${prefix}${warnNumber}: ${message}`
}

let errorToText = item => {
  let message = diagnosticMessage(item, "Unknown compiler error")
  `${formatLocation(item)}: ${message}`
}

let normalizeFailure = (compileOutput, elapsedMs): result => {
  let errors = switch compileOutput->CompileResult.errors->Nullable.toOption {
  | Some(errors) => errors->Array.map(errorToText)
  | None => []
  }
  let warnings = switch compileOutput->CompileResult.warnings->Nullable.toOption {
  | Some(warnings) => warnings->Array.map(warningToText)
  | None => []
  }
  let message = switch compileOutput->CompileResult.msg->Nullable.toOption {
  | Some(message) => message
  | None =>
    switch compileOutput->CompileResult.shortMsg->Nullable.toOption {
    | Some(message) => message
    | None =>
      switch compileOutput->CompileResult.fullMsg->Nullable.toOption {
      | Some(message) => message
      | None =>
        switch errors->Array.get(0) {
        | Some(error) => error
        | None => "Compilation failed"
        }
      }
    }
  }

  Failure({
    errors,
    warnings,
    message,
    time: elapsedMs,
  })
}

let normalizeSuccess = (compileOutput, elapsedMs): result => {
  let warnings = switch compileOutput->CompileResult.warnings->Nullable.toOption {
  | Some(warnings) => warnings->Array.map(warningToText)
  | None => []
  }

  Success({
    jsCode: compileOutput->CompileResult.jsCode->Nullable.getOr(""),
    parsetree: compileOutput->CompileResult.parsetree->Nullable.toOption,
    typedtree: compileOutput->CompileResult.typedtree->Nullable.toOption,
    lambda_: compileOutput->CompileResult.lambda_->Nullable.toOption,
    lam: compileOutput->CompileResult.lam->Nullable.toOption,
    warnings,
    time: elapsedMs,
  })
}

let loadRuntimeLibraries = async version => {
  let selectedVersion = versionOrDefault(version)
  switch activeLibraryVersion.contents {
  | Some(activeVersion) if activeVersion === selectedVersion => ()
  | _ =>
    let root = versionRoot(selectedVersion)
    let _ = await loadScript(`${root}/compiler-builtins/cmij.js`, ~cache=false)

    let libraries = try {
      let _ = await loadScript(`${root}/@rescript/react/cmij.js`, ~cache=false)
      ["compiler-builtins", "@rescript/react"]
    } catch {
    | _ => ["compiler-builtins"]
    }

    loadedLibrariesByVersion->Map.set(selectedVersion, libraries)
    activeLibraryVersion := Some(selectedVersion)
  }
}

let getMapValueOrThrow = (map: Map.t<string, 'value>, key, message): 'value =>
  switch map->Map.get(key) {
  | Some(value) => value
  | None => JsError.throwWithMessage(message)
  }

let ensureCompilerApi = async version => {
  let selectedVersion = versionOrDefault(version)
  if compilerApis->Map.has(selectedVersion) {
    let _ = await loadRuntimeLibraries(selectedVersion)
    compilerApis->getMapValueOrThrow(selectedVersion, "Compiler API was not cached")
  } else {
    let root = versionRoot(selectedVersion)
    let _ = await loadScript(`${root}/compiler.js`)
    let _ = await loadRuntimeLibraries(selectedVersion)

    let api = switch Api.global->Nullable.toOption {
    | Some(api) if hasFunction(api, "make") => api
    | _ => JsError.throwWithMessage("rescript_compiler global was not registered by compiler.js")
    }

    compilerApis->Map.set(selectedVersion, api)
    api
  }
}

let ensureCompiler = async version => {
  let selectedVersion = versionOrDefault(version)
  let api = await ensureCompilerApi(selectedVersion)

  if compilers->Map.has(selectedVersion) {
    compilers->getMapValueOrThrow(selectedVersion, "Compiler instance was not cached")
  } else {
    let instance = api->Api.makeCompiler
    applyConfig(
      instance,
      ~moduleSystem=Esmodule,
      ~warnFlags=defaultWarnFlags,
      ~jsxPreserveMode=false,
      ~experimentalFeatures=[],
    )

    compilers->Map.set(selectedVersion, instance)
    instance
  }
}

let instanceVersion = instance =>
  switch instance->Instance.version->Nullable.toOption {
  | Some(version) => version
  | None => instance->Instance.rescript->Rescript.version->Nullable.getOr("unknown")
  }

let apiVersion = api =>
  switch api {
  | Some(api) => api->Api.apiVersion->Nullable.getOr("unknown")
  | None => "unknown"
  }

let resultIsSuccess = compileOutput =>
  switch compileOutput->CompileResult.type_->Nullable.toOption {
  | Some("success") => true
  | _ => false
  }

let init = async version => {
  let selectedVersion = versionOrDefault(version)
  let instance = await ensureCompiler(selectedVersion)
  let config = normalizeConfig(instance->getConfigIfAvailable)

  let libraries = switch loadedLibrariesByVersion->Map.get(selectedVersion) {
  | Some(libraries) => libraries
  | None => ["compiler-builtins"]
  }

  {
    bundleId: selectedVersion,
    version: instance->instanceVersion,
    apiVersion: compilerApis->Map.get(selectedVersion)->apiVersion,
    moduleSystem: config.moduleSystem,
    warnFlags: config.warnFlags,
    jsxPreserveMode: config.jsxPreserveMode,
    experimentalFeatures: config.experimentalFeatures,
    libraries,
  }
}

let compile = async (source, config) => {
  let selectedVersion = versionOrDefault(config.compilerVersion)
  let instance = await ensureCompiler(selectedVersion)
  applyConfig(
    instance,
    ~moduleSystem=config.moduleSystem,
    ~warnFlags=config.warnFlags,
    ~jsxPreserveMode=config.jsxPreserveMode,
    ~experimentalFeatures=config.experimentalFeatures,
  )

  let start = Performance.now()
  let rescript = instance->Instance.rescript
  let compileOutput = if hasFunction(rescript, "compileWithDebug") {
    rescript->Rescript.compileWithDebug(source)
  } else {
    rescript->Rescript.compile(source)
  }
  let elapsedMs = Performance.now() -. start

  if compileOutput->resultIsSuccess {
    normalizeSuccess(compileOutput, elapsedMs)
  } else {
    normalizeFailure(compileOutput, elapsedMs)
  }
}
