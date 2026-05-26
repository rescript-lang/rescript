open PlaygroundConfig

type info = {
  bundleId: string,
  version: string,
  apiVersion: string,
  moduleSystem: PlaygroundConfig.moduleSystem,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<PlaygroundConfig.experimentalFeature>,
  libraries: array<string>,
}

type config = {
  compilerVersion: string,
  moduleSystem: PlaygroundConfig.moduleSystem,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<PlaygroundConfig.experimentalFeature>,
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
  moduleSystem: PlaygroundConfig.moduleSystem,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<PlaygroundConfig.experimentalFeature>,
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
  external viteDefaultCompilerVersion: option<string> =
    "import.meta.env.VITE_DEFAULT_COMPILER_VERSION"
  @val external viteCompilerVersions: option<string> = "import.meta.env.VITE_COMPILER_VERSIONS"
  @val external viteBaseUrl: option<string> = "import.meta.env.BASE_URL"
  @val external locationOrigin: string = "globalThis.location.origin"
}

module Url = {
  @new external make: (string, string) => jsUrl = "URL"
  @get external href: jsUrl => string = "href"
  @get external pathname: jsUrl => string = "pathname"
}

module DynamicProperty = {
  @get_index external get: ('value, string) => option<unknown> = ""
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
  @get external jsCode: compileResult => option<string> = "js_code"
  @get external parsetree: compileResult => option<string> = "parsetree"
  @get external typedtree: compileResult => option<string> = "typedtree"
  @get external lambda_: compileResult => option<string> = "lambda"
  @get external lam: compileResult => option<string> = "lam"
  @get external errors: compileResult => option<array<diagnostic>> = "errors"
  @get external warnings: compileResult => option<array<diagnostic>> = "warnings"
  @get external msg: compileResult => option<string> = "msg"
  @get external shortMsg: compileResult => option<string> = "shortMsg"
  @get external fullMsg: compileResult => option<string> = "fullMsg"
}

module Performance = {
  @val @scope("performance") external now: unit => float = "now"
}

let defaultWarnFlags = "+a-4-9-20-40-41-42-50-61-102-109"

let defaultCompilerVersion = Env.viteDefaultCompilerVersion->Option.getOr("local")

let pathFromBase = relativePath => {
  let baseUrl = switch Env.viteBaseUrl {
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

let jsonStringField = (item, name) =>
  switch item->Dict.get(name) {
  | Some(JSON.String(value)) => Some(value)
  | _ => None
  }

let compilerVersionFromJson = json =>
  switch json {
  | JSON.Object(item) =>
    let? Some(id) = item->jsonStringField("id")
    let? Some(label) = item->jsonStringField("label")
    Some({id, label})
  | _ => None
  }

let parseCompilerVersions = defaultVersion => {
  let fallback = [{id: defaultVersion, label: defaultVersion}]
  switch Env.viteCompilerVersions {
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
  switch value->DynamicProperty.get(name) {
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
  switch configValue->Config.moduleSystem {
  | Some(moduleSystem) =>
    switch moduleSystem->PlaygroundConfig.parseModuleSystem {
    | Some(moduleSystem) => moduleSystem
    | None => Esmodule
    }
  | None => Esmodule
  }

let experimentalFeaturesFromConfig = configValue =>
  switch configValue->Config.experimentalFeatures {
  | Some(experimentalFeatures) =>
    experimentalFeatures->Array.filterMap(PlaygroundConfig.parseExperimentalFeature)
  | None => []
  }

let normalizeConfig = (configValue: option<compilerConfig>): normalizedConfig =>
  switch configValue {
  | None => {
      moduleSystem: Esmodule,
      warnFlags: defaultWarnFlags,
      jsxPreserveMode: false,
      experimentalFeatures: [],
    }
  | Some(configValue) => {
      moduleSystem: configValue->moduleSystemFromConfig,
      warnFlags: switch configValue->Config.warnFlags {
      | Some(warnFlags) => warnFlags
      | None => defaultWarnFlags
      },
      jsxPreserveMode: switch configValue->Config.jsxPreserveMode {
      | Some(jsxPreserveMode) => jsxPreserveMode
      | None => false
      },
      experimentalFeatures: configValue->experimentalFeaturesFromConfig,
    }
  }

let getConfigIfAvailable = (instance: compilerInstance): option<compilerConfig> =>
  if hasFunction(instance, "getConfig") {
    Some(instance->Instance.getConfig)
  } else {
    None
  }

let diagnosticMessage = (item, fallback) =>
  switch item->Diagnostic.shortMsg {
  | Some(message) => message
  | None =>
    switch item->Diagnostic.fullMsg {
    | Some(message) => message
    | None => fallback
    }
  }

let formatLocation = item => {
  let row = switch item->Diagnostic.row {
  | Some(row) => row
  | None => 0
  }
  let column = switch item->Diagnostic.column {
  | Some(column) => column
  | None => 0
  }
  row > 0 ? `Line ${row->Int.toString}, ${column->Int.toString}` : "Compiler"
}

let warningToText = item => {
  let prefix = switch item->Diagnostic.isError {
  | Some(true) => "error"
  | Some(false) | None => "warning"
  }
  let warnNumber = switch item->Diagnostic.warnNumber {
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
  let errors = switch compileOutput->CompileResult.errors {
  | Some(errors) => errors->Array.map(errorToText)
  | None => []
  }
  let warnings = switch compileOutput->CompileResult.warnings {
  | Some(warnings) => warnings->Array.map(warningToText)
  | None => []
  }
  let message = switch compileOutput->CompileResult.msg {
  | Some(message) => message
  | None =>
    switch compileOutput->CompileResult.shortMsg {
    | Some(message) => message
    | None =>
      switch compileOutput->CompileResult.fullMsg {
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
  let warnings = switch compileOutput->CompileResult.warnings {
  | Some(warnings) => warnings->Array.map(warningToText)
  | None => []
  }

  Success({
    jsCode: switch compileOutput->CompileResult.jsCode {
    | Some(jsCode) => jsCode
    | None => ""
    },
    parsetree: compileOutput->CompileResult.parsetree,
    typedtree: compileOutput->CompileResult.typedtree,
    lambda_: compileOutput->CompileResult.lambda_,
    lam: compileOutput->CompileResult.lam,
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

    let api = switch Api.global {
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
  switch instance->Instance.version {
  | Some(version) => version
  | None =>
    switch instance->Instance.rescript->Rescript.version {
    | Some(version) => version
    | None => "unknown"
    }
  }

let apiVersion = api =>
  switch api {
  | Some(api) =>
    switch api->Api.apiVersion {
    | Some(apiVersion) => apiVersion
    | None => "unknown"
    }
  | None => "unknown"
  }

let resultIsSuccess = compileOutput =>
  switch compileOutput->CompileResult.type_ {
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
