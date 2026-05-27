type compilerVersion = {
  versionId: string,
  versionLabel: string,
  versionRoot: option<string>,
}

module Version = {
  type t = {
    id: string,
    label: string,
  }

  let jsonStringField = (item, name) =>
    switch item->Dict.get(name) {
    | Some(JSON.String(value)) => Some(value)
    | _ => None
    }

  let normalizeRoot = root =>
    if root->String.endsWith("/") {
      root->String.slice(~start=0, ~end=root->String.length - 1)
    } else {
      root
    }

  let toPublic = (version: compilerVersion): t => {
    id: version.versionId,
    label: version.versionLabel,
  }

  let fromJson = json =>
    switch json {
    | JSON.Object(item) =>
      let? Some(id) = item->jsonStringField("id")
      let? Some(label) = item->jsonStringField("label")
      Some({
        versionId: id,
        versionLabel: label,
        versionRoot: item->jsonStringField("root")->Option.map(normalizeRoot),
      })
    | _ => None
    }
}

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

type success = {
  jsCode: string,
  parsetree: string,
  typedtree: string,
  lambda: string,
  lam: string,
  warnings: array<string>,
  time: float,
}

type failure = {
  errors: array<string>,
  warnings: array<string>,
  message: string,
  time: float,
}

type compileResult = result<success, failure>
type formatResult = result<string, failure>

type normalizedConfig = {
  moduleSystem: PlaygroundConfig.moduleSystem,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<PlaygroundConfig.experimentalFeature>,
}

let defaultWarnFlags = "+a-4-9-20-40-41-42-50-61-102-109"

let defaultCompilerVersion = Env.viteDefaultCompilerVersion->Option.getOr("local")

let defaultConfig: PlaygroundConfig.t = {
  compilerVersion: defaultCompilerVersion,
  moduleSystem: Esmodule,
  warnFlags: defaultWarnFlags,
  jsxPreserveMode: false,
  experimentalFeatures: [],
}

let pathFromBase = relativePath => {
  let baseUrl = switch Env.viteBaseUrl {
  | Some("") | None => "/"
  | Some(baseUrl) => baseUrl
  }

  let pathname = Url.make(relativePath, Url.make(baseUrl, Location.origin)->Url.href)->Url.pathname

  if pathname->String.endsWith("/") {
    pathname->String.slice(~start=0, ~end=pathname->String.length - 1)
  } else {
    pathname
  }
}

let parseCompilerVersions = defaultVersion => {
  let fallback = [{versionId: defaultVersion, versionLabel: defaultVersion, versionRoot: None}]
  switch Env.viteCompilerVersions {
  | None | Some("") => fallback
  | Some(versionJson) =>
    switch JSON.parseOrThrow(versionJson) {
    | JSON.Array(items) =>
      let versions = items->Array.filterMap(Version.fromJson)
      versions->Array.length === items->Array.length ? versions : fallback
    | _ => fallback
    | exception _ => fallback
    }
  }
}

let compilerVersions = parseCompilerVersions(defaultConfig.compilerVersion)
let availableCompilerVersions = compilerVersions->Array.map(Version.toPublic)
let compilerRoot = pathFromBase("playground-bundles")
let compilerPreviewRoot = switch Env.viteCompilerPreviewRoot {
| Some(root) => root === "" ? None : Some(root->Version.normalizeRoot)
| None => None
}
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

let versionOrDefault = version => version === "" ? defaultConfig.compilerVersion : version

let isPreviewVersion = version => version->String.search(/^pr-[0-9]+$/) === 0

let previewVersionRoot = version =>
  switch compilerPreviewRoot {
  | Some(root) if version->isPreviewVersion => Some(`${root}/${version}/bundle`)
  | _ => None
  }

let versionRoot = version => {
  let selectedVersion = versionOrDefault(version)
  switch compilerVersions->Array.findMap(version =>
    version.versionId === selectedVersion ? version.versionRoot : None
  ) {
  | Some(root) => root
  | None =>
    switch selectedVersion->previewVersionRoot {
    | Some(root) => root
    | None => `${compilerRoot}/${selectedVersion}`
    }
  }
}

let isConfiguredVersion = version =>
  compilerVersions->Array.some(compilerVersion => compilerVersion.versionId === version)

let isLoadableVersion = version =>
  version->isConfiguredVersion || version->previewVersionRoot->Option.isSome

let selectableCompilerVersions = activeVersion =>
  if activeVersion->isConfiguredVersion || !(activeVersion->previewVersionRoot->Option.isSome) {
    availableCompilerVersions
  } else {
    Array.concat(availableCompilerVersions, [{Version.id: activeVersion, label: activeVersion}])
  }

let createScriptLoadPromise = src =>
  Promise.make((resolve, reject) => {
    let document = Document.current
    let script = document->Document.createScriptElement
    script->ScriptElement.setSrc(src)
    script->ScriptElement.setAsync(true)
    script->ScriptElement.setOnLoad(_ => resolve())
    script->ScriptElement.setOnError(_ => reject(JsError.make(`Could not load ${src}`)))
    document->Document.head->Element.appendChild(script)
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

let applyConfig = (
  instance,
  ~moduleSystem: PlaygroundConfig.moduleSystem,
  ~warnFlags,
  ~jsxPreserveMode,
  ~experimentalFeatures: array<PlaygroundConfig.experimentalFeature>,
) => {
  if hasFunction(instance, "setModuleSystem") {
    instance->Instance.setModuleSystem((moduleSystem :> string))
  }
  if hasFunction(instance, "setWarnFlags") {
    instance->Instance.setWarnFlags(warnFlags === "" ? defaultConfig.warnFlags : warnFlags)
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
      warnFlags: defaultConfig.warnFlags,
      jsxPreserveMode: false,
      experimentalFeatures: [],
    }
  | Some(configValue) => {
      moduleSystem: configValue->moduleSystemFromConfig,
      warnFlags: switch configValue->Config.warnFlags {
      | Some(warnFlags) => warnFlags
      | None => defaultConfig.warnFlags
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
  item->Diagnostic.shortMsg->Option.orElse(item->Diagnostic.fullMsg)->Option.getOr(fallback)

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

let failureFromCompileOutput = (compileOutput, elapsedMs): failure => {
  let errors = switch compileOutput->CompileResult.errors {
  | Some(errors) => errors->Array.map(errorToText)
  | None => []
  }

  let warnings = switch compileOutput->CompileResult.warnings {
  | Some(warnings) => warnings->Array.map(warningToText)
  | None => []
  }

  let message =
    compileOutput
    ->CompileResult.msg
    ->Option.orElse(compileOutput->CompileResult.shortMsg)
    ->Option.orElse(compileOutput->CompileResult.fullMsg)
    ->Option.orElse(errors->Array.get(0))
    ->Option.getOr("Compilation failed")

  {errors, warnings, message, time: elapsedMs}
}

let normalize = (compileOutput, elapsedMs): compileResult => {
  switch (
    compileOutput->CompileResult.parsetree,
    compileOutput->CompileResult.typedtree,
    compileOutput->CompileResult.lambda,
    compileOutput->CompileResult.lam,
  ) {
  | (Some(parsetree), Some(typedtree), Some(lambda), Some(lam)) =>
    let warnings = switch compileOutput->CompileResult.warnings {
    | Some(warnings) => warnings->Array.map(warningToText)
    | None => []
    }

    let jsCode = switch compileOutput->CompileResult.jsCode {
    | Some(jsCode) => jsCode
    | None => ""
    }

    Ok({jsCode, parsetree, typedtree, lambda, lam, warnings, time: elapsedMs})

  | _ => Error(failureFromCompileOutput(compileOutput, elapsedMs))
  }
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
    await loadRuntimeLibraries(selectedVersion)
    compilerApis->getMapValueOrThrow(selectedVersion, "Compiler API was not cached")
  } else {
    let root = versionRoot(selectedVersion)
    await loadScript(`${root}/compiler.js`)
    let api = switch Api.global {
    | Some(api) if hasFunction(api, "make") => api
    | _ => JsError.throwWithMessage("rescript_compiler global was not registered by compiler.js")
    }

    // Load runtime .cmijs only after the global is captured to prevent loading
    // the wrong version after selecting a different compiler.
    await loadRuntimeLibraries(selectedVersion)

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
      ~warnFlags=defaultConfig.warnFlags,
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

let compile = async (source, config: PlaygroundConfig.t) => {
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
  let elapsedMs = Performance.now() - start

  normalize(compileOutput, elapsedMs)
}

let format = async (source, config: PlaygroundConfig.t) => {
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
  let formatOutput = if hasFunction(rescript, "format") {
    rescript->Rescript.format(source)
  } else {
    JsError.throwWithMessage("This compiler bundle does not expose formatting")
  }
  let elapsedMs = Performance.now() - start

  if formatOutput->resultIsSuccess {
    switch formatOutput->CompileResult.code {
    | Some(code) => Ok(code)
    | None =>
      Error({
        errors: [],
        warnings: [],
        message: "Formatting did not return code",
        time: elapsedMs,
      })
    }
  } else {
    Error(failureFromCompileOutput(formatOutput, elapsedMs))
  }
}
