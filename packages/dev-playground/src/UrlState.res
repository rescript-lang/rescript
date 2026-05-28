let maxEncodedCodeLength = 300 * 1024
let maxDecodedSourceLength = 200 * 1024
let replaceSequence = ref(0)

type state = {
  source: string,
  config: PlaygroundConfig.t,
}

let getParam = name => UrlSearchParams.make(Location.search)->UrlSearchParams.get(name)

let applyUrlState = (~encoded, ~config: PlaygroundConfig.t) => {
  let moduleSystem = (config.moduleSystem :> string)
  let experimentalFeatures = config.experimentalFeatures->Array.map(feature => (feature :> string))
  let params = UrlSearchParams.make(Location.search)
  params->UrlSearchParams.set("code", encoded)
  params->UrlSearchParams.set("version", config.compilerVersion)
  params->UrlSearchParams.set("module", moduleSystem)
  params->UrlSearchParams.set("warn", config.warnFlags)

  if config.jsxPreserveMode {
    params->UrlSearchParams.set("jsxPreserve", "true")
  } else {
    params->UrlSearchParams.delete("jsxPreserve")
  }

  if experimentalFeatures->Array.length > 0 {
    params->UrlSearchParams.set("experimental", experimentalFeatures->Array.join(","))
  } else {
    params->UrlSearchParams.delete("experimental")
  }

  if config.gentypeEnabled {
    params->UrlSearchParams.set("gentype", "true")
  } else {
    params->UrlSearchParams.delete("gentype")
  }

  switch config.sourceMapMode {
  | Disabled =>
    params->UrlSearchParams.delete("sourceMap")
    params->UrlSearchParams.delete("sourceMapSourcesContent")
    params->UrlSearchParams.delete("sourceMapRoot")
  | sourceMapMode =>
    params->UrlSearchParams.set("sourceMap", (sourceMapMode :> string))
    params->UrlSearchParams.set(
      "sourceMapSourcesContent",
      config.sourceMapSourcesContent ? "true" : "false",
    )
    if config.sourceMapRoot === "" {
      params->UrlSearchParams.delete("sourceMapRoot")
    } else {
      params->UrlSearchParams.set("sourceMapRoot", config.sourceMapRoot)
    }
  }

  let query = params->UrlSearchParams.toString
  let nextUrl = Location.pathname ++ (query === "" ? "" : "?" ++ query) ++ Location.hash
  History.replaceState(nextUrl)
}

let initialSource = async defaultSource => {
  switch getParam("code") {
  | None => defaultSource
  | Some(encoded)
    if encoded === "" || encoded->String.length > maxEncodedCodeLength => defaultSource
  | Some(encoded) =>
    switch await SharedCode.decode(encoded) {
    | decoded => decoded->String.length <= maxDecodedSourceLength ? decoded : defaultSource
    | exception error =>
      Console.warn2("Could not restore shared playground source", error)
      defaultSource
    }
  }
}

let queryCompilerVersion = defaultVersion =>
  switch getParam("version") {
  | Some(version) if version !== "" => version
  | _ => defaultVersion
  }

let queryModuleSystem = defaultModuleSystem =>
  switch getParam("module") {
  | Some(value) =>
    switch value->PlaygroundConfig.parseModuleSystem {
    | Some(moduleSystem) => moduleSystem
    | None => defaultModuleSystem
    }
  | None => defaultModuleSystem
  }

let queryWarnFlags = defaultWarnFlags =>
  switch getParam("warn") {
  | Some(warnFlags) if warnFlags !== "" => warnFlags
  | _ => defaultWarnFlags
  }

let queryJsxPreserveMode = defaultValue =>
  switch getParam("jsxPreserve") {
  | None => defaultValue
  | Some(value) => value === "true" || value === "1"
  }

let queryExperimentalFeatures = defaultExperimentalFeatures =>
  switch getParam("experimental") {
  | Some(value) if value !== "" =>
    value->String.split(",")->Array.filterMap(PlaygroundConfig.parseExperimentalFeature)
  | _ => defaultExperimentalFeatures
  }

let querySourceMapMode = defaultValue =>
  switch getParam("sourceMap") {
  | Some(value) =>
    switch value->PlaygroundConfig.parseSourceMapMode {
    | Some(sourceMapMode) => sourceMapMode
    | None => defaultValue
    }
  | None => defaultValue
  }

let queryBool = (~name, ~defaultValue) =>
  switch getParam(name) {
  | Some(value) if value === "true" || value === "1" => true
  | Some(value) if value === "false" || value === "0" => false
  | _ => defaultValue
  }

let querySourceMapRoot = defaultValue =>
  switch getParam("sourceMapRoot") {
  | Some(value) => value
  | None => defaultValue
  }

let queryConfig = (~defaultConfig: PlaygroundConfig.t) => {
  let requestedCompilerVersion = queryCompilerVersion(defaultConfig.compilerVersion)
  let compilerVersion =
    requestedCompilerVersion->CompilerApi.isLoadableVersion
      ? requestedCompilerVersion
      : defaultConfig.compilerVersion

  {
    PlaygroundConfig.compilerVersion,
    moduleSystem: queryModuleSystem(defaultConfig.moduleSystem),
    warnFlags: queryWarnFlags(defaultConfig.warnFlags),
    jsxPreserveMode: queryJsxPreserveMode(defaultConfig.jsxPreserveMode),
    experimentalFeatures: queryExperimentalFeatures(defaultConfig.experimentalFeatures),
    gentypeEnabled: queryBool(~name="gentype", ~defaultValue=defaultConfig.gentypeEnabled),
    sourceMapMode: querySourceMapMode(defaultConfig.sourceMapMode),
    sourceMapSourcesContent: queryBool(
      ~name="sourceMapSourcesContent",
      ~defaultValue=defaultConfig.sourceMapSourcesContent,
    ),
    sourceMapRoot: querySourceMapRoot(defaultConfig.sourceMapRoot),
  }
}

let init = async (~defaultSource): state => {
  let source = await initialSource(defaultSource)
  let config = queryConfig(~defaultConfig=CompilerApi.defaultConfig)
  {source, config}
}

let replace = async (~source, ~config) => {
  replaceSequence := replaceSequence.contents + 1
  let sequence = replaceSequence.contents
  let encoded = await SharedCode.encode(source)

  if sequence === replaceSequence.contents {
    applyUrlState(~encoded, ~config)
  }
}

let windowHref = () => Location.href

let copy = async (~source, ~config): result<unit, string> => {
  replaceSequence := replaceSequence.contents + 1
  let sequence = replaceSequence.contents

  let? Ok(encoded) = switch await SharedCode.encode(source) {
  | encoded => Ok(encoded)
  | exception _ => Error("Could not copy link")
  }

  if sequence !== replaceSequence.contents {
    Error("Link changed before it could be copied")
  } else {
    applyUrlState(~encoded, ~config)

    switch await windowHref()->Clipboard.writeText {
    | () => Ok()
    | exception _ => Error("Could not copy link")
    }
  }
}
