let maxEncodedCodeLength = 300 * 1024
let maxDecodedSourceLength = 200 * 1024
let replaceSequence = ref(0)

let getParam = name => {
  UrlSearchParams.make(Location.search)->UrlSearchParams.get(name)
}

let applyUrlState = (
  ~encoded,
  ~compilerVersion,
  ~moduleSystem: PlaygroundConfig.moduleSystem,
  ~warnFlags,
  ~jsxPreserveMode,
  ~experimentalFeatures: array<PlaygroundConfig.experimentalFeature>,
) => {
  let moduleSystem = (moduleSystem :> string)
  let experimentalFeatures = experimentalFeatures->Array.map(feature => (feature :> string))
  let params = UrlSearchParams.make(Location.search)
  params->UrlSearchParams.set("code", encoded)
  params->UrlSearchParams.set("version", compilerVersion)
  params->UrlSearchParams.set("module", moduleSystem)
  params->UrlSearchParams.set("warn", warnFlags)

  if jsxPreserveMode {
    params->UrlSearchParams.set("jsxPreserve", "true")
  } else {
    params->UrlSearchParams.delete("jsxPreserve")
  }

  if experimentalFeatures->Array.length > 0 {
    params->UrlSearchParams.set("experimental", experimentalFeatures->Array.join(","))
  } else {
    params->UrlSearchParams.delete("experimental")
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

let queryExperimentalFeatures = () =>
  switch getParam("experimental") {
  | Some(value) if value !== "" =>
    value->String.split(",")->Array.filterMap(PlaygroundConfig.parseExperimentalFeature)
  | _ => []
  }

let replaceUrlState = async (
  ~source,
  ~compilerVersion,
  ~moduleSystem,
  ~warnFlags,
  ~jsxPreserveMode,
  ~experimentalFeatures,
) => {
  replaceSequence := replaceSequence.contents + 1
  let sequence = replaceSequence.contents
  let encoded = await SharedCode.encode(source)

  if sequence === replaceSequence.contents {
    applyUrlState(
      ~encoded,
      ~compilerVersion,
      ~moduleSystem,
      ~warnFlags,
      ~jsxPreserveMode,
      ~experimentalFeatures,
    )
  }
}

let windowHref = () => Location.href

let copyUrlState = async (
  ~source,
  ~compilerVersion,
  ~moduleSystem,
  ~warnFlags,
  ~jsxPreserveMode,
  ~experimentalFeatures,
): result<unit, string> => {
  replaceSequence := replaceSequence.contents + 1
  let sequence = replaceSequence.contents

  let? Ok(encoded) = switch await SharedCode.encode(source) {
  | encoded => Ok(encoded)
  | exception _ => Error("Could not copy link")
  }

  if sequence !== replaceSequence.contents {
    Error("Link changed before it could be copied")
  } else {
    applyUrlState(
      ~encoded,
      ~compilerVersion,
      ~moduleSystem,
      ~warnFlags,
      ~jsxPreserveMode,
      ~experimentalFeatures,
    )

    switch await windowHref()->Clipboard.writeText {
    | () => Ok()
    | exception _ => Error("Could not copy link")
    }
  }
}
