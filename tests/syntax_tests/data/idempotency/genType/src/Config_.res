module ModuleNameMap = Map.Make(ModuleName)

let bsbProjectRoot = ref("")
let projectRoot = ref("")

type language =
  | Flow
  | TypeScript
  | Untyped

type module_ =
  | CommonJS
  | ES6

type importPath =
  | Relative
  | Node

type bsVersion = (int, int, int)

type config = {
  bsBlockPath: option<string>,
  bsCurryPath: option<string>,
  bsDependencies: list<string>,
  mutable emitCreateBucklescriptBlock: bool,
  mutable emitFlowAny: bool,
  mutable emitImportCurry: bool,
  mutable emitImportPropTypes: bool,
  mutable emitImportReact: bool,
  mutable emitTypePropDone: bool,
  exportInterfaces: bool,
  fileHeader: option<string>,
  generatedFileExtension: option<string>,
  importPath: importPath,
  language: language,
  module_: module_,
  modulesAsObjects: bool,
  namespace: option<string>,
  propTypes: bool,
  reasonReactPath: string,
  recordsAsObjects: bool,
  shimsMap: ModuleNameMap.t<ModuleName.t>,
  sources: option<Ext_json_types.t>,
  useUnboxedAnnotations: bool,
}

let default = {
  bsBlockPath: None,
  bsCurryPath: None,
  bsDependencies: list{},
  emitCreateBucklescriptBlock: false,
  emitFlowAny: false,
  emitImportCurry: false,
  emitImportPropTypes: false,
  emitImportReact: false,
  emitTypePropDone: false,
  exportInterfaces: false,
  fileHeader: None,
  generatedFileExtension: None,
  importPath: Relative,
  language: Flow,
  module_: ES6,
  modulesAsObjects: false,
  namespace: None,
  propTypes: false,
  reasonReactPath: "reason-react/src/ReasonReact.js",
  recordsAsObjects: false,
  shimsMap: ModuleNameMap.empty,
  sources: None,
  useUnboxedAnnotations: false,
}

let bsPlatformLib = (~config) =>
  switch config.module_ {
  | ES6 => "bs-platform/lib/es6"
  | CommonJS => "bs-platform/lib/js"
  }

let getBsBlockPath = (~config) =>
  switch config.bsBlockPath {
  | None => bsPlatformLib(~config) ++ "/block.js"
  | Some(s) => s
  }

let getBsCurryPath = (~config) =>
  switch config.bsCurryPath {
  | None => bsPlatformLib(~config) ++ "/curry.js"
  | Some(s) => s
  }

let getBool = (s, json) =>
  switch json {
  | Ext_json_types.Obj({map}) =>
    switch map->String_map.find_opt(s) {
    | Some(True(_)) => Some(true)
    | Some(False(_)) => Some(false)
    | _ => None
    }
  | _ => None
  }

let getString = (s, json) =>
  switch json {
  | Ext_json_types.Obj({map}) =>
    switch map->String_map.find_opt(s) {
    | Some(Str({str})) => str
    | _ => ""
    }
  | _ => ""
  }

let getStringOption = (s, json) =>
  switch json {
  | Ext_json_types.Obj({map}) =>
    switch map->String_map.find_opt(s) {
    | Some(Str({str})) => Some(str)
    | _ => None
    }
  | _ => None
  }

let getShims = json => {
  let shims = ref(list{})
  switch json {
  | Ext_json_types.Obj({map}) =>
    switch map->String_map.find_opt("shims") {
    | Some(Ext_json_types.Obj({map: shimsMap})) =>
      shimsMap->String_map.iter((fromModule, toModule) =>
        switch toModule {
        | Ext_json_types.Str({str}) => shims := list{(fromModule, str), ...shims.contents}
        | _ => ()
        }
      )
    | Some(Arr({content})) =>
      /* To be deprecated: array of strings */
      content->Array.iter(x =>
        switch x {
        | Ext_json_types.Str({str}) =>
          let fromTo = Str.split(Str.regexp("="), str)->Array.of_list
          assert (Array.length(fromTo) === 2)
          shims := list{(fromTo[0], fromTo[1]), ...shims.contents}
        | _ => ()
        }
      )
    | _ => ()
    }
  | _ => ()
  }
  shims.contents
}

let getDebug = json =>
  switch json {
  | Ext_json_types.Obj({map}) =>
    switch map->String_map.find_opt("debug") {
    | Some(Ext_json_types.Obj({map})) => map->String_map.iter(Debug.setItem)
    | _ => ()
    }
  | _ => ()
  }

let readConfig = (~bsVersion, ~getConfigFile, ~getBsConfigFile, ~namespace) => {
  let fromJson = json => {
    let languageString = json->getString("language")
    let moduleString = json->getString("module")
    let importPathString = json->getString("importPath")
    let reasonReactPathString = json->getString("reasonReactPath")
    let fileHeader = json->getStringOption("fileHeader")
    let bsBlockPathString = json->getString("bsBlockPath")
    let bsCurryPathString = json->getString("bsCurryPath")
    let exportInterfacesBool = json->getBool("exportInterfaces")
    let generatedFileExtensionStringOption = json->getStringOption("generatedFileExtension")
    let propTypesBool = json->getBool("propTypes")
    let shimsMap = json->getShims->List.fold_left((map, (fromModule, toModule)) => {
        let moduleName: ModuleName.t = fromModule->ModuleName.fromStringUnsafe
        let shimModuleName = toModule->ModuleName.fromStringUnsafe
        ModuleNameMap.add(moduleName, shimModuleName, map)
      }, ModuleNameMap.empty)
    json->getDebug
    let language = switch languageString {
    | "typescript" => TypeScript
    | "untyped" => Untyped
    | _ => Flow
    }
    let module_ = switch moduleString {
    | "commonjs" => CommonJS
    | "es6" => ES6
    | _ => default.module_
    }
    let importPath = switch importPathString {
    | "relative" => Relative
    | "node" => Node
    | _ => default.importPath
    }
    let reasonReactPath = switch reasonReactPathString {
    | "" => default.reasonReactPath
    | _ => reasonReactPathString
    }
    let bsBlockPath = switch bsBlockPathString {
    | "" => None
    | _ => Some(bsBlockPathString)
    }
    let bsCurryPath = switch bsCurryPathString {
    | "" => None
    | _ => Some(bsCurryPathString)
    }
    let exportInterfaces = switch exportInterfacesBool {
    | None => default.exportInterfaces
    | Some(b) => b
    }
    let propTypes = switch propTypesBool {
    | None => default.propTypes
    | Some(b) => b
    }
    let fileHeader = fileHeader
    let generatedFileExtension = generatedFileExtensionStringOption
    let bsVersion = switch bsVersion {
    | None => (0, 0, 0)
    | Some(s) =>
      switch s->Str.split(Str.regexp(Str.quote("."))) {
      | list{x1, x2, x3, ..._} =>
        let v1 = int_of_string(x1)
        let v2 = int_of_string(x2)
        let v3 = switch x3->Str.split(Str.regexp("-")) {
        | list{x3, ..._} => int_of_string(x3)
        | _ => 0
        }
        (v1, v2, v3)
      | _ => (0, 0, 0)
      }
    }
    let (v1, v2, v3) = bsVersion
    let modulesAsObjects = switch v1 {
    | 5 => bsVersion >= (5, 2, 0)
    | 6 => bsVersion >= (6, 2, 0)
    | _ => v1 > 6
    }
    let recordsAsObjects = switch v1 {
    | 5 => bsVersion >= (5, 3, 0)
    | 6 => bsVersion >= (6, 3, 0)
    | _ => v1 > 6
    }
    let useUnboxedAnnotations = switch v1 {
    | 7 => bsVersion > (7, 0, 1)
    | _ => v1 > 7
    }
    if Debug.config.contents {
      Log_.item("Project root: %s\n", projectRoot.contents)
      if bsbProjectRoot.contents != projectRoot.contents {
        Log_.item("bsb project root: %s\n", bsbProjectRoot.contents)
      }
      Log_.item(
        "Config language:%s module:%s importPath:%s shims:%d entries bsVersion:%d.%d.%d\n",
        languageString,
        moduleString,
        importPathString,
        shimsMap->ModuleNameMap.cardinal,
        v1,
        v2,
        v3,
      )
    }

    {
      bsBlockPath: bsBlockPath,
      bsCurryPath: bsCurryPath,
      bsDependencies: list{},
      emitCreateBucklescriptBlock: false,
      emitFlowAny: false,
      emitImportCurry: false,
      emitImportPropTypes: false,
      emitImportReact: false,
      emitTypePropDone: false,
      exportInterfaces: exportInterfaces,
      fileHeader: fileHeader,
      generatedFileExtension: generatedFileExtension,
      importPath: importPath,
      language: language,
      module_: module_,
      modulesAsObjects: modulesAsObjects,
      namespace: None,
      propTypes: propTypes,
      reasonReactPath: reasonReactPath,
      recordsAsObjects: recordsAsObjects,
      shimsMap: shimsMap,
      sources: None,
      useUnboxedAnnotations: useUnboxedAnnotations,
    }
  }

  let fromBsConfig = json =>
    switch json {
    | Ext_json_types.Obj({map}) =>
      let config = switch map->String_map.find_opt("gentypeconfig") {
      | Some(jsonGenFlowConfig) => jsonGenFlowConfig->fromJson
      | _ => default
      }
      let config = switch map->String_map.find_opt("namespace") {
      | Some(True(_)) => {...config, namespace: namespace}
      | _ => config
      }
      let config = switch map->String_map.find_opt("bs-dependencies") {
      | Some(Arr({content})) =>
        let strings = ref(list{})
        content->Array.iter(x =>
          switch x {
          | Ext_json_types.Str({str}) => strings := list{str, ...strings.contents}
          | _ => ()
          }
        )
        {...config, bsDependencies: strings.contents}
      | _ => config
      }
      let config = switch map->String_map.find_opt("sources") {
      | Some(sourceItem) => {...config, sources: Some(sourceItem)}
      | _ => config
      }
      config
    | _ => default
    }
  switch getConfigFile() {
  | Some(configFile) =>
    try configFile->Ext_json_parse.parse_json_from_file->fromJson catch {
    | _ => default
    }
  | None =>
    switch getBsConfigFile() {
    | Some(bsConfigFile) =>
      try bsConfigFile->Ext_json_parse.parse_json_from_file->fromBsConfig catch {
      | _ => default
      }
    | None => default
    }
  }
}
