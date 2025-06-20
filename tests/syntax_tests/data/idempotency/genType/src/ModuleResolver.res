open GenTypeCommon

module ModuleNameMap = Map.Make(ModuleName)

let \"+++" = Filename.concat

/* Read all the dirs from a library in node_modules */
let readBsDependenciesDirs = (~root) => {
  let dirs = ref(list{})
  let rec findSubDirs = dir => {
    let absDir = dir == "" ? root : \"+++"(root, dir)
    if Sys.file_exists(absDir) && Sys.is_directory(absDir) {
      dirs := list{dir, ...dirs.contents}
      absDir->Sys.readdir->Array.iter(d => findSubDirs(\"+++"(dir, d)))
    }
  }
  findSubDirs("")
  dirs.contents
}

type pkgs = {
  dirs: list<string>,
  pkgs: Hashtbl.t<string, string>,
}

let readDirsFromConfig = (~configSources) => {
  let dirs = ref(list{})
  let root = projectRoot.contents
  let \"+++" = Filename.concat

  let rec processDir = (~subdirs, dir) => {
    let absDir = dir == "" ? root : \"+++"(root, dir)
    if Sys.file_exists(absDir) && Sys.is_directory(absDir) {
      dirs := list{dir, ...dirs.contents}
      if subdirs {
        absDir->Sys.readdir->Array.iter(d => processDir(~subdirs, \"+++"(dir, d)))
      }
    }
  }

  let rec processSourceItem = (sourceItem: Ext_json_types.t) =>
    switch sourceItem {
    | Str({str}) => str->processDir(~subdirs=false)
    | Obj({map}) =>
      switch map->String_map.find_opt("dir") {
      | Some(Str({str})) =>
        let subdirs = switch map->String_map.find_opt("subdirs") {
        | Some(True(_)) => true
        | Some(False(_)) => false
        | _ => false
        }
        str->processDir(~subdirs)
      | _ => ()
      }
    | Arr({content}) => Array.iter(processSourceItem, content)
    | _ => ()
    }

  switch configSources {
  | Some(sourceItem) => processSourceItem(sourceItem)
  | None => ()
  }
  dirs.contents
}

let readSourceDirs = (~configSources) => {
  let sourceDirs =
    list{"lib", "bs", ".sourcedirs.json"}->List.fold_left(\"+++", bsbProjectRoot.contents)
  let dirs = ref(list{})
  let pkgs = Hashtbl.create(1)

  let readDirs = json =>
    switch json {
    | Ext_json_types.Obj({map}) =>
      switch map->String_map.find_opt("dirs") {
      | Some(Arr({content})) =>
        content->Array.iter(x =>
          switch x {
          | Ext_json_types.Str({str}) => dirs := list{str, ...dirs.contents}
          | _ => ()
          }
        )
        ()
      | _ => ()
      }
    | _ => ()
    }

  let readPkgs = json =>
    switch json {
    | Ext_json_types.Obj({map}) =>
      switch map->String_map.find_opt("pkgs") {
      | Some(Arr({content})) =>
        content->Array.iter(x =>
          switch x {
          | Ext_json_types.Arr({content: [Str({str: name}), Str({str: path})]}) =>
            Hashtbl.add(pkgs, name, path)
          | _ => ()
          }
        )
        ()
      | _ => ()
      }
    | _ => ()
    }

  if sourceDirs->Sys.file_exists {
    try {
      let json = sourceDirs->Ext_json_parse.parse_json_from_file
      if bsbProjectRoot.contents != projectRoot.contents {
        dirs := readDirsFromConfig(~configSources)
      } else {
        readDirs(json)
      }
      readPkgs(json)
    } catch {
    | _ => ()
    }
  } else {
    Log_.item("Warning: can't find source dirs: %s\n", sourceDirs)
    Log_.item("Types for cross-references will not be found by genType.\n")
    dirs := readDirsFromConfig(~configSources)
  }
  {dirs: dirs.contents, pkgs: pkgs}
}

/* Read the project's .sourcedirs.json file if it exists
   and build a map of the files with the given extension
   back to the directory where they belong. */
let sourcedirsJsonToMap = (~config, ~extensions, ~excludeFile) => {
  let rec chopExtensions = fname =>
    switch fname->Filename.chop_extension {
    | fnameChopped => fnameChopped->chopExtensions
    | exception _ => fname
    }

  let fileMap = ref(ModuleNameMap.empty)
  let bsDependenciesFileMap = ref(ModuleNameMap.empty)

  let filterGivenExtension = fileName =>
    extensions->List.exists(ext => Filename.check_suffix(fileName, ext)) && !excludeFile(fileName)

  let addDir = (~dirOnDisk, ~dirEmitted, ~filter, ~map) =>
    dirOnDisk
    ->Sys.readdir
    ->Array.iter(fname =>
      if fname->filter {
        map :=
          map.contents->ModuleNameMap.add(
            fname->chopExtensions->ModuleName.fromStringUnsafe,
            dirEmitted,
          )
      }
    )
  let {dirs, pkgs} = readSourceDirs(~configSources=config.sources)
  dirs->List.iter(dir =>
    addDir(
      ~dirEmitted=dir,
      ~dirOnDisk=\"+++"(projectRoot.contents, dir),
      ~filter=filterGivenExtension,
      ~map=fileMap,
    )
  )

  config.bsDependencies->List.iter(packageName =>
    switch Hashtbl.find(pkgs, packageName) {
    | path =>
      let root = list{"lib", "bs"}->List.fold_left(\"+++", path)
      let filter = fileName =>
        list{".cmt", ".cmti"}->List.exists(ext => Filename.check_suffix(fileName, ext))
      readBsDependenciesDirs(~root)->List.iter(dir => {
        let dirOnDisk = \"+++"(root, dir)
        let dirEmitted = \"+++"(packageName, dir)
        addDir(~dirEmitted, ~dirOnDisk, ~filter, ~map=bsDependenciesFileMap)
      })
    | exception Not_found => ()
    }
  )

  (fileMap.contents, bsDependenciesFileMap.contents)
}

type case =
  | Lowercase
  | Uppercase

type resolver = {
  lazyFind: Lazy.t<(~useBsDependencies: bool, ModuleName.t) => option<(string, case, bool)>>,
}

let createLazyResolver = (~config, ~extensions, ~excludeFile) => {
  lazyFind: Lazy.from_fun(() => {
    let (moduleNameMap, bsDependenciesFileMap) = sourcedirsJsonToMap(
      ~config,
      ~extensions,
      ~excludeFile,
    )
    let find = (~bsDependencies, ~map, moduleName) =>
      switch map->ModuleNameMap.find(moduleName) {
      | resolvedModuleDir => Some((resolvedModuleDir, Uppercase, bsDependencies))
      | exception Not_found =>
        switch map->ModuleNameMap.find(moduleName->ModuleName.uncapitalize) {
        | resolvedModuleDir => Some((resolvedModuleDir, Lowercase, bsDependencies))
        | exception Not_found => None
        }
      }
    (~useBsDependencies, moduleName) =>
      switch moduleName->find(~bsDependencies=false, ~map=moduleNameMap) {
      | None if useBsDependencies =>
        moduleName->find(~bsDependencies=true, ~map=bsDependenciesFileMap)
      | res => res
      }
  }),
}

let apply = (~resolver, ~useBsDependencies, moduleName) =>
  moduleName->Lazy.force(resolver.lazyFind, ~useBsDependencies)

/* Resolve a reference to ModuleName, and produce a path suitable for require.
 E.g. require "../foo/bar/ModuleName.ext" where ext is ".re" or ".js". */
let resolveModule = (
  ~importExtension,
  ~outputFileRelative,
  ~resolver,
  ~useBsDependencies,
  moduleName,
) => {
  let outputFileRelativeDir = /* e.g. src if we're generating src/File.re.js */
  Filename.dirname(outputFileRelative)
  let outputFileAbsoluteDir = \"+++"(projectRoot.contents, outputFileRelativeDir)
  let moduleNameReFile = /* Check if the module is in the same directory as the file being generated.
   So if e.g. project_root/src/ModuleName.re exists. */
  \"+++"(outputFileAbsoluteDir, ModuleName.toString(moduleName) ++ ".re")
  let candidate = /* e.g. import "./Modulename.ext" */
  moduleName->ImportPath.fromModule(~dir=Filename.current_dir_name, ~importExtension)
  if Sys.file_exists(moduleNameReFile) {
    candidate
  } else {
    let rec pathToList = path => {
      let isRoot = path->Filename.basename == path
      isRoot
        ? list{path}
        : list{path->Filename.basename, ...path->Filename.dirname->pathToList}
    }
    switch moduleName->apply(~resolver, ~useBsDependencies) {
    | None => candidate
    | Some((resolvedModuleDir, case, bsDependencies)) =>
      /* e.g. "dst" in case of dst/ModuleName.re */

      let walkUpOutputDir =
        /* e.g. ".." in case dst is a path of length 1 */
        outputFileRelativeDir
        ->pathToList
        ->List.map(_ => Filename.parent_dir_name)
        ->(
          l =>
            switch l {
            | list{} => ""
            | list{_, ...rest} => rest->List.fold_left(\"+++", Filename.parent_dir_name)
            }
        )

      let fromOutputDirToModuleDir = /* e.g. "../dst" */
      bsDependencies ? resolvedModuleDir : \"+++"(walkUpOutputDir, resolvedModuleDir)

      /* e.g. import "../dst/ModuleName.ext" */

      (
        case == Uppercase ? moduleName : moduleName->ModuleName.uncapitalize
      )->ImportPath.fromModule(~dir=fromOutputDirToModuleDir, ~importExtension)
    }
  }
}

let resolveGeneratedModule = (~config, ~outputFileRelative, ~resolver, moduleName) => {
  if Debug.moduleResolution.contents {
    Log_.item("Resolve Generated Module: %s\n", moduleName->ModuleName.toString)
  }
  let importPath = resolveModule(
    ~importExtension=EmitType.generatedModuleExtension(~config),
    ~outputFileRelative,
    ~resolver,
    ~useBsDependencies=true,
    moduleName,
  )
  if Debug.moduleResolution.contents {
    Log_.item("Import Path: %s\n", importPath->ImportPath.dump)
  }
  importPath
}

@ocaml.doc("
 * Returns the path to import a given Reason module name.
 ")
let importPathForReasonModuleName = (~config, ~outputFileRelative, ~resolver, moduleName) => {
  if Debug.moduleResolution.contents {
    Log_.item("Resolve Reason Module: %s\n", moduleName->ModuleName.toString)
  }
  switch config.shimsMap->ModuleNameMap.find(moduleName) {
  | shimModuleName =>
    if Debug.moduleResolution.contents {
      Log_.item("ShimModuleName: %s\n", shimModuleName->ModuleName.toString)
    }
    let importPath = resolveModule(
      ~importExtension=".shim",
      ~outputFileRelative,
      ~resolver,
      ~useBsDependencies=false,
      shimModuleName,
    )
    if Debug.moduleResolution.contents {
      Log_.item("Import Path: %s\n", importPath->ImportPath.dump)
    }
    importPath
  | exception Not_found =>
    moduleName->resolveGeneratedModule(~config, ~outputFileRelative, ~resolver)
  }
}
