open GenTypeCommon

let rec handleNamespace = (~name, dep) =>
  switch dep {
  | External(_)
  | Internal(_) => dep
  | Dot(External(s), moduleName) if s == name => External(moduleName)
  | Dot(dep1, s) => Dot(dep1->handleNamespace(~name), s)
  }

let rec fromPath1 = (~config, ~typeEnv, path: Path.t) =>
  switch path {
  | Pident(id) =>
    let name = id->Ident.name
    switch typeEnv->TypeEnv.lookup(~name) {
    | None => External(name)
    | Some(typeEnv1) =>
      switch typeEnv1->TypeEnv.expandAliasToExternalModule(~name) {
      | Some(dep) => dep
      | None =>
        let resolvedName = name->TypeEnv.addModulePath(~typeEnv=typeEnv1)
        Internal(resolvedName)
      }
    }

  | Pdot(Pident(id), s, _pos) if id->ScopedPackage.isGeneratedModule(~config) =>
    External(s->ScopedPackage.addGeneratedModule(~generatedModule=id))

  | Pdot(p, s, _pos) => Dot(p->fromPath1(~config, ~typeEnv), s)
  | Papply(_) => Internal("__Papply_unsupported_genType__"->ResolvedName.fromString)
  }

let rec isInternal = dep =>
  switch dep {
  | External(_) => false
  | Internal(_) => true
  | Dot(d, _) => d->isInternal
  }

let fromPath = (~config, ~typeEnv, path) => {
  let dep = path->fromPath1(~config, ~typeEnv)
  if Debug.typeResolution.contents {
    Log_.item(
      "fromPath path:%s typeEnv:%s %s resolved:%s\n",
      path->Path.name,
      typeEnv->TypeEnv.toString,
      dep->isInternal ? "Internal" : "External",
      dep->depToString,
    )
  }
  switch config.namespace {
  | None => dep
  | Some(name) => dep->handleNamespace(~name)
  }
}

let rec getOuterModuleName = dep =>
  switch dep {
  | External(name) => name->ModuleName.fromStringUnsafe
  | Internal(resolvedName) => resolvedName->ResolvedName.toString->ModuleName.fromStringUnsafe
  | Dot(dep1, _) => dep1->getOuterModuleName
  }

let rec removeExternalOuterModule = dep =>
  switch dep {
  | External(_)
  | Internal(_) => dep
  | Dot(External(_), s) => External(s)
  | Dot(dep1, s) => Dot(dep1->removeExternalOuterModule, s)
  }
