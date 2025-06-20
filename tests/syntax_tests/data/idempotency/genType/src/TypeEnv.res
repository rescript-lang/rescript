open GenTypeCommon

type moduleEquation = {
  internal: bool,
  dep: dep,
}

type rec t = {
  mutable componentModuleItem: Runtime.moduleItem,
  mutable map: StringMap.t<entry>,
  mutable mapModuleTypes: StringMap.t<(Typedtree.signature, t)>,
  mutable moduleEquation: option<moduleEquation>,
  mutable moduleItem: Runtime.moduleItem,
  name: string,
  parent: option<t>,
  typeEquations: StringMap.t<type_>,
}
and entry =
  | Module(t)
  | Type(string)

let createTypeEnv = (~name, parent) => {
  let moduleItem = Runtime.moduleItemGen()->Runtime.newModuleItem(~name)
  {
    componentModuleItem: moduleItem,
    map: StringMap.empty,
    mapModuleTypes: StringMap.empty,
    moduleEquation: None,
    moduleItem: moduleItem,
    name: name,
    parent: parent,
    typeEquations: StringMap.empty,
  }
}

let root = () => None->createTypeEnv(~name="__root__")

let toString = typeEnv => typeEnv.name

let newModule = (~name, typeEnv) => {
  if Debug.typeEnv.contents {
    Log_.item("TypeEnv.newModule %s %s\n", typeEnv->toString, name)
  }
  let newTypeEnv = Some(typeEnv)->createTypeEnv(~name)
  typeEnv.map = typeEnv.map->StringMap.add(name, Module(newTypeEnv))
  newTypeEnv
}

let newModuleType = (~name, ~signature, typeEnv) => {
  if Debug.typeEnv.contents {
    Log_.item("TypeEnv.newModuleType %s %s\n", typeEnv->toString, name)
  }
  let newTypeEnv = Some(typeEnv)->createTypeEnv(~name)
  typeEnv.mapModuleTypes = typeEnv.mapModuleTypes->StringMap.add(name, (signature, newTypeEnv))
  newTypeEnv
}

let newType = (~name, typeEnv) => {
  if Debug.typeEnv.contents {
    Log_.item("TypeEnv.newType %s %s\n", typeEnv->toString, name)
  }
  typeEnv.map = typeEnv.map->StringMap.add(name, Type(name))
}

let getModule = (~name, typeEnv) =>
  switch typeEnv.map->StringMap.find(name) {
  | Module(typeEnv1) => Some(typeEnv1)
  | Type(_) => None
  | exception Not_found => None
  }

let expandAliasToExternalModule = (~name, typeEnv) =>
  switch typeEnv->getModule(~name) {
  | Some({moduleEquation: Some({internal: false, dep})}) =>
    if Debug.typeEnv.contents {
      Log_.item(
        "TypeEnv.expandAliasToExternalModule %s %s aliased to %s\n",
        typeEnv->toString,
        name,
        dep->depToString,
      )
    }
    Some(dep)
  | _ => None
  }

let addModuleEquation = (~dep, ~internal, typeEnv) => {
  if Debug.typeEnv.contents {
    Log_.item(
      "Typenv.addModuleEquation %s %s dep:%s\n",
      typeEnv->toString,
      internal ? "Internal" : "External",
      dep->depToString,
    )
  }
  typeEnv.moduleEquation = Some({internal: internal, dep: dep})
}

let rec addTypeEquation = (~flattened, ~type_, typeEnv) =>
  switch flattened {
  | list{name} => {
      ...typeEnv,
      typeEquations: typeEnv.typeEquations->StringMap.add(name, type_),
    }
  | list{moduleName, ...rest} =>
    switch typeEnv->getModule(~name=moduleName) {
    | Some(typeEnv1) => {
        ...typeEnv,
        map: typeEnv.map->StringMap.add(
          moduleName,
          Module(typeEnv1->addTypeEquation(~flattened=rest, ~type_)),
        ),
      }
    | None => typeEnv
    }
  | list{} => typeEnv
  }

let addTypeEquations = (~typeEquations, typeEnv) =>
  typeEquations->List.fold_left(
    (te, (longIdent, type_)) =>
      te->addTypeEquation(~flattened=longIdent->Longident.flatten, ~type_),
    typeEnv,
  )

let applyTypeEquations = (~config, ~path, typeEnv) =>
  switch path {
  | Path.Pident(id) =>
    switch typeEnv.typeEquations->StringMap.find(id->Ident.name) {
    | type_ =>
      if Debug.typeResolution.contents {
        Log_.item(
          "Typenv.applyTypeEquations %s name:%s type_:%s\n",
          typeEnv->toString,
          id->Ident.name,
          type_->EmitType.typeToString(~config, ~typeNameIsInterface=_ => false),
        )
      }

      Some(type_)
    | exception Not_found => None
    }
  | _ => None
  }

let rec lookup = (~name, typeEnv) =>
  switch typeEnv.map->StringMap.find(name) {
  | _ => Some(typeEnv)
  | exception Not_found =>
    switch typeEnv.parent {
    | None => None
    | Some(parent) => parent->lookup(~name)
    }
  }

let rec lookupModuleType = (~path, typeEnv) =>
  switch path {
  | list{moduleTypeName} =>
    if Debug.typeEnv.contents {
      Log_.item(
        "Typenv.lookupModuleType %s moduleTypeName:%s\n",
        typeEnv->toString,
        moduleTypeName,
      )
    }
    switch typeEnv.mapModuleTypes->StringMap.find(moduleTypeName) {
    | x => Some(x)
    | exception Not_found =>
      switch typeEnv.parent {
      | None => None
      | Some(parent) => parent->lookupModuleType(~path)
      }
    }
  | list{moduleName, ...path1} =>
    if Debug.typeEnv.contents {
      Log_.item("Typenv.lookupModuleType %s moduleName:%s\n", typeEnv->toString, moduleName)
    }
    switch typeEnv.map->StringMap.find(moduleName) {
    | Module(typeEnv1) => typeEnv1->lookupModuleType(~path=path1)
    | Type(_) => None
    | exception Not_found =>
      switch typeEnv.parent {
      | None => None
      | Some(parent) => parent->lookupModuleType(~path)
      }
    }
  | list{} => None
  }

let rec pathToList = path =>
  switch path {
  | Path.Pident(id) => list{id->Ident.name}
  | Path.Pdot(p, s, _) => list{s, ...p->pathToList}
  | Path.Papply(_) => list{}
  }

let lookupModuleTypeSignature = (~path, typeEnv) => {
  if Debug.typeEnv.contents {
    Log_.item("TypeEnv.lookupModuleTypeSignature %s %s\n", typeEnv->toString, path->Path.name)
  }

  typeEnv->lookupModuleType(~path=path->pathToList->List.rev)
}

let getNestedModuleName = typeEnv =>
  typeEnv.parent == None ? None : Some(typeEnv.name->ModuleName.fromStringUnsafe)

let updateModuleItem = (~nameOpt=None, ~moduleItem, typeEnv) => {
  switch nameOpt {
  | Some("component") => typeEnv.componentModuleItem = moduleItem
  | _ => ()
  }
  typeEnv.moduleItem = moduleItem
}

let rec addModulePath = (~typeEnv, name) =>
  switch typeEnv.parent {
  | None => name->ResolvedName.fromString
  | Some(parent) => typeEnv.name->addModulePath(~typeEnv=parent)->ResolvedName.dot(name)
  }

let rec getModuleEquations = (typeEnv): list<ResolvedName.eq> => {
  let subEquations =
    typeEnv.map
    ->StringMap.bindings
    ->List.map(((_, entry)) =>
      switch entry {
      | Module(te) => te->getModuleEquations
      | Type(_) => list{}
      }
    )
    ->List.concat
  switch (typeEnv.moduleEquation, typeEnv.parent) {
  | (None, _)
  | (_, None) => subEquations
  | (Some({dep}), Some(parent)) => list{
      (dep->depToResolvedName, typeEnv.name->addModulePath(~typeEnv=parent)),
    }
  }
}

let getModuleAccessPath = (~component=false, ~name, typeEnv) => {
  let rec accessPath = typeEnv =>
    switch typeEnv.parent {
    | None => Runtime.Root(name) /* not nested */
    | Some(parent) =>
      Dot(
        parent.parent == None ? Root(typeEnv.name) : parent->accessPath,
        component ? typeEnv.componentModuleItem : typeEnv.moduleItem,
      )
    }
  typeEnv->accessPath
}
