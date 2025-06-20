open GenTypeCommon

type rec exportModuleItem = Hashtbl.t<string, exportModuleValue>
and exportModuleValue =
  | S(string, type_)
  | M(exportModuleItem)

type exportModuleItems = Hashtbl.t<string, exportModuleItem>

type types = {
  typeForValue: type_,
  typeForType: type_,
}

let rec exportModuleValueToType = exportModuleValue =>
  switch exportModuleValue {
  | S(s, type_) => {typeForValue: ident(s), typeForType: type_}
  | M(exportModuleItem) =>
    let (fieldsForValue, fieldsForType) = exportModuleItem->exportModuleItemToFields->List.split
    {
      typeForValue: Object(Open, fieldsForValue),
      typeForType: Object(Open, fieldsForType),
    }
  }
and exportModuleItemToFields: exportModuleItem => list<(field, field)> = exportModuleItem =>
  Hashtbl.fold((fieldName, exportModuleValue, fields) => {
    let {typeForValue, typeForType} = exportModuleValue->exportModuleValueToType
    let fieldForType = {
      mutable_: Mutable,
      nameJS: fieldName,
      nameRE: fieldName,
      optional: Mandatory,
      type_: typeForType,
    }
    let fieldForValue = {...fieldForType, type_: typeForValue}
    list{(fieldForValue, fieldForType), ...fields}
  }, exportModuleItem, list{})

let rec extendExportModuleItem = (x, ~exportModuleItem: exportModuleItem, ~type_, ~valueName) =>
  switch x {
  | list{} => ()
  | list{fieldName} => Hashtbl.replace(exportModuleItem, fieldName, S(valueName, type_))
  | list{fieldName, ...rest} =>
    let innerExportModuleItem = switch Hashtbl.find(exportModuleItem, fieldName) {
    | M(innerExportModuleItem) => innerExportModuleItem
    | S(_) => assert(false)
    | exception Not_found =>
      let innerExportModuleItem = Hashtbl.create(1)
      Hashtbl.replace(exportModuleItem, fieldName, M(innerExportModuleItem))
      innerExportModuleItem
    }
    rest->extendExportModuleItem(~exportModuleItem=innerExportModuleItem, ~valueName, ~type_)
  }

let extendExportModuleItems = (x, ~exportModuleItems: exportModuleItems, ~type_, ~valueName) =>
  switch x {
  | list{} => assert(false)
  | list{_valueName} => ()
  | list{moduleName, ...rest} =>
    let exportModuleItem = switch Hashtbl.find(exportModuleItems, moduleName) {
    | exportModuleItem => exportModuleItem
    | exception Not_found =>
      let exportModuleItem = Hashtbl.create(1)
      Hashtbl.replace(exportModuleItems, moduleName, exportModuleItem)
      exportModuleItem
    }
    rest->extendExportModuleItem(~exportModuleItem, ~type_, ~valueName)
  }

let createModuleItemsEmitter: unit => exportModuleItems = () => Hashtbl.create(1)

let rev_fold = (f, tbl, base) => {
  let list = Hashtbl.fold((k, v, l) => list{(k, v), ...l}, tbl, list{})
  List.fold_left((x, (k, v)) => f(k, v, x), base, list)
}

let emitAllModuleItems = (~config, ~emitters, ~fileName, exportModuleItems: exportModuleItems) =>
  emitters->rev_fold((moduleName, exportModuleItem, emitters) => {
    let {typeForValue, typeForType} = M(exportModuleItem)->exportModuleValueToType
    let emittedModuleItem = config.modulesAsObjects
      ? ModuleName.forInnerModule(~fileName, ~innerModuleName=moduleName)->ModuleName.toString
      : typeForValue->EmitType.typeToString(
          /* abuse type to print object */
          ~config={...config, language: Flow},
          ~typeNameIsInterface=_ => false,
        )
    emittedModuleItem->EmitType.emitExportConst(
      ~config,
      ~emitters,
      ~name=moduleName,
      ~type_=typeForType,
      ~typeNameIsInterface=_ => false,
    )
  }, exportModuleItems)

let extendExportModules = (~moduleItemsEmitter: exportModuleItems, ~type_, resolvedName) =>
  resolvedName
  ->ResolvedName.toList
  ->extendExportModuleItems(
    ~exportModuleItems=moduleItemsEmitter,
    ~type_,
    ~valueName=resolvedName->ResolvedName.toString,
  )
