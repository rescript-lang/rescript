open GenTypeCommon

type env = {
  requiresEarly: ModuleNameMap.t<(ImportPath.t, bool)>,
  requires: ModuleNameMap.t<(ImportPath.t, bool)>,
  /* For each .cmt we import types from, keep the map of exported types. */
  cmtToExportTypeMap: StringMap.t<CodeItem.exportTypeMap>,
  /* Map of types imported from other files. */
  exportTypeMapFromOtherFiles: CodeItem.exportTypeMap,
  importedValueOrComponent: bool,
}

let requireModule = (~\"import", ~env, ~importPath, ~strict=false, moduleName) => {
  let requires = \"import" ? env.requiresEarly : env.requires
  let requiresNew = requires->ModuleNameMap.add(moduleName, (importPath, strict))
  \"import" ? {...env, requiresEarly: requiresNew} : {...env, requires: requiresNew}
}

let createExportTypeMap = (
  ~config,
  ~file,
  typeDeclarations: list<CodeItem.typeDeclaration>,
): CodeItem.exportTypeMap => {
  if Debug.codeItems.contents {
    Log_.item("Create Type Map for %s\n", file)
  }
  let updateExportTypeMap = (
    exportTypeMap: CodeItem.exportTypeMap,
    typeDeclaration: CodeItem.typeDeclaration,
  ): CodeItem.exportTypeMap => {
    let addExportType = (~annotation, {resolvedTypeName, type_, typeVars}: CodeItem.exportType) => {
      if Debug.codeItems.contents {
        Log_.item(
          "Type Map: %s%s%s\n",
          resolvedTypeName->ResolvedName.toString,
          typeVars == list{} ? "" : "(" ++ ((typeVars->String.concat(",")) ++ ")"),
          " " ++
          ((annotation->Annotation.toString->EmitText.comment) ++
          (" = " ++ (type_->EmitType.typeToString(~config, ~typeNameIsInterface=_ => false)))),
        )
      }

      exportTypeMap->StringMap.add(
        resolvedTypeName->ResolvedName.toString,
        {CodeItem.typeVars: typeVars, type_: type_, annotation: annotation},
      )
    }
    switch typeDeclaration.exportFromTypeDeclaration {
    | {exportType, annotation} => exportType->addExportType(~annotation)
    }
  }
  typeDeclarations->List.fold_left(updateExportTypeMap, StringMap.empty)
}

let codeItemToString = (~config, ~typeNameIsInterface, codeItem: CodeItem.t) =>
  switch codeItem {
  | ExportComponent({nestedModuleName}) =>
    "ExportComponent nestedModuleName:" ++
    switch nestedModuleName {
    | Some(moduleName) => moduleName->ModuleName.toString
    | None => ""
    }
  | ExportValue({resolvedName, type_}) =>
    "ExportValue" ++
    (" resolvedName:" ++
    (ResolvedName.toString(resolvedName) ++
    (" type:" ++
    EmitType.typeToString(~config, ~typeNameIsInterface, type_))))
  | ImportComponent({importAnnotation}) =>
    "ImportComponent " ++ (importAnnotation.importPath->ImportPath.dump)
  | ImportValue({importAnnotation}) =>
    "ImportValue " ++ (importAnnotation.importPath->ImportPath.dump)
  }

let emitExportType = (
  ~early=?,
  ~emitters,
  ~config,
  ~typeGetNormalized,
  ~typeNameIsInterface,
  {CodeItem.nameAs: nameAs, opaque, type_, typeVars, resolvedTypeName},
) => {
  let (opaque, type_) = switch opaque {
  | Some(opaque) => (opaque, type_)
  | None =>
    let normalized = type_->typeGetNormalized
    (false, normalized)
  }
  resolvedTypeName
  ->ResolvedName.toString
  ->EmitType.emitExportType(
    ~early?,
    ~config,
    ~emitters,
    ~nameAs,
    ~opaque,
    ~type_,
    ~typeNameIsInterface,
    ~typeVars,
  )
}

let typeNameIsInterface = (
  ~exportTypeMap: CodeItem.exportTypeMap,
  ~exportTypeMapFromOtherFiles: CodeItem.exportTypeMap,
  typeName,
) => {
  let typeIsInterface = type_ =>
    switch type_ {
    | Object(_)
    | Record(_) => true
    | _ => false
    }
  switch exportTypeMap->StringMap.find(typeName) {
  | {type_} => type_->typeIsInterface
  | exception Not_found =>
    switch exportTypeMapFromOtherFiles->StringMap.find(typeName) {
    | {type_} => type_->typeIsInterface
    | exception Not_found => false
    }
  }
}

let emitExportFromTypeDeclaration = (
  ~config,
  ~emitters,
  ~typeGetNormalized,
  ~env,
  ~typeNameIsInterface,
  exportFromTypeDeclaration: CodeItem.exportFromTypeDeclaration,
) => (
  env,
  exportFromTypeDeclaration.exportType->emitExportType(
    ~emitters,
    ~config,
    ~typeGetNormalized,
    ~typeNameIsInterface,
  ),
)

let emitExportFromTypeDeclarations = (
  ~config,
  ~emitters,
  ~env,
  ~typeGetNormalized,
  ~typeNameIsInterface,
  exportFromTypeDeclarations,
) =>
  exportFromTypeDeclarations->List.fold_left(
    ((env, emitters)) =>
      emitExportFromTypeDeclaration(
        ~config,
        ~emitters,
        ~env,
        ~typeGetNormalized,
        ~typeNameIsInterface,
      ),
    (env, emitters),
  )

let rec emitCodeItem = (
  ~config,
  ~emitters,
  ~moduleItemsEmitter,
  ~env,
  ~fileName,
  ~outputFileRelative,
  ~resolver,
  ~typeGetConverter,
  ~typeGetInlined,
  ~typeGetNormalized,
  ~typeNameIsInterface,
  ~variantTables,
  codeItem,
) => {
  let language = config.language
  if Debug.codeItems.contents {
    Log_.item("Code Item: %s\n", codeItem->codeItemToString(~config, ~typeNameIsInterface))
  }
  let indent = Some("")

  switch codeItem {
  | ImportComponent({
      asPath,
      childrenTyp,
      exportType,
      importAnnotation,
      propsFields,
      propsTypeName,
    }) =>
    let importPath = importAnnotation.importPath
    let name = importAnnotation.name

    let es6 = switch (language, config.module_) {
    | (_, ES6)
    | (TypeScript, _) => true
    | (Flow | Untyped, _) => false
    }

    let (firstNameInPath, restOfPath, lastNameInPath) = switch asPath->Str.split(
      Str.regexp("\\."),
    ) {
    | list{x, ...y} =>
      let lastNameInPath = switch y->List.rev {
      | list{last, ..._} => last
      | list{} => x
      }
      es6
        ? (x, list{"", ...y}->String.concat("."), lastNameInPath)
        : (name, list{"", x, ...y}->String.concat("."), lastNameInPath)
    | _ => (name, "", name)
    }

    let componentPath = firstNameInPath ++ restOfPath

    let nameGen = EmitText.newNameGen()

    let (emitters, env) = if es6 {
      /* emit an import {... as ...} immediately */
      let emitters =
        importPath->EmitType.emitImportValueAsEarly(
          ~config,
          ~emitters,
          ~name=firstNameInPath,
          ~nameAs=firstNameInPath == name ? None : Some(firstNameInPath),
        )
      (emitters, env)
    } else {
      /* add an early require(...) */
      let env =
        firstNameInPath
        ->ModuleName.fromStringUnsafe
        ->requireModule(~\"import"=true, ~env, ~importPath, ~strict=true)
      (emitters, env)
    }
    let componentNameTypeChecked = lastNameInPath ++ "TypeChecked"

    /* Check the type of the component */
    config.emitImportReact = true
    let emitters = emitExportType(
      ~early=true,
      ~config,
      ~emitters,
      ~typeGetNormalized,
      ~typeNameIsInterface,
      exportType,
    )
    let emitters =
      config.language == Untyped
        ? emitters
        : ("(" ++
          (("props"->EmitType.ofType(
            ~config,
            ~typeNameIsInterface,
            ~type_=ident(propsTypeName),
          )) ++
          ")")
            ->EmitType.ofType(
              ~config,
              ~typeNameIsInterface,
              ~type_=EmitType.typeReactElement(~config),
            )) ++ (" {\n  return <" ++ (componentPath ++ " {...props}/>;\n}"))
            ->EmitType.emitExportFunction(
              ~early=true,
              ~emitters,
              ~name=componentNameTypeChecked,
              ~config,
              ~comment="In case of type error, check the type of '" ++
              ("make" ++
              ("' in '" ++
              ((fileName->ModuleName.toString) ++
              (".re'" ++
              (" and the props of '" ++
              ((importPath->ImportPath.emit(~config)) ++ "'.")))))),
            )

    /* Wrap the component */
    let emitters =
      "function " ++
      (EmitText.parens(
        \"@"(propsFields->List.map(({nameJS}: field) => nameJS), list{"children"})->List.map(
          EmitType.ofTypeAny(~config),
        ),
      ) ++
      (" { return ReasonReact.wrapJsForReason" ++
      (EmitText.parens(list{
        componentPath,
        "{" ++
        ((propsFields
        ->List.map(({nameJS: propName, optional, type_: propTyp}) =>
          propName ++
          (": " ++
          (propName->Converter.toJS(
            ~config,
            ~converter=(optional == Mandatory ? propTyp : Option(propTyp))->typeGetConverter,
            ~indent,
            ~nameGen,
            ~variantTables,
          )))
        )
        ->String.concat(", ")) ++
        "}"),
        "children"->Converter.toJS(
          ~config,
          ~converter=childrenTyp->typeGetConverter,
          ~indent,
          ~nameGen,
          ~variantTables,
        ),
      }) ++ "; }"))) ++ ";"
        ->EmitType.emitExportConstEarly(
          ~comment="Export '" ++
          ("make" ++
          "' early to allow circular import from the '.bs.js' file."),
          ~config,
          ~emitters,
          ~name="make",
          ~type_=mixedOrUnknown(~config),
          ~typeNameIsInterface,
        )
    let env =
      ModuleName.reasonReact->requireModule(
        ~\"import"=true,
        ~env,
        ~importPath=ImportPath.reasonReactPath(~config),
      )
    ({...env, importedValueOrComponent: true}, emitters)

  | ImportValue({asPath, importAnnotation, type_, valueName}) =>
    let nameGen = EmitText.newNameGen()
    let importPath = importAnnotation.importPath
    let importFile = importAnnotation.name

    let (firstNameInPath, restOfPath) =
      valueName == asPath
        ? (valueName, "")
        : switch asPath->Str.split(Str.regexp("\\.")) {
          | list{x, ...y} => (x, list{"", ...y}->String.concat("."))
          | _ => (asPath, "")
          }
    let importFileVariable = "$$" ++ importFile
    let (emitters, importedAsName, env) = switch (language, config.module_) {
    | (_, ES6)
    | (TypeScript, _) =>
      /* emit an import {... as ...} immediately */
      let valueNameNotChecked = valueName ++ "NotChecked"
      let emitters =
        importPath->EmitType.emitImportValueAsEarly(
          ~config,
          ~emitters,
          ~name=firstNameInPath,
          ~nameAs=Some(valueNameNotChecked),
        )
      (emitters, valueNameNotChecked, env)
    | (Flow | Untyped, _) =>
      /* add an early require(...) */
      let importedAsName =
        firstNameInPath == "default"
          ? importFileVariable
          : importFileVariable ++ ("." ++ firstNameInPath)
      let env =
        importFileVariable
        ->ModuleName.fromStringUnsafe
        ->requireModule(~\"import"=true, ~env, ~importPath, ~strict=true)
      (emitters, importedAsName, env)
    }

    let type_ = switch type_ {
    | Function({argTypes: list{{aType: Object(_)}}, retType} as function_)
      if retType->EmitType.isTypeReactElement(~config) =>
      let componentName = switch importFile {
      | "."
      | ".." =>
        None
      | _ => Some(importFile)
      }
      Function({...function_, componentName: componentName})
    | _ => type_
    }

    let converter = type_->typeGetConverter

    let valueNameTypeChecked = valueName ++ "TypeChecked"

    let emitters =
      importedAsName ++ restOfPath ++ ";"
        ->EmitType.emitExportConstEarly(
          ~config,
          ~comment="In case of type error, check the type of '" ++
          (valueName ++
          ("' in '" ++
          ((fileName->ModuleName.toString) ++
          (".re'" ++ (" and '" ++ ((importPath->ImportPath.emit(~config)) ++ "'.")))))),
          ~emitters,
          ~name=valueNameTypeChecked,
          ~type_,
          ~typeNameIsInterface,
        )
    let valueNameNotDefault = valueName == "default" ? Runtime.default : valueName
    let emitters =
      (valueNameTypeChecked
      ->Converter.toReason(~config, ~converter, ~indent, ~nameGen, ~variantTables)
      ->EmitType.emitTypeCast(~config, ~type_, ~typeNameIsInterface)) ++ ";"
        ->EmitType.emitExportConstEarly(
          ~comment="Export '" ++
          (valueNameNotDefault ++
          "' early to allow circular import from the '.bs.js' file."),
          ~config,
          ~emitters,
          ~name=valueNameNotDefault,
          ~type_=mixedOrUnknown(~config),
          ~typeNameIsInterface,
        )
    let emitters =
      valueName == "default"
        ? EmitType.emitExportDefault(~emitters, ~config, valueNameNotDefault)
        : emitters

    ({...env, importedValueOrComponent: true}, emitters)

  | ExportComponent({componentAccessPath, exportType, moduleAccessPath, nestedModuleName, type_}) =>
    let nameGen = EmitText.newNameGen()
    let converter = type_->typeGetConverter
    let importPath =
      fileName->ModuleResolver.resolveModule(
        ~importExtension=".bs",
        ~outputFileRelative,
        ~resolver,
        ~useBsDependencies=false,
      )
    let moduleNameBs = fileName->ModuleName.forBsFile
    let moduleName = switch nestedModuleName {
    | Some(moduleName) => moduleName
    | None => fileName
    }
    let propsTypeName = exportType.resolvedTypeName->ResolvedName.toString
    let componentType = EmitType.typeReactComponent(~config, ~propsType=ident(propsTypeName))

    let name = EmitType.componentExportName(~config, ~fileName, ~moduleName)
    let jsProps = "jsProps"
    let jsPropsDot = s => jsProps ++ ("." ++ s)

    let (propConverters, childrenConverter) = switch converter {
    | FunctionC({argConverters}) =>
      switch argConverters {
      | list{GroupConverter(propConverters), ArgConverter(childrenConverter), ..._} => (
          propConverters,
          childrenConverter,
        )
      | list{ArgConverter(childrenConverter), ..._} => (list{}, childrenConverter)

      | _ => (list{}, IdentC)
      }

    | _ => (list{}, IdentC)
    }

    let args = \"@"(
      propConverters->List.map(((s, optional, argConverter)) =>
        jsPropsDot(s)->Converter.toReason(
          ~config,
          ~converter=optional == Optional &&
            !(argConverter->Converter.converterIsIdentity(~toJS=false))
            ? OptionC(argConverter)
            : argConverter,
          ~indent,
          ~nameGen,
          ~variantTables,
        )
      ),
      list{
        jsPropsDot("children")->Converter.toReason(
          ~config,
          ~converter=childrenConverter,
          ~indent,
          ~nameGen,
          ~variantTables,
        ),
      },
    )

    let emitters = emitExportType(
      ~emitters,
      ~config,
      ~typeGetNormalized,
      ~typeNameIsInterface,
      exportType,
    )

    let emitters = EmitType.emitExportConst(
      ~config,
      ~emitters,
      ~name,
      ~type_=componentType,
      ~typeNameIsInterface,
      list{
        "ReasonReact.wrapReasonForJs(",
        "  " ++
        (ModuleName.toString(moduleNameBs) ++
        ("." ++ ((componentAccessPath->Runtime.emitModuleAccessPath(~config)) ++ ","))),
        "  (function _(" ++
        (EmitType.ofType(~config, ~typeNameIsInterface, ~type_=ident(propsTypeName), jsProps) ++
        ") {"),
        "     return " ++
        ((ModuleName.toString(moduleNameBs) ++
        ("." ++
        (moduleAccessPath->Runtime.emitModuleAccessPath(~config)))
          ->EmitText.curry(~args, ~numArgs=args->List.length)) ++
        ";"),
        "  }));",
      }->String.concat("\n"),
    )

    let emitters = switch exportType.type_ {
    | GroupOfLabeledArgs(fields) if config.language == Untyped && config.propTypes =>
      fields
      ->List.map((field: field) => {
        let type_ = field.type_->typeGetInlined
        {...field, type_: type_}
      })
      ->EmitType.emitPropTypes(~config, ~name, ~emitters, ~indent)
    | _ => emitters
    }

    let emitters =
      /* only export default for the top level component in the file */
      fileName == moduleName ? EmitType.emitExportDefault(~emitters, ~config, name) : emitters

    let env = moduleNameBs->requireModule(~\"import"=false, ~env, ~importPath)

    let env =
      ModuleName.reasonReact->requireModule(
        ~\"import"=false,
        ~env,
        ~importPath=ImportPath.reasonReactPath(~config),
      )

    let numArgs = args->List.length
    let useCurry = numArgs >= 2
    config.emitImportCurry = config.emitImportCurry || useCurry
    (env, emitters)

  | ExportValue({docString, moduleAccessPath, originalName, resolvedName, type_}) =>
    resolvedName->ExportModule.extendExportModules(~moduleItemsEmitter, ~type_)
    let resolvedName = ResolvedName.toString(resolvedName)
    let nameGen = EmitText.newNameGen()
    let importPath =
      fileName->ModuleResolver.resolveModule(
        ~importExtension=".bs",
        ~outputFileRelative,
        ~resolver,
        ~useBsDependencies=false,
      )
    let fileNameBs = fileName->ModuleName.forBsFile
    let envWithRequires = fileNameBs->requireModule(~\"import"=false, ~env, ~importPath)

    let default = "default"
    let make = "make"

    let name = originalName == default ? Runtime.default : resolvedName

    module HookType = {
      type t = {
        propsType: type_,
        resolvedTypeName: ResolvedName.t,
        retType: type_,
        typeVars: list<string>,
      }
    }

    let (type_, hookType) = switch type_ {
    | Function(
        {argTypes: list{{aType: Object(closedFlags, fields)}}, retType, typeVars} as function_,
      ) if retType->EmitType.isTypeReactElement(~config) =>
      let propsType = {
        let fields =
          fields->List.map((field: field) =>
            field.nameJS == "children" && field.type_->EmitType.isTypeReactElement(~config)
              ? {...field, type_: EmitType.typeReactChild(~config)}
              : field
          )
        Object(closedFlags, fields)
      }
      let function_ = {
        ...function_,
        argTypes: list{{aName: "", aType: propsType}},
      }
      let chopSuffix = suffix =>
        resolvedName == suffix
          ? ""
          : Filename.check_suffix(resolvedName, "_" ++ suffix)
          ? Filename.chop_suffix(resolvedName, "_" ++ suffix)
          : resolvedName
      let suffix = if originalName == default {
        chopSuffix(default)
      } else if originalName == make {
        chopSuffix(make)
      } else {
        resolvedName
      }
      let hookName = (fileName->ModuleName.toString) ++ (suffix == "" ? suffix : "_" ++ suffix)
      let resolvedTypeName = if (
        !config.emitTypePropDone && (originalName == default || originalName == make)
      ) {
        config.emitTypePropDone = true
        ResolvedName.fromString("Props")
      } else {
        ResolvedName.fromString(name)->ResolvedName.dot("Props")
      }

      (
        Function({...function_, componentName: Some(hookName)}),
        Some({
          HookType.propsType: propsType,
          resolvedTypeName: resolvedTypeName,
          retType: retType,
          typeVars: typeVars,
        }),
      )
    | _ => (type_, None)
    }
    /* Work around Flow issue with function components.
     If type annotated direcly, they are not checked. But typeof() works. */
    let flowFunctionTypeWorkaround = hookType != None && config.language == Flow

    let converter = type_->typeGetConverter

    let hookNameForTypeof = name ++ "$$forTypeof"
    let type_ = flowFunctionTypeWorkaround ? ident("typeof(" ++ (hookNameForTypeof ++ ")")) : type_

    let emitters = switch hookType {
    | Some({propsType, retType, typeVars}) if flowFunctionTypeWorkaround =>
      EmitType.emitHookTypeAsFunction(
        ~config,
        ~emitters,
        ~name=hookNameForTypeof,
        ~propsType,
        ~retType,
        ~retValue="null",
        ~typeNameIsInterface,
        ~typeVars,
      )
    | _ => emitters
    }

    let emitters = switch hookType {
    | Some({propsType, resolvedTypeName, typeVars}) =>
      let exportType: CodeItem.exportType = {
        nameAs: None,
        opaque: Some(false),
        type_: propsType,
        typeVars: typeVars,
        resolvedTypeName: resolvedTypeName,
      }
      if config.language == TypeScript {
        config.emitImportReact = true // For doc gen (https://github.com/cristianoc/genType/issues/342)
      }
      emitExportType(~emitters, ~config, ~typeGetNormalized, ~typeNameIsInterface, exportType)
    | _ => emitters
    }

    let emitters =
      ((fileNameBs->ModuleName.toString) ++
        ("." ++
        (moduleAccessPath->Runtime.emitModuleAccessPath(~config)))
        ->Converter.toJS(~config, ~converter, ~indent, ~nameGen, ~variantTables)) ++ ";"
        ->EmitType.emitExportConst(
          ~config,
          ~docString,
          ~emitters,
          ~name,
          ~type_,
          ~typeNameIsInterface,
        )

    let emitters = switch hookType {
    | Some({propsType: Object(_, fields)}) if config.language == Untyped && config.propTypes =>
      fields
      ->List.map((field: field) => {
        let type_ = field.type_->typeGetInlined
        {...field, type_: type_}
      })
      ->EmitType.emitPropTypes(~config, ~name, ~emitters, ~indent)
    | _ => emitters
    }

    let emitters =
      originalName == default
        ? EmitType.emitExportDefault(~emitters, ~config, Runtime.default)
        : emitters

    (envWithRequires, emitters)
  }
}
and emitCodeItems = (
  ~config,
  ~outputFileRelative,
  ~emitters,
  ~moduleItemsEmitter,
  ~env,
  ~fileName,
  ~resolver,
  ~typeNameIsInterface,
  ~typeGetConverter,
  ~typeGetInlined,
  ~typeGetNormalized,
  ~variantTables,
  codeItems,
) =>
  codeItems->List.fold_left(
    ((env, emitters)) =>
      emitCodeItem(
        ~config,
        ~emitters,
        ~moduleItemsEmitter,
        ~env,
        ~fileName,
        ~outputFileRelative,
        ~resolver,
        ~typeGetConverter,
        ~typeGetInlined,
        ~typeGetNormalized,
        ~typeNameIsInterface,
        ~variantTables,
      ),
    (env, emitters),
  )

let emitRequires = (~importedValueOrComponent, ~early, ~config, ~requires, emitters) =>
  ModuleNameMap.fold(
    (moduleName, (importPath, strict), emitters) =>
      importPath->EmitType.emitRequire(
        ~importedValueOrComponent,
        ~early,
        ~emitters,
        ~config,
        ~moduleName,
        ~strict,
      ),
    requires,
    emitters,
  )

let emitVariantTables = (~config, ~emitters, variantTables) => {
  let typeAnnotation = config.language == TypeScript ? ": { [key: string]: any }" : ""
  let emitTable = (~table, ~toJS, variantC: Converter.variantC) =>
    "const " ++
    (table ++
    (typeAnnotation ++
    (" = {" ++
    ((variantC.noPayloads
    ->List.map(case => {
      let js = case.labelJS->labelJSToString(~alwaysQuotes=!toJS)
      let re =
        case.label->Runtime.emitVariantLabel(~comment=false, ~polymorphic=variantC.polymorphic)
      toJS ? (re->EmitText.quotes) ++ (": " ++ js) : js ++ (": " ++ re)
    })
    ->String.concat(", ")) ++ "};"))))
  Hashtbl.fold(((_, toJS), variantC, l) => list{(variantC, toJS), ...l}, variantTables, list{})
  ->List.sort(((variantC1, toJS1), (variantC2, toJS2)) => {
    let n = compare(variantC1.Converter.hash, variantC2.hash)
    n != 0 ? n : compare(toJS2, toJS1)
  })
  ->List.fold_left(
    (emitters, (variantC, toJS)) =>
      variantC
      ->emitTable(~table=variantC.Converter.hash->variantTable(~toJS), ~toJS)
      ->Emitters.requireEarly(~emitters),
    emitters,
  )
}

let typeGetInlined = (~config, ~exportTypeMap, type_) =>
  type_->Converter.typeGetNormalized(
    ~config,
    ~inline=true,
    ~lookupId=s => exportTypeMap->StringMap.find(s),
    ~typeNameIsInterface=_ => false,
  )

/* Read the cmt file referenced in an import type,
 and recursively for the import types obtained from reading the cmt file. */
let rec readCmtFilesRecursively = (
  ~config,
  ~env,
  ~inputCmtTranslateTypeDeclarations,
  ~outputFileRelative,
  ~resolver,
  {CodeItem.typeName: typeName, asTypeName, importPath},
) => {
  let updateTypeMapFromOtherFiles = (~asType, ~exportTypeMapFromCmt, env) =>
    switch exportTypeMapFromCmt->StringMap.find(typeName) {
    | (exportTypeItem: CodeItem.exportTypeItem) =>
      let type_ =
        exportTypeItem.type_->typeGetInlined(~config, ~exportTypeMap=exportTypeMapFromCmt)
      {
        ...env,
        exportTypeMapFromOtherFiles: env.exportTypeMapFromOtherFiles->StringMap.add(
          asType,
          {...exportTypeItem, type_: type_},
        ),
      }
    | exception Not_found => env
    }
  let cmtFile = importPath->ImportPath.toCmt(~config, ~outputFileRelative)->Paths.getCmtFile
  switch asTypeName {
  | Some(asType) if cmtFile != "" =>
    switch env.cmtToExportTypeMap->StringMap.find(cmtFile) {
    | exportTypeMapFromCmt => env->updateTypeMapFromOtherFiles(~asType, ~exportTypeMapFromCmt)

    | exception Not_found =>
      /* cmt file not read before: this ensures termination */
      let typeDeclarations =
        Cmt_format.read_cmt(cmtFile)
        ->inputCmtTranslateTypeDeclarations(~config, ~outputFileRelative, ~resolver)
        ->((x: CodeItem.translation) => x.typeDeclarations)

      let exportTypeMapFromCmt =
        typeDeclarations->createExportTypeMap(
          ~config,
          ~file=cmtFile->Filename.basename->Filename.chop_extension,
        )
      let cmtToExportTypeMap =
        env.cmtToExportTypeMap->StringMap.add(cmtFile, exportTypeMapFromCmt)
      let env =
        {...env, cmtToExportTypeMap: cmtToExportTypeMap}->updateTypeMapFromOtherFiles(
          ~asType,
          ~exportTypeMapFromCmt,
        )

      let newImportTypes =
        typeDeclarations
        ->List.map((typeDeclaration: CodeItem.typeDeclaration) => typeDeclaration.importTypes)
        ->List.concat

      newImportTypes->List.fold_left(
        (env, newImportType) =>
          newImportType->readCmtFilesRecursively(
            ~config,
            ~env,
            ~inputCmtTranslateTypeDeclarations,
            ~outputFileRelative,
            ~resolver,
          ),
        env,
      )
    }
  | _ => env
  }
}

let emitImportType = (
  ~config,
  ~emitters,
  ~env,
  ~inputCmtTranslateTypeDeclarations,
  ~outputFileRelative,
  ~resolver,
  ~typeNameIsInterface,
  {CodeItem.typeName: typeName, asTypeName, importPath} as importType,
) => {
  let env =
    importType->readCmtFilesRecursively(
      ~config,
      ~env,
      ~inputCmtTranslateTypeDeclarations,
      ~outputFileRelative,
      ~resolver,
    )
  let emitters = EmitType.emitImportTypeAs(
    ~emitters,
    ~config,
    ~typeName,
    ~asTypeName,
    ~typeNameIsInterface=typeNameIsInterface(~env),
    ~importPath,
  )
  (env, emitters)
}

let emitImportTypes = (
  ~config,
  ~emitters,
  ~env,
  ~inputCmtTranslateTypeDeclarations,
  ~outputFileRelative,
  ~resolver,
  ~typeNameIsInterface,
  importTypes,
) =>
  importTypes->List.fold_left(
    ((env, emitters)) =>
      emitImportType(
        ~config,
        ~emitters,
        ~env,
        ~inputCmtTranslateTypeDeclarations,
        ~outputFileRelative,
        ~resolver,
        ~typeNameIsInterface,
      ),
    (env, emitters),
  )

let getAnnotatedTypedDeclarations = (~annotatedSet, typeDeclarations) =>
  typeDeclarations
  ->List.map(typeDeclaration => {
    let nameInAnnotatedSet =
      annotatedSet->StringSet.mem(
        typeDeclaration.CodeItem.exportFromTypeDeclaration.exportType.resolvedTypeName->ResolvedName.toString,
      )
    if nameInAnnotatedSet {
      {
        ...typeDeclaration,
        exportFromTypeDeclaration: {
          ...typeDeclaration.exportFromTypeDeclaration,
          annotation: GenType,
        },
      }
    } else {
      typeDeclaration
    }
  })
  ->List.filter(({exportFromTypeDeclaration: {annotation}}: CodeItem.typeDeclaration) =>
    annotation != NoGenType
  )

let propagateAnnotationToSubTypes = (~codeItems, typeMap: CodeItem.exportTypeMap) => {
  let annotatedSet = ref(StringSet.empty)
  let initialAnnotatedTypes =
    typeMap
    ->StringMap.bindings
    ->List.filter(((_, {CodeItem.annotation: annotation})) => annotation == Annotation.GenType)
    ->List.map(((_, {CodeItem.type_: type_})) => type_)
  let typesOfExportedValue = (codeItem: CodeItem.t) =>
    switch codeItem {
    | ExportComponent({type_})
    | ExportValue({type_})
    | ImportValue({type_}) => list{type_}
    | _ => list{}
    }
  let typesOfExportedValues = codeItems->List.map(typesOfExportedValue)->List.concat

  let visitTypAndUpdateMarked = type0 => {
    let visited = ref(StringSet.empty)
    let rec visit = type_ =>
      switch type_ {
      | Ident({name: typeName, typeArgs}) =>
        if visited.contents->StringSet.mem(typeName) {
          ()
        } else {
          visited := visited.contents->StringSet.add(typeName)
          typeArgs->List.iter(visit)
          switch typeMap->StringMap.find(typeName) {
          | {annotation: GenType | GenTypeOpaque} => ()
          | {type_: type1, annotation: NoGenType} =>
            if Debug.translation.contents {
              Log_.item("Marking Type As Annotated %s\n", typeName)
            }
            annotatedSet := annotatedSet.contents->StringSet.add(typeName)
            type1->visit
          | exception Not_found => annotatedSet := annotatedSet.contents->StringSet.add(typeName)
          }
        }
      | Array(t, _) => t->visit
      | Function({argTypes, retType}) =>
        argTypes->List.iter(({aType}) => visit(aType))
        retType->visit
      | GroupOfLabeledArgs(fields)
      | Object(_, fields)
      | Record(fields) =>
        fields->List.iter(({type_}) => type_->visit)
      | Option(t)
      | Null(t)
      | Nullable(t)
      | Promise(t) =>
        t->visit
      | Tuple(innerTypes) => innerTypes->List.iter(visit)
      | TypeVar(_) => ()
      | Variant({payloads}) => payloads->List.iter(((_, _, t)) => t->visit)
      }
    type0->visit
  }
  \"@"(initialAnnotatedTypes, typesOfExportedValues)->List.iter(visitTypAndUpdateMarked)
  let newTypeMap = typeMap->StringMap.mapi((
    typeName,
    exportTypeItem: CodeItem.exportTypeItem,
  ) => {
    ...exportTypeItem,
    annotation: annotatedSet.contents->StringSet.mem(typeName)
      ? Annotation.GenType
      : exportTypeItem.annotation,
  })

  (newTypeMap, annotatedSet.contents)
}

let emitTranslationAsString = (
  ~config,
  ~fileName,
  ~inputCmtTranslateTypeDeclarations,
  ~outputFileRelative,
  ~resolver,
  translation: Translation.t,
) => {
  let initialEnv = {
    requires: ModuleNameMap.empty,
    requiresEarly: ModuleNameMap.empty,
    cmtToExportTypeMap: StringMap.empty,
    exportTypeMapFromOtherFiles: StringMap.empty,
    importedValueOrComponent: false,
  }
  let variantTables = Hashtbl.create(1)

  let (exportTypeMap, annotatedSet) =
    translation.typeDeclarations
    ->createExportTypeMap(~config, ~file=fileName->ModuleName.toString)
    ->propagateAnnotationToSubTypes(~codeItems=translation.codeItems)

  let annotatedTypeDeclarations =
    translation.typeDeclarations->getAnnotatedTypedDeclarations(~annotatedSet)

  let importTypesFromTypeDeclarations =
    annotatedTypeDeclarations
    ->List.map((typeDeclaration: CodeItem.typeDeclaration) => typeDeclaration.importTypes)
    ->List.concat

  let exportFromTypeDeclarations =
    annotatedTypeDeclarations->List.map((typeDeclaration: CodeItem.typeDeclaration) =>
      typeDeclaration.exportFromTypeDeclaration
    )

  let typeNameIsInterface = (~env) =>
    typeNameIsInterface(
      ~exportTypeMap,
      ~exportTypeMapFromOtherFiles=env.exportTypeMapFromOtherFiles,
    )

  let lookupId_ = (~env, s) =>
    try exportTypeMap->StringMap.find(s) catch {
    | Not_found => env.exportTypeMapFromOtherFiles->StringMap.find(s)
    }

  let typeGetNormalized_ = (~env, ~inline=false, type_) =>
    type_->Converter.typeGetNormalized(
      ~config,
      ~inline,
      ~lookupId=lookupId_(~env),
      ~typeNameIsInterface=typeNameIsInterface(~env),
    )

  let typeGetConverter_ = (~env, type_) =>
    type_->Converter.typeGetConverter(
      ~config,
      ~lookupId=lookupId_(~env),
      ~typeNameIsInterface=typeNameIsInterface(~env),
    )

  let emitters = Emitters.initial
  and moduleItemsEmitter = ExportModule.createModuleItemsEmitter()
  and env = initialEnv

  let (env, emitters) =
    /* imports from type declarations go first to build up type tables */
    \"@"(importTypesFromTypeDeclarations, translation.importTypes)
    ->List.sort_uniq(Translation.importTypeCompare)
    ->emitImportTypes(
      ~config,
      ~emitters,
      ~env,
      ~inputCmtTranslateTypeDeclarations,
      ~outputFileRelative,
      ~resolver,
      ~typeNameIsInterface,
    )

  let (env, emitters) =
    exportFromTypeDeclarations->emitExportFromTypeDeclarations(
      ~config,
      ~emitters,
      ~typeGetNormalized=typeGetNormalized_(~env),
      ~env,
      ~typeNameIsInterface=typeNameIsInterface(~env),
    )

  let (env, emitters) =
    translation.codeItems->emitCodeItems(
      ~config,
      ~emitters,
      ~moduleItemsEmitter,
      ~env,
      ~fileName,
      ~outputFileRelative,
      ~resolver,
      ~typeGetInlined=typeGetNormalized_(~env, ~inline=true),
      ~typeGetNormalized=typeGetNormalized_(~env),
      ~typeGetConverter=typeGetConverter_(~env),
      ~typeNameIsInterface=typeNameIsInterface(~env),
      ~variantTables,
    )
  let emitters = config.emitImportReact ? EmitType.emitImportReact(~emitters, ~config) : emitters

  let env = config.emitImportCurry
    ? ModuleName.curry->requireModule(
        ~\"import"=true,
        ~env,
        ~importPath=ImportPath.bsCurryPath(~config),
      )
    : env

  let env = config.emitImportPropTypes
    ? ModuleName.propTypes->requireModule(~\"import"=true, ~env, ~importPath=ImportPath.propTypes)
    : env

  let finalEnv = config.emitCreateBucklescriptBlock
    ? ModuleName.createRescriptBlock->requireModule(
        ~\"import"=true,
        ~env,
        ~importPath=ImportPath.bsBlockPath(~config),
      )
    : env

  let emitters = variantTables->emitVariantTables(~config, ~emitters)
  let emitters =
    moduleItemsEmitter->ExportModule.emitAllModuleItems(~config, ~emitters, ~fileName)

  emitters
  ->emitRequires(
    ~importedValueOrComponent=false,
    ~early=true,
    ~config,
    ~requires=finalEnv.requiresEarly,
  )
  ->emitRequires(
    ~importedValueOrComponent=finalEnv.importedValueOrComponent,
    ~early=false,
    ~config,
    ~requires=finalEnv.requires,
  )
  ->Emitters.toString(~separator="\n\n")
}
