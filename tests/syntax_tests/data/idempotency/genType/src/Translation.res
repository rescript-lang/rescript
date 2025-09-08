open GenTypeCommon

type t = CodeItem.translation

let empty: t = {importTypes: list{}, codeItems: list{}, typeDeclarations: list{}}

let getImportTypeUniqueName = ({typeName, asTypeName}: CodeItem.importType) =>
  typeName ++
  switch asTypeName {
  | None => ""
  | Some(s) => "_as_" ++ s
  }

let importTypeCompare = (i1, i2) =>
  compare(i1->getImportTypeUniqueName, i2->getImportTypeUniqueName)

let combine = (translations: list<t>): t =>
  translations
  ->List.map(({CodeItem.importTypes: importTypes, codeItems, typeDeclarations}) => (
    (importTypes, codeItems),
    typeDeclarations,
  ))
  ->List.split
  ->(((x, y)) => (x->List.split, y))
  ->(
    (((importTypes, codeItems), typeDeclarations)) => {
      CodeItem.importTypes: importTypes->List.concat,
      codeItems: codeItems->List.concat,
      typeDeclarations: typeDeclarations->List.concat,
    }
  )

/* Applies type parameters to types (for all) */
let abstractTheTypeParameters = (~typeVars, type_) =>
  switch type_ {
  | Function(function_) => Function({...function_, typeVars: typeVars})
  | _ => type_
  }

let depToImportType = (~config, ~outputFileRelative, ~resolver, dep: dep) =>
  switch dep {
  | _ if dep->Dependencies.isInternal => list{}
  | External(name) if name == "list" => list{
      {
        CodeItem.typeName: "list",
        asTypeName: None,
        importPath: ModuleName.reasonPervasives->ModuleResolver.importPathForReasonModuleName(
          ~config,
          ~outputFileRelative,
          ~resolver,
        ),
      },
    }
  | External(_) => list{}
  | Internal(_) => list{}

  | Dot(_) =>
    let moduleName = dep->Dependencies.getOuterModuleName
    let typeName = dep->Dependencies.removeExternalOuterModule->depToString
    let asTypeName = dep->Dependencies.isInternal ? None : Some(dep->depToString)
    let importPath =
      moduleName->ModuleResolver.importPathForReasonModuleName(
        ~config,
        ~outputFileRelative,
        ~resolver,
      )
    list{{typeName: typeName, asTypeName: asTypeName, importPath: importPath}}
  }

let translateDependencies = (~config, ~outputFileRelative, ~resolver, dependencies): list<
  CodeItem.importType,
> =>
  dependencies->List.map(depToImportType(~config, ~outputFileRelative, ~resolver))->List.concat

let translateValue = (
  ~attributes,
  ~config,
  ~docString,
  ~outputFileRelative,
  ~resolver,
  ~typeEnv,
  ~typeExpr,
  ~addAnnotationsToFunction: type_ => type_,
  name,
): t => {
  let nameAs = switch Annotation.getGenTypeAsRenaming(attributes) {
  | Some(s) => s
  | _ => name
  }
  let typeExprTranslation =
    typeExpr->TranslateTypeExprFromTypes.translateTypeExprFromTypes(~config, ~typeEnv)
  let typeVars = typeExprTranslation.type_->TypeVars.free
  let type_ =
    typeExprTranslation.type_->abstractTheTypeParameters(~typeVars)->addAnnotationsToFunction
  let resolvedNameOriginal = name->TypeEnv.addModulePath(~typeEnv)->ResolvedName.toString
  let resolvedName = nameAs->TypeEnv.addModulePath(~typeEnv)
  let moduleAccessPath = typeEnv->TypeEnv.getModuleAccessPath(~name=resolvedNameOriginal)

  let codeItems = list{
    CodeItem.ExportValue({
      docString: docString,
      moduleAccessPath: moduleAccessPath,
      originalName: name,
      resolvedName: resolvedName,
      type_: type_,
    }),
  }
  {
    importTypes: typeExprTranslation.dependencies->translateDependencies(
      ~config,
      ~outputFileRelative,
      ~resolver,
    ),
    codeItems: codeItems,
    typeDeclarations: list{},
  }
}

/*
 * The `make` function is typically of the type:
 *
 *    (~named, ~args=?, 'childrenType) => ReasonReactComponentSpec<
 *      State,
 *      State,
 *      RetainedProps,
 *      RetainedProps,
 *      Action,
 *    >)
 *
 * We take a reference to that function and turn it into a React component of
 * type:
 *
 *
 *     exports.component = (component : React.Component<Props>);
 *
 * Where `Props` is of type:
 *
 *     {named: number, args?: number}
 */
let translateComponent = (
  ~attributes,
  ~config,
  ~docString,
  ~outputFileRelative,
  ~resolver,
  ~typeEnv,
  ~typeExpr,
  ~addAnnotationsToFunction: type_ => type_,
  name,
): t => {
  let typeExprTranslation_ = typeExpr->TranslateTypeExprFromTypes.translateTypeExprFromTypes(
    ~config,
    /* Only get the dependencies for the prop types.
     The return type is a ReasonReact component. */
    ~noFunctionReturnDependencies=true,
    ~typeEnv,
  )
  let typeExprTranslation = {
    ...typeExprTranslation_,
    type_: typeExprTranslation_.type_->addAnnotationsToFunction,
  }

  let freeTypeVarsSet = typeExprTranslation.type_->TypeVars.free_

  /* Replace type variables in props/children with any. */
  let (typeVars, type_) = (
    list{},
    typeExprTranslation.type_->TypeVars.substitute(~f=s =>
      if freeTypeVarsSet->StringSet.mem(s) {
        Some(mixedOrUnknown(~config))
      } else {
        None
      }
    ),
  )
  switch type_ {
  | Function(
      {
        argTypes: list{propOrChildren, ...childrenOrNil},
        retType: Ident(
          {
            name:
              "ReasonReact_componentSpec"
              | "React_componentSpec"
              | "ReasonReact_component"
              | "React_component",
            typeArgs: list{_state, ..._},
          } as id,
        ),
        _,
      } as function_,
    ) =>
    let type_ = Function({...function_, retType: Ident({...id, typeArgs: list{}})})

    /* Add children?:any to props type */
    let propsType = switch childrenOrNil {
    /* Then we only extracted a function that accepts children, no props */
    | list{} =>
      GroupOfLabeledArgs(list{
        {
          mutable_: Immutable,
          nameJS: "children",
          nameRE: "children",
          optional: Optional,
          type_: mixedOrUnknown(~config),
        },
      })
    /* Then we had both props and children. */
    | list{{aType: childrenType}, ..._} =>
      switch propOrChildren.aType {
      | GroupOfLabeledArgs(fields) =>
        GroupOfLabeledArgs(
          \"@"(
            fields,
            list{
              {
                mutable_: Immutable,
                nameJS: "children",
                nameRE: "children",
                optional: Optional,
                type_: childrenType,
              },
            },
          ),
        )
      | t => t
      }
    }
    let resolvedTypeName = "Props"->TypeEnv.addModulePath(~typeEnv)

    let nestedModuleName = typeEnv->TypeEnv.getNestedModuleName

    let moduleAccessPath = typeEnv->TypeEnv.getModuleAccessPath(~name="make")
    let componentAccessPath =
      typeEnv->TypeEnv.getModuleAccessPath(~component=true, ~name="component")

    let codeItems = list{
      CodeItem.ExportComponent({
        componentAccessPath: componentAccessPath,
        exportType: {
          nameAs: None,
          opaque: Some(false),
          type_: propsType,
          typeVars: typeVars,
          resolvedTypeName: resolvedTypeName,
        },
        moduleAccessPath: moduleAccessPath,
        nestedModuleName: nestedModuleName,
        type_: type_,
      }),
    }
    {
      importTypes: typeExprTranslation.dependencies->translateDependencies(
        ~config,
        ~outputFileRelative,
        ~resolver,
      ),
      codeItems: codeItems,
      typeDeclarations: list{},
    }

  | _ =>
    /* not a component: treat make as a normal function */
    name->translateValue(
      ~attributes,
      ~config,
      ~docString,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
      ~typeExpr,
      ~addAnnotationsToFunction,
    )
  }
}

@ocaml.doc("
 * [@genType]
 * [@module] external myBanner : ReasonReact.reactClass = \"./MyBanner\";
 ")
let translatePrimitive = (
  ~config,
  ~outputFileRelative,
  ~resolver,
  ~typeEnv,
  valueDescription: Typedtree.value_description,
): t => {
  if Debug.translation.contents {
    Log_.item("Translate Primitive\n")
  }
  let valueName = switch valueDescription.val_prim {
  | list{"", ..._}
  | list{} =>
    valueDescription.val_id->Ident.name
  | list{nameOfExtern, ..._} => /* extern foo : someType = "abc"
     The first element of val_prim is "abc" */
    nameOfExtern
  }
  let typeExprTranslation =
    valueDescription.val_desc->TranslateCoreType.translateCoreType(~config, ~typeEnv)

  let (attributeImport, attributeRenaming) =
    valueDescription.val_attributes->Annotation.getAttributeImportRenaming
  switch (typeExprTranslation.type_, attributeImport) {
  | (
      Function({
        argTypes: list{_, ..._},
        retType: Ident({
          name:
            "ReasonReact_componentSpec"
            | "React_componentSpec"
            | "ReasonReact_component"
            | "React_component",
          typeArgs: list{_state, ..._},
        }),
        _,
      }),
      Some(importString),
    ) if valueName == "make" =>
    let asPath = switch attributeRenaming {
    | Some(asPath) => asPath
    | None => ""
    }
    let typeExprTranslation = valueDescription.val_desc->TranslateCoreType.translateCoreType(
      ~config,
      /* Only get the dependencies for the prop types.
       The return type is a ReasonReact component. */
      ~noFunctionReturnDependencies=true,
      ~typeEnv,
    )

    let freeTypeVarsSet = typeExprTranslation.type_->TypeVars.free_

    /* Replace type variables in props/children with any. */
    let (typeVars, type_) = (
      list{},
      typeExprTranslation.type_->TypeVars.substitute(~f=s =>
        if freeTypeVarsSet->StringSet.mem(s) {
          Some(mixedOrUnknown(~config))
        } else {
          None
        }
      ),
    )

    let (propsFields, childrenTyp) = switch type_ {
    | Function({argTypes: list{propOrChildren, ...childrenOrNil}}) =>
      switch childrenOrNil {
      | list{} => (list{}, mixedOrUnknown(~config))
      | list{{aType: children}, ..._} =>
        switch propOrChildren {
        | {aType: GroupOfLabeledArgs(fields)} => (
            fields->List.map(({optional, type_} as field) =>
              switch (type_, optional) {
              | (Option(type1), Optional) => {
                  ...field,
                  optional: Optional,
                  type_: type1,
                }
              | _ => field
              }
            ),
            children,
          )
        | _ => (list{}, mixedOrUnknown(~config))
        }
      }
    | _ => (list{}, mixedOrUnknown(~config))
    }
    let propsTyp = Object(Closed, propsFields)
    let resolvedTypeName = "Props"->TypeEnv.addModulePath(~typeEnv)
    let propsTypeName = resolvedTypeName->ResolvedName.toString

    let codeItems = list{
      CodeItem.ImportComponent({
        asPath: asPath,
        childrenTyp: childrenTyp,
        exportType: {
          nameAs: None,
          opaque: Some(false),
          type_: propsTyp,
          typeVars: typeVars,
          resolvedTypeName: resolvedTypeName,
        },
        importAnnotation: importString->Annotation.importFromString,
        propsFields: propsFields,
        propsTypeName: propsTypeName,
      }),
    }
    {
      importTypes: typeExprTranslation.dependencies->translateDependencies(
        ~config,
        ~outputFileRelative,
        ~resolver,
      ),
      codeItems: codeItems,
      typeDeclarations: list{},
    }

  | (_, Some(importString)) =>
    let asPath = switch attributeRenaming {
    | Some(asPath) => asPath
    | None => valueName
    }

    let typeVars = typeExprTranslation.type_->TypeVars.free
    let type_ = typeExprTranslation.type_->abstractTheTypeParameters(~typeVars)

    {
      importTypes: typeExprTranslation.dependencies->translateDependencies(
        ~config,
        ~outputFileRelative,
        ~resolver,
      ),
      codeItems: list{
        ImportValue({
          asPath: asPath,
          importAnnotation: importString->Annotation.importFromString,
          type_: type_,
          valueName: valueName,
        }),
      },
      typeDeclarations: list{},
    }

  | _ => {importTypes: list{}, codeItems: list{}, typeDeclarations: list{}}
  }
}

let addTypeDeclarationsFromModuleEquations = (~typeEnv, translation: t) => {
  let eqs = typeEnv->TypeEnv.getModuleEquations
  let newTypeDeclarations =
    translation.typeDeclarations
    ->List.map((typeDeclaration: CodeItem.typeDeclaration) => {
      let exportType = typeDeclaration.exportFromTypeDeclaration.exportType
      let equations = exportType.resolvedTypeName->ResolvedName.applyEquations(~eqs)
      equations->List.map(((x, y)) => {
        let newExportType = {
          ...exportType,
          nameAs: None,
          type_: y
          ->ResolvedName.toString
          ->ident(~builtin=false, ~typeArgs=exportType.typeVars->List.map(s => TypeVar(s))),
          resolvedTypeName: x,
        }
        {
          CodeItem.exportFromTypeDeclaration: {
            CodeItem.exportType: newExportType,
            annotation: typeDeclaration.exportFromTypeDeclaration.annotation,
          },
          importTypes: list{},
        }
      })
    })
    ->List.concat
  newTypeDeclarations == list{}
    ? translation
    : {
        ...translation,
        typeDeclarations: \"@"(translation.typeDeclarations, newTypeDeclarations),
      }
}
