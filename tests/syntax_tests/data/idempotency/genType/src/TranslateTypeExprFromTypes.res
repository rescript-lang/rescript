open GenTypeCommon

type translation = {
  dependencies: list<dep>,
  type_: type_,
}

let rec removeOption = (~label: Asttypes.arg_label, typeExpr: Types.type_expr) =>
  switch (typeExpr.desc, label) {
  | (Tconstr(Path.Pident(id), list{t}, _), Optional(lbl)) if Ident.name(id) == "option" =>
    Some((lbl, t))
  | (Tconstr(Pdot(Path.Pident(nameSpace), id, _), list{t}, _), Optional(lbl))
    if Ident.name(nameSpace) == "FB" && id == "option" =>
    Some((lbl, t))
  | (Tlink(t), _) => t->removeOption(~label)
  | _ => None
  }

let rec pathToList = path =>
  switch path {
  | Path.Pident(id) => list{id->Ident.name}
  | Path.Pdot(p, s, _) => list{s, ...p->pathToList}
  | Path.Papply(_) => list{}
  }

let translateConstr = (
  ~config,
  ~fieldsTranslations,
  ~closedFlag,
  ~paramsTranslation,
  ~path: Path.t,
  ~typeEnv,
) => {
  let defaultCase = () => {
    let typeArgs = paramsTranslation->List.map(({type_}: translation) => type_)
    let typeParamDeps =
      paramsTranslation->List.map(({dependencies}) => dependencies)->List.concat

    switch typeEnv->TypeEnv.applyTypeEquations(~config, ~path) {
    | Some(type_) => {dependencies: typeParamDeps, type_: type_}
    | None =>
      let dep = path->Dependencies.fromPath(~config, ~typeEnv)
      {
        dependencies: list{dep, ...typeParamDeps},
        type_: Ident({builtin: false, name: dep->depToString, typeArgs: typeArgs}),
      }
    }
  }
  switch (path->pathToList->List.rev, paramsTranslation) {
  | (list{"FB", "bool"} | list{"bool"}, list{}) => {dependencies: list{}, type_: booleanT}

  | (list{"FB", "int"} | list{"int"}, list{}) => {dependencies: list{}, type_: numberT}

  | (list{"Int64", "t"} | list{"int64"}, list{}) => {dependencies: list{}, type_: int64T}

  | (list{"FB", "float"} | list{"float"}, list{}) => {dependencies: list{}, type_: numberT}

  | (
      list{"FB", "string"}
      | list{"string"}
      | list{"String", "t"}
      | list{"Js", "String" | "String2", "t"},
      list{},
    ) => {
      dependencies: list{},
      type_: stringT,
    }

  | (list{"Js", "Date", "t"}, list{}) => {dependencies: list{}, type_: dateT}

  | (list{"FB", "unit"} | list{"unit"}, list{}) => {dependencies: list{}, type_: unitT}

  | (
      list{"FB", "array"} | list{"array"} | list{"Js", "Array" | "Array2", "t"},
      list{paramTranslation},
    ) => {
      ...paramTranslation,
      type_: Array(paramTranslation.type_, Mutable),
    }

  | (list{"ImmutableArray", "t"}, list{paramTranslation}) => {
      ...paramTranslation,
      type_: Array(paramTranslation.type_, Immutable),
    }

  | (list{"Pervasives", "ref"}, list{paramTranslation}) => {
      dependencies: paramTranslation.dependencies,
      type_: Tuple(list{paramTranslation.type_}),
    }

  | (
      list{"Pervasives", "result"} | list{"Belt", "Result", "t"},
      list{paramTranslation1, paramTranslation2},
    ) =>
    let case = (n, name, type_) => ({label: string_of_int(n), labelJS: StringLabel(name)}, 1, type_)
    let variant = createVariant(
      ~noPayloads=list{},
      ~payloads=list{
        case(0, "Ok", paramTranslation1.type_),
        case(1, "Error", paramTranslation2.type_),
      },
      ~polymorphic=false,
    )
    {
      dependencies: \"@"(paramTranslation1.dependencies, paramTranslation2.dependencies),
      type_: variant,
    }

  | (list{"React", "callback"}, list{fromTranslation, toTranslation}) => {
      dependencies: \"@"(fromTranslation.dependencies, toTranslation.dependencies),
      type_: Function({
        argTypes: list{{aName: "", aType: fromTranslation.type_}},
        componentName: None,
        retType: toTranslation.type_,
        typeVars: list{},
        uncurried: false,
      }),
    }

  | (list{"React", "componentLike"}, list{propsTranslation, retTranslation}) => {
      dependencies: \"@"(propsTranslation.dependencies, retTranslation.dependencies),
      type_: Function({
        argTypes: list{{aName: "", aType: propsTranslation.type_}},
        componentName: None,
        retType: retTranslation.type_,
        typeVars: list{},
        uncurried: false,
      }),
    }

  | (list{"React", "component"}, list{propsTranslation}) => {
      dependencies: propsTranslation.dependencies,
      type_: Function({
        argTypes: list{{aName: "", aType: propsTranslation.type_}},
        componentName: None,
        retType: EmitType.typeReactElement(~config),
        typeVars: list{},
        uncurried: false,
      }),
    }

  | (list{"React", "Context", "t"}, list{paramTranslation}) => {
      dependencies: paramTranslation.dependencies,
      type_: EmitType.typeReactContext(~config, ~type_=paramTranslation.type_),
    }

  | (list{"React", "Ref", "t"}, list{paramTranslation}) => {
      dependencies: paramTranslation.dependencies,
      type_: EmitType.typeReactRef(~type_=paramTranslation.type_),
    }

  | (list{"ReactDOMRe", "domRef"}, list{}) => {
      dependencies: list{},
      type_: EmitType.typeReactDOMReDomRef(~config),
    }

  | (list{"ReactDOMRe", "Ref", "currentDomRef"}, list{}) => {
      dependencies: list{},
      type_: EmitType.typeAny(~config),
    }

  | (list{"React", "element"} | list{"ReasonReact", "reactElement"}, list{}) => {
      dependencies: list{},
      type_: EmitType.typeReactElement(~config),
    }

  | (list{"FB", "option"} | list{"option"}, list{paramTranslation}) => {
      ...paramTranslation,
      type_: Option(paramTranslation.type_),
    }

  | (list{"Js", "Null", "t"} | list{"Js", "null"}, list{paramTranslation}) => {
      ...paramTranslation,
      type_: Null(paramTranslation.type_),
    }

  | (
      list{"Js", "Nullable", "t"}
      | list{"Js", "nullable"}
      | list{"Js", "Null_undefined", "t"}
      | list{"Js", "null_undefined"},
      list{paramTranslation},
    ) => {
      ...paramTranslation,
      type_: Nullable(paramTranslation.type_),
    }
  | (list{"Js", "Promise", "t"}, list{paramTranslation}) => {
      ...paramTranslation,
      type_: Promise(paramTranslation.type_),
    }
  | (
      list{"Js", "Internal", "fn"},
      list{{dependencies: argsDependencies, type_: Tuple(ts)}, ret},
    ) => {
      dependencies: \"@"(argsDependencies, ret.dependencies),
      type_: Function({
        argTypes: ts->List.map(type_ => {aName: "", aType: type_}),
        componentName: None,
        retType: ret.type_,
        typeVars: list{},
        uncurried: true,
      }),
    }

  | (
      list{"Js", "Internal", "fn"},
      list{
        {dependencies: argsDependencies, type_: Variant({noPayloads: list{{label: "Arity_0"}}})},
        ret,
      },
    ) => {
      dependencies: \"@"(argsDependencies, ret.dependencies),
      type_: Function({
        argTypes: list{},
        componentName: None,
        retType: ret.type_,
        typeVars: list{},
        uncurried: true,
      }),
    }
  | (list{"Js", "Fn", "arity0"}, list{ret}) => {
      dependencies: ret.dependencies,
      type_: Function({
        argTypes: list{},
        componentName: None,
        retType: ret.type_,
        typeVars: list{},
        uncurried: true,
      }),
    }
  | (
      list{
        "Js" | "Js_OO",
        "Fn" | "Meth",
        "arity1"
        | "arity2"
        | "arity3"
        | "arity4"
        | "arity5"
        | "arity6"
        | "arity7"
        | "arity8"
        | "arity9"
        | "arity10"
        | "arity11"
        | "arity12"
        | "arity13"
        | "arity14"
        | "arity15"
        | "arity16"
        | "arity17"
        | "arity18"
        | "arity19"
        | "arity20"
        | "arity21"
        | "arity22",
      },
      list{arg},
    ) => {
      dependencies: arg.dependencies,
      type_: switch arg.type_ {
      | Function(function_) => Function({...function_, uncurried: true})
      | _ => arg.type_
      },
    }
  | (list{"Js", "Internal", "fn"}, list{{dependencies: argsDependencies, type_: singleT}, ret}) =>
    let argTypes = switch singleT {
    | Variant({payloads: list{(_, _, Tuple(argTypes))}}) => argTypes
    | Variant({payloads: list{(_, _, type_)}}) => list{type_}
    | _ => list{singleT}
    }->List.map(type_ => {aName: "", aType: type_})
    {
      dependencies: \"@"(argsDependencies, ret.dependencies),
      type_: Function({
        argTypes: argTypes,
        componentName: None,
        retType: ret.type_,
        typeVars: list{},
        uncurried: true,
      }),
    }
  | (
      list{"Js", "Internal", "meth"} | list{"Js_internalOO", "meth"},
      list{
        {
          dependencies: argsDependencies,
          type_: Variant({payloads: list{({label: "Arity_1"}, _, type_)}}),
        },
        ret,
      },
    ) => {
      dependencies: \"@"(argsDependencies, ret.dependencies),
      type_: Function({
        argTypes: list{{aName: "", aType: type_}},
        componentName: None,
        retType: ret.type_,
        typeVars: list{},
        uncurried: true,
      }),
    }
  | (
      list{"Js", "Internal", "meth"} | list{"Js_internalOO", "meth"},
      list{
        {dependencies: argsDependencies, type_: Variant({payloads: list{(_, _, Tuple(ts))}})},
        ret,
      },
    ) => {
      dependencies: \"@"(argsDependencies, ret.dependencies),
      type_: Function({
        argTypes: ts->List.map(type_ => {aName: "", aType: type_}),
        componentName: None,
        retType: ret.type_,
        typeVars: list{},
        uncurried: true,
      }),
    }
  | (list{"Js", "t"}, _) =>
    let dependencies =
      fieldsTranslations->List.map(((_, {dependencies})) => dependencies)->List.concat
    let rec checkMutableField = (~acc=list{}, fields) =>
      switch fields {
      | list{(previousName, {type_: _}), (name, {type_}), ...rest}
        if Runtime.checkMutableObjectField(~previousName, ~name) =>
        /* The field was annotated "@set" */
        rest->checkMutableField(~acc=list{(name, type_, Mutable), ...acc})
      | list{(name, {type_}), ...rest} =>
        rest->checkMutableField(~acc=list{(name, type_, Immutable), ...acc})
      | list{} => acc->List.rev
      }
    let fields =
      fieldsTranslations
      ->checkMutableField
      ->List.map(((name, t, mutable_)) => {
        let (optional, type_) = switch t {
        | Option(t) => (Optional, t)
        | _ => (Mandatory, t)
        }
        let name = name->Runtime.mangleObjectField
        {mutable_: mutable_, nameJS: name, nameRE: name, optional: optional, type_: type_}
      })
    let type_ = Object(closedFlag, fields)
    {dependencies: dependencies, type_: type_}

  | _ => defaultCase()
  }
}

type processVariant = {
  noPayloads: list<string>,
  payloads: list<(string, Types.type_expr)>,
  unknowns: list<string>,
}

let processVariant = rowFields => {
  let rec loop = (~noPayloads, ~payloads, ~unknowns, fields) =>
    switch fields {
    | list{
        (
          label,
          Types.Rpresent(/* no payload */ None)
          | Reither(/* constant constructor */ true, _, _, _),
        ),
        ...otherFields,
      } =>
      otherFields->loop(~noPayloads=list{label, ...noPayloads}, ~payloads, ~unknowns)
    | list{(label, Rpresent(Some(payload))), ...otherFields} =>
      otherFields->loop(~noPayloads, ~payloads=list{(label, payload), ...payloads}, ~unknowns)
    | list{(label, Rabsent | Reither(false, _, _, _)), ...otherFields} =>
      otherFields->loop(~noPayloads, ~payloads, ~unknowns=list{label, ...unknowns})
    | list{} => {
        noPayloads: noPayloads->List.rev,
        payloads: payloads->List.rev,
        unknowns: unknowns->List.rev,
      }
    }
  rowFields->loop(~noPayloads=list{}, ~payloads=list{}, ~unknowns=list{})
}

let rec translateArrowType = (
  ~config,
  ~typeVarsGen,
  ~noFunctionReturnDependencies=false,
  ~typeEnv,
  ~revArgDeps,
  ~revArgs,
  typeExpr: Types.type_expr,
) =>
  switch typeExpr.desc {
  | Tlink(t) =>
    translateArrowType(
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      ~typeEnv,
      ~revArgDeps,
      ~revArgs,
      t,
    )
  | Tarrow(Nolabel, typeExpr1, typeExpr2, _) =>
    let {dependencies, type_} =
      typeExpr1->translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv, _)
    let nextRevDeps = List.rev_append(dependencies, revArgDeps)
    typeExpr2->translateArrowType(
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      ~typeEnv,
      ~revArgDeps=nextRevDeps,
      ~revArgs=list{(Nolabel, type_), ...revArgs},
    )
  | Tarrow((Labelled(lbl) | Optional(lbl)) as label, typeExpr1, typeExpr2, _) =>
    switch typeExpr1->removeOption(~label) {
    | None =>
      let {dependencies, type_: type1} =
        typeExpr1->translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv)
      let nextRevDeps = List.rev_append(dependencies, revArgDeps)
      typeExpr2->translateArrowType(
        ~config,
        ~typeVarsGen,
        ~noFunctionReturnDependencies,
        ~typeEnv,
        ~revArgDeps=nextRevDeps,
        ~revArgs=list{(Label(lbl->Runtime.mangleObjectField), type1), ...revArgs},
      )
    | Some((lbl, t1)) =>
      let {dependencies, type_: type1} =
        t1->translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv)
      let nextRevDeps = List.rev_append(dependencies, revArgDeps)
      typeExpr2->translateArrowType(
        ~config,
        ~typeVarsGen,
        ~noFunctionReturnDependencies,
        ~typeEnv,
        ~revArgDeps=nextRevDeps,
        ~revArgs=list{(OptLabel(lbl->Runtime.mangleObjectField), type1), ...revArgs},
      )
    }
  | _ =>
    let {dependencies, type_: retType} =
      typeExpr->translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv)
    let allDeps = List.rev_append(revArgDeps, noFunctionReturnDependencies ? list{} : dependencies)

    let labeledConvertableTypes = revArgs->List.rev
    let argTypes = labeledConvertableTypes->NamedArgs.group

    let functionType = Function({
      argTypes: argTypes,
      componentName: None,
      retType: retType,
      typeVars: list{},
      uncurried: false,
    })

    {dependencies: allDeps, type_: functionType}
  }
and translateTypeExprFromTypes_ = (
  ~config,
  ~typeVarsGen,
  ~noFunctionReturnDependencies=false,
  ~typeEnv,
  typeExpr: Types.type_expr,
) =>
  switch typeExpr.desc {
  | Tvar(None) =>
    let typeName = GenIdent.jsTypeNameForAnonymousTypeID(~typeVarsGen, typeExpr.id)
    {dependencies: list{}, type_: TypeVar(typeName)}

  | Tvar(Some(s)) => {dependencies: list{}, type_: TypeVar(s)}

  | Tconstr(Pdot(Pident({name: "Js"}), "t", _) as path, list{{desc: Tobject(tObj, _)}}, _) =>
    let rec getFieldTypes = (texp: Types.type_expr) =>
      switch texp.desc {
      | Tfield(name, _, t1, t2) =>
        let (closedFlafg, fields) = t2->getFieldTypes
        (
          closedFlafg,
          list{
            (
              name,
              name->Runtime.isMutableObjectField
                ? {dependencies: list{}, type_: ident("")}
                : t1->translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv),
            ),
            ...fields,
          },
        )
      | Tlink(te) => te->getFieldTypes
      | Tvar(None) => (Open, list{})
      | _ => (Closed, list{})
      }
    let (closedFlag, fieldsTranslations) = tObj->getFieldTypes
    translateConstr(
      ~config,
      ~fieldsTranslations,
      ~closedFlag,
      ~paramsTranslation=list{},
      ~path,
      ~typeEnv,
    )

  | Tconstr(path, list{{desc: Tlink(te)}}, r) =>
    {...typeExpr, desc: Types.Tconstr(path, list{te}, r)}->translateTypeExprFromTypes_(
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies=false,
      ~typeEnv,
    )

  | Tconstr(path, typeParams, _) =>
    let paramsTranslation =
      typeParams->translateTypeExprsFromTypes_(~config, ~typeVarsGen, ~typeEnv)
    translateConstr(
      ~config,
      ~fieldsTranslations=list{},
      ~closedFlag=Closed,
      ~paramsTranslation,
      ~path,
      ~typeEnv,
    )

  | Tpoly(t, list{}) =>
    t->translateTypeExprFromTypes_(~config, ~typeVarsGen, ~noFunctionReturnDependencies, ~typeEnv)

  | Tarrow(_) =>
    typeExpr->translateArrowType(
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      ~typeEnv,
      ~revArgDeps=list{},
      ~revArgs=list{},
    )
  | Ttuple(listExp) =>
    let innerTypesTranslation =
      listExp->translateTypeExprsFromTypes_(~config, ~typeVarsGen, ~typeEnv)
    let innerTypes = innerTypesTranslation->List.map(({type_}) => type_)
    let innerTypesDeps =
      innerTypesTranslation->List.map(({dependencies}) => dependencies)->List.concat

    let tupleType = Tuple(innerTypes)

    {dependencies: innerTypesDeps, type_: tupleType}

  | Tlink(t) =>
    t->translateTypeExprFromTypes_(~config, ~typeVarsGen, ~noFunctionReturnDependencies, ~typeEnv)

  | Tvariant(rowDesc) =>
    switch rowDesc.row_fields->processVariant {
    | {noPayloads, payloads: list{}, unknowns: list{}} =>
      let noPayloads = noPayloads->List.map(label => {label: label, labelJS: StringLabel(label)})
      let type_ = createVariant(~noPayloads, ~payloads=list{}, ~polymorphic=true)
      {dependencies: list{}, type_: type_}

    | {noPayloads: list{}, payloads: list{(_label, t)}, unknowns: list{}} =>
      /* Handle ReScript's "Arity_" encoding in first argument of Js.Internal.fn(_,_) for uncurried functions.
       Return the argument tuple. */
      t->translateTypeExprFromTypes_(
        ~config,
        ~typeVarsGen,
        ~noFunctionReturnDependencies,
        ~typeEnv,
      )

    | {noPayloads, payloads, unknowns: list{}} =>
      let noPayloads = noPayloads->List.map(label => {label: label, labelJS: StringLabel(label)})
      let payloadTranslations =
        payloads->List.map(((label, payload)) => (
          label,
          payload->translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv),
        ))
      let payloads = payloadTranslations->List.map(((label, translation)) => {
        let numArgs = 1
        ({label: label, labelJS: StringLabel(label)}, numArgs, translation.type_)
      })
      let type_ = createVariant(~noPayloads, ~payloads, ~polymorphic=true)
      let dependencies =
        payloadTranslations->List.map(((_, {dependencies})) => dependencies)->List.concat
      {dependencies: dependencies, type_: type_}

    | {unknowns: list{_, ..._}} => {
        dependencies: list{},
        type_: mixedOrUnknown(~config),
      }
    }

  | Tpackage(path, ids, types) =>
    switch typeEnv->TypeEnv.lookupModuleTypeSignature(~path) {
    | Some((signature, typeEnv)) =>
      let typeEquationsTranslation =
        List.combine(ids, types)->List.map(((x, t)) => (
          x,
          t->translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv),
        ))
      let typeEquations =
        typeEquationsTranslation->List.map(((x, translation)) => (x, translation.type_))
      let dependenciesFromTypeEquations =
        typeEquationsTranslation
        ->List.map(((_, translation)) => translation.dependencies)
        ->List.flatten
      let typeEnv1 = typeEnv->TypeEnv.addTypeEquations(~typeEquations)
      let (dependenciesFromRecordType, type_) =
        signature.sig_type->signatureToModuleRuntimeRepresentation(
          ~config,
          ~typeVarsGen,
          ~typeEnv=typeEnv1,
        )
      {
        dependencies: \"@"(dependenciesFromTypeEquations, dependenciesFromRecordType),
        type_: type_,
      }
    | None => {dependencies: list{}, type_: mixedOrUnknown(~config)}
    }

  | Tfield(_)
  | Tnil
  | Tobject(_)
  | Tpoly(_)
  | Tsubst(_)
  | Tunivar(_) => {dependencies: list{}, type_: mixedOrUnknown(~config)}
  }
and translateTypeExprsFromTypes_ = (~config, ~typeVarsGen, ~typeEnv, typeExprs): list<
  translation,
> => typeExprs->List.map(translateTypeExprFromTypes_(~config, ~typeVarsGen, ~typeEnv))
and signatureToModuleRuntimeRepresentation = (~config, ~typeVarsGen, ~typeEnv, signature) => {
  let dependenciesAndFields = signature->List.map(signatureItem =>
    switch signatureItem {
    | Types.Sig_value(_id, {val_kind: Val_prim(_)}) => (list{}, list{})
    | Types.Sig_value(id, {val_type: typeExpr}) =>
      let {dependencies, type_} =
        typeExpr->translateTypeExprFromTypes_(
          ~config,
          ~typeVarsGen,
          ~noFunctionReturnDependencies=false,
          ~typeEnv,
        )
      let field = {
        mutable_: Immutable,
        nameJS: id->Ident.name,
        nameRE: id->Ident.name,
        optional: Mandatory,
        type_: type_,
      }
      (dependencies, list{field})

    | Types.Sig_module(id, moduleDeclaration, _recStatus) =>
      let typeEnv1 = switch typeEnv->TypeEnv.getModule(~name=id->Ident.name) {
      | Some(typeEnv1) => typeEnv1
      | None => typeEnv
      }
      let (dependencies, type_) = switch moduleDeclaration.md_type {
      | Mty_signature(signature) =>
        signature->signatureToModuleRuntimeRepresentation(
          ~config,
          ~typeVarsGen,
          ~typeEnv=typeEnv1,
        )
      | Mty_ident(_)
      | Mty_functor(_)
      | Mty_alias(_) => (list{}, mixedOrUnknown(~config))
      }
      let field = {
        mutable_: Immutable,
        nameJS: id->Ident.name,
        nameRE: id->Ident.name,
        optional: Mandatory,
        type_: type_,
      }
      (dependencies, list{field})

    | Types.Sig_type(_)
    | Types.Sig_typext(_)
    | Types.Sig_modtype(_)
    | Types.Sig_class(_)
    | Types.Sig_class_type(_) => (list{}, list{})
    }
  )
  let (dependencies, fields) = {
    let (dl, fl) = dependenciesAndFields->List.split
    (dl->List.concat, fl->List.concat)
  }
  (dependencies, config.modulesAsObjects ? Object(Closed, fields) : Record(fields))
}

let translateTypeExprFromTypes = (~config, ~noFunctionReturnDependencies=?, ~typeEnv, typeExpr) => {
  let typeVarsGen = GenIdent.createTypeVarsGen()
  let translation =
    typeExpr->translateTypeExprFromTypes_(
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies?,
      ~typeEnv,
    )

  if Debug.dependencies.contents {
    translation.dependencies->List.iter(dep => Log_.item("Dependency: %s\n", dep->depToString))
  }
  translation
}

let translateTypeExprsFromTypes = (~config, ~typeEnv, typeExprs) => {
  let typeVarsGen = GenIdent.createTypeVarsGen()
  let translations = typeExprs->translateTypeExprsFromTypes_(~config, ~typeVarsGen, ~typeEnv)

  if Debug.dependencies.contents {
    translations->List.iter(translation =>
      translation.dependencies->List.iter(dep =>
        Log_.item("Dependency: %s\n", dep->depToString)
      )
    )
  }
  translations
}
