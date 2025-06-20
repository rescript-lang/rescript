open GenTypeCommon

open! TranslateTypeExprFromTypes

let removeOption = (~label: Asttypes.arg_label, coreType: Typedtree.core_type) =>
  switch (coreType.ctyp_desc, label) {
  | (Ttyp_constr(Path.Pident(id), _, list{t}), Optional(lbl)) if Ident.name(id) == "option" =>
    Some((lbl, t))
  | (Ttyp_constr(Pdot(Path.Pident(nameSpace), id, _), _, list{t}), Optional(lbl))
    /* This has a different representation in 4.03+ */
    if Ident.name(nameSpace) == "FB" && id == "option" =>
    Some((lbl, t))
  | _ => None
  }

type processVariant = {
  noPayloads: list<(string, Typedtree.attributes)>,
  payloads: list<(string, Typedtree.attributes, Typedtree.core_type)>,
  unknowns: list<string>,
}

let processVariant = rowFields => {
  let rec loop = (~noPayloads, ~payloads, ~unknowns, fields) =>
    switch fields {
    | list{
        Typedtree.Ttag({txt: label}, attributes, _, /* only variants with no payload */ list{}),
        ...otherFields,
      } =>
      otherFields->loop(
        ~noPayloads=list{(label, attributes), ...noPayloads},
        ~payloads,
        ~unknowns,
      )
    | list{Ttag({txt: label}, attributes, _, list{payload}), ...otherFields} =>
      otherFields->loop(
        ~noPayloads,
        ~payloads=list{(label, attributes, payload), ...payloads},
        ~unknowns,
      )
    | list{Ttag(_, _, _, list{_, _, ..._}) | Tinherit(_), ...otherFields} =>
      otherFields->loop(~noPayloads, ~payloads, ~unknowns=list{"Tinherit", ...unknowns})
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
  ~noFunctionReturnDependencies,
  ~typeEnv,
  ~revArgDeps,
  ~revArgs,
  coreType: Typedtree.core_type,
) =>
  switch coreType.ctyp_desc {
  | Ttyp_arrow(Nolabel, coreType1, coreType2) =>
    let {dependencies, type_} = coreType1->translateCoreType_(~config, ~typeVarsGen, ~typeEnv, _)
    let nextRevDeps = List.rev_append(dependencies, revArgDeps)
    coreType2->translateArrowType(
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      ~typeEnv,
      ~revArgDeps=nextRevDeps,
      ~revArgs=list{(Nolabel, type_), ...revArgs},
    )
  | Ttyp_arrow((Labelled(lbl) | Optional(lbl)) as label, coreType1, coreType2) =>
    let asLabel = switch coreType.ctyp_attributes->Annotation.getGenTypeAsRenaming {
    | Some(s) => s
    | None => ""
    }
    switch coreType1->removeOption(~label) {
    | None =>
      let {dependencies, type_: type1} =
        coreType1->translateCoreType_(~config, ~typeVarsGen, ~typeEnv)
      let nextRevDeps = List.rev_append(dependencies, revArgDeps)
      coreType2->translateArrowType(
        ~config,
        ~typeVarsGen,
        ~noFunctionReturnDependencies,
        ~typeEnv,
        ~revArgDeps=nextRevDeps,
        ~revArgs=list{
          (Label(asLabel == "" ? lbl->Runtime.mangleObjectField : asLabel), type1),
          ...revArgs,
        },
      )
    | Some((lbl, t1)) =>
      let {dependencies, type_: type1} = t1->translateCoreType_(~config, ~typeVarsGen, ~typeEnv)
      let nextRevDeps = List.rev_append(dependencies, revArgDeps)
      coreType2->translateArrowType(
        ~config,
        ~typeVarsGen,
        ~noFunctionReturnDependencies,
        ~typeEnv,
        ~revArgDeps=nextRevDeps,
        ~revArgs=list{
          (OptLabel(asLabel == "" ? lbl->Runtime.mangleObjectField : asLabel), type1),
          ...revArgs,
        },
      )
    }
  | _ =>
    let {dependencies, type_: retType} =
      coreType->translateCoreType_(~config, ~typeVarsGen, ~typeEnv)
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
and translateCoreType_ = (
  ~config,
  ~typeVarsGen,
  ~noFunctionReturnDependencies=false,
  ~typeEnv,
  coreType: Typedtree.core_type,
) =>
  switch coreType.ctyp_desc {
  | Ttyp_alias(ct, _) =>
    ct->translateCoreType_(~config, ~typeVarsGen, ~noFunctionReturnDependencies=false, ~typeEnv)

  | Ttyp_constr(
      Pdot(Pident({name: "Js"}), "t", _) as path,
      _,
      list{{
        ctyp_desc:
          Ttyp_object(tObj, closedFlag)
          | Ttyp_alias({ctyp_desc: Ttyp_object(tObj, closedFlag)}, _),
        _,
      }},
    ) =>
    let getFieldType = objectField =>
      switch objectField {
      | Typedtree.OTtag({txt: name}, _, t) => (
          name,
          name->Runtime.isMutableObjectField
            ? {dependencies: list{}, type_: ident("")}
            : t->translateCoreType_(~config, ~typeVarsGen, ~typeEnv),
        )
      | OTinherit(t) => ("Inherit", t->translateCoreType_(~config, ~typeVarsGen, ~typeEnv))
      }
    let fieldsTranslations = tObj->List.map(getFieldType)
    translateConstr(
      ~config,
      ~fieldsTranslations,
      ~closedFlag=closedFlag == Closed ? Closed : Open,
      ~paramsTranslation=list{},
      ~path,
      ~typeEnv,
    )

  | Ttyp_constr(path, _, typeParams) =>
    let paramsTranslation = typeParams->translateCoreTypes_(~config, ~typeVarsGen, ~typeEnv)
    TranslateTypeExprFromTypes.translateConstr(
      ~config,
      ~fieldsTranslations=list{},
      ~closedFlag=Closed,
      ~paramsTranslation,
      ~path,
      ~typeEnv,
    )

  | Ttyp_poly(_, t) =>
    t->translateCoreType_(~config, ~typeVarsGen, ~noFunctionReturnDependencies, ~typeEnv)

  | Ttyp_arrow(_) =>
    coreType->translateArrowType(
      ~config,
      ~typeVarsGen,
      ~noFunctionReturnDependencies,
      ~typeEnv,
      ~revArgDeps=list{},
      ~revArgs=list{},
    )

  | Ttyp_tuple(listExp) =>
    let innerTypesTranslation = listExp->translateCoreTypes_(~config, ~typeVarsGen, ~typeEnv)
    let innerTypes = innerTypesTranslation->List.map(({type_}) => type_)
    let innerTypesDeps =
      innerTypesTranslation->List.map(({dependencies}) => dependencies)->List.concat

    let tupleType = Tuple(innerTypes)

    {dependencies: innerTypesDeps, type_: tupleType}

  | Ttyp_var(s) => {dependencies: list{}, type_: TypeVar(s)}

  | Ttyp_variant(rowFields, _, _) =>
    switch rowFields->processVariant {
    | {noPayloads, payloads, unknowns: list{}} =>
      let noPayloads =
        noPayloads->List.map(((label, _attibutes)) => {label: label, labelJS: StringLabel(label)})
      let payloadsTranslations =
        payloads->List.map(((label, attributes, payload)) => (
          label,
          attributes,
          payload->translateCoreType_(~config, ~typeVarsGen, ~typeEnv),
        ))
      let payloads = payloadsTranslations->List.map(((label, _attributes, translation)) => {
        let numArgs = 1
        ({label: label, labelJS: StringLabel(label)}, numArgs, translation.type_)
      })
      let type_ = createVariant(~noPayloads, ~payloads, ~polymorphic=true)
      let dependencies =
        payloadsTranslations->List.map(((_, _, {dependencies})) => dependencies)->List.concat
      {dependencies: dependencies, type_: type_}

    | _ => {dependencies: list{}, type_: mixedOrUnknown(~config)}
    }

  | Ttyp_package({pack_path, pack_fields}) =>
    switch typeEnv->TypeEnv.lookupModuleTypeSignature(~path=pack_path) {
    | Some((signature, typeEnv)) =>
      let typeEquationsTranslation =
        pack_fields->List.map(((x, t)) => (
          x.Asttypes.txt,
          t->translateCoreType_(~config, ~typeVarsGen, ~typeEnv),
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

  | Ttyp_any
  | Ttyp_class(_)
  | Ttyp_object(_) => {dependencies: list{}, type_: mixedOrUnknown(~config)}
  }
and translateCoreTypes_ = (~config, ~typeVarsGen, ~typeEnv, typeExprs): list<translation> =>
  typeExprs->List.map(translateCoreType_(~config, ~typeVarsGen, ~typeEnv))

let translateCoreType = (~config, ~noFunctionReturnDependencies=?, ~typeEnv, coreType) => {
  let typeVarsGen = GenIdent.createTypeVarsGen()
  let translation =
    coreType->translateCoreType_(~config, ~typeVarsGen, ~noFunctionReturnDependencies?, ~typeEnv)

  if Debug.dependencies.contents {
    translation.dependencies->List.iter(dep => Log_.item("Dependency: %s\n", dep->depToString))
  }
  translation
}
