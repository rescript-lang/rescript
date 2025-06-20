open GenTypeCommon

type declarationKind =
  | RecordDeclarationFromTypes(list<Types.label_declaration>)
  | GeneralDeclaration(option<Typedtree.core_type>)
  | GeneralDeclarationFromTypes(
      option<Types.type_expr>,
    ) /* As the above, but from Types not Typedtree */
  | VariantDeclarationFromTypes(list<Types.constructor_declaration>)
  | NoDeclaration

let createExportTypeFromTypeDeclaration = (
  ~annotation,
  ~nameAs,
  ~opaque,
  ~type_,
  ~typeEnv,
  typeName,
  ~typeVars,
): CodeItem.exportFromTypeDeclaration => {
  let resolvedTypeName = typeName->TypeEnv.addModulePath(~typeEnv)
  {
    exportType: {
      nameAs: nameAs,
      opaque: opaque,
      type_: type_,
      typeVars: typeVars,
      resolvedTypeName: resolvedTypeName,
    },
    annotation: annotation,
  }
}

let createCase = ((label, attributes)) =>
  switch attributes->Annotation.getAttributePayload(Annotation.tagIsGenTypeAs) {
  | Some(BoolPayload(b)) => {label: label, labelJS: BoolLabel(b)}
  | Some(FloatPayload(s)) => {label: label, labelJS: FloatLabel(s)}
  | Some(IntPayload(i)) => {label: label, labelJS: IntLabel(i)}
  | Some(StringPayload(asLabel)) => {label: label, labelJS: StringLabel(asLabel)}
  | _ => {label: label, labelJS: StringLabel(label)}
  }

// Rename record fields.
// If @genType.as is used, perform renaming conversion.
// If @as is used (with records-as-objects active), no conversion is required.
let renameRecordField = (~config, ~attributes, ~nameRE) =>
  switch attributes->Annotation.getGenTypeAsRenaming {
  | Some(nameJS) => (nameJS, nameRE)
  | None =>
    if config.recordsAsObjects {
      switch attributes->Annotation.getAsRenaming {
      | Some(name) => (name, name)
      | None => (nameRE, nameRE)
      }
    } else {
      (nameRE, nameRE)
    }
  }

let traslateDeclarationKind = (
  ~config,
  ~outputFileRelative,
  ~resolver,
  ~typeAttributes,
  ~typeEnv,
  ~typeName,
  ~typeVars,
  declarationKind,
): list<CodeItem.typeDeclaration> => {
  let annotation = typeAttributes->Annotation.fromAttributes
  let opaque =
    annotation == Annotation.GenTypeOpaque ? Some(true) : None /* None means don't know */
  let (importStringOpt, nameAs) = typeAttributes->Annotation.getAttributeImportRenaming
  let unboxedAnnotation = typeAttributes->Annotation.hasAttribute(Annotation.tagIsUnboxed)

  let returnTypeDeclaration = (typeDeclaration: CodeItem.typeDeclaration) =>
    opaque == Some(true) ? list{{...typeDeclaration, importTypes: list{}}} : list{typeDeclaration}

  let handleGeneralDeclaration = (translation: TranslateTypeExprFromTypes.translation) => {
    let exportFromTypeDeclaration =
      typeName->createExportTypeFromTypeDeclaration(
        ~annotation,
        ~nameAs,
        ~opaque,
        ~type_=translation.type_,
        ~typeEnv,
        ~typeVars,
      )
    let importTypes =
      translation.dependencies->Translation.translateDependencies(
        ~config,
        ~outputFileRelative,
        ~resolver,
      )
    {CodeItem.importTypes: importTypes, exportFromTypeDeclaration: exportFromTypeDeclaration}
  }

  let translateLabelDeclarations = labelDeclarations => {
    let fieldTranslations = labelDeclarations->List.map(({
      Types.ld_id: ld_id,
      ld_mutable,
      ld_type,
      ld_attributes,
    }) => {
      let (nameJS, nameRE) = renameRecordField(
        ~attributes=ld_attributes,
        ~config,
        ~nameRE=ld_id->Ident.name,
      )
      let mutability = ld_mutable == Mutable ? Mutable : Immutable
      (
        nameJS,
        nameRE,
        mutability,
        ld_type->TranslateTypeExprFromTypes.translateTypeExprFromTypes(~config, ~typeEnv),
      )
    })

    let dependencies =
      fieldTranslations
      ->List.map(((_, _, _, {TranslateTypeExprFromTypes.dependencies: dependencies})) =>
        dependencies
      )
      ->List.concat

    let fields = fieldTranslations->List.map(((
      nameJS,
      nameRE,
      mutable_,
      {TranslateTypeExprFromTypes.type_: type_},
    )) => {
      let (optional, type1) = switch type_ {
      | Option(type1) => (Optional, type1)
      | _ => (Mandatory, type_)
      }
      {mutable_: mutable_, nameJS: nameJS, nameRE: nameRE, optional: optional, type_: type1}
    })
    let type_ = switch fields {
    | list{field} if unboxedAnnotation && config.useUnboxedAnnotations => field.type_
    | _ => config.recordsAsObjects ? Object(Closed, fields) : Record(fields)
    }
    {TranslateTypeExprFromTypes.dependencies: dependencies, type_: type_}
  }

  switch (declarationKind, importStringOpt) {
  | (_, Some(importString)) =>
    /* import type */
    let typeName_ = typeName
    let nameWithModulePath = typeName_->TypeEnv.addModulePath(~typeEnv)->ResolvedName.toString
    let (typeName, asTypeName) = switch nameAs {
    | Some(asString) => (asString, "$$" ++ nameWithModulePath)
    | None => (nameWithModulePath, "$$" ++ nameWithModulePath)
    }
    let importTypes = list{
      {
        CodeItem.typeName: typeName,
        asTypeName: Some(asTypeName),
        importPath: importString->ImportPath.fromStringUnsafe,
      },
    }
    let exportFromTypeDeclaration =
      /* Make the imported type usable from other modules by exporting it too. */
      typeName_->createExportTypeFromTypeDeclaration(
        ~annotation=GenType,
        ~nameAs=None,
        ~opaque=Some(false),
        ~type_=asTypeName->ident(~typeArgs=typeVars->List.map(s => TypeVar(s))),
        ~typeEnv,
        ~typeVars,
      )
    list{{CodeItem.importTypes: importTypes, exportFromTypeDeclaration: exportFromTypeDeclaration}}

  | (GeneralDeclarationFromTypes(None) | GeneralDeclaration(None), None) =>
    {
      CodeItem.importTypes: list{},
      exportFromTypeDeclaration: typeName->createExportTypeFromTypeDeclaration(
        ~annotation,
        ~nameAs,
        ~opaque=Some(true),
        ~type_=mixedOrUnknown(~config),
        ~typeEnv,
        ~typeVars,
      ),
    }->returnTypeDeclaration

  | (GeneralDeclarationFromTypes(Some(typeExpr)), None) =>
    let translation =
      typeExpr->TranslateTypeExprFromTypes.translateTypeExprFromTypes(~config, ~typeEnv)
    translation->handleGeneralDeclaration->returnTypeDeclaration

  | (GeneralDeclaration(Some(coreType)), None) =>
    let translation = coreType->TranslateCoreType.translateCoreType(~config, ~typeEnv)

    let type_ = switch (coreType, translation.type_) {
    | ({ctyp_desc: Ttyp_variant(rowFields, _, _)}, Variant(variant)) =>
      let rowFieldsVariants = rowFields->TranslateCoreType.processVariant
      let noPayloads = rowFieldsVariants.noPayloads->List.map(createCase)
      let payloads = if (
        variant.payloads->List.length == (rowFieldsVariants.payloads->List.length)
      ) {
        List.combine(variant.payloads, rowFieldsVariants.payloads)->List.map(((
          (_case, i, type_),
          (label, attributes, _),
        )) => {
          let case = (label, attributes)->createCase
          (case, i, type_)
        })
      } else {
        variant.payloads
      }

      createVariant(~noPayloads, ~payloads, ~polymorphic=true)
    | _ => translation.type_
    }
    {...translation, type_: type_}->handleGeneralDeclaration->returnTypeDeclaration

  | (RecordDeclarationFromTypes(labelDeclarations), None) =>
    let {TranslateTypeExprFromTypes.dependencies: dependencies, type_} =
      labelDeclarations->translateLabelDeclarations

    let importTypes =
      dependencies->Translation.translateDependencies(~config, ~outputFileRelative, ~resolver)

    {
      CodeItem.importTypes: importTypes,
      exportFromTypeDeclaration: typeName->createExportTypeFromTypeDeclaration(
        ~annotation,
        ~nameAs,
        ~opaque,
        ~type_,
        ~typeEnv,
        ~typeVars,
      ),
    }->returnTypeDeclaration

  | (VariantDeclarationFromTypes(constructorDeclarations), None) =>
    let recordGen = Runtime.recordGen()
    let variants = constructorDeclarations->List.map(constructorDeclaration => {
      let constructorArgs = constructorDeclaration.Types.cd_args
      let name = constructorDeclaration.Types.cd_id->Ident.name
      let attributes = constructorDeclaration.Types.cd_attributes
      let argsTranslation = switch constructorArgs {
      | Cstr_tuple(typeExprs) =>
        typeExprs->TranslateTypeExprFromTypes.translateTypeExprsFromTypes(~config, ~typeEnv)
      | Cstr_record(labelDeclarations) => list{labelDeclarations->translateLabelDeclarations}
      }
      let inlineRecord = switch constructorArgs {
      | Cstr_tuple(_) => false
      | Cstr_record(_) => true
      }

      let argTypes =
        argsTranslation->List.map(({TranslateTypeExprFromTypes.type_: type_}) => type_)
      let importTypes =
        argsTranslation
        ->List.map(({TranslateTypeExprFromTypes.dependencies: dependencies}) => dependencies)
        ->List.concat
        ->Translation.translateDependencies(~config, ~outputFileRelative, ~resolver)

      let recordValue =
        recordGen->Runtime.newRecordValue(~unboxed=constructorArgs == Cstr_tuple(list{}))
      (
        name,
        attributes,
        argTypes,
        importTypes,
        inlineRecord,
        recordValue->Runtime.recordValueToString,
      )
    })
    let (variantsNoPayload, variantsWithPayload) =
      variants->List.partition(((_, _, argTypes, _, _, _)) => argTypes == list{})

    let noPayloads =
      variantsNoPayload->List.map(((
        name,
        attributes,
        _argTypes,
        _importTypes,
        _inlineRecord,
        recordValue,
      )) => {...(name, attributes)->createCase, label: recordValue})
    let payloads = variantsWithPayload->List.map(((
      name,
      attributes,
      argTypes,
      _importTypes,
      inlineRecord,
      recordValue,
    )) => {
      let type_ = switch argTypes {
      | list{type_} => type_
      | _ => Tuple(argTypes)
      }
      let numArgs = inlineRecord ? 0 : argTypes->List.length
      ({...(name, attributes)->createCase, label: recordValue}, numArgs, type_)
    })

    let variantTyp = switch (noPayloads, payloads) {
    | (list{}, list{(_c, _, type_)}) if unboxedAnnotation && config.useUnboxedAnnotations => type_
    | _ => createVariant(~noPayloads, ~payloads, ~polymorphic=false)
    }
    let resolvedTypeName = typeName->TypeEnv.addModulePath(~typeEnv)

    let exportFromTypeDeclaration = {
      CodeItem.exportType: {
        nameAs: nameAs,
        opaque: opaque,
        type_: variantTyp,
        typeVars: typeVars,
        resolvedTypeName: resolvedTypeName,
      },
      annotation: annotation,
    }
    let importTypes =
      variants->List.map(((_, _, _, importTypes, _, _)) => importTypes)->List.concat

    {
      CodeItem.exportFromTypeDeclaration: exportFromTypeDeclaration,
      importTypes: importTypes,
    }->returnTypeDeclaration

  | (NoDeclaration, None) => list{}
  }
}

let hasSomeGADTLeaf = constructorDeclarations =>
  List.exists(declaration => declaration.Types.cd_res !== None, constructorDeclarations)

let translateTypeDeclaration = (
  ~config,
  ~outputFileRelative,
  ~resolver,
  ~typeEnv,
  {typ_attributes, typ_id, typ_manifest, typ_params, typ_type}: Typedtree.type_declaration,
): list<CodeItem.typeDeclaration> => {
  if Debug.translation.contents {
    Log_.item("Translate Type Declaration %s\n", typ_id->Ident.name)
  }
  typeEnv->TypeEnv.newType(~name=typ_id->Ident.name)

  let typeName = Ident.name(typ_id)

  let typeVars = typ_params->List.map(((coreType, _)) => coreType)->TypeVars.extractFromCoreType

  let declarationKind = switch typ_type.type_kind {
  | Type_record(labelDeclarations, _) => RecordDeclarationFromTypes(labelDeclarations)

  | Type_variant(constructorDeclarations) => VariantDeclarationFromTypes(constructorDeclarations)

  | Type_abstract => GeneralDeclaration(typ_manifest)

  | _ => NoDeclaration
  }

  declarationKind->traslateDeclarationKind(
    ~config,
    ~outputFileRelative,
    ~resolver,
    ~typeAttributes=typ_attributes,
    ~typeEnv,
    ~typeName,
    ~typeVars,
  )
}

let translateTypeDeclarations = (
  ~config,
  ~outputFileRelative,
  ~resolver,
  ~typeEnv,
  typeDeclarations: list<Typedtree.type_declaration>,
): list<CodeItem.typeDeclaration> =>
  typeDeclarations
  ->List.map(translateTypeDeclaration(~config, ~outputFileRelative, ~resolver, ~typeEnv))
  ->List.concat
