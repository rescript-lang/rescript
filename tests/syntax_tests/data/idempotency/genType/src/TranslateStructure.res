open GenTypeCommon

let rec addAnnotationsToTypes_ = (~config, ~expr: Typedtree.expression, argTypes: list<argType>) =>
  switch (expr.exp_desc, expr.exp_type.desc, argTypes) {
  | (_, _, list{{aName, aType: GroupOfLabeledArgs(fields)}, ...nextTypes}) =>
    let (fields1, nextTypes1) = addAnnotationsToFields(~config, expr, fields, nextTypes)
    list{{aName: aName, aType: GroupOfLabeledArgs(fields1)}, ...nextTypes1}
  | (Texp_function({param, cases: list{{c_rhs}}}), _, list{{aType}, ...nextTypes}) =>
    let nextTypes1 = nextTypes->addAnnotationsToTypes_(~config, ~expr=c_rhs)
    let aName = Ident.name(param)
    list{{aName: aName, aType: aType}, ...nextTypes1}
  | (Texp_record({fields: [({lbl_name: "I"}, Overridden(_, exprRecord))]}), Tconstr(path, _, _), _)
    if switch path->TranslateTypeExprFromTypes.pathToList->List.rev {
    | list{"Js", "Fn", _arity} => true
    | _ => false
    } =>
    // let uncurried1: Js.Fn.arity1(_) = {I: x => x->string_of_int};
    addAnnotationsToTypes_(~config, ~expr=exprRecord, argTypes)
  | (Texp_apply({exp_desc: Texp_ident(path, _, _)}, list{(_, Some(expr1))}), _, _) =>
    switch path->TranslateTypeExprFromTypes.pathToList->List.rev {
    | list{"Js", "Internal", fn_mk}
      // Uncurried function definition uses Js.Internal.fn_mkX(...)
      if String.length(fn_mk) >= 5 && String.sub(fn_mk, 0, 5) == "fn_mk" =>
      argTypes->addAnnotationsToTypes_(~config, ~expr=expr1)
    | _ => argTypes
    }
  | _ => argTypes
  }
and addAnnotationsToTypes = (~config, ~expr: Typedtree.expression, argTypes: list<argType>) => {
  let argTypes = addAnnotationsToTypes_(~config, ~expr, argTypes)
  if argTypes->List.filter(({aName}) => aName == "param")->List.length > 1 {
    // Underscore "_" appears as "param", can occur more than once
    argTypes->List.mapi((i, {aName, aType}) => {
      aName: aName ++ ("_" ++ string_of_int(i)),
      aType: aType,
    })
  } else {
    argTypes
  }
}
and addAnnotationsToFields = (
  ~config,
  expr: Typedtree.expression,
  fields: fields,
  argTypes: list<argType>,
) =>
  switch (expr.exp_desc, fields, argTypes) {
  | (_, list{}, _) => (list{}, argTypes->addAnnotationsToTypes(~config, ~expr))
  | (Texp_function({cases: list{{c_rhs}}}), list{field, ...nextFields}, _) =>
    let (nextFields1, types1) = addAnnotationsToFields(~config, c_rhs, nextFields, argTypes)
    let (nameJS, nameRE) = TranslateTypeDeclarations.renameRecordField(
      ~attributes=expr.exp_attributes,
      ~config,
      ~nameRE=field.nameRE,
    )
    (list{{...field, nameJS: nameJS, nameRE: nameRE}, ...nextFields1}, types1)

  | _ => (fields, argTypes)
  }

/* Recover from expr the renaming annotations on named arguments. */
let addAnnotationsToFunctionType = (~config, expr: Typedtree.expression, type_: type_) =>
  switch type_ {
  | Function(function_) =>
    let argTypes = function_.argTypes->addAnnotationsToTypes(~config, ~expr)
    Function({...function_, argTypes: argTypes})
  | _ => type_
  }

let removeValueBindingDuplicates = structureItems => {
  let rec processBindings = (bindings: list<Typedtree.value_binding>, ~seen) =>
    switch bindings {
    | list{{vb_pat: {pat_desc: Tpat_var(id, _)}} as binding, ...otherBindings} =>
      let name = Ident.name(id)
      if seen.contents->StringSet.mem(name) {
        otherBindings->processBindings(~seen)
      } else {
        seen := seen.contents->StringSet.add(name)
        list{binding, ...otherBindings->processBindings(~seen)}
      }
    | list{binding, ...otherBindings} => list{binding, ...otherBindings->processBindings(~seen)}
    | list{} => list{}
    }
  let rec processItems = (items: list<Typedtree.structure_item>, ~acc, ~seen) =>
    switch items {
    | list{{Typedtree.str_desc: Tstr_value(loc, valueBindings)} as item, ...otherItems} =>
      let bindings = valueBindings->processBindings(~seen)
      let item = {...item, str_desc: Tstr_value(loc, bindings)}
      otherItems->processItems(~acc=list{item, ...acc}, ~seen)
    | list{item, ...otherItems} => otherItems->processItems(~acc=list{item, ...acc}, ~seen)
    | list{} => acc
    }
  structureItems->List.rev->processItems(~acc=list{}, ~seen=ref(StringSet.empty))
}

let translateValueBinding = (
  ~config,
  ~outputFileRelative,
  ~resolver,
  ~moduleItemGen,
  ~typeEnv,
  {Typedtree.vb_pat: vb_pat, vb_attributes, vb_expr},
): Translation.t =>
  switch vb_pat.pat_desc {
  | Tpat_var(id, _) =>
    let name = id->Ident.name
    if Debug.translation.contents {
      Log_.item("Translate Value Binding %s\n", name)
    }
    let moduleItem = moduleItemGen->Runtime.newModuleItem(~name)
    typeEnv->TypeEnv.updateModuleItem(~nameOpt=Some(name), ~moduleItem)

    if Annotation.fromAttributes(vb_attributes) == GenType {
      id
      ->Ident.name
      ->{
        open Translation
        Ident.name(id) == "make" ? translateComponent : translateValue
      }(
        ~attributes=vb_attributes,
        ~config,
        ~docString=Annotation.getDocString(vb_attributes),
        ~outputFileRelative,
        ~resolver,
        ~typeEnv,
        ~typeExpr=vb_expr.exp_type,
        ~addAnnotationsToFunction=addAnnotationsToFunctionType(~config, vb_expr),
      )
    } else {
      Translation.empty
    }

  | _ => Translation.empty
  }

let rec removeDuplicateValueBindings = (structureItems: list<Typedtree.structure_item>) =>
  switch structureItems {
  | list{{Typedtree.str_desc: Tstr_value(loc, valueBindings)} as structureItem, ...rest} =>
    let (boundInRest, filteredRest) = rest->removeDuplicateValueBindings
    let valueBindingsFiltered = valueBindings->List.filter(valueBinding =>
      switch valueBinding {
      | {Typedtree.vb_pat: {pat_desc: Tpat_var(id, _)}} =>
        !(boundInRest->StringSet.mem(id->Ident.name))
      | _ => true
      }
    )
    let bound = valueBindings->List.fold_left((bound, valueBinding: Typedtree.value_binding) =>
      switch valueBinding {
      | {vb_pat: {pat_desc: Tpat_var(id, _)}} => bound->StringSet.add(id->Ident.name)
      | _ => bound
      }
    , boundInRest)
    (
      bound,
      list{{...structureItem, str_desc: Tstr_value(loc, valueBindingsFiltered)}, ...filteredRest},
    )
  | list{structureItem, ...rest} =>
    let (boundInRest, filteredRest) = rest->removeDuplicateValueBindings
    (boundInRest, list{structureItem, ...filteredRest})

  | list{} => (StringSet.empty, list{})
  }

let rec translateModuleBinding = (
  ~config,
  ~outputFileRelative,
  ~resolver,
  ~typeEnv,
  ~moduleItemGen,
  {mb_id, mb_expr, mb_attributes}: Typedtree.module_binding,
): Translation.t => {
  let name = mb_id->Ident.name
  if Debug.translation.contents {
    Log_.item("Translate Module Binding %s\n", name)
  }
  let moduleItem = moduleItemGen->Runtime.newModuleItem(~name)
  typeEnv->TypeEnv.updateModuleItem(~moduleItem)
  let typeEnv = typeEnv->TypeEnv.newModule(~name)

  switch mb_expr.mod_desc {
  | Tmod_structure(structure) =>
    let isLetPrivate = mb_attributes->Annotation.hasAttribute(Annotation.tagIsInternLocal)
    if isLetPrivate {
      Translation.empty
    } else {
      structure
      ->translateStructure(~config, ~outputFileRelative, ~resolver, ~typeEnv)
      ->Translation.combine
    }
  | Tmod_apply(_) =>
    /* Only look at the resulting type of the module */
    switch mb_expr.mod_type {
    | Mty_signature(signature) =>
      signature
      ->TranslateSignatureFromTypes.translateSignatureFromTypes(
        ~config,
        ~outputFileRelative,
        ~resolver,
        ~typeEnv,
      )
      ->Translation.combine

    | Mty_ident(_) =>
      logNotImplemented("Mty_ident " ++ __LOC__)
      Translation.empty
    | Mty_functor(_) =>
      logNotImplemented("Mty_functor " ++ __LOC__)
      Translation.empty
    | Mty_alias(_) =>
      logNotImplemented("Mty_alias " ++ __LOC__)
      Translation.empty
    }

  | Tmod_unpack(_, moduleType) =>
    switch moduleType {
    | Mty_signature(signature) =>
      signature
      ->TranslateSignatureFromTypes.translateSignatureFromTypes(
        ~config,
        ~outputFileRelative,
        ~resolver,
        ~typeEnv,
      )
      ->Translation.combine

    | Mty_ident(path) =>
      switch typeEnv->TypeEnv.lookupModuleTypeSignature(~path) {
      | None => Translation.empty
      | Some((signature, _)) =>
        signature
        ->TranslateSignature.translateSignature(~config, ~outputFileRelative, ~resolver, ~typeEnv)
        ->Translation.combine
      }

    | Mty_functor(_) =>
      logNotImplemented("Mty_functor " ++ __LOC__)
      Translation.empty
    | Mty_alias(_) =>
      logNotImplemented("Mty_alias " ++ __LOC__)
      Translation.empty
    }

  | Tmod_ident(path, _) =>
    let dep = path->Dependencies.fromPath(~config, ~typeEnv)
    let internal = dep->Dependencies.isInternal
    typeEnv->TypeEnv.addModuleEquation(~dep, ~internal)
    Translation.empty

  | Tmod_functor(_) =>
    logNotImplemented("Tmod_functor " ++ __LOC__)
    Translation.empty
  | Tmod_constraint(_, Mty_ident(path), Tmodtype_explicit(_), Tcoerce_none) =>
    switch typeEnv->TypeEnv.lookupModuleTypeSignature(~path) {
    | None => Translation.empty
    | Some((signature, _)) =>
      signature
      ->TranslateSignature.translateSignature(~config, ~outputFileRelative, ~resolver, ~typeEnv)
      ->Translation.combine
    }

  | Tmod_constraint(_, Mty_signature(signature), Tmodtype_explicit(_), Tcoerce_none) =>
    signature
    ->TranslateSignatureFromTypes.translateSignatureFromTypes(
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
    )
    ->Translation.combine

  | Tmod_constraint(
      {mod_desc: Tmod_structure(structure)},
      _,
      Tmodtype_implicit,
      Tcoerce_structure(_),
    ) =>
    {
      ...structure,
      str_items: structure.str_items->removeDuplicateValueBindings->snd,
    }
    ->translateStructure(~config, ~outputFileRelative, ~resolver, ~typeEnv)
    ->Translation.combine

  | Tmod_constraint(_) =>
    logNotImplemented("Tmod_constraint " ++ __LOC__)
    Translation.empty
  }
}
and translateStructureItem = (
  ~config,
  ~outputFileRelative,
  ~resolver,
  ~moduleItemGen,
  ~typeEnv,
  structItem,
): Translation.t =>
  switch structItem {
  | {Typedtree.str_desc: Typedtree.Tstr_type(_, typeDeclarations)} => {
      importTypes: list{},
      codeItems: list{},
      typeDeclarations: typeDeclarations->TranslateTypeDeclarations.translateTypeDeclarations(
        ~config,
        ~outputFileRelative,
        ~resolver,
        ~typeEnv,
      ),
    }

  | {Typedtree.str_desc: Tstr_value(_loc, valueBindings)} =>
    valueBindings
    ->List.map(
      translateValueBinding(~config, ~outputFileRelative, ~resolver, ~moduleItemGen, ~typeEnv),
    )
    ->Translation.combine

  | {Typedtree.str_desc: Tstr_primitive(valueDescription)} =>
    /* external declaration */
    valueDescription->Translation.translatePrimitive(
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
    )

  | {Typedtree.str_desc: Tstr_module(moduleBinding)} =>
    moduleBinding->translateModuleBinding(
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
      ~moduleItemGen,
    )

  | {Typedtree.str_desc: Tstr_modtype(moduleTypeDeclaration)} =>
    moduleTypeDeclaration->TranslateSignature.translateModuleTypeDeclaration(
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
    )

  | {Typedtree.str_desc: Tstr_recmodule(moduleBindings)} =>
    moduleBindings
    ->List.map(
      translateModuleBinding(~config, ~outputFileRelative, ~resolver, ~typeEnv, ~moduleItemGen),
    )
    ->Translation.combine

  | {
      /* ReScript's encoding of @module: include with constraint. */
      Typedtree.str_desc: Tstr_include({
        incl_mod: {
          mod_desc: Tmod_constraint(
            {
              mod_desc: Tmod_structure({
                str_items: list{{str_desc: Tstr_primitive(_)} as structItem1},
              }),
            },
            _,
            _,
            _,
          ),
        },
        _,
      }),
      _,
    } =>
    structItem1->translateStructureItem(
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
      ~typeEnv,
    )

  | {Typedtree.str_desc: Tstr_include({incl_type: signature})} =>
    signature
    ->TranslateSignatureFromTypes.translateSignatureFromTypes(
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~typeEnv,
    )
    ->Translation.combine

  | {Typedtree.str_desc: Tstr_eval(_)} =>
    logNotImplemented("Tstr_eval " ++ __LOC__)
    Translation.empty
  | {Typedtree.str_desc: Tstr_typext(_)} =>
    logNotImplemented("Tstr_typext " ++ __LOC__)
    Translation.empty
  | {Typedtree.str_desc: Tstr_exception(_)} =>
    logNotImplemented("Tstr_exception " ++ __LOC__)
    Translation.empty
  | {Typedtree.str_desc: Tstr_open(_)} =>
    logNotImplemented("Tstr_open " ++ __LOC__)
    Translation.empty
  | {Typedtree.str_desc: Tstr_class(_)} =>
    logNotImplemented("Tstr_class " ++ __LOC__)
    Translation.empty
  | {Typedtree.str_desc: Tstr_class_type(_)} =>
    logNotImplemented("Tstr_class_type " ++ __LOC__)
    Translation.empty
  | {Typedtree.str_desc: Tstr_attribute(_)} =>
    logNotImplemented("Tstr_attribute " ++ __LOC__)
    Translation.empty
  }
and translateStructure = (~config, ~outputFileRelative, ~resolver, ~typeEnv, structure): list<
  Translation.t,
> => {
  if Debug.translation.contents {
    Log_.item("Translate Structure\n")
  }
  let moduleItemGen = Runtime.moduleItemGen()
  structure.Typedtree.str_items
  ->removeValueBindingDuplicates
  ->List.map(structItem =>
    structItem->translateStructureItem(
      ~config,
      ~outputFileRelative,
      ~resolver,
      ~moduleItemGen,
      ~typeEnv,
    )
  )
}
