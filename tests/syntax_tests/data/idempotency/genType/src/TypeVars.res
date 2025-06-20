open GenTypeCommon

let extractFromTypeExpr = typeParams => typeParams->List.fold_left((soFar, typeExpr) =>
    switch typeExpr {
    | {Types.desc: Tvar(Some(s))} =>
      let typeName = s
      list{typeName, ...soFar}
    | _ => assert(false)
    }
  , list{})->List.rev

let extractFromCoreType = typeParams => typeParams->List.fold_left((soFar, typeExpr) =>
    switch typeExpr.Typedtree.ctyp_desc {
    | Ttyp_var(s) =>
      let typeName = s
      list{typeName, ...soFar}
    | _ => soFar
    }
  , list{})->List.rev

let rec substitute = (~f, type0) =>
  switch type0 {
  | Array(t, arrayKind) => Array(t->substitute(~f), arrayKind)
  | Function(function_) =>
    Function({
      ...function_,
      argTypes: function_.argTypes->List.map(({aName, aType: t}) => {
        aName: aName,
        aType: t->substitute(~f),
      }),
      retType: function_.retType->substitute(~f),
    })
  | GroupOfLabeledArgs(fields) =>
    GroupOfLabeledArgs(
      fields->List.map(field => {...field, type_: field.type_->substitute(~f)}),
    )
  | Ident({typeArgs: list{}}) => type0
  | Ident({typeArgs} as ident) => Ident({...ident, typeArgs: typeArgs->List.map(substitute(~f))})
  | Null(type_) => Null(type_->substitute(~f))
  | Nullable(type_) => Nullable(type_->substitute(~f))
  | Object(closedFlag, fields) =>
    Object(
      closedFlag,
      fields->List.map(field => {...field, type_: field.type_->substitute(~f)}),
    )
  | Option(type_) => Option(type_->substitute(~f))
  | Promise(type_) => Promise(type_->substitute(~f))
  | Record(fields) =>
    Record(fields->List.map(field => {...field, type_: field.type_->substitute(~f)}))
  | Tuple(innerTypes) => Tuple(innerTypes->List.map(substitute(~f)))
  | TypeVar(s) =>
    switch f(s) {
    | None => type0
    | Some(type1) => type1
    }
  | Variant(variant) =>
    Variant({
      ...variant,
      payloads: variant.payloads->List.map(((case, numArgs, t)) => (
        case,
        numArgs,
        t->substitute(~f),
      )),
    })
  }

let rec free_ = (type0): StringSet.t =>
  switch type0 {
  | Array(t, _) => t->free_
  | Function({argTypes, retType, typeVars}) =>
    StringSet.diff(\"+++"(argTypes->freeOfList_, retType->free_), typeVars->StringSet.of_list)
  | GroupOfLabeledArgs(fields)
  | Object(_, fields)
  | Record(fields) =>
    fields->List.fold_left((s, {type_}) => StringSet.union(s, type_->free_), StringSet.empty)
  | Ident({typeArgs}) =>
    typeArgs->List.fold_left(
      (s, typeArg) => StringSet.union(s, typeArg->free_),
      StringSet.empty,
    )
  | Null(type_)
  | Nullable(type_) =>
    type_->free_
  | Option(type_)
  | Promise(type_) =>
    type_->free_
  | Tuple(innerTypes) =>
    innerTypes->List.fold_left(
      (s, typeArg) => StringSet.union(s, typeArg->free_),
      StringSet.empty,
    )
  | TypeVar(s) => s->StringSet.singleton
  | Variant({payloads}) =>
    payloads->List.fold_left((s, (_, _, t)) => StringSet.union(s, t->free_), StringSet.empty)
  }
and freeOfList_ = types =>
  types->List.fold_left((s, {aType}) => \"+++"(s, aType->free_), StringSet.empty)
and \"+++" = StringSet.union

let free = type_ => type_->free_->StringSet.elements
