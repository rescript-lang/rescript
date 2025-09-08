open GenTypeCommon

type rec t =
  | ArrayC(t)
  | CircularC(string, t)
  | FunctionC(functionC)
  | IdentC
  | NullableC(t)
  | ObjectC(fieldsC)
  | OptionC(t)
  | PromiseC(t)
  | RecordC(fieldsC)
  | TupleC(list<t>)
  | VariantC(variantC)
and groupedArgConverter =
  | ArgConverter(t)
  | GroupConverter(list<(string, optional, t)>)
and fieldC = {
  lblJS: string,
  lblRE: string,
  c: t,
}
and fieldsC = list<fieldC>
and functionC = {
  argConverters: list<groupedArgConverter>,
  componentName: option<string>,
  isHook: bool,
  retConverter: t,
  typeVars: list<string>,
  uncurried: bool,
}
and variantC = {
  hash: int,
  noPayloads: list<case>,
  withPayload: list<(case, int, t)>,
  polymorphic: bool,
  unboxed: bool,
}

let rec toString = converter =>
  switch converter {
  | ArrayC(c) => "array(" ++ (toString(c) ++ ")")

  | CircularC(s, c) => "circular(" ++ (s ++ (" " ++ (toString(c) ++ ")")))

  | FunctionC({argConverters, retConverter, uncurried}) =>
    "fn" ++
    ((uncurried ? "Uncurried" : "") ++
    ("(" ++
    ((argConverters
    ->List.map(groupedArgConverter =>
      switch groupedArgConverter {
      | ArgConverter(conv) => "(" ++ ("_" ++ (":" ++ (toString(conv) ++ ")")))
      | GroupConverter(groupConverters) =>
        "{|" ++
        ((groupConverters
        ->List.map(((s, optional, argConverter)) =>
          s ++ ((optional == Optional ? "?" : "") ++ (":" ++ toString(argConverter)))
        )
        ->String.concat(", ")) ++
        "|}")
      }
    )
    ->String.concat(", ")) ++
    (" -> " ++ (toString(retConverter) ++ ")")))))

  | IdentC => "id"

  | NullableC(c) => "nullable(" ++ (toString(c) ++ ")")

  | ObjectC(fieldsC)
  | RecordC(fieldsC) =>
    let dot = switch converter {
    | ObjectC(_) => ". "
    | _ => ""
    }
    "{" ++
    (dot ++
    ((fieldsC
    ->List.map(({lblJS, lblRE, c}) =>
      (lblJS == lblRE ? lblJS : "(" ++ (lblJS ++ ("/" ++ (lblRE ++ ")")))) ++
      (":" ++
      (c->toString))
    )
    ->String.concat(", ")) ++ "}"))

  | OptionC(c) => "option(" ++ (toString(c) ++ ")")
  | PromiseC(c) => "promise(" ++ (toString(c) ++ ")")
  | TupleC(innerTypesC) =>
    "[" ++ ((innerTypesC->List.map(toString)->String.concat(", ")) ++ "]")

  | VariantC({noPayloads, withPayload}) =>
    "variant(" ++
    ((\"@"(
      noPayloads->List.map(case => case.labelJS->labelJSToString),
      withPayload->List.map(((case, numArgs, c)) =>
        (case.labelJS->labelJSToString) ++
          (":" ++
          (string_of_int(numArgs) ++ (":" ++ (c->toString))))
      ),
    )->String.concat(", ")) ++
    ")")
  }

let typeGetConverterNormalized = (~config, ~inline, ~lookupId, ~typeNameIsInterface, type0) => {
  let circular = ref("")
  let expandOneLevel = type_ =>
    switch type_ {
    | Ident({builtin: false, name}) =>
      switch name->lookupId {
      | (t: CodeItem.exportTypeItem) => t.type_
      | exception Not_found => type_
      }
    | _ => type_
    }
  let rec visit = (~visited: StringSet.t, type_) => {
    let normalized_ = type_
    switch type_ {
    | Array(t, mutable_) =>
      let (tConverter, tNormalized) = t->visit(~visited)
      (ArrayC(tConverter), Array(tNormalized, mutable_))

    | Function({argTypes, componentName, retType, typeVars, uncurried} as function_) =>
      let argConverted = argTypes->List.map(argTypeToGroupedArgConverter(~visited))
      let argConverters = argConverted->List.map(fst)
      let (retConverter, retNormalized) = retType->visit(~visited)
      let isHook = switch argTypes {
      | list{{aType: Object(_)}} => retType->EmitType.isTypeReactElement(~config)
      | _ => false
      }
      (
        FunctionC({
          argConverters: argConverters,
          componentName: componentName,
          isHook: isHook,
          retConverter: retConverter,
          typeVars: typeVars,
          uncurried: uncurried,
        }),
        Function({
          ...function_,
          argTypes: argConverted->List.map(snd),
          retType: retNormalized,
        }),
      )

    | GroupOfLabeledArgs(_) => /* This case should only fire from withing a function */
      (IdentC, normalized_)

    | Ident({builtin: true}) => (IdentC, normalized_)

    | Ident({builtin: false, name, typeArgs}) =>
      if visited->StringSet.mem(name) {
        circular := name
        (IdentC, normalized_)
      } else {
        let visited = visited->StringSet.add(name)
        switch name->lookupId {
        | {annotation: GenTypeOpaque} => (IdentC, normalized_)
        | {annotation: NoGenType} => (IdentC, normalized_)
        | {typeVars, type_} =>
          let pairs = try List.combine(typeVars, typeArgs) catch {
          | Invalid_argument(_) => list{}
          }

          let f = typeVar =>
            switch pairs->List.find(((typeVar1, _)) => typeVar == typeVar1) {
            | (_, typeArgument) => Some(typeArgument)
            | exception Not_found => None
            }
          let (converter, inlined) = type_->TypeVars.substitute(~f)->visit(~visited)
          (converter, inline ? inlined : normalized_)
        | exception Not_found =>
          if inline {
            let typeArgs = typeArgs->List.map(t => t->visit(~visited)->snd)
            (IdentC, Ident({builtin: false, name: name, typeArgs: typeArgs}))
          } else {
            (IdentC, normalized_)
          }
        }
      }

    | Null(t) =>
      let (tConverter, tNormalized) = t->visit(~visited)
      (NullableC(tConverter), Null(tNormalized))

    | Nullable(t) =>
      let (tConverter, tNormalized) = t->visit(~visited)
      (NullableC(tConverter), Nullable(tNormalized))

    /* | Object(closedFlag, fields) => */
    /* let fieldsConverted = */
    /* fields */
    /*->List.map(({type_} as field) => */
    /* (field, type_->visit(~visited)) */
    /* ); */
    /* ( */
    /* ObjectC( */
    /* fieldsConverted */
    /*->List.map((({nameJS, nameRE, optional}, (converter, _))) => */
    /* { */
    /* lblJS: nameJS, */
    /* lblRE: nameRE, */
    /* c: optional == Mandatory ? converter : OptionC(converter), */
    /* } */
    /* ), */
    /* ), */
    /* Object( */
    /* closedFlag, */
    /* fieldsConverted */
    /*->List.map(((field, (_, tNormalized))) => */
    /* {...field, type_: tNormalized} */
    /* ), */
    /* ), */
    /* ); */

    | Option(t) =>
      let (tConverter, tNormalized) = t->visit(~visited)
      (OptionC(tConverter), Option(tNormalized))

    | Promise(t) =>
      let (tConverter, tNormalized) = t->visit(~visited)
      (PromiseC(tConverter), Promise(tNormalized))

    | Record(fields) =>
      let fieldsConverted =
        fields->List.map(({type_} as field) => (field, type_->visit(~visited)))
    /* ( */
    /* RecordC( */
    /* fieldsConverted */
    /*->List.map((({nameJS, nameRE, optional}, (converter, _))) => */
    /* { */
    /* lblJS: nameJS, */
    /* lblRE: nameRE, */
    /* c: optional == Mandatory ? converter : OptionC(converter), */
    /* } */
    /* ), */
    /* ), */
    /* Record( */
    /* fieldsConverted */
    /*->List.map(((field, (_, tNormalized))) => */
    /* {...field, type_: tNormalized} */
    /* ), */
    /* ), */
    /* ); */

    | Tuple(innerTypes) =>
      let (innerConversions, normalizedList) = innerTypes->List.map(visit(~visited))->List.split
      (TupleC(innerConversions), Tuple(normalizedList))

    | TypeVar(_) => (IdentC, normalized_)

    | Variant(variant) =>
      let (withPayload, normalized, unboxed) = switch variant.payloads->List.map(((
        case,
        numArgs,
        t,
      )) => (case, numArgs, t->visit(~visited))) {
      | list{} => (list{}, normalized_, variant.unboxed)
      | list{(case, numArgs, (converter, tNormalized))} =>
        let unboxed = tNormalized->expandOneLevel->typeIsObject
        let normalized = Variant({
          ...variant,
          payloads: list{(case, numArgs, tNormalized)},
          unboxed: unboxed ? true : variant.unboxed,
        })
        (list{(case, numArgs, converter)}, normalized, unboxed)
      | list{_, _, ..._} as withPayloadConverted =>
        let withPayloadNormalized =
          withPayloadConverted->List.map(((case, numArgs, (_, tNormalized))) => (
            case,
            numArgs,
            tNormalized,
          ))
        let normalized = Variant({...variant, payloads: withPayloadNormalized})
        (
          withPayloadConverted->List.map(((case, numArgs, (converter, _))) => (
            case,
            numArgs,
            converter,
          )),
          normalized,
          variant.unboxed,
        )
      }
      let converter = VariantC({
        hash: variant.hash,
        noPayloads: variant.noPayloads,
        withPayload: withPayload,
        polymorphic: variant.polymorphic,
        unboxed: unboxed,
      })
      (converter, normalized)
    }
  }
  and argTypeToGroupedArgConverter = (~visited, {aName, aType}) =>
    switch aType {
    | GroupOfLabeledArgs(fields) =>
      let fieldsConverted =
        fields->List.map(({type_} as field) => (field, type_->visit(~visited)))
      let tNormalized = GroupOfLabeledArgs(
        fieldsConverted->List.map(((field, (_, t))) => {...field, type_: t}),
      )
      let converter = GroupConverter(
        fieldsConverted->List.map((({nameJS, optional}, (converter, _))) => (
          nameJS,
          optional,
          converter,
        )),
      )
      (converter, {aName: aName, aType: tNormalized})
    | _ =>
      let (converter, tNormalized) = aType->visit(~visited)
      let converter = ArgConverter(converter)
      (converter, {aName: aName, aType: tNormalized})
    }

  let (converter, normalized) = type0->visit(~visited=StringSet.empty)
  let finalConverter = circular.contents != "" ? CircularC(circular.contents, converter) : converter
  if Debug.converter.contents {
    Log_.item(
      "Converter type0:%s converter:%s\n",
      type0->EmitType.typeToString(~config, ~typeNameIsInterface),
      finalConverter->toString,
    )
  }
  (finalConverter, normalized)
}

let typeGetConverter = (~config, ~lookupId, ~typeNameIsInterface, type_) =>
  type_
  ->typeGetConverterNormalized(~config, ~inline=false, ~lookupId, ~typeNameIsInterface)
  ->fst

let typeGetNormalized = (~config, ~inline, ~lookupId, ~typeNameIsInterface, type_) =>
  type_->typeGetConverterNormalized(~config, ~inline, ~lookupId, ~typeNameIsInterface)->snd

let rec converterIsIdentity = (~toJS, converter) =>
  switch converter {
  | ArrayC(c) => c->converterIsIdentity(~toJS)

  | CircularC(_, c) => c->converterIsIdentity(~toJS)

  | FunctionC({argConverters, retConverter, uncurried}) =>
    retConverter->converterIsIdentity(~toJS) &&
      ((!toJS || (uncurried || argConverters->List.length <= 1)) &&
      argConverters->List.for_all(groupedArgConverter =>
        switch groupedArgConverter {
        | ArgConverter(argConverter) => argConverter->converterIsIdentity(~toJS=!toJS)
        | GroupConverter(_) => false
        }
      ))

  | IdentC => true

  | NullableC(c) => c->converterIsIdentity(~toJS)

  | ObjectC(fieldsC) =>
    fieldsC->List.for_all(({lblJS, lblRE, c}) =>
      lblJS == lblRE &&
        switch c {
        | OptionC(c1) => c1->converterIsIdentity(~toJS)
        | _ => c->converterIsIdentity(~toJS)
        }
    )

  | OptionC(c) =>
    if toJS {
      c->converterIsIdentity(~toJS)
    } else {
      false
    }

  | PromiseC(c) => c->converterIsIdentity(~toJS)

  | RecordC(_) => false

  | TupleC(innerTypesC) => innerTypesC->List.for_all(converterIsIdentity(~toJS))

  | VariantC(_) => false
  }

let rec apply = (~config, ~converter, ~indent, ~nameGen, ~toJS, ~variantTables, value) =>
  switch converter {
  | _ if converter->converterIsIdentity(~toJS) => value

  | ArrayC(c) =>
    let x = "ArrayItem"->EmitText.name(~nameGen)
    value ++
    (".map(function _element(" ++
    ((x->EmitType.ofTypeAny(~config)) ++
      (") { return " ++
      ((x->apply(~config, ~converter=c, ~indent, ~nameGen, ~toJS, ~variantTables)) ++ "})"))))

  | CircularC(s, c) =>
    value
    ->EmitText.addComment(
      ~comment="WARNING: circular type " ++ (s ++ ". Only shallow converter applied."),
    )
    ->apply(~config, ~converter=c, ~indent, ~nameGen, ~toJS, ~variantTables)

  | FunctionC({argConverters, componentName, isHook, retConverter, typeVars, uncurried}) =>
    let resultName = EmitText.resultName(~nameGen)
    let indent1 = indent->Indent.more
    let indent2 = indent1->Indent.more

    let mkReturn = x =>
      "const " ++
      (resultName ++
      (" = " ++
      (x ++
      (";" ++
      (Indent.break(~indent=indent1) ++
      ("return " ++
      (resultName->apply(
        ~config,
        ~converter=retConverter,
        ~indent=indent2,
        ~nameGen,
        ~toJS,
        ~variantTables,
      ))))))))

    let convertArg = (i, groupedArgConverter) =>
      switch groupedArgConverter {
      | ArgConverter(argConverter) =>
        let varName = i + 1->EmitText.argi(~nameGen)
        let notToJS = !toJS
        (
          list{varName},
          list{
            varName->apply(
              ~config,
              ~converter=argConverter,
              ~indent=indent2,
              ~nameGen,
              ~toJS=notToJS,
              ~variantTables,
            ),
          },
        )
      | GroupConverter(groupConverters) =>
        let notToJS = !toJS
        if toJS {
          let varName = i + 1->EmitText.argi(~nameGen)
          (
            list{varName},
            groupConverters->List.map(((label, optional, argConverter)) =>
              varName
              ->EmitText.fieldAccess(~label)
              ->apply(
                ~config,
                ~converter=optional == Optional && !(argConverter->converterIsIdentity(~toJS))
                  ? OptionC(argConverter)
                  : argConverter,
                ~indent=indent2,
                ~nameGen,
                ~toJS=notToJS,
                ~variantTables,
              )
            ),
          )
        } else {
          let varNames =
            groupConverters->List.map(((s, _optional, _argConverter)) =>
              s->EmitText.arg(~nameGen)
            )

          let varNamesArr = varNames->Array.of_list
          let fieldValues =
            groupConverters
            ->List.mapi((i, (s, _optional, argConverter)) =>
              s ++
              (":" ++
              (varNamesArr[i]->apply(
                ~config,
                ~converter=argConverter,
                ~indent=indent2,
                ~nameGen,
                ~toJS=notToJS,
                ~variantTables,
              )))
            )
            ->String.concat(", ")
          (varNames, list{"{" ++ (fieldValues ++ "}")})
        }
      }

    let mkBody = bodyArgs => {
      let useCurry = !uncurried && (toJS && List.length(bodyArgs) > 1)
      config.emitImportCurry = config.emitImportCurry || useCurry
      let functionName = isHook ? "React.createElement" : value
      if isHook {
        config.emitImportReact = true
      }
      let (declareProps, args) = switch bodyArgs {
      | list{props} if isHook =>
        let propsName = "$props"->EmitText.name(~nameGen)
        (
          Indent.break(~indent=indent1) ++ ("const " ++ (propsName ++ (" = " ++ (props ++ ";")))),
          list{value, propsName},
        )

      | _ => ("", bodyArgs)
      }

      declareProps ++
      (Indent.break(~indent=indent1) ++
      (functionName->EmitText.funCall(~args, ~useCurry)->mkReturn))
    }

    let convertedArgs = argConverters->List.mapi(convertArg)
    let args = convertedArgs->List.map(fst)->List.concat
    let funParams = args->List.map(v => v->EmitType.ofTypeAny(~config))
    let bodyArgs = convertedArgs->List.map(snd)->List.concat
    EmitText.funDef(
      ~bodyArgs,
      ~functionName=componentName,
      ~funParams,
      ~indent,
      ~mkBody,
      ~typeVars=switch config.language {
      | Flow
      | TypeScript => typeVars
      | Untyped => list{}
      },
    )

  | IdentC => value

  | NullableC(c) =>
    EmitText.parens(list{
      value ++
      (" == null ? " ++
      (value ++
      (" : " ++
      (value->apply(~config, ~converter=c, ~indent, ~nameGen, ~toJS, ~variantTables))))),
    })

  | ObjectC(fieldsC) =>
    let simplifyFieldConverted = fieldConverter =>
      switch fieldConverter {
      | OptionC(converter1) if converter1->converterIsIdentity(~toJS) => IdentC
      | _ => fieldConverter
      }
    let fieldValues =
      fieldsC
      ->List.map(({lblJS, lblRE, c: fieldConverter}) =>
        (toJS ? lblJS : lblRE) ++
        (":" ++
        (value
        ->EmitText.fieldAccess(~label=toJS ? lblRE : lblJS)
        ->apply(
          ~config,
          ~converter=fieldConverter->simplifyFieldConverted,
          ~indent,
          ~nameGen,
          ~toJS,
          ~variantTables,
        )))
      )
      ->String.concat(", ")
    "{" ++ (fieldValues ++ "}")

  | OptionC(c) =>
    if toJS {
      EmitText.parens(list{
        value ++
        (" == null ? " ++
        (value ++
        (" : " ++
        (value->apply(~config, ~converter=c, ~indent, ~nameGen, ~toJS, ~variantTables))))),
      })
    } else {
      EmitText.parens(list{
        value ++
        (" == null ? undefined : " ++
        (value->apply(~config, ~converter=c, ~indent, ~nameGen, ~toJS, ~variantTables))),
      })
    }

  | PromiseC(c) =>
    let x = "$promise"->EmitText.name(~nameGen)
    value ++
    (".then(function _element(" ++
    ((x->EmitType.ofTypeAny(~config)) ++
      (") { return " ++
      ((x->apply(~config, ~converter=c, ~indent, ~nameGen, ~toJS, ~variantTables)) ++ "})"))))

  | RecordC(fieldsC) =>
    let simplifyFieldConverted = fieldConverter =>
      switch fieldConverter {
      | OptionC(converter1) if converter1->converterIsIdentity(~toJS) => IdentC
      | _ => fieldConverter
      }
    if toJS {
      let fieldValues =
        fieldsC
        ->List.mapi((index, {lblJS, c: fieldConverter}) =>
          lblJS ++
          (":" ++
          (value
          ->EmitText.arrayAccess(~index)
          ->apply(
            ~config,
            ~converter=fieldConverter->simplifyFieldConverted,
            ~indent,
            ~nameGen,
            ~toJS,
            ~variantTables,
          )))
        )
        ->String.concat(", ")
      fieldsC == list{} && config.language == Flow
        ? "Object.freeze({})"
        : "{" ++ (fieldValues ++ "}")
    } else {
      let fieldValues =
        fieldsC
        ->List.map(({lblJS, c: fieldConverter}) =>
          value
          ->EmitText.fieldAccess(~label=lblJS)
          ->apply(
            ~config,
            ~converter=fieldConverter->simplifyFieldConverted,
            ~indent,
            ~nameGen,
            ~toJS,
            ~variantTables,
          )
        )
        ->String.concat(", ")
      "[" ++ (fieldValues ++ "]")
    }
  | TupleC(innerTypesC) =>
    "[" ++
    ((innerTypesC
    ->List.mapi((index, c) =>
      value
      ->EmitText.arrayAccess(~index)
      ->apply(~config, ~converter=c, ~indent, ~nameGen, ~toJS, ~variantTables)
    )
    ->String.concat(", ")) ++
    "]")

  | VariantC({noPayloads: list{case}, withPayload: list{}, polymorphic}) =>
    toJS ? case.labelJS->labelJSToString : case.label->Runtime.emitVariantLabel(~polymorphic)

  | VariantC(variantC) =>
    if variantC.noPayloads != list{} {
      Hashtbl.replace(variantTables, (variantC.hash, toJS), variantC)
    }
    let convertToString =
      !toJS &&
      variantC.noPayloads->List.exists(({labelJS}) =>
        labelJS == BoolLabel(true) || labelJS == BoolLabel(false)
      )
        ? ".toString()"
        : ""
    let table = variantC.hash->variantTable(~toJS)
    let accessTable = v => table ++ EmitText.array(list{v ++ convertToString})
    switch variantC.withPayload {
    | list{} => value->accessTable

    | list{(case, numArgs, objConverter)} if variantC.unboxed =>
      let casesWithPayload = (~indent) =>
        if toJS {
          value
          ->Runtime.emitVariantGetPayload(~numArgs, ~polymorphic=variantC.polymorphic)
          ->apply(~config, ~converter=objConverter, ~indent, ~nameGen, ~toJS, ~variantTables)
        } else {
          value
          ->apply(~config, ~converter=objConverter, ~indent, ~nameGen, ~toJS, ~variantTables)
          ->Runtime.emitVariantWithPayload(
            ~config,
            ~label=case.label,
            ~numArgs,
            ~polymorphic=variantC.polymorphic,
          )
        }
      variantC.noPayloads == list{}
        ? casesWithPayload(~indent)
        : EmitText.ifThenElse(
            ~indent,
            (~indent as _) => value->EmitText.typeOfObject,
            casesWithPayload,
            (~indent as _) => value->accessTable,
          )

    | list{_, ..._} =>
      let convertCaseWithPayload = (~indent, ~numArgs, ~objConverter, case) =>
        value
        ->(toJS
          ? Runtime.emitVariantGetPayload(~numArgs, ~polymorphic=variantC.polymorphic)
          : Runtime.emitJSVariantGetPayload)
        ->apply(~config, ~converter=objConverter, ~indent, ~nameGen, ~toJS, ~variantTables)
        ->(
          toJS
            ? Runtime.emitJSVariantWithPayload(~label=case.labelJS->labelJSToString)
            : Runtime.emitVariantWithPayload(
                ~config,
                ~label=case.label,
                ~numArgs,
                ~polymorphic=variantC.polymorphic,
              )
        )
      let switchCases = (~indent) =>
        variantC.withPayload->List.map(((case, numArgs, objConverter)) => (
          toJS
            ? case.label->Runtime.emitVariantLabel(~polymorphic=variantC.polymorphic)
            : case.labelJS->labelJSToString,
          case->convertCaseWithPayload(~indent, ~numArgs, ~objConverter),
        ))
      let casesWithPayload = (~indent) =>
        value
        ->{
          open Runtime
          toJS ? emitVariantGetLabel(~polymorphic=variantC.polymorphic) : emitJSVariantGetLabel
        }
        ->EmitText.\"switch"(~indent, ~cases=switchCases(~indent))
      variantC.noPayloads == list{}
        ? casesWithPayload(~indent)
        : EmitText.ifThenElse(
            ~indent,
            (~indent as _) => value->EmitText.typeOfObject,
            casesWithPayload,
            (~indent as _) => value->accessTable,
          )
    }
  }

let toJS = (~config, ~converter, ~indent, ~nameGen, ~variantTables, value) =>
  value->apply(~config, ~converter, ~indent, ~nameGen, ~variantTables, ~toJS=true)

let toReason = (~config, ~converter, ~indent, ~nameGen, ~variantTables, value) =>
  value->apply(~config, ~converter, ~indent, ~nameGen, ~toJS=false, ~variantTables)
