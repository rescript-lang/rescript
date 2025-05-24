open GenTypeCommon

let flowExpectedError = "// $FlowExpectedError: Reason checked type sufficiently\n"

let flowTypeAny = flowExpectedError ++ "type $any = any;\n"

let fileHeader = (~config, ~sourceFile) => {
  let makeHeader = (~lines) => {
    let lines = switch config.fileHeader {
    | Some(header) => list{header, ...lines}
    | None => lines
    }
    switch lines {
    | list{line} => "/* " ++ (line ++ " */\n")
    | _ =>
      "/** \n" ++ ((lines->List.map(line => " * " ++ line)->String.concat("\n")) ++ "\n */\n")
    }
  }

  switch config.language {
  | Flow =>
    makeHeader(~lines=list{"@flow strict", "@" ++ ("generated from " ++ sourceFile), "@nolint"}) ++
    ("/* eslint-disable */\n" ++
    (config.emitFlowAny ? flowTypeAny : ""))
  | TypeScript =>
    makeHeader(
      ~lines=list{"TypeScript file generated from " ++ (sourceFile ++ " by genType.")},
    ) ++ "/* eslint-disable import/first */
\n"
  | Untyped =>
    makeHeader(
      ~lines=list{"Untyped file generated from " ++ (sourceFile ++ " by genType.")},
    ) ++ "/* eslint-disable */\n"
  }
}

let generatedFilesExtension = (~config) =>
  switch config.generatedFileExtension {
  | Some(s) => s
  | None => ".gen"
  }

let outputFileSuffix = (~config) =>
  switch config.language {
  | Flow
  | Untyped =>
    generatedFilesExtension(~config) ++ ".js"
  | TypeScript => generatedFilesExtension(~config) ++ ".tsx"
  }

let generatedModuleExtension = (~config) => generatedFilesExtension(~config)

let shimExtension = (~config) =>
  switch config.language {
  | Flow => ".shim.js"
  | TypeScript => ".shim.ts"
  | Untyped => ".shim.not.used"
  }

let interfaceName = (~config, name) => config.exportInterfaces ? "I" ++ name : name

let typeReactComponent = (~config, ~propsType) =>
  (config.language == Flow ? "React$ComponentType" : "React.ComponentType")->ident(
    ~builtin=true,
    ~typeArgs=list{propsType},
  )

let typeReactContext = (~config, ~type_) =>
  (config.language == Flow ? "React$Context" : "React.Context")->ident(
    ~builtin=true,
    ~typeArgs=list{type_},
  )

let typeReactElementFlow = ident(~builtin=true, "React$Node")

let typeReactElementTypeScript = ident(~builtin=true, "JSX.Element")

let typeReactChildTypeScript = ident(~builtin=true, "React.ReactNode")

let typeReactElement = (~config) =>
  config.language == Flow ? typeReactElementFlow : typeReactElementTypeScript

let typeReactChild = (~config) =>
  config.language == Flow ? typeReactElementFlow : typeReactChildTypeScript

let isTypeReactElement = (~config, type_) => type_ === typeReactElement(~config)

let typeReactDOMReDomRef = (~config) =>
  (config.language == Flow ? "React$Ref" : "React.Ref")->ident(
    ~builtin=true,
    ~typeArgs=list{mixedOrUnknown(~config)},
  )

let typeReactRef = (~type_) => Object(
  Closed,
  list{
    {
      mutable_: Mutable,
      nameJS: "current",
      nameRE: "current",
      optional: Mandatory,
      type_: Null(type_),
    },
  },
)

let componentExportName = (~config, ~fileName, ~moduleName) =>
  switch config.language {
  | Flow => fileName == moduleName ? "component" : moduleName->ModuleName.toString
  | _ => moduleName->ModuleName.toString
  }

let typeAny = (~config) =>
  ident(
    ~builtin=true,
    config.language == Flow
      ? {
          config.emitFlowAny = true
          "$any"
        }
      : "any",
  )

let rec renderType = (~config, ~indent=None, ~typeNameIsInterface, ~inFunType, type0) =>
  switch type0 {
  | Array(t, arrayKind) =>
    let typeIsSimple = switch t {
    | Ident(_)
    | TypeVar(_) => true
    | _ => false
    }

    if config.language == TypeScript && (typeIsSimple && arrayKind == Mutable) {
      (t->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType)) ++ "[]"
    } else {
      let arrayName =
        arrayKind == Mutable
          ? "Array"
          : config.language == Flow
          ? "$ReadOnlyArray"
          : "ReadonlyArray"
      arrayName ++
      ("<" ++
      ((t->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType)) ++ ">"))
    }

  | Function({argTypes: list{{aType: Object(closedFlag, fields)}}, retType, typeVars})
    if retType->isTypeReactElement(~config) =>
    let fields = fields->List.map(field => {
      ...field,
      type_: field.type_->TypeVars.substitute(~f=s =>
        if typeVars->List.mem(s) {
          Some(typeAny(~config))
        } else {
          None
        }
      ),
    })
    let componentType = typeReactComponent(~config, ~propsType=Object(closedFlag, fields))
    componentType->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType)

  | Function({argTypes, retType, typeVars}) =>
    renderFunType(~config, ~indent, ~inFunType, ~typeNameIsInterface, ~typeVars, argTypes, retType)

  | GroupOfLabeledArgs(fields)
  | Object(_, fields)
  | Record(fields) =>
    let indent1 = fields->Indent.heuristicFields(~indent)
    let config = switch type0 {
    | GroupOfLabeledArgs(_) => {...config, exportInterfaces: false}
    | _ => config
    }
    let closedFlag = switch type0 {
    | Object(closedFlag, _) => closedFlag
    | _ => Closed
    }
    fields->renderFields(~closedFlag, ~config, ~indent=indent1, ~inFunType, ~typeNameIsInterface)

  | Ident({builtin, name, typeArgs}) =>
    (
      !builtin && (config.exportInterfaces && name->typeNameIsInterface)
        ? name->interfaceName(~config)
        : name
    ) ++
    EmitText.genericsString(
      ~typeVars=typeArgs->List.map(
        renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType),
      ),
    )
  | Null(type_) =>
    "(null | " ++ ((type_->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType)) ++ ")")
  | Nullable(type_)
  | Option(type_) =>
    switch config.language {
    | Flow
    | Untyped =>
      let isComplex = switch type_ {
      | Variant(_) => true
      | _ => false
      }
      "?" ++
      ((isComplex ? " (" : "") ++
      ((type_->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType)) ++ (
          isComplex ? ")" : ""
        )))
    | TypeScript =>
      "(null | undefined | " ++
      ((type_->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType)) ++
      ")")
    }
  | Promise(type_) =>
    "Promise" ++
    ("<" ++
    ((type_->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType)) ++ ">"))
  | Tuple(innerTypes) =>
    "[" ++
    ((innerTypes
    ->List.map(renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType))
    ->String.concat(", ")) ++
    "]")
  | TypeVar(s) => s

  | Variant({noPayloads, payloads, unboxed}) =>
    let noPayloadsRendered = noPayloads->List.map(case => case.labelJS->labelJSToString)
    let field = (~name, value) => {
      mutable_: Mutable,
      nameJS: name,
      nameRE: name,
      optional: Mandatory,
      type_: TypeVar(value),
    }
    let fields = fields =>
      fields->renderFields(~closedFlag=Closed, ~config, ~indent, ~inFunType, ~typeNameIsInterface)
    let payloadsRendered = payloads->List.map(((case, _numArgs, type_)) => {
      let typeRendered = type_->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType)
      unboxed
        ? typeRendered
        : list{
            case.labelJS->labelJSToString->field(~name=Runtime.jsVariantTag),
            typeRendered->field(~name=Runtime.jsVariantValue),
          }->fields
    })
    let rendered = \"@"(noPayloadsRendered, payloadsRendered)
    let indent1 = rendered->Indent.heuristicVariants(~indent)
    (indent1 == None ? "" : Indent.break(~indent=indent1) ++ "  ") ++
    (rendered->String.concat((indent1 == None ? " " : Indent.break(~indent=indent1)) ++ "| "))
  }
and renderField = (
  ~config,
  ~indent,
  ~typeNameIsInterface,
  ~inFunType,
  {mutable_, nameJS: lbl, optional, type_},
) => {
  let optMarker = optional === Optional ? "?" : ""
  let mutMarker = mutable_ == Immutable ? config.language == Flow ? "+" : "readonly " : ""
  Indent.break(~indent) ++
  (mutMarker ++
  (lbl ++
  (optMarker ++
  (": " ++ (type_->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType))))))
}
and renderFields = (~closedFlag, ~config, ~indent, ~inFunType, ~typeNameIsInterface, fields) => {
  let indent1 = indent->Indent.more
  let exact = config.language == Flow && (!config.exportInterfaces && closedFlag == Closed)
  let space = indent == None && fields != list{} ? " " : ""
  let renderedFields =
    fields->List.map(renderField(~config, ~indent=indent1, ~typeNameIsInterface, ~inFunType))
  let dotdotdot =
    config.language == Flow && !exact ? list{Indent.break(~indent=indent1) ++ "..."} : list{}
  (exact ? "{|" : "{") ++
  space ++
  (String.concat(config.language == TypeScript ? "; " : ", ", \"@"(renderedFields, dotdotdot)) ++
  (Indent.break(~indent) ++ (space ++ (exact ? "|}" : "}"))))
}
and renderFunType = (
  ~config,
  ~indent,
  ~inFunType,
  ~typeNameIsInterface,
  ~typeVars,
  argTypes,
  retType,
) =>
  (inFunType ? "(" : "") ++
  (EmitText.genericsString(~typeVars) ++
  ("(" ++ (String.concat(", ", List.mapi((i, {aName, aType}) => {
      let parameterName = if config.language == Flow {
        ""
      } else {
        (aName == "" ? "_" ++ string_of_int(i + 1) : aName) ++ ":"
      }
      parameterName ++
      (aType->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType=true))
    }, argTypes)) ++ (") => " ++
  ((retType->renderType(~config, ~indent, ~typeNameIsInterface, ~inFunType)) ++
  (inFunType ? ")" : ""))))))

let typeToString = (~config, ~typeNameIsInterface, type_) =>
  type_->renderType(~config, ~typeNameIsInterface, ~inFunType=false)

let ofType = (~config, ~typeNameIsInterface=_ => false, ~type_, s) =>
  config.language == Untyped
    ? s
    : s ++ (": " ++ (type_->typeToString(~config, ~typeNameIsInterface)))

let emitHookTypeAsFunction = (
  ~config,
  ~emitters,
  ~name,
  ~propsType,
  ~retType,
  ~retValue,
  ~typeNameIsInterface,
  ~typeVars,
) =>
  "// Type annotated function components are not checked by Flow, but typeof() works.\n" ++
  ("const " ++
  (name ++
  (" = function " ++
  (EmitText.genericsString(~typeVars) ++
  ("(" ++
  ("_: " ++
  (propsType->renderType(~config, ~typeNameIsInterface, ~inFunType=true)) ++
  (")" ++
  ((" "->ofType(~config, ~typeNameIsInterface, ~type_=retType)) ++
    (" { return " ++
    (retValue ++ " };"))))))))))->Emitters.\"export"(~emitters)

let emitExportConst_ = (
  ~early,
  ~comment="",
  ~config,
  ~docString="",
  ~emitters,
  ~name,
  ~type_,
  ~typeNameIsInterface,
  line,
) =>
  (comment == "" ? comment : "// " ++ (comment ++ "\n")) ++
  (docString ++
  switch (config.module_, config.language) {
  | (_, TypeScript)
  | (ES6, _) =>
    "export const " ++ ((name->ofType(~config, ~typeNameIsInterface, ~type_)) ++ (" = " ++ line))
  | (CommonJS, _) =>
    "const " ++
    ((name->ofType(~config, ~typeNameIsInterface, ~type_)) ++
    (" = " ++ (line ++ (";\nexports." ++ (name ++ (" = " ++ name))))))
  })->(early ? Emitters.exportEarly : Emitters.\"export")(~emitters)

let emitExportConst = emitExportConst_(~early=false)

let emitExportConstEarly = emitExportConst_(~early=true)

let emitExportFunction = (~early, ~comment, ~emitters, ~name, ~config, line) =>
  "// " ++
  (comment ++ "\n") ++
  switch (config.module_, config.language) {
  | (_, TypeScript)
  | (ES6, _) =>
    "export function " ++ (name ++ line)
  | (CommonJS, _) => "function " ++ (name ++ (line ++ (";\nexports." ++ (name ++ (" = " ++ name)))))
  }->(early ? Emitters.exportEarly : Emitters.\"export")(~emitters)

let emitExportDefault = (~emitters, ~config, name) =>
  switch (config.module_, config.language) {
  | (_, TypeScript)
  | (ES6, _) =>
    "export default " ++ (name ++ ";")->Emitters.\"export"(~emitters)
  | (CommonJS, _) => "exports.default = " ++ (name ++ ";")->Emitters.\"export"(~emitters)
  }

let emitExportType = (
  ~early=false,
  ~config,
  ~emitters,
  ~nameAs,
  ~opaque,
  ~type_,
  ~typeNameIsInterface,
  ~typeVars,
  resolvedTypeName,
) => {
  let \"export" = early ? Emitters.exportEarly : Emitters.\"export"
  let typeParamsString = EmitText.genericsString(~typeVars)
  let isInterface = resolvedTypeName->typeNameIsInterface
  let resolvedTypeName =
    config.exportInterfaces && isInterface
      ? resolvedTypeName->interfaceName(~config)
      : resolvedTypeName
  let exportNameAs = switch nameAs {
  | None => ""
  | Some(s) =>
    "\nexport type " ++
    (s ++
    (typeParamsString ++ (" = " ++ (resolvedTypeName ++ (typeParamsString ++ ";")))))
  }

  switch config.language {
  | Flow =>
    if config.exportInterfaces && (isInterface && !opaque) {
      "export interface " ++
      (resolvedTypeName ++
      (typeParamsString ++
      (" " ++
      (((opaque ? mixedOrUnknown(~config) : type_)->typeToString(
        ~config,
        ~typeNameIsInterface,
      )) ++ (";" ++ exportNameAs)))))->\"export"(~emitters)
    } else {
      "export" ++
      ((opaque ? " opaque " : " ") ++
      ("type " ++
      (resolvedTypeName ++
      (typeParamsString ++
      (" = " ++
      (((opaque ? mixedOrUnknown(~config) : type_)->typeToString(
        ~config,
        ~typeNameIsInterface,
      )) ++ (";" ++ exportNameAs)))))))->\"export"(~emitters)
    }
  | TypeScript =>
    if opaque {
      /* Represent an opaque type as an absract class with a field called 'opaque'.
         Any type parameters must occur in the type of opaque, so that different
         instantiations are considered different types. */
      let typeOfOpaqueField = typeVars == list{} ? "any" : typeVars->String.concat(" | ")
      "// tslint:disable-next-line:max-classes-per-file \n" ++
      ((
        String.capitalize_ascii(resolvedTypeName) != resolvedTypeName
          ? "// tslint:disable-next-line:class-name\n"
          : ""
      ) ++
      ("export abstract class " ++
      (resolvedTypeName ++
      (typeParamsString ++
      (" { protected opaque!: " ++
      (typeOfOpaqueField ++ (" }; /* simulate opaque types */" ++ exportNameAs)))))))
        ->\"export"(~emitters)
    } else {
      if isInterface && config.exportInterfaces {
        "export interface " ++ (resolvedTypeName ++ (typeParamsString ++ " "))
      } else {
        "// tslint:disable-next-line:interface-over-type-literal\n" ++
        ("export type " ++
        (resolvedTypeName ++ (typeParamsString ++ " = ")))
      } ++
      (switch type_ {
      | _ => type_->typeToString(~config, ~typeNameIsInterface)
      } ++
      (";" ++ exportNameAs))->\"export"(~emitters)
    }
  | Untyped => emitters
  }
}

let emitImportValueAsEarly = (~config, ~emitters, ~name, ~nameAs, importPath) => {
  let commentBeforeImport =
    config.language == Flow ? "// flowlint-next-line nonstrict-import:off\n" : ""
  commentBeforeImport ++
  ("import " ++
  (switch nameAs {
  | Some(nameAs) => "{" ++ (name ++ (" as " ++ (nameAs ++ "}")))
  | None => name
  } ++
  (" from " ++
  ("'" ++ ((importPath->ImportPath.emit(~config)) ++ "';")))))->Emitters.requireEarly(~emitters)
}

let emitRequire = (
  ~importedValueOrComponent,
  ~early,
  ~emitters,
  ~config,
  ~moduleName,
  ~strict,
  importPath,
) => {
  let commentBeforeRequire = switch config.language {
  | TypeScript => "// eslint-disable-next-line @typescript-eslint/no-var-requires\n"
  | Flow => strict ? early ? "// flowlint-next-line nonstrict-import:off\n" : "" : flowExpectedError
  | Untyped => ""
  }
  switch config.module_ {
  | ES6 if !importedValueOrComponent && config.language != TypeScript =>
    commentBeforeRequire ++
    ("import * as " ++
    (ModuleName.toString(moduleName) ++
    (" from '" ++
    ((importPath->ImportPath.emit(~config)) ++ "';"))))
      ->(early ? Emitters.requireEarly : Emitters.require)(~emitters)
  | _ =>
    commentBeforeRequire ++
    ("const " ++
    (ModuleName.toString(moduleName) ++
    (" = require('" ++
    ((importPath->ImportPath.emit(~config)) ++ "');"))))
      ->(early ? Emitters.requireEarly : Emitters.require)(~emitters)
  }
}

let require = (~early) => early ? Emitters.requireEarly : Emitters.require

let emitImportReact = (~emitters, ~config) =>
  switch config.language {
  | Flow
  | Untyped =>
    emitRequire(
      ~importedValueOrComponent=false,
      ~early=true,
      ~emitters,
      ~config,
      ~moduleName=ModuleName.react,
      ~strict=false,
      ImportPath.react,
    )
  | TypeScript => "import * as React from 'react';"->require(~early=true, ~emitters)
  }

let emitPropTypes = (~config, ~emitters, ~indent, ~name, fields) => {
  let indent1 = indent->Indent.more
  let prefix = s => "PropTypes." ++ s
  let rec emitType = (~indent, type_: type_) =>
    switch type_ {
    | Array(t, _) => prefix("arrayOf") ++ ("(" ++ ((t->emitType(~indent)) ++ ")"))
    | Ident({name: ("bool" | "number" | "string") as id}) => id->prefix
    | Function(_) => "func"->prefix
    | GroupOfLabeledArgs(fields)
    | Object(_, fields)
    | Record(fields) =>
      let indent1 = indent->Indent.more
      prefix("shape") ++
      ("({" ++
      (Indent.break(~indent=indent1) ++
      ((fields
      ->List.filter(({nameJS}: field) => nameJS != "children")
      ->List.map(emitField(~indent=indent1))
      ->String.concat("," ++ Indent.break(~indent=indent1))) ++
      (Indent.break(~indent) ++ "})"))))

    | Ident(_)
    | Null(_)
    | Nullable(_)
    | Option(_)
    | Promise(_)
    | Tuple(_)
    | TypeVar(_)
    | Variant(_) =>
      "any"->prefix
    }
  and emitField = (~indent, {nameJS, optional, type_}: field) =>
    nameJS ++
    (" : " ++
    ((type_->emitType(~indent)) ++ (optional == Mandatory ? ".isRequired" : "")))

  config.emitImportPropTypes = true

  name ++
  (".propTypes = " ++
  ("{" ++
  (Indent.break(~indent=indent1) ++
  ((fields
  ->List.filter(({nameJS}: field) => nameJS != "children")
  ->List.map(emitField(~indent=indent1))
  ->String.concat("," ++ Indent.break(~indent=indent1))) ++ (Indent.break(~indent) ++ "};")))))
    ->Emitters.\"export"(~emitters)
}

let emitImportTypeAs = (
  ~emitters,
  ~config,
  ~typeName,
  ~asTypeName,
  ~typeNameIsInterface,
  ~importPath,
) => {
  let (typeName, asTypeName) = switch asTypeName {
  | Some(asName) =>
    asName->typeNameIsInterface
      ? (typeName->interfaceName(~config), Some(asName->interfaceName(~config)))
      : (typeName, asTypeName)
  | None => (typeName, asTypeName)
  }
  let strictLocalPrefix =
    config.language == Flow ? "// flowlint-next-line nonstrict-import:off\n" : ""
  switch config.language {
  | Flow
  | TypeScript =>
    strictLocalPrefix ++
    ("import " ++
    ((config.language == Flow ? "type " : "") ++
    ("{" ++
    (typeName ++
    (switch asTypeName {
    | Some(asT) => " as " ++ asT
    | None => ""
    } ++
    ("} from '" ++ ((importPath->ImportPath.emit(~config)) ++ "';")))))))
      ->Emitters.\"import"(~emitters)
  | Untyped => emitters
  }
}

let ofTypeAny = (~config, s) => s->ofType(~config, ~type_=typeAny(~config))

let emitTypeCast = (~config, ~type_, ~typeNameIsInterface, s) =>
  switch config.language {
  | TypeScript => s ++ (" as " ++ (type_->typeToString(~config, ~typeNameIsInterface)))
  | Untyped
  | Flow => s
  }
