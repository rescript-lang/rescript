open GenTypeCommon

let componentExportName: (
  ~config: config,
  ~fileName: ModuleName.t,
  ~moduleName: ModuleName.t,
) => string

let emitExportConst: (
  ~comment: string=?,
  ~config: config,
  ~docString: string=?,
  ~emitters: Emitters.t,
  ~name: string,
  ~type_: type_,
  ~typeNameIsInterface: string => bool,
  string,
) => Emitters.t

let emitExportConstEarly: (
  ~comment: string=?,
  ~config: config,
  ~docString: string=?,
  ~emitters: Emitters.t,
  ~name: string,
  ~type_: type_,
  ~typeNameIsInterface: string => bool,
  string,
) => Emitters.t

let emitExportDefault: (~emitters: Emitters.t, ~config: config, string) => Emitters.t

let emitExportFunction: (
  ~early: bool,
  ~comment: string,
  ~emitters: Emitters.t,
  ~name: string,
  ~config: config,
  string,
) => Emitters.t

let emitExportType: (
  ~early: bool=?,
  ~config: config,
  ~emitters: Emitters.t,
  ~nameAs: option<string>,
  ~opaque: bool,
  ~type_: type_,
  ~typeNameIsInterface: string => bool,
  ~typeVars: list<string>,
  string,
) => Emitters.t

let emitHookTypeAsFunction: (
  ~config: config,
  ~emitters: Emitters.t,
  ~name: string,
  ~propsType: type_,
  ~retType: type_,
  ~retValue: string,
  ~typeNameIsInterface: string => bool,
  ~typeVars: list<string>,
) => Emitters.t

let emitImportReact: (~emitters: Emitters.t, ~config: config) => Emitters.t

let emitImportTypeAs: (
  ~emitters: Emitters.t,
  ~config: config,
  ~typeName: string,
  ~asTypeName: option<string>,
  ~typeNameIsInterface: string => bool,
  ~importPath: ImportPath.t,
) => Emitters.t

let emitImportValueAsEarly: (
  ~config: config,
  ~emitters: Emitters.t,
  ~name: string,
  ~nameAs: option<string>,
  ImportPath.t,
) => Emitters.t

let emitPropTypes: (
  ~config: config,
  ~emitters: Emitters.t,
  ~indent: Indent.t,
  ~name: string,
  list<field>,
) => Emitters.t

let emitRequire: (
  ~importedValueOrComponent: bool,
  ~early: bool,
  ~emitters: Emitters.t,
  ~config: config,
  ~moduleName: ModuleName.t,
  ~strict: bool,
  ImportPath.t,
) => Emitters.t

let emitTypeCast: (
  ~config: config,
  ~type_: type_,
  ~typeNameIsInterface: string => bool,
  string,
) => string

let fileHeader: (~config: config, ~sourceFile: string) => string

let generatedModuleExtension: (~config: config) => string

let isTypeReactElement: (~config: config, type_) => bool

let ofType: (
  ~config: config,
  ~typeNameIsInterface: string => bool=?,
  ~type_: type_,
  string,
) => string

@ocaml.doc(" Help type-checking by making the argument of type any ")
let ofTypeAny: (~config: config, string) => string

let outputFileSuffix: (~config: config) => string

let shimExtension: (~config: config) => string

let typeReactChild: (~config: config) => type_

let typeReactComponent: (~config: config, ~propsType: type_) => type_

let typeReactContext: (~config: config, ~type_: type_) => type_

let typeReactDOMReDomRef: (~config: config) => type_

let typeReactElement: (~config: config) => type_

let typeReactRef: (~type_: type_) => type_

let typeAny: (~config: config) => type_

let typeToString: (~config: config, ~typeNameIsInterface: string => bool, type_) => string
