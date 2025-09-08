open GenTypeCommon

type t

let addModuleEquation: (~dep: dep, ~internal: bool, t) => unit

let addModulePath: (~typeEnv: t, string) => ResolvedName.t

let addTypeEquations: (~typeEquations: list<(Longident.t, type_)>, t) => t

let applyTypeEquations: (~config: config, ~path: Path.t, t) => option<type_>

let expandAliasToExternalModule: (~name: string, t) => option<dep>

let getModuleEquations: t => list<ResolvedName.eq>

let getNestedModuleName: t => option<ModuleName.t>

/* Access path for the value in the module.
   It can be the value name if the module is not nested.
   Or TopLevelModule[x][y] if accessing a value in a doubly nested module */
let getModuleAccessPath: (~component: bool=?, ~name: string, t) => Runtime.moduleAccessPath

let getModule: (~name: string, t) => option<t>

let lookup: (~name: string, t) => option<t>

let lookupModuleTypeSignature: (~path: Path.t, t) => option<(Typedtree.signature, t)>

let newModule: (~name: string, t) => t

let newModuleType: (~name: string, ~signature: Typedtree.signature, t) => t

let newType: (~name: string, t) => unit

let root: unit => t

let toString: t => string

let updateModuleItem: (~nameOpt: option<string>=?, ~moduleItem: Runtime.moduleItem, t) => unit
