let str s = if s = "" then "\"\"" else s
let list l = "[" ^ (l |> List.map str |> String.concat ", ") ^ "]"
let ident l = l |> List.map str |> String.concat "."

type path = string list

type typedFnArg = Asttypes.arg_label * Types.type_expr

let pathToString (path : path) = path |> String.concat "."

module ModulePath = struct
  type t =
    | File of Uri.t * string
    | NotVisible
    | IncludedModule of Path.t * t
    | ExportedModule of {name: string; modulePath: t; isType: bool}

  let toPath modulePath tipName : path =
    let rec loop modulePath current =
      match modulePath with
      | File _ -> current
      | IncludedModule (_, inner) -> loop inner current
      | ExportedModule {name; modulePath = inner} -> loop inner (name :: current)
      | NotVisible -> current
    in
    loop modulePath [tipName]
end

type field = {
  stamp: int;
  fname: string Location.loc;
  typ: Types.type_expr;
  optional: bool;
  docstring: string list;
  deprecated: string option;
}

type constructorArgs =
  | InlineRecord of field list
  | Args of (Types.type_expr * Location.t) list

module Constructor = struct
  type t = {
    stamp: int;
    cname: string Location.loc;
    args: constructorArgs;
    res: Types.type_expr option;
    typeDecl: string * Types.type_declaration;
    docstring: string list;
    deprecated: string option;
  }
end

module Type = struct
  type kind =
    | Abstract of (Path.t * Types.type_expr list) option
    | Open
    | Tuple of Types.type_expr list
    | Record of field list
    | Variant of Constructor.t list

  type t = {
    kind: kind;
    decl: Types.type_declaration;
    name: string;
    attributes: Parsetree.attributes;
  }
end

module Exported = struct
  type namedStampMap = (string, int) Hashtbl.t

  type t = {
    types_: namedStampMap;
    values_: namedStampMap;
    modules_: namedStampMap;
  }

  type kind = Type | Value | Module

  let init () =
    {
      types_ = Hashtbl.create 10;
      values_ = Hashtbl.create 10;
      modules_ = Hashtbl.create 10;
    }

  let add t kind name x =
    let tbl =
      match kind with
      | Type -> t.types_
      | Value -> t.values_
      | Module -> t.modules_
    in
    if Hashtbl.mem tbl name then false
    else
      let () = Hashtbl.add tbl name x in
      true

  let find t kind name =
    let tbl =
      match kind with
      | Type -> t.types_
      | Value -> t.values_
      | Module -> t.modules_
    in
    Hashtbl.find_opt tbl name

  let iter t kind f =
    let tbl =
      match kind with
      | Type -> t.types_
      | Value -> t.values_
      | Module -> t.modules_
    in
    Hashtbl.iter f tbl
end

module Module = struct
  type kind =
    | Value of Types.type_expr
    | Type of Type.t * Types.rec_status
    | Module of {type_: t; isModuleType: bool}

  and item = {
    kind: kind;
    name: string;
    loc: Location.t;
    docstring: string list;
    deprecated: string option;
  }

  and structure = {
    name: string;
    docstring: string list;
    exported: Exported.t;
    items: item list;
    deprecated: string option;
  }

  and t = Ident of Path.t | Structure of structure | Constraint of t * t
end

module Declared = struct
  type 'item t = {
    name: string Location.loc;
    extentLoc: Location.t;
    stamp: int;
    modulePath: ModulePath.t;
    isExported: bool;
    deprecated: string option;
    docstring: string list;
    item: 'item;
  }
end

module Stamps : sig
  type t

  val addConstructor : t -> int -> Constructor.t Declared.t -> unit
  val addModule : t -> int -> Module.t Declared.t -> unit
  val addType : t -> int -> Type.t Declared.t -> unit
  val addValue : t -> int -> Types.type_expr Declared.t -> unit
  val findModule : t -> int -> Module.t Declared.t option
  val findType : t -> int -> Type.t Declared.t option
  val findValue : t -> int -> Types.type_expr Declared.t option
  val init : unit -> t
  val iterConstructors : (int -> Constructor.t Declared.t -> unit) -> t -> unit
  val iterModules : (int -> Module.t Declared.t -> unit) -> t -> unit
  val iterTypes : (int -> Type.t Declared.t -> unit) -> t -> unit
  val iterValues : (int -> Types.type_expr Declared.t -> unit) -> t -> unit
end = struct
  type 't stampMap = (int, 't Declared.t) Hashtbl.t

  type kind =
    | KType of Type.t Declared.t
    | KValue of Types.type_expr Declared.t
    | KModule of Module.t Declared.t
    | KConstructor of Constructor.t Declared.t

  type t = (int, kind) Hashtbl.t

  let init () = Hashtbl.create 10

  let addConstructor (stamps : t) stamp declared =
    Hashtbl.add stamps stamp (KConstructor declared)

  let addModule stamps stamp declared =
    Hashtbl.add stamps stamp (KModule declared)

  let addType stamps stamp declared = Hashtbl.add stamps stamp (KType declared)

  let addValue stamps stamp declared =
    Hashtbl.add stamps stamp (KValue declared)

  let findModule stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KModule declared) -> Some declared
    | _ -> None

  let findType stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KType declared) -> Some declared
    | _ -> None

  let findValue stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KValue declared) -> Some declared
    | _ -> None

  let iterModules f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KModule d -> f stamp d
        | _ -> ())
      stamps

  let iterTypes f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KType d -> f stamp d
        | _ -> ())
      stamps

  let iterValues f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KValue d -> f stamp d
        | _ -> ())
      stamps

  let iterConstructors f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KConstructor d -> f stamp d
        | _ -> ())
      stamps
end

module File = struct
  type t = {
    uri: Uri.t;
    stamps: Stamps.t;
    moduleName: string;
    structure: Module.structure;
  }

  let create moduleName uri =
    {
      uri;
      stamps = Stamps.init ();
      moduleName;
      structure =
        {
          name = moduleName;
          docstring = [];
          exported = Exported.init ();
          items = [];
          deprecated = None;
        };
    }
end

module QueryEnv : sig
  type t = private {
    file: File.t;
    exported: Exported.t;
    pathRev: path;
    parent: t option;
  }
  val fromFile : File.t -> t
  val enterStructure : t -> Module.structure -> t

  (* Express a path starting from the module represented by the env.
     E.g. the env is at A.B.C and the path is D.
     The result is A.B.C.D if D is inside C.
     Or A.B.D or A.D or D if it's in one of its parents. *)
  val pathFromEnv : t -> path -> bool * path

  val toString : t -> string
end = struct
  type t = {file: File.t; exported: Exported.t; pathRev: path; parent: t option}

  let toString {file; pathRev} =
    file.moduleName :: List.rev pathRev |> String.concat "."

  let fromFile (file : File.t) =
    {file; exported = file.structure.exported; pathRev = []; parent = None}

  (* Prune a path and find a parent environment that contains the module name *)
  let rec prunePath pathRev env name =
    if Exported.find env.exported Module name <> None then (true, pathRev)
    else
      match (pathRev, env.parent) with
      | _ :: rest, Some env -> prunePath rest env name
      | _ -> (false, [])

  let pathFromEnv env path =
    match path with
    | [] -> (true, env.pathRev |> List.rev)
    | name :: _ ->
      let found, prunedPathRev = prunePath env.pathRev env name in
      (found, List.rev_append prunedPathRev path)

  let enterStructure env (structure : Module.structure) =
    let name = structure.name in
    let pathRev = name :: snd (prunePath env.pathRev env name) in
    {env with exported = structure.exported; pathRev; parent = Some env}
end

type typeArgContext = {
  env: QueryEnv.t;
  typeArgs: Types.type_expr list;
  typeParams: Types.type_expr list;
}

type polyVariantConstructor = {
  name: string;
  displayName: string;
  args: Types.type_expr list;
}

(* TODO(env-stuff) All envs for bool string etc can be removed. *)
type innerType = TypeExpr of Types.type_expr | ExtractedType of completionType
and completionType =
  | Tuple of QueryEnv.t * Types.type_expr list * Types.type_expr
  | Texn of QueryEnv.t
  | Tpromise of QueryEnv.t * Types.type_expr
  | Toption of QueryEnv.t * innerType
  | Tresult of {
      env: QueryEnv.t;
      okType: Types.type_expr;
      errorType: Types.type_expr;
    }
  | Tbool of QueryEnv.t
  | Tarray of QueryEnv.t * innerType
  | Tstring of QueryEnv.t
  | TtypeT of {env: QueryEnv.t; path: Path.t}
  | Tvariant of {
      env: QueryEnv.t;
      constructors: Constructor.t list;
      variantDecl: Types.type_declaration;
      variantName: string;
    }
  | Tpolyvariant of {
      env: QueryEnv.t;
      constructors: polyVariantConstructor list;
      typeExpr: Types.type_expr;
    }
  | Trecord of {
      env: QueryEnv.t;
      fields: field list;
      definition:
        [ `NameOnly of string
          (** When we only have the name, like when pulling the record from a declared type. *)
        | `TypeExpr of Types.type_expr
          (** When we have the full type expr from the compiler. *) ];
    }
  | TinlineRecord of {env: QueryEnv.t; fields: field list}
  | Tfunction of {
      env: QueryEnv.t;
      args: typedFnArg list;
      typ: Types.type_expr;
      uncurried: bool;
      returnType: Types.type_expr;
    }

module Env = struct
  type t = {stamps: Stamps.t; modulePath: ModulePath.t}
  let addExportedModule ~name ~isType env =
    {
      env with
      modulePath = ExportedModule {name; modulePath = env.modulePath; isType};
    }
  let addModule ~name env = env |> addExportedModule ~name ~isType:false
  let addModuleType ~name env = env |> addExportedModule ~name ~isType:true
end

type filePath = string

type paths =
  | Impl of {cmt: filePath; res: filePath}
  | Namespace of {cmt: filePath}
  | IntfAndImpl of {
      cmti: filePath;
      resi: filePath;
      cmt: filePath;
      res: filePath;
    }

let showPaths paths =
  match paths with
  | Impl {cmt; res} ->
    Printf.sprintf "Impl cmt:%s res:%s" (Utils.dumpPath cmt)
      (Utils.dumpPath res)
  | Namespace {cmt} -> Printf.sprintf "Namespace cmt:%s" (Utils.dumpPath cmt)
  | IntfAndImpl {cmti; resi; cmt; res} ->
    Printf.sprintf "IntfAndImpl cmti:%s resi:%s cmt:%s res:%s"
      (Utils.dumpPath cmti) (Utils.dumpPath resi) (Utils.dumpPath cmt)
      (Utils.dumpPath res)

let getSrc p =
  match p with
  | Impl {res} -> [res]
  | Namespace _ -> []
  | IntfAndImpl {resi; res} -> [resi; res]

let getUri p =
  match p with
  | Impl {res} -> Uri.fromPath res
  | Namespace {cmt} -> Uri.fromPath cmt
  | IntfAndImpl {resi} -> Uri.fromPath resi

let getUris p =
  match p with
  | Impl {res} -> [Uri.fromPath res]
  | Namespace {cmt} -> [Uri.fromPath cmt]
  | IntfAndImpl {res; resi} -> [Uri.fromPath res; Uri.fromPath resi]

let getCmtPath ~uri p =
  match p with
  | Impl {cmt} -> cmt
  | Namespace {cmt} -> cmt
  | IntfAndImpl {cmti; cmt} ->
    let interface = Utils.endsWith (Uri.toPath uri) "i" in
    if interface then cmti else cmt

module Tip = struct
  type t = Value | Type | Field of string | Constructor of string | Module

  let toString tip =
    match tip with
    | Value -> "Value"
    | Type -> "Type"
    | Field f -> "Field(" ^ f ^ ")"
    | Constructor a -> "Constructor(" ^ a ^ ")"
    | Module -> "Module"
end

let rec pathIdentToString (p : Path.t) =
  match p with
  | Pident {name} -> name
  | Pdot (nextPath, id, _) ->
    Printf.sprintf "%s.%s" (pathIdentToString nextPath) id
  | Papply _ -> ""

type locKind =
  | LocalReference of int * Tip.t
  | GlobalReference of string * string list * Tip.t
  | NotFound
  | Definition of int * Tip.t

type locType =
  | Typed of string * Types.type_expr * locKind
  | Constant of Asttypes.constant
  | LModule of locKind
  | TopLevelModule of string
  | TypeDefinition of string * Types.type_declaration * int

type locItem = {loc: Location.t; locType: locType}

module LocationSet = Set.Make (struct
  include Location

  let compare loc1 loc2 = compare loc2 loc1

  (* polymorphic compare should be OK *)
end)

type extra = {
  internalReferences: (int, Location.t list) Hashtbl.t;
  externalReferences:
    (string, (string list * Tip.t * Location.t) list) Hashtbl.t;
  fileReferences: (string, LocationSet.t) Hashtbl.t;
  mutable locItems: locItem list;
}

type file = string

module FileSet = Set.Make (String)

type builtInCompletionModules = {
  arrayModulePath: string list;
  optionModulePath: string list;
  stringModulePath: string list;
  intModulePath: string list;
  floatModulePath: string list;
  promiseModulePath: string list;
  listModulePath: string list;
  resultModulePath: string list;
  exnModulePath: string list;
  regexpModulePath: string list;
}

type package = {
  genericJsxModule: string option;
  suffix: string;
  rootPath: filePath;
  projectFiles: FileSet.t;
  dependenciesFiles: FileSet.t;
  pathsForModule: (file, paths) Hashtbl.t;
  namespace: string option;
  builtInCompletionModules: builtInCompletionModules;
  opens: path list;
  uncurried: bool;
  rescriptVersion: int * int;
}

let allFilesInPackage package =
  FileSet.union package.projectFiles package.dependenciesFiles

type full = {extra: extra; file: File.t; package: package}

let initExtra () =
  {
    internalReferences = Hashtbl.create 10;
    externalReferences = Hashtbl.create 10;
    fileReferences = Hashtbl.create 10;
    locItems = [];
  }

type state = {
  packagesByRoot: (string, package) Hashtbl.t;
  rootForUri: (Uri.t, string) Hashtbl.t;
  cmtCache: (filePath, File.t) Hashtbl.t;
}

(* There's only one state, so it can as well be global *)
let state =
  {
    packagesByRoot = Hashtbl.create 1;
    rootForUri = Hashtbl.create 30;
    cmtCache = Hashtbl.create 30;
  }

let locKindToString = function
  | LocalReference (_, tip) -> "(LocalReference " ^ Tip.toString tip ^ ")"
  | GlobalReference _ -> "GlobalReference"
  | NotFound -> "NotFound"
  | Definition (_, tip) -> "(Definition " ^ Tip.toString tip ^ ")"

let locTypeToString = function
  | Typed (name, e, locKind) ->
    "Typed " ^ name ^ " " ^ Shared.typeToString e ^ " "
    ^ locKindToString locKind
  | Constant _ -> "Constant"
  | LModule locKind -> "LModule " ^ locKindToString locKind
  | TopLevelModule _ -> "TopLevelModule"
  | TypeDefinition _ -> "TypeDefinition"

let locItemToString {loc = {Location.loc_start; loc_end}; locType} =
  let pos1 = Utils.cmtPosToPosition loc_start in
  let pos2 = Utils.cmtPosToPosition loc_end in
  Printf.sprintf "%d:%d-%d:%d %s" pos1.line pos1.character pos2.line
    pos2.character (locTypeToString locType)

(* needed for debugging *)
let _ = locItemToString

module Completable = struct
  (* Completion context *)
  type completionContext = Type | Value | Module | Field | ValueOrField

  type argumentLabel =
    | Unlabelled of {argumentPosition: int}
    | Labelled of string
    | Optional of string

  (** Additional context for nested completion where needed. *)
  type nestedContext =
    | RecordField of {seenFields: string list}
        (** Completing for a record field, and we already saw the following fields... *)
    | CameFromRecordField of string
        (** We just came from this field (we leverage use this for better
            completion names etc) *)

  type nestedPath =
    | NTupleItem of {itemNum: int}
    | NFollowRecordField of {fieldName: string}
    | NRecordBody of {seenFields: string list}
    | NVariantPayload of {constructorName: string; itemNum: int}
    | NPolyvariantPayload of {constructorName: string; itemNum: int}
    | NArray

  let nestedPathToString p =
    match p with
    | NTupleItem {itemNum} -> "tuple($" ^ string_of_int itemNum ^ ")"
    | NFollowRecordField {fieldName} -> "recordField(" ^ fieldName ^ ")"
    | NRecordBody _ -> "recordBody"
    | NVariantPayload {constructorName; itemNum} ->
      "variantPayload::" ^ constructorName ^ "($" ^ string_of_int itemNum ^ ")"
    | NPolyvariantPayload {constructorName; itemNum} ->
      "polyvariantPayload::" ^ constructorName ^ "($" ^ string_of_int itemNum
      ^ ")"
    | NArray -> "array"

  type contextPath =
    | CPString
    | CPArray of contextPath option
    | CPInt
    | CPFloat
    | CPBool
    | CPOption of contextPath
    | CPApply of contextPath * Asttypes.arg_label list
    | CPId of {
        path: string list;
        completionContext: completionContext;
        loc: Location.t;
      }
    | CPField of {
        contextPath: contextPath;
        fieldName: string;
        posOfDot: (int * int) option;
        exprLoc: Location.t;
      }
    | CPObj of contextPath * string
    | CPAwait of contextPath
    | CPPipe of {
        synthetic: bool;  (** Whether this pipe completion is synthetic. *)
        contextPath: contextPath;
        id: string;
        inJsx: bool;  (** Whether this pipe was found in a JSX context. *)
        lhsLoc: Location.t;
            (** The loc item for the left hand side of the pipe. *)
      }
    | CTuple of contextPath list
    | CArgument of {
        functionContextPath: contextPath;
        argumentLabel: argumentLabel;
      }
    | CJsxPropValue of {
        pathToComponent: string list;
        propName: string;
        emptyJsxPropNameHint: string option;
            (* This helps handle a special case in JSX prop completion. More info where this is used. *)
      }
    | CPatternPath of {rootCtxPath: contextPath; nested: nestedPath list}
    | CTypeAtPos of Location.t
        (** A position holding something that might have a *compiled* type. *)

  type patternMode = Default | Destructuring

  type decoratorPayload =
    | Module of string
    | ModuleWithImportAttributes of {nested: nestedPath list; prefix: string}
    | JsxConfig of {nested: nestedPath list; prefix: string}

  type t =
    | Cdecorator of string  (** e.g. @module *)
    | CdecoratorPayload of decoratorPayload
    | CextensionNode of string  (** e.g. %todo *)
    | CnamedArg of contextPath * string * string list
        (** e.g. (..., "label", ["l1", "l2"]) for ...(...~l1...~l2...~label...) *)
    | Cnone  (** e.g. don't complete inside strings *)
    | Cpath of contextPath
    | Cjsx of string list * string * string list
        (** E.g. (["M", "Comp"], "id", ["id1", "id2"]) for <M.Comp id1=... id2=... ... id *)
    | Cexpression of {
        contextPath: contextPath;
        nested: nestedPath list;
        prefix: string;
      }
    | Cpattern of {
        contextPath: contextPath;
        nested: nestedPath list;
        prefix: string;
        patternMode: patternMode;
        fallback: t option;
      }
    | CexhaustiveSwitch of {contextPath: contextPath; exprLoc: Location.t}
    | ChtmlElement of {prefix: string}

  let completionContextToString = function
    | Value -> "Value"
    | Type -> "Type"
    | Module -> "Module"
    | Field -> "Field"
    | ValueOrField -> "ValueOrField"

  let rec contextPathToString = function
    | CPString -> "string"
    | CPInt -> "int"
    | CPFloat -> "float"
    | CPBool -> "bool"
    | CPAwait ctxPath -> "await " ^ contextPathToString ctxPath
    | CPOption ctxPath -> "option<" ^ contextPathToString ctxPath ^ ">"
    | CPApply (cp, labels) ->
      contextPathToString cp ^ "("
      ^ (labels
        |> List.map (function
             | Asttypes.Nolabel -> "Nolabel"
             | Labelled s -> "~" ^ s
             | Optional s -> "?" ^ s)
        |> String.concat ", ")
      ^ ")"
    | CPArray (Some ctxPath) -> "array<" ^ contextPathToString ctxPath ^ ">"
    | CPArray None -> "array"
    | CPId {path; completionContext} ->
      completionContextToString completionContext ^ list path
    | CPField {contextPath = cp; fieldName = s} ->
      contextPathToString cp ^ "." ^ str s
    | CPObj (cp, s) -> contextPathToString cp ^ "[\"" ^ s ^ "\"]"
    | CPPipe {contextPath; id; inJsx} ->
      contextPathToString contextPath
      ^ "->" ^ id
      ^ if inJsx then " <<jsx>>" else ""
    | CTuple ctxPaths ->
      "CTuple("
      ^ (ctxPaths |> List.map contextPathToString |> String.concat ", ")
      ^ ")"
    | CArgument {functionContextPath; argumentLabel} ->
      "CArgument "
      ^ contextPathToString functionContextPath
      ^ "("
      ^ (match argumentLabel with
        | Unlabelled {argumentPosition} -> "$" ^ string_of_int argumentPosition
        | Labelled name -> "~" ^ name
        | Optional name -> "~" ^ name ^ "=?")
      ^ ")"
    | CJsxPropValue {pathToComponent; propName} ->
      "CJsxPropValue " ^ (pathToComponent |> list) ^ " " ^ propName
    | CPatternPath {rootCtxPath; nested} ->
      "CPatternPath("
      ^ contextPathToString rootCtxPath
      ^ ")" ^ "->"
      ^ (nested
        |> List.map (fun nestedPath -> nestedPathToString nestedPath)
        |> String.concat "->")
    | CTypeAtPos _loc -> "CTypeAtPos()"

  let toString = function
    | Cpath cp -> "Cpath " ^ contextPathToString cp
    | Cdecorator s -> "Cdecorator(" ^ str s ^ ")"
    | CextensionNode s -> "CextensionNode(" ^ str s ^ ")"
    | CdecoratorPayload (Module s) -> "CdecoratorPayload(module=" ^ s ^ ")"
    | CdecoratorPayload (ModuleWithImportAttributes _) ->
      "CdecoratorPayload(moduleWithImportAttributes)"
    | CdecoratorPayload (JsxConfig _) -> "JsxConfig"
    | CnamedArg (cp, s, sl2) ->
      "CnamedArg("
      ^ (cp |> contextPathToString)
      ^ ", " ^ str s ^ ", " ^ (sl2 |> list) ^ ")"
    | Cnone -> "Cnone"
    | Cjsx (sl1, s, sl2) ->
      "Cjsx(" ^ (sl1 |> list) ^ ", " ^ str s ^ ", " ^ (sl2 |> list) ^ ")"
    | Cpattern {contextPath; nested; prefix} -> (
      "Cpattern "
      ^ contextPathToString contextPath
      ^ (if prefix = "" then "" else "=" ^ prefix)
      ^
      match nested with
      | [] -> ""
      | nestedPaths ->
        "->"
        ^ (nestedPaths
          |> List.map (fun nestedPath -> nestedPathToString nestedPath)
          |> String.concat ", "))
    | Cexpression {contextPath; nested; prefix} -> (
      "Cexpression "
      ^ contextPathToString contextPath
      ^ (if prefix = "" then "" else "=" ^ prefix)
      ^
      match nested with
      | [] -> ""
      | nestedPaths ->
        "->"
        ^ (nestedPaths
          |> List.map (fun nestedPath -> nestedPathToString nestedPath)
          |> String.concat ", "))
    | CexhaustiveSwitch {contextPath} ->
      "CexhaustiveSwitch " ^ contextPathToString contextPath
    | ChtmlElement {prefix} -> "ChtmlElement <" ^ prefix
end

module ScopeTypes = struct
  type item =
    | Constructor of string * Location.t
    | Field of string * Location.t
    | Module of string * Location.t
    | Open of string list
    | Type of string * Location.t
    | Value of string * Location.t * Completable.contextPath option * item list
end

module Completion = struct
  type kind =
    | Module of {docstring: string list; module_: Module.t}
    | Value of Types.type_expr
    | ObjLabel of Types.type_expr
    | Label of string
    | Type of Type.t
    | Constructor of Constructor.t * string
    | PolyvariantConstructor of polyVariantConstructor * string
    | Field of field * string
    | FileModule of string
    | Snippet of string
    | ExtractedType of completionType * [`Value | `Type]
    | FollowContextPath of Completable.contextPath * ScopeTypes.item list

  type t = {
    name: string;
    sortText: string option;
    insertText: string option;
    filterText: string option;
    insertTextFormat: Protocol.insertTextFormat option;
    env: QueryEnv.t;
    deprecated: string option;
    docstring: string list;
    kind: kind;
    detail: string option;
    typeArgContext: typeArgContext option;
    data: (string * string) list option;
    additionalTextEdits: Protocol.textEdit list option;
    synthetic: bool;
        (** Whether this item is an made up, synthetic item or not. *)
  }

  let create ?(synthetic = false) ?additionalTextEdits ?data ?typeArgContext
      ?(includesSnippets = false) ?insertText ~kind ~env ?sortText ?deprecated
      ?filterText ?detail ?(docstring = []) name =
    {
      name;
      env;
      deprecated;
      docstring;
      kind;
      sortText;
      insertText;
      insertTextFormat =
        (if includesSnippets then Some Protocol.Snippet else None);
      filterText;
      detail;
      typeArgContext;
      data;
      additionalTextEdits;
      synthetic;
    }

  (* https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_completion *)
  (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind *)
  let kindToInt kind =
    match kind with
    | Module _ -> 9
    | FileModule _ -> 9
    | Constructor (_, _) | PolyvariantConstructor (_, _) -> 4
    | ObjLabel _ -> 4
    | Label _ -> 4
    | Field (_, _) -> 5
    | Type _ | ExtractedType (_, `Type) -> 22
    | Value _ | ExtractedType (_, `Value) -> 12
    | Snippet _ | FollowContextPath _ -> 15
end

let kindFromInnerType (t : innerType) =
  match t with
  | ExtractedType extractedType ->
    Completion.ExtractedType (extractedType, `Value)
  | TypeExpr typ -> Value typ

module CursorPosition = struct
  type t = NoCursor | HasCursor | EmptyLoc

  let classifyLoc loc ~pos =
    if loc |> Loc.hasPos ~pos then HasCursor
    else if loc |> Loc.end_ = (Location.none |> Loc.end_) then EmptyLoc
    else NoCursor

  let classifyLocationLoc (loc : 'a Location.loc) ~pos =
    if Loc.start loc.Location.loc <= pos && pos <= Loc.end_ loc.loc then
      HasCursor
    else if loc.loc |> Loc.end_ = (Location.none |> Loc.end_) then EmptyLoc
    else NoCursor

  let classifyPositions pos ~posStart ~posEnd =
    if posStart <= pos && pos <= posEnd then HasCursor
    else if posEnd = (Location.none |> Loc.end_) then EmptyLoc
    else NoCursor

  let locHasCursor loc ~pos = loc |> classifyLoc ~pos = HasCursor

  let locIsEmpty loc ~pos = loc |> classifyLoc ~pos = EmptyLoc
end

type labelled = {
  name: string;
  opt: bool;
  posStart: int * int;
  posEnd: int * int;
}

type label = labelled option
type arg = {label: label; exp: Parsetree.expression}

let extractExpApplyArgs ~args =
  let rec processArgs ~acc args =
    match args with
    | (((Asttypes.Labelled s | Optional s) as label), (e : Parsetree.expression))
      :: rest -> (
      let namedArgLoc =
        e.pexp_attributes
        |> List.find_opt (fun ({Asttypes.txt}, _) -> txt = "res.namedArgLoc")
      in
      match namedArgLoc with
      | Some ({loc}, _) ->
        let labelled =
          {
            name = s;
            opt =
              (match label with
              | Optional _ -> true
              | _ -> false);
            posStart = Loc.start loc;
            posEnd = Loc.end_ loc;
          }
        in
        processArgs ~acc:({label = Some labelled; exp = e} :: acc) rest
      | None -> processArgs ~acc rest)
    | (Asttypes.Nolabel, (e : Parsetree.expression)) :: rest ->
      if e.pexp_loc.loc_ghost then processArgs ~acc rest
      else processArgs ~acc:({label = None; exp = e} :: acc) rest
    | [] -> List.rev acc
  in
  args |> processArgs ~acc:[]
