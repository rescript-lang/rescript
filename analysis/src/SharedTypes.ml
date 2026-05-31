let str s = if s = "" then "\"\"" else s
let list l = "[" ^ (l |> List.map str |> String.concat ", ") ^ "]"
let ident l = l |> List.map str |> String.concat "."

type path = string list

type typed_fn_arg = Asttypes.arg_label * Types.type_expr

let path_to_string (path : path) = path |> String.concat "."

module ModulePath = struct
  type t =
    | File of Uri.t * string
    | NotVisible
    | IncludedModule of Path.t * t
    | ExportedModule of {name: string; module_path: t; is_type: bool}

  let to_path module_path tip_name : path =
    let rec loop module_path current =
      match module_path with
      | File _ -> current
      | IncludedModule (_, inner) -> loop inner current
      | ExportedModule {name; module_path = inner} ->
        loop inner (name :: current)
      | NotVisible -> current
    in
    loop module_path [tip_name]

  let to_path_with_prefix module_path prefix : path =
    let rec loop module_path current =
      match module_path with
      | File _ -> current
      | IncludedModule (_, inner) -> loop inner current
      | ExportedModule {name; module_path = inner} ->
        loop inner (name :: current)
      | NotVisible -> current
    in
    prefix :: loop module_path []
end

type field = {
  stamp: int;
  fname: string Location.loc;
  typ: Types.type_expr;
  optional: bool;
  docstring: string list;
  deprecated: string option;
}

type constructor_args =
  | InlineRecord of field list
  | Args of (Types.type_expr * Location.t) list

module Constructor = struct
  type t = {
    stamp: int;
    cname: string Location.loc;
    args: constructor_args;
    res: Types.type_expr option;
    type_decl: string * Types.type_declaration;
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
  type named_stamp_map = (string, int) Hashtbl.t

  type t = {
    types_: named_stamp_map;
    values_: named_stamp_map;
    modules_: named_stamp_map;
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
    | Module of {type_: t; is_module_type: bool}

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
    extent_loc: Location.t;
    stamp: int;
    module_path: ModulePath.t;
    is_exported: bool;
    deprecated: string option;
    docstring: string list;
    item: 'item;
  }
end

module Stamps : sig
  type kind =
    | KType of Type.t Declared.t
    | KValue of Types.type_expr Declared.t
    | KModule of Module.t Declared.t
    | KConstructor of Constructor.t Declared.t

  val loc_of_kind : kind -> Warnings.loc

  type t

  val add_constructor : t -> int -> Constructor.t Declared.t -> unit
  val add_module : t -> int -> Module.t Declared.t -> unit
  val add_type : t -> int -> Type.t Declared.t -> unit
  val add_value : t -> int -> Types.type_expr Declared.t -> unit
  val find_module : t -> int -> Module.t Declared.t option
  val find_type : t -> int -> Type.t Declared.t option
  val find_value : t -> int -> Types.type_expr Declared.t option
  val init : unit -> t
  val iter_constructors : (int -> Constructor.t Declared.t -> unit) -> t -> unit
  val iter_modules : (int -> Module.t Declared.t -> unit) -> t -> unit
  val iter_types : (int -> Type.t Declared.t -> unit) -> t -> unit
  val iter_values : (int -> Types.type_expr Declared.t -> unit) -> t -> unit
  val get_entries : t -> (int * kind) list
end = struct
  type 't stamp_map = (int, 't Declared.t) Hashtbl.t

  type kind =
    | KType of Type.t Declared.t
    | KValue of Types.type_expr Declared.t
    | KModule of Module.t Declared.t
    | KConstructor of Constructor.t Declared.t

  let loc_of_kind = function
    | KType declared -> declared.extent_loc
    | KValue declared -> declared.extent_loc
    | KModule declared -> declared.extent_loc
    | KConstructor declared -> declared.extent_loc

  type t = (int, kind) Hashtbl.t

  let init () = Hashtbl.create 10

  let add_constructor (stamps : t) stamp declared =
    Hashtbl.add stamps stamp (KConstructor declared)

  let add_module stamps stamp declared =
    Hashtbl.add stamps stamp (KModule declared)

  let add_type stamps stamp declared = Hashtbl.add stamps stamp (KType declared)

  let add_value stamps stamp declared =
    Hashtbl.add stamps stamp (KValue declared)

  let find_module stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KModule declared) -> Some declared
    | _ -> None

  let find_type stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KType declared) -> Some declared
    | _ -> None

  let find_value stamps stamp =
    match Hashtbl.find_opt stamps stamp with
    | Some (KValue declared) -> Some declared
    | _ -> None

  let iter_modules f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KModule d -> f stamp d
        | _ -> ())
      stamps

  let iter_types f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KType d -> f stamp d
        | _ -> ())
      stamps

  let iter_values f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KValue d -> f stamp d
        | _ -> ())
      stamps

  let iter_constructors f stamps =
    Hashtbl.iter
      (fun stamp d ->
        match d with
        | KConstructor d -> f stamp d
        | _ -> ())
      stamps

  let get_entries t = t |> Hashtbl.to_seq |> List.of_seq
end

module File = struct
  type t = {
    uri: Uri.t;
    stamps: Stamps.t;
    module_name: string;
    structure: Module.structure;
  }

  let create module_name uri =
    {
      uri;
      stamps = Stamps.init ();
      module_name;
      structure =
        {
          name = module_name;
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
    path_rev: path;
    parent: t option;
  }
  val from_file : File.t -> t
  val enter_structure : t -> Module.structure -> t

  (* Express a path starting from the module represented by the env.
     E.g. the env is at A.B.C and the path is D.
     The result is A.B.C.D if D is inside C.
     Or A.B.D or A.D or D if it's in one of its parents. *)
  val path_from_env : t -> path -> bool * path

  val to_string : t -> string
end = struct
  type t = {
    file: File.t;
    exported: Exported.t;
    path_rev: path;
    parent: t option;
  }

  let to_string {file; path_rev} =
    file.module_name :: List.rev path_rev |> String.concat "."

  let from_file (file : File.t) =
    {file; exported = file.structure.exported; path_rev = []; parent = None}

  (* Prune a path and find a parent environment that contains the module name *)
  let rec prune_path path_rev env name =
    if Exported.find env.exported Module name <> None then (true, path_rev)
    else
      match (path_rev, env.parent) with
      | _ :: rest, Some env -> prune_path rest env name
      | _ -> (false, [])

  let path_from_env env path =
    match path with
    | [] -> (true, env.path_rev |> List.rev)
    | name :: _ ->
      let found, pruned_path_rev = prune_path env.path_rev env name in
      (found, List.rev_append pruned_path_rev path)

  let enter_structure env (structure : Module.structure) =
    let name = structure.name in
    let path_rev = name :: snd (prune_path env.path_rev env name) in
    {env with exported = structure.exported; path_rev; parent = Some env}
end

type type_arg_context = {
  env: QueryEnv.t;
  type_args: Types.type_expr list;
  type_params: Types.type_expr list;
}

type poly_variant_constructor = {
  name: string;
  display_name: string;
  args: Types.type_expr list;
}

(* TODO(env-stuff) All envs for bool string etc can be removed. *)
type inner_type =
  | TypeExpr of Types.type_expr
  | ExtractedType of completion_type
and completion_type =
  | Tuple of QueryEnv.t * Types.type_expr list * Types.type_expr
  | Texn of QueryEnv.t
  | Tpromise of QueryEnv.t * Types.type_expr
  | Toption of QueryEnv.t * inner_type
  | Tresult of {
      env: QueryEnv.t;
      ok_type: Types.type_expr;
      error_type: Types.type_expr;
    }
  | Tbool of QueryEnv.t
  | Tarray of QueryEnv.t * inner_type
  | Tstring of QueryEnv.t
  | TtypeT of {env: QueryEnv.t; path: Path.t}
  | Tvariant of {
      env: QueryEnv.t;
      constructors: Constructor.t list;
      variant_decl: Types.type_declaration;
      variant_name: string;
    }
  | Tpolyvariant of {
      env: QueryEnv.t;
      constructors: poly_variant_constructor list;
      type_expr: Types.type_expr;
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
      args: typed_fn_arg list;
      typ: Types.type_expr;
      return_type: Types.type_expr;
    }

module Env = struct
  type t = {stamps: Stamps.t; module_path: ModulePath.t}
  let add_exported_module ~name ~is_type env =
    {
      env with
      module_path =
        ExportedModule {name; module_path = env.module_path; is_type};
    }
  let add_module ~name env = env |> add_exported_module ~name ~is_type:false
  let add_module_type ~name env = env |> add_exported_module ~name ~is_type:true
end

type file_path = string

type paths =
  | Impl of {cmt: file_path; res: file_path}
  | Namespace of {cmt: file_path}
  | IntfAndImpl of {
      cmti: file_path;
      resi: file_path;
      cmt: file_path;
      res: file_path;
    }

let show_paths paths =
  match paths with
  | Impl {cmt; res} ->
    Printf.sprintf "Impl cmt:%s res:%s" (Utils.dump_path cmt)
      (Utils.dump_path res)
  | Namespace {cmt} -> Printf.sprintf "Namespace cmt:%s" (Utils.dump_path cmt)
  | IntfAndImpl {cmti; resi; cmt; res} ->
    Printf.sprintf "IntfAndImpl cmti:%s resi:%s cmt:%s res:%s"
      (Utils.dump_path cmti) (Utils.dump_path resi) (Utils.dump_path cmt)
      (Utils.dump_path res)

let get_src p =
  match p with
  | Impl {res} -> [res]
  | Namespace _ -> []
  | IntfAndImpl {resi; res} -> [resi; res]

let get_uri p =
  match p with
  | Impl {res} -> Uri.from_path res
  | Namespace {cmt} -> Uri.from_path cmt
  | IntfAndImpl {resi} -> Uri.from_path resi

let get_uris p =
  match p with
  | Impl {res} -> [Uri.from_path res]
  | Namespace {cmt} -> [Uri.from_path cmt]
  | IntfAndImpl {res; resi} -> [Uri.from_path res; Uri.from_path resi]

let get_cmt_path ~uri p =
  match p with
  | Impl {cmt} -> cmt
  | Namespace {cmt} -> cmt
  | IntfAndImpl {cmti; cmt} ->
    let interface = Utils.ends_with (Uri.to_path uri) "i" in
    if interface then cmti else cmt

module Tip = struct
  type t = Value | Type | Field of string | Constructor of string | Module

  let to_string tip =
    match tip with
    | Value -> "Value"
    | Type -> "Type"
    | Field f -> "Field(" ^ f ^ ")"
    | Constructor a -> "Constructor(" ^ a ^ ")"
    | Module -> "Module"
end

let rec path_ident_to_string (p : Path.t) =
  match p with
  | Pident {name} -> name
  | Pdot (next_path, id, _) ->
    Printf.sprintf "%s.%s" (path_ident_to_string next_path) id
  | Papply _ -> ""

type loc_kind =
  | LocalReference of int * Tip.t
  | GlobalReference of string * string list * Tip.t
  | NotFound
  | Definition of int * Tip.t

type loc_type =
  | Typed of string * Types.type_expr * loc_kind
  | Constant of Asttypes.constant
  | LModule of loc_kind
  | TopLevelModule of string
  | TypeDefinition of string * Types.type_declaration * int

type loc_item = {loc: Location.t; loc_type: loc_type}

module LocationSet = Set.Make (struct
  include Location

  let compare loc1 loc2 = compare loc2 loc1

  (* polymorphic compare should be OK *)
end)

type extra = {
  internal_references: (int, Location.t list) Hashtbl.t;
  external_references:
    (string, (string list * Tip.t * Location.t) list) Hashtbl.t;
  file_references: (string, LocationSet.t) Hashtbl.t;
  mutable loc_items: loc_item list;
}

type file = string

module FileSet = Set.Make (String)

type package = {
  generic_jsx_module: string option;
  suffix: string;
  root_path: file_path;
  project_files: FileSet.t;
  dependencies_files: FileSet.t;
  paths_for_module: (file, paths) Hashtbl.t;
  namespace: string option;
  opens: path list;
  rescript_version: int * int;
  autocomplete: file list Misc.StringMap.t;
}

let all_files_in_package package =
  FileSet.union package.project_files package.dependencies_files

type full = {extra: extra; file: File.t; package: package}

let init_extra () =
  {
    internal_references = Hashtbl.create 10;
    external_references = Hashtbl.create 10;
    file_references = Hashtbl.create 10;
    loc_items = [];
  }

type state = {
  packages_by_root: (string, package) Hashtbl.t;
  root_for_uri: (Uri.t, string) Hashtbl.t;
  cmt_cache: (file_path, File.t) Hashtbl.t;
}

(* There's only one state, so it can as well be global *)
let state =
  {
    packages_by_root = Hashtbl.create 1;
    root_for_uri = Hashtbl.create 30;
    cmt_cache = Hashtbl.create 30;
  }

let loc_kind_to_string = function
  | LocalReference (_, tip) -> "(LocalReference " ^ Tip.to_string tip ^ ")"
  | GlobalReference _ -> "GlobalReference"
  | NotFound -> "NotFound"
  | Definition (_, tip) -> "(Definition " ^ Tip.to_string tip ^ ")"

let loc_type_to_string = function
  | Typed (name, e, loc_kind) ->
    "Typed " ^ name ^ " " ^ Shared.type_to_string e ^ " "
    ^ loc_kind_to_string loc_kind
  | Constant _ -> "Constant"
  | LModule loc_kind -> "LModule " ^ loc_kind_to_string loc_kind
  | TopLevelModule _ -> "TopLevelModule"
  | TypeDefinition _ -> "TypeDefinition"

let loc_item_to_string {loc = {Location.loc_start; loc_end}; loc_type} =
  let pos1 = Utils.cmt_pos_to_position loc_start in
  let pos2 = Utils.cmt_pos_to_position loc_end in
  Printf.sprintf "%d:%d-%d:%d %s" pos1.line pos1.character pos2.line
    pos2.character
    (loc_type_to_string loc_type)

(* needed for debugging *)
let _ = loc_item_to_string

module Completable = struct
  (* Completion context *)
  type completion_context = Type | Value | Module | Field | ValueOrField

  type argument_label =
    | Unlabelled of {argument_position: int}
    | Labelled of string
    | Optional of string

  (** Additional context for nested completion where needed. *)
  type nested_context =
    | RecordField of {seen_fields: string list}
        (** Completing for a record field, and we already saw the following fields... *)
    | CameFromRecordField of string
        (** We just came from this field (we leverage use this for better
            completion names etc) *)

  type nested_path =
    | NTupleItem of {item_num: int}
    | NFollowRecordField of {field_name: string}
    | NRecordBody of {seen_fields: string list}
    | NVariantPayload of {constructor_name: string; item_num: int}
    | NPolyvariantPayload of {constructor_name: string; item_num: int}
    | NArray

  let nested_path_to_string p =
    match p with
    | NTupleItem {item_num} -> "tuple($" ^ string_of_int item_num ^ ")"
    | NFollowRecordField {field_name} -> "recordField(" ^ field_name ^ ")"
    | NRecordBody _ -> "recordBody"
    | NVariantPayload {constructor_name; item_num} ->
      "variantPayload::" ^ constructor_name ^ "($" ^ string_of_int item_num
      ^ ")"
    | NPolyvariantPayload {constructor_name; item_num} ->
      "polyvariantPayload::" ^ constructor_name ^ "($" ^ string_of_int item_num
      ^ ")"
    | NArray -> "array"

  type context_path =
    | CPString
    | CPArray of context_path option
    | CPInt
    | CPFloat
    | CPBool
    | CPOption of context_path
    | CPApply of context_path * Asttypes.arg_label list
    | CPId of {
        path: string list;
        completion_context: completion_context;
        loc: Location.t;
      }
    | CPField of {
        context_path: context_path;
        field_name: string;
        pos_of_dot: (int * int) option;
        expr_loc: Location.t;
        in_jsx: bool;
            (** Whether this field access was found in a JSX context. *)
      }
    | CPObj of context_path * string
    | CPAwait of context_path
    | CPPipe of {
        synthetic: bool;  (** Whether this pipe completion is synthetic. *)
        context_path: context_path;
        id: string;
        in_jsx: bool;  (** Whether this pipe was found in a JSX context. *)
        lhs_loc: Location.t;
            (** The loc item for the left hand side of the pipe. *)
      }
    | CTuple of context_path list
    | CArgument of {
        function_context_path: context_path;
        argument_label: argument_label;
      }
    | CJsxPropValue of {
        path_to_component: string list;
        prop_name: string;
        empty_jsx_prop_name_hint: string option;
            (* This helps handle a special case in JSX prop completion. More info where this is used. *)
      }
    | CPatternPath of {root_ctx_path: context_path; nested: nested_path list}
    | CTypeAtPos of Location.t
        (** A position holding something that might have a *compiled* type. *)

  type pattern_mode = Default | Destructuring

  type decorator_payload =
    | Module of string
    | ModuleWithImportAttributes of {nested: nested_path list; prefix: string}
    | JsxConfig of {nested: nested_path list; prefix: string}

  type t =
    | Cdecorator of string  (** e.g. @module *)
    | CdecoratorPayload of decorator_payload
    | CextensionNode of string  (** e.g. %todo *)
    | CnamedArg of context_path * string * string list
        (** e.g. (..., "label", ["l1", "l2"]) for ...(...~l1...~l2...~label...) *)
    | Cnone  (** e.g. don't complete inside strings *)
    | Cpath of context_path
    | Cjsx of string list * string * string list
        (** E.g. (["M", "Comp"], "id", ["id1", "id2"]) for <M.Comp id1=... id2=... ... id *)
    | Cexpression of {
        context_path: context_path;
        nested: nested_path list;
        prefix: string;
      }
    | Cpattern of {
        context_path: context_path;
        nested: nested_path list;
        prefix: string;
        pattern_mode: pattern_mode;
        fallback: t option;
      }
    | CexhaustiveSwitch of {context_path: context_path; expr_loc: Location.t}
    | ChtmlElement of {prefix: string}

  let completion_context_to_string = function
    | Value -> "Value"
    | Type -> "Type"
    | Module -> "Module"
    | Field -> "Field"
    | ValueOrField -> "ValueOrField"

  let rec context_path_to_string = function
    | CPString -> "string"
    | CPInt -> "int"
    | CPFloat -> "float"
    | CPBool -> "bool"
    | CPAwait ctx_path -> "await " ^ context_path_to_string ctx_path
    | CPOption ctx_path -> "option<" ^ context_path_to_string ctx_path ^ ">"
    | CPApply (cp, labels) ->
      context_path_to_string cp ^ "("
      ^ (labels
        |> List.map (function
             | Asttypes.Nolabel -> "Nolabel"
             | Labelled {txt} -> "~" ^ txt
             | Optional {txt} -> "?" ^ txt)
        |> String.concat ", ")
      ^ ")"
    | CPArray (Some ctx_path) ->
      "array<" ^ context_path_to_string ctx_path ^ ">"
    | CPArray None -> "array"
    | CPId {path; completion_context} ->
      completion_context_to_string completion_context ^ list path
    | CPField {context_path = cp; field_name = s} ->
      context_path_to_string cp ^ "." ^ str s
    | CPObj (cp, s) -> context_path_to_string cp ^ "[\"" ^ s ^ "\"]"
    | CPPipe {context_path; id; in_jsx} ->
      context_path_to_string context_path
      ^ "->" ^ id
      ^ if in_jsx then " <<jsx>>" else ""
    | CTuple ctx_paths ->
      "CTuple("
      ^ (ctx_paths |> List.map context_path_to_string |> String.concat ", ")
      ^ ")"
    | CArgument {function_context_path; argument_label} ->
      "CArgument "
      ^ context_path_to_string function_context_path
      ^ "("
      ^ (match argument_label with
        | Unlabelled {argument_position} ->
          "$" ^ string_of_int argument_position
        | Labelled name -> "~" ^ name
        | Optional name -> "~" ^ name ^ "=?")
      ^ ")"
    | CJsxPropValue {path_to_component; prop_name} ->
      "CJsxPropValue " ^ (path_to_component |> list) ^ " " ^ prop_name
    | CPatternPath {root_ctx_path; nested} ->
      "CPatternPath("
      ^ context_path_to_string root_ctx_path
      ^ ")" ^ "->"
      ^ (nested
        |> List.map (fun nested_path -> nested_path_to_string nested_path)
        |> String.concat "->")
    | CTypeAtPos _loc -> "CTypeAtPos()"

  let to_string = function
    | Cpath cp -> "Cpath " ^ context_path_to_string cp
    | Cdecorator s -> "Cdecorator(" ^ str s ^ ")"
    | CextensionNode s -> "CextensionNode(" ^ str s ^ ")"
    | CdecoratorPayload (Module s) -> "CdecoratorPayload(module=" ^ s ^ ")"
    | CdecoratorPayload (ModuleWithImportAttributes _) ->
      "CdecoratorPayload(moduleWithImportAttributes)"
    | CdecoratorPayload (JsxConfig _) -> "JsxConfig"
    | CnamedArg (cp, s, sl2) ->
      "CnamedArg("
      ^ (cp |> context_path_to_string)
      ^ ", " ^ str s ^ ", " ^ (sl2 |> list) ^ ")"
    | Cnone -> "Cnone"
    | Cjsx (sl1, s, sl2) ->
      "Cjsx(" ^ (sl1 |> list) ^ ", " ^ str s ^ ", " ^ (sl2 |> list) ^ ")"
    | Cpattern {context_path; nested; prefix} -> (
      "Cpattern "
      ^ context_path_to_string context_path
      ^ (if prefix = "" then "" else "=" ^ prefix)
      ^
      match nested with
      | [] -> ""
      | nested_paths ->
        "->"
        ^ (nested_paths
          |> List.map (fun nested_path -> nested_path_to_string nested_path)
          |> String.concat ", "))
    | Cexpression {context_path; nested; prefix} -> (
      "Cexpression "
      ^ context_path_to_string context_path
      ^ (if prefix = "" then "" else "=" ^ prefix)
      ^
      match nested with
      | [] -> ""
      | nested_paths ->
        "->"
        ^ (nested_paths
          |> List.map (fun nested_path -> nested_path_to_string nested_path)
          |> String.concat ", "))
    | CexhaustiveSwitch {context_path} ->
      "CexhaustiveSwitch " ^ context_path_to_string context_path
    | ChtmlElement {prefix} -> "ChtmlElement <" ^ prefix
end

module ScopeTypes = struct
  type item =
    | Constructor of string * Location.t
    | Field of string * Location.t
    | Module of string * Location.t
    | Open of string list
    | Type of string * Location.t
    | Value of string * Location.t * Completable.context_path option * item list
    | Include of string * Location.t

  let item_to_string = function
    | Constructor (name, loc) ->
      "Constructor " ^ name ^ " " ^ Warnings.loc_to_string loc
    | Field (name, loc) -> "Field " ^ name ^ " " ^ Warnings.loc_to_string loc
    | Module (name, loc) -> "Module " ^ name ^ " " ^ Warnings.loc_to_string loc
    | Open path -> "Open " ^ (path |> String.concat ".")
    | Type (name, loc) -> "Type " ^ name ^ " " ^ Warnings.loc_to_string loc
    | Value (name, loc, _, _) ->
      "Value " ^ name ^ " " ^ Warnings.loc_to_string loc
    | Include (name, loc) ->
      "Include " ^ name ^ " " ^ Warnings.loc_to_string loc
end

module Completion = struct
  type kind =
    | Module of {docstring: string list; module_: Module.t}
    | Value of Types.type_expr
    | ObjLabel of Types.type_expr
    | Label of string
    | Type of Type.t
    | Constructor of Constructor.t * string
    | PolyvariantConstructor of poly_variant_constructor * string
    | Field of field * string
    | FileModule of string
    | Snippet of string
    | ExtractedType of completion_type * [`Value | `Type]
    | FollowContextPath of Completable.context_path * ScopeTypes.item list

  type t = {
    name: string;
    sort_text: string option;
    insert_text: string option;
    filter_text: string option;
    insert_text_format: Lsp.Types.InsertTextFormat.t option;
    env: QueryEnv.t;
    deprecated: string option;
    docstring: string list;
    kind: kind;
    detail: string option;
    type_arg_context: type_arg_context option;
    data: (string * string) list option;
    additional_text_edits: Lsp.Types.TextEdit.t list option;
    synthetic: bool;
        (** Whether this item is an made up, synthetic item or not. *)
  }

  let create ?(synthetic = false) ?additional_text_edits ?data ?type_arg_context
      ?(includes_snippets = false) ?insert_text ~kind ~env ?sort_text
      ?deprecated ?filter_text ?detail ?(docstring = []) name =
    {
      name;
      env;
      deprecated;
      docstring;
      kind;
      sort_text;
      insert_text;
      insert_text_format =
        (if includes_snippets then Some Lsp.Types.InsertTextFormat.Snippet
         else None);
      filter_text;
      detail;
      type_arg_context;
      data;
      additional_text_edits;
      synthetic;
    }

  (* https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_completion *)
  (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind *)
  let kind_to_lsp_completion_item kind =
    match kind with
    | Module _ -> Lsp.Types.CompletionItemKind.Module
    | FileModule _ -> Lsp.Types.CompletionItemKind.Module
    | Constructor (_, _) | PolyvariantConstructor (_, _) ->
      Lsp.Types.CompletionItemKind.Constructor
    | ObjLabel _ -> Lsp.Types.CompletionItemKind.Constructor
    | Label _ -> Lsp.Types.CompletionItemKind.Constructor
    | Field (_, _) -> Lsp.Types.CompletionItemKind.Field
    | Type _ | ExtractedType (_, `Type) -> Lsp.Types.CompletionItemKind.Struct
    | Value _ | ExtractedType (_, `Value) -> Lsp.Types.CompletionItemKind.Value
    | Snippet _ | FollowContextPath _ -> Lsp.Types.CompletionItemKind.Snippet
end

let kind_from_inner_type (t : inner_type) =
  match t with
  | ExtractedType extracted_type ->
    Completion.ExtractedType (extracted_type, `Value)
  | TypeExpr typ -> Value typ

module CursorPosition = struct
  type t = NoCursor | HasCursor | EmptyLoc

  let classify_loc loc ~pos =
    if loc |> Loc.has_pos ~pos then HasCursor
    else if loc |> Loc.end_ = (Location.none |> Loc.end_) then EmptyLoc
    else NoCursor

  let classify_location_loc (loc : 'a Location.loc) ~pos =
    if Loc.start loc.Location.loc <= pos && pos <= Loc.end_ loc.loc then
      HasCursor
    else if loc.loc |> Loc.end_ = (Location.none |> Loc.end_) then EmptyLoc
    else NoCursor

  let classify_positions pos ~pos_start ~pos_end =
    if pos_start <= pos && pos <= pos_end then HasCursor
    else if pos_end = (Location.none |> Loc.end_) then EmptyLoc
    else NoCursor

  let loc_has_cursor loc ~pos = loc |> classify_loc ~pos = HasCursor

  let loc_is_empty loc ~pos = loc |> classify_loc ~pos = EmptyLoc
end

type labelled = {
  name: string;
  opt: bool;
  pos_start: int * int;
  pos_end: int * int;
}

type label = labelled option
type arg = {label: label; exp: Parsetree.expression}

let extract_exp_apply_args ~args =
  let rec process_args ~acc args =
    match args with
    | ( ((Asttypes.Labelled {txt = s; loc} | Optional {txt = s; loc}) as label),
        (e : Parsetree.expression) )
      :: rest -> (
      let named_arg_loc = if loc = Location.none then None else Some loc in
      match named_arg_loc with
      | Some loc ->
        let labelled =
          {
            name = s;
            opt =
              (match label with
              | Optional _ -> true
              | _ -> false);
            pos_start = Loc.start loc;
            pos_end = Loc.end_ loc;
          }
        in
        process_args ~acc:({label = Some labelled; exp = e} :: acc) rest
      | None -> process_args ~acc rest)
    | (Nolabel, (e : Parsetree.expression)) :: rest ->
      if e.pexp_loc.loc_ghost then process_args ~acc rest
      else process_args ~acc:({label = None; exp = e} :: acc) rest
    | [] -> List.rev acc
  in
  args |> process_args ~acc:[]
