(** 
  TypeScript Codegen IR and its printer

  A typed intermediate representation for generating
  TypeScript/JavaScript with type annotations and JSDoc.

  TODO(refactor): Unify the codegen IR with the existing JavaScript IR.
  TODO(refactor): Split the core IR and printer as separate modules.
*)

type type_ref = {
  name: string;
      (** Display name for the type (may include module path and @as renaming) *)
  args: ts_type list;  (** Type arguments *)
}
(**
  Simplified type representation for TypeScript output.
  This is derived from Types.type_expr but simplified for code generation. 
 *)

and ts_type =
  | Any
  | Unknown
  | Never
  | Void  (** Used for functions returning unit *)
  | Null
  | Undefined
  | Boolean
  | Number
  | Bigint
  | String
  | Symbol
  | Array of ts_type
  | Tuple of ts_type list
  | Object of object_type
  | Function of fn_type
  | Union of ts_type list
  | Intersection of ts_type list
  | TypeRef of type_ref  (** Reference to a named type *)
  | TypeVar of string  (** Type variable 'a -> A *)
  | Literal of literal_type
  | Readonly of ts_type
  | Promise of ts_type
  | RuntimeType of runtime_type
      (** Reference to a type from @rescript/runtime *)

and runtime_type = {
  rt_name: string;  (** Type name, e.g. "result", "list" *)
  rt_args: ts_type list;  (** Type arguments *)
}
(** Types defined in @rescript/runtime/types.d.ts 
    TODO: make it as variant *)

and literal_type =
  | LitString of string
  | LitNumber of float
  | LitBigint of string
  | LitBoolean of bool

and object_type = {
  properties: property_sig list;
  index_sig: index_sig option;
  call_sig: fn_type option;
}

and property_sig = {
  prop_name: string;
  prop_type: ts_type;
  prop_optional: bool;
  prop_readonly: bool;
}

and index_sig = {
  index_key: ts_type;  (** Usually String or Number *)
  index_value: ts_type;
}

and fn_type = {
  fn_params: param_type list;
  fn_rest: param_type option;
  fn_return: ts_type;
  fn_type_params: type_param list;
  fn_async: bool;
}

and param_type = {
  param_name: string option;
  param_type: ts_type;
  param_optional: bool;
}

and type_param = {
  tp_name: string;
  tp_constraint: ts_type option;
  tp_default: ts_type option;
}

(** Helper to create Object ts_type - needed because expression_desc.Object
    shadows ts_type.Object after the expression type definition *)
let make_object_type ~properties ~index_sig ~call_sig : ts_type =
  Object {properties; index_sig; call_sig}

(** {1 Type Declarations} *)

(** Payload representation for variant constructors *)
type variant_payload =
  | NoPayload  (** Nullary constructor - just the tag string *)
  | TuplePayload of ts_type list  (** Tuple payload: _0, _1, etc. *)
  | InlineRecord of property_sig list
      (** Inline record: named fields flattened *)

type variant_case = {
  vc_name: string;
  vc_tag: string;  (** The TAG value, e.g. "Ok", "Error" *)
  vc_payload: variant_payload;  (** Payload representation *)
}
(** Variant constructor for tagged unions *)

type variant_config = {
  vc_unboxed: bool;  (** @unboxed - payload is the value directly *)
  vc_tag_name: string;  (** Tag field name, default Js_dump_lit.tag ("TAG") *)
}
(** Configuration for how a variant type is represented at runtime *)

type gadt_case = {
  gc_name: string;  (** Constructor name *)
  gc_tag: string;  (** The TAG value *)
  gc_payload: variant_payload;  (** Payload representation *)
  gc_result_type: ts_type;
      (** The specific result type for this constructor (e.g., t<int> for Int constructor) *)
}
(** GADT constructor - each constructor has its own result type constraint *)

(** How to import an external type *)
type ext_import_kind =
  | ExtNamed of string  (** Named import: import type { Name } from "module" *)
  | ExtDefault  (** Default import: import type LocalName from "module" *)
  | ExtNamespace
      (** Namespace import: import type * as LocalName from "module" *)

type external_type_constraint = {
  etc_name: string;  (** The external TypeScript type name or local name *)
  etc_module: string option;  (** Optional module to import from *)
  etc_use_external: bool;
      (** If true, use the external type instead of ReScript shape *)
  etc_import_kind: ext_import_kind;  (** How to import this type *)
}
(** External type constraint for validating ReScript shape against external type *)

type type_decl =
  | TypeAlias of {
      name: string;
      type_params: type_param list;
      body: ts_type;
      external_type: external_type_constraint option;
          (** Optional external type to validate against *)
      is_opaque: bool;
          (** If true, type has phantom params and should be intersected with opaque brand *)
    }
  | Interface of {
      name: string;
      type_params: type_param list;
      extends: type_ref list;
      body: object_type;
    }
  | VariantType of {
      name: string;
      type_params: type_param list;
      cases: variant_case list;
      config: variant_config;
    }
  | GadtType of {
      name: string;
      type_params: type_param list;
      cases: gadt_case list;
      config: variant_config;
    }
      (** GADT type - generates separate type for each constructor.
          For: type t<_> = Int(int): t<int> | Float(float): t<float>
          Generates: type t$Int = ...; type t$Float = ...; type t = t$Int | t$Float *)
  | OpaqueType of {
      name: string;
      type_params: type_param list;
      underlying: ts_type option;
          (** If Some, this is a branded opaque type from @opaque with underlying type.
              If None, this is a pure abstract type. *)
    }  (** Opaque abstract type - represented with unique symbol *)
  | ExternalType of {
      name: string;
      type_params: type_param list;
      external_name: string;
          (** The external TypeScript type name (or local name) *)
      external_module: string option;  (** Optional module to import from *)
      external_import_kind: ext_import_kind;  (** How to import this type *)
    }  (** Abstract external TypeScript type binding via @external *)
  | ModuleDecl of module_decl  (** Module with types and values *)

and module_decl = {
  mod_name: string;
  mod_types: type_decl list;  (** Types defined in the module (for namespace) *)
  mod_values: module_value list;  (** Values defined in the module *)
  mod_submodules: module_decl list;  (** Nested submodules *)
}
(** Module declaration - represents a ReScript module as TypeScript namespace + type *)

and module_value = {mv_name: string; mv_type: ts_type}
(** A value in a module *)

(** {1 Runtime Type Collection} *)

(** The namespace alias for runtime types import *)
let runtime_types_namespace = "rescript"

module StringSet = Set.Make (String)
module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)

(** {1 Unified Codegen State}
    
    All mutable state for TypeScript code generation is centralized in the State module.
    This makes it easy to reset all state at entry points and reason about what state exists.
    
    The State module provides submodules for different aspects of state management:
    - RuntimeTypes: tracking runtime type imports from @rescript/runtime
    - TypeModuleDeps: tracking module dependencies from type references
    - OpaqueTypes: tracking opaque/branded types for assertions
    - ExternalTypeImports: tracking @external type imports
    - LocalTypeQualifier: mapping type identifiers to qualified paths
    - ExportedTypes: tracking exported value types
    - ExportedModules: tracking exported module paths
    - Context: module name, path, and environment for code generation *)

(** Import kind for external type imports *)
type external_import_kind =
  | ImportNamed of string  (** import type \{ TypeName \} from "module" *)
  | ImportDefault of string  (** import type LocalName from "module" *)
  | ImportNamespace of string  (** import type * as LocalName from "module" *)

module State = struct
  (** {2 Internal State Record} *)

  type t = {
    (* Import tracking *)
    mutable runtime_types: StringSet.t;
    mutable type_module_deps: StringSet.t;
    mutable external_type_imports: external_import_kind list StringMap.t;
    (* Opaque type tracking *)
    mutable opaque_decls: (string * int * ts_type option) list;
    (* GADT type parameter constraints: type name -> list of (param position, constraint) *)
    mutable gadt_constraints: ts_type option list StringMap.t;
    (* Name resolution *)
    mutable local_type_qualifiers_by_stamp: string IntMap.t;
    mutable local_type_qualifiers_by_name: string list StringMap.t;
    mutable exported_types: Types.type_expr StringMap.t;
    mutable exported_modules: string StringMap.t;
    (* Context *)
    mutable module_name: string;
    mutable module_path: string list;
    mutable env: Env.t option;
  }

  let state : t =
    {
      runtime_types = StringSet.empty;
      type_module_deps = StringSet.empty;
      external_type_imports = StringMap.empty;
      opaque_decls = [];
      gadt_constraints = StringMap.empty;
      local_type_qualifiers_by_stamp = IntMap.empty;
      local_type_qualifiers_by_name = StringMap.empty;
      exported_types = StringMap.empty;
      exported_modules = StringMap.empty;
      module_name = "";
      module_path = [];
      env = None;
    }

  (** Reset all codegen state. Call this at the start of each file's code generation. *)
  let reset () =
    state.runtime_types <- StringSet.empty;
    state.type_module_deps <- StringSet.empty;
    state.external_type_imports <- StringMap.empty;
    state.opaque_decls <- [];
    state.gadt_constraints <- StringMap.empty;
    state.local_type_qualifiers_by_stamp <- IntMap.empty;
    state.local_type_qualifiers_by_name <- StringMap.empty;
    state.exported_types <- StringMap.empty;
    state.exported_modules <- StringMap.empty;
    state.module_name <- "";
    state.module_path <- [];
    state.env <- None

  (** {2 Runtime Types}
      
      Tracks runtime types that need to be imported from @rescript/runtime/types.d.ts
      (e.g., "option", "result", "list", "opaque") *)
  module RuntimeTypes = struct
    let reset () = state.runtime_types <- StringSet.empty
    let add name = state.runtime_types <- StringSet.add name state.runtime_types
    let use_opaque () = add "opaque"
    let use_external () = add "external"
    let get_used () = StringSet.elements state.runtime_types
    let has_any () = not (StringSet.is_empty state.runtime_types)
  end

  (** {2 Type Module Dependencies}
      
      Tracks module dependencies from qualified type references.
      When we see a type like Stdlib_Array.arrayLike, we need to track
      that Stdlib_Array needs to be imported. *)
  module TypeModuleDeps = struct
    let reset () = state.type_module_deps <- StringSet.empty

    let add name =
      state.type_module_deps <- StringSet.add name state.type_module_deps

    let get_used () = StringSet.elements state.type_module_deps
    let has_any () = not (StringSet.is_empty state.type_module_deps)
  end

  (** {2 Opaque Types}
      
      Tracks opaque types for rescript.opaque brand generation.
      Also tracks branded opaque types (those with @opaque attribute) for return assertions.
      Each entry is (name, type_param_count, underlying_type option).
      underlying_type is Some for branded opaque types, None for pure abstract types. *)
  module OpaqueTypes = struct
    let reset () = state.opaque_decls <- []

    let add name param_count =
      state.opaque_decls <- (name, param_count, None) :: state.opaque_decls

    let add_branded name param_count underlying =
      state.opaque_decls <-
        (name, param_count, Some underlying) :: state.opaque_decls

    let get_all () =
      List.rev_map (fun (name, count, _) -> (name, count)) state.opaque_decls

    let has_any () = state.opaque_decls <> []

    (** Check if a type name is registered as opaque.
        Checks both the exact name and mangled names (e.g., "t" matches "Email.t" -> "Email$t") *)
    let is_opaque name =
      List.exists
        (fun (opaque_name, _, _) ->
          opaque_name = name
          || String.ends_with ~suffix:("." ^ name) opaque_name
          || String.ends_with ~suffix:("$" ^ name) opaque_name)
        state.opaque_decls

    (** Get underlying type for a branded opaque type, if it exists *)
    let get_underlying name =
      List.find_map
        (fun (opaque_name, _, underlying) ->
          if
            opaque_name = name
            || String.ends_with ~suffix:("." ^ name) opaque_name
            || String.ends_with ~suffix:("$" ^ name) opaque_name
          then underlying
          else None)
        state.opaque_decls

    (** Check if a type name is a branded opaque type (has underlying type) *)
    let is_branded name =
      List.exists
        (fun (opaque_name, _, underlying) ->
          underlying <> None
          && (opaque_name = name
             || String.ends_with ~suffix:("." ^ name) opaque_name
             || String.ends_with ~suffix:("$" ^ name) opaque_name))
        state.opaque_decls

    (** Get the full brand name for a local type name.
        E.g., "t" might return "Modules.Email.t" if that's registered. *)
    let get_full_name name =
      List.find_map
        (fun (opaque_name, _, _) ->
          if
            opaque_name = name
            || String.ends_with ~suffix:("." ^ name) opaque_name
          then Some opaque_name
          else None)
        state.opaque_decls
  end

  (** {2 GADT Constraints}
      
      Tracks GADT type parameter constraints for function signature generation.
      Maps type name to list of constraint types for each type parameter position. *)
  module GadtConstraints = struct
    let reset () = state.gadt_constraints <- StringMap.empty

    (** Register constraints for a GADT type.
        @param name The type name (e.g., "t" or "expr")
        @param constraints List of constraint types for each parameter position *)
    let add name constraints =
      state.gadt_constraints <-
        StringMap.add name constraints state.gadt_constraints

    (** Get constraints for a type by name.
        Returns list of (position, constraint) pairs. *)
    let get name = StringMap.find_opt name state.gadt_constraints

    (** Get constraint for a specific type parameter position *)
    let get_constraint_at name pos =
      match StringMap.find_opt name state.gadt_constraints with
      | Some constraints -> List.nth_opt constraints pos |> Option.join
      | None -> None
  end

  (** {2 External Type Imports}
      
      Tracks external type imports from @external(("package", "Type")) attribute.
      Maps module path to list of import kinds. *)
  module ExternalTypeImports = struct
    let reset () = state.external_type_imports <- StringMap.empty

    let add ~module_path ~import_kind =
      let current =
        match StringMap.find_opt module_path state.external_type_imports with
        | Some kinds -> kinds
        | None -> []
      in
      let already_exists =
        List.exists
          (fun k ->
            match (k, import_kind) with
            | ImportNamed n1, ImportNamed n2 -> n1 = n2
            | ImportDefault n1, ImportDefault n2 -> n1 = n2
            | ImportNamespace n1, ImportNamespace n2 -> n1 = n2
            | _ -> false)
          current
      in
      if not already_exists then
        state.external_type_imports <-
          StringMap.add module_path (import_kind :: current)
            state.external_type_imports

    let get_all () =
      StringMap.fold
        (fun module_path kinds acc -> (module_path, List.rev kinds) :: acc)
        state.external_type_imports []
      |> List.rev

    let has_any () = not (StringMap.is_empty state.external_type_imports)
  end

  (** {2 Local Type Qualifier}
      
      Mapping from type identifiers (by stamp) to their qualified paths.
      Populated from type_exports to enable qualifying local type references
      in implementation code. Maps ident stamp to qualified path like "Outer.Nested.t". *)
  module LocalTypeQualifier = struct
    let reset () =
      state.local_type_qualifiers_by_stamp <- IntMap.empty;
      state.local_type_qualifiers_by_name <- StringMap.empty

    (** Add a type with its qualified path.
        @param stamp The ident stamp for the type
        @param name Simple type name (e.g., "t")
        @param qualified Full qualified path (e.g., "Outer.Nested.t") *)
    let add ~stamp ~name ~qualified =
      state.local_type_qualifiers_by_stamp <-
        IntMap.add stamp qualified state.local_type_qualifiers_by_stamp;
      let current =
        match StringMap.find_opt name state.local_type_qualifiers_by_name with
        | Some paths -> paths
        | None -> []
      in
      if not (List.mem qualified current) then
        state.local_type_qualifiers_by_name <-
          StringMap.add name (qualified :: current)
            state.local_type_qualifiers_by_name

    (** Get the qualified path for a type identifier by stamp *)
    let get_qualified_by_stamp stamp =
      IntMap.find_opt stamp state.local_type_qualifiers_by_stamp

    (** Get the qualified path for a type by name (only if unambiguous) *)
    let get_qualified_by_name name =
      match StringMap.find_opt name state.local_type_qualifiers_by_name with
      | Some [qualified] -> Some qualified
      | _ -> None

    let has_any () = not (IntMap.is_empty state.local_type_qualifiers_by_stamp)
  end

  (** {2 Exported Types}
      
      Maps exported value names to their types for annotation lookup. *)
  module ExportedTypes = struct
    let reset () = state.exported_types <- StringMap.empty

    let add name ty =
      state.exported_types <- StringMap.add name ty state.exported_types

    let find name = StringMap.find_opt name state.exported_types
  end

  (** {2 Exported Modules}
      
      Maps module names to qualified type paths for module object annotations
      (e.g., "Nested" -> "Outer.Nested"). *)
  module ExportedModules = struct
    let reset () = state.exported_modules <- StringMap.empty

    let add name qualified_path =
      state.exported_modules <-
        StringMap.add name qualified_path state.exported_modules

    let get_type_path name = StringMap.find_opt name state.exported_modules
  end

  (** {2 Context}
      
      Current context for code generation including module name, path, and environment. *)
  module Context = struct
    let set_module_name name = state.module_name <- name
    let get_module_name () = state.module_name
    let set_module_path path = state.module_path <- path
    let get_module_path () = state.module_path

    let push_module_path name = state.module_path <- state.module_path @ [name]

    let pop_module_path () =
      match List.rev state.module_path with
      | _ :: rest -> state.module_path <- List.rev rest
      | [] -> ()

    let set_env env = state.env <- Some env
    let clear_env () = state.env <- None
    let get_env () = state.env
  end
end

(** {1 State Module Aliases}
    
    For backward compatibility and convenience, we expose the State submodules
    at the top level. *)

let reset_state = State.reset

module RuntimeTypes = State.RuntimeTypes
module TypeModuleDeps = State.TypeModuleDeps
module OpaqueTypes = State.OpaqueTypes
module GadtConstraints = State.GadtConstraints
module ExternalTypeImports = State.ExternalTypeImports
module LocalTypeQualifier = State.LocalTypeQualifier
module ExportedTypes = State.ExportedTypes
module ExportedModules = State.ExportedModules

(** Context accessors exposed at top level for convenience *)
let set_module_name = State.Context.set_module_name

let get_module_name = State.Context.get_module_name
let set_module_path = State.Context.set_module_path
let get_module_path = State.Context.get_module_path
let push_module_path = State.Context.push_module_path
let pop_module_path = State.Context.pop_module_path
let set_env = State.Context.set_env
let clear_env = State.Context.clear_env
let get_env = State.Context.get_env

(** Collect all type variable names used in a ts_type *)
let rec collect_type_vars (ty : ts_type) : StringSet.t =
  match ty with
  | TypeVar name -> StringSet.singleton name
  | Array ty | Readonly ty | Promise ty -> collect_type_vars ty
  | Tuple types | Union types | Intersection types ->
    List.fold_left
      (fun acc t -> StringSet.union acc (collect_type_vars t))
      StringSet.empty types
  | Object {properties; index_sig; call_sig} ->
    let prop_vars =
      List.fold_left
        (fun acc p -> StringSet.union acc (collect_type_vars p.prop_type))
        StringSet.empty properties
    in
    let index_vars =
      match index_sig with
      | Some {index_key; index_value} ->
        StringSet.union
          (collect_type_vars index_key)
          (collect_type_vars index_value)
      | None -> StringSet.empty
    in
    let call_vars =
      match call_sig with
      | Some fn -> collect_type_vars_fn fn
      | None -> StringSet.empty
    in
    StringSet.union prop_vars (StringSet.union index_vars call_vars)
  | Function fn -> collect_type_vars_fn fn
  | TypeRef {args; _} ->
    List.fold_left
      (fun acc t -> StringSet.union acc (collect_type_vars t))
      StringSet.empty args
  | RuntimeType {rt_args; _} ->
    List.fold_left
      (fun acc t -> StringSet.union acc (collect_type_vars t))
      StringSet.empty rt_args
  | Literal _ | Any | Unknown | Never | Void | Null | Undefined | Boolean
  | Number | Bigint | String | Symbol ->
    StringSet.empty

and collect_type_vars_fn (fn : fn_type) : StringSet.t =
  let param_vars =
    List.fold_left
      (fun acc p -> StringSet.union acc (collect_type_vars p.param_type))
      StringSet.empty fn.fn_params
  in
  let rest_vars =
    match fn.fn_rest with
    | Some p -> collect_type_vars p.param_type
    | None -> StringSet.empty
  in
  let return_vars = collect_type_vars fn.fn_return in
  StringSet.union param_vars (StringSet.union rest_vars return_vars)

(** Check if a type alias has phantom type parameters (params not used in body) *)
let has_phantom_params (type_params : type_param list) (body : ts_type) : bool =
  if type_params = [] then false
  else
    let declared_params =
      List.fold_left
        (fun acc (tp : type_param) -> StringSet.add tp.tp_name acc)
        StringSet.empty type_params
    in
    let used_params = collect_type_vars body in
    (* If any declared param is not used in the body, we have phantom params *)
    not (StringSet.subset declared_params used_params)

(** Extract module name from a qualified type path.
    e.g., "Stdlib_Array.arrayLike" -> Some "Stdlib_Array"
          "arrayLike" -> None *)
let extract_module_from_type_path (path : string) : string option =
  match String.index_opt path '.' with
  | Some idx -> Some (String.sub path 0 idx)
  | None -> None

(** Forward reference for printing a ts_type (set later to break cyclic dependency) *)
let pp_ts_type_ref : (Ext_pp.t -> ts_type -> unit) ref = ref (fun _ _ -> ())

(** Print opaque type using $res.opaque<"Brand", Params, Underlying> format.
    @param brand_name The full brand name (e.g., "Email.t" or "Outer.Nested.t")
    @param type_params Type parameters for phantom type support
    @param underlying Optional underlying type; if None, only brand is used *)
let pp_opaque_type (f : Ext_pp.t) ~(brand_name : string)
    ~(type_params : type_param list) ~(underlying : ts_type option) : unit =
  RuntimeTypes.add "opaque";
  Ext_pp.string f runtime_types_namespace;
  Ext_pp.string f ".opaque<\"";
  Ext_pp.string f brand_name;
  Ext_pp.string f "\", [";
  (* Print type params as tuple for phantom type support *)
  (if type_params <> [] then
     let param_names =
       List.map (fun (tp : type_param) -> tp.tp_name) type_params
     in
     Ext_pp.string f (String.concat ", " param_names));
  Ext_pp.string f "]";
  (match underlying with
  | Some ty ->
    Ext_pp.string f ", ";
    !pp_ts_type_ref f ty
  | None -> ());
  Ext_pp.string f ">"

(** Collect runtime types and module dependencies from a ts_type *)
let rec collect_type_deps (ty : ts_type) : unit =
  match ty with
  | RuntimeType {rt_name; rt_args} ->
    RuntimeTypes.add rt_name;
    List.iter collect_type_deps rt_args
  | Array elem -> collect_type_deps elem
  | Tuple types -> List.iter collect_type_deps types
  | Object {properties; index_sig; call_sig} -> (
    List.iter (fun p -> collect_type_deps p.prop_type) properties;
    (match index_sig with
    | Some {index_key; index_value} ->
      collect_type_deps index_key;
      collect_type_deps index_value
    | None -> ());
    match call_sig with
    | Some fn -> collect_type_deps_fn fn
    | None -> ())
  | Function fn -> collect_type_deps_fn fn
  | Union types | Intersection types -> List.iter collect_type_deps types
  | TypeRef {name; args} ->
    (* Track module dependency from qualified type names *)
    (match extract_module_from_type_path name with
    | Some module_name -> TypeModuleDeps.add module_name
    | None -> ());
    List.iter collect_type_deps args
  | Readonly ty | Promise ty -> collect_type_deps ty
  | Literal _ | TypeVar _ | Any | Unknown | Never | Void | Null | Undefined
  | Boolean | Number | Bigint | String | Symbol ->
    ()

and collect_type_deps_fn (fn : fn_type) : unit =
  List.iter (fun p -> collect_type_deps p.param_type) fn.fn_params;
  (match fn.fn_rest with
  | Some p -> collect_type_deps p.param_type
  | None -> ());
  collect_type_deps fn.fn_return

(** Collect runtime types from a ts_type (legacy, calls collect_type_deps) *)
let collect_runtime_types = collect_type_deps

let collect_runtime_types_fn = collect_type_deps_fn

(** Collect type dependencies from a type declaration *)
let rec collect_type_deps_decl (decl : type_decl) : unit =
  match decl with
  | TypeAlias {body; external_type; is_opaque; _} -> (
    collect_type_deps body;
    if is_opaque then RuntimeTypes.use_opaque ();
    match external_type with
    | Some {etc_name; etc_module; etc_import_kind; _} -> (
      RuntimeTypes.use_external ();
      match etc_module with
      | Some module_path ->
        let import_kind =
          match etc_import_kind with
          | ExtNamed name -> ImportNamed name
          | ExtDefault -> ImportDefault etc_name
          | ExtNamespace -> ImportNamespace etc_name
        in
        ExternalTypeImports.add ~module_path ~import_kind
      | None -> ())
    | None -> ())
  | Interface {body; _} -> (
    List.iter (fun p -> collect_type_deps p.prop_type) body.properties;
    (match body.index_sig with
    | Some {index_key; index_value} ->
      collect_type_deps index_key;
      collect_type_deps index_value
    | None -> ());
    match body.call_sig with
    | Some fn -> collect_type_deps_fn fn
    | None -> ())
  | VariantType {cases; _} ->
    List.iter
      (fun case ->
        match case.vc_payload with
        | NoPayload -> ()
        | TuplePayload types -> List.iter collect_type_deps types
        | InlineRecord props ->
          List.iter (fun p -> collect_type_deps p.prop_type) props)
      cases
  | GadtType {name; type_params; cases; _} ->
    (* Register GADT constraints for function signature generation *)
    let constraints = List.map (fun tp -> tp.tp_constraint) type_params in
    GadtConstraints.add name constraints;
    List.iter
      (fun (case : gadt_case) ->
        (match case.gc_payload with
        | NoPayload -> ()
        | TuplePayload types -> List.iter collect_type_deps types
        | InlineRecord props ->
          List.iter (fun p -> collect_type_deps p.prop_type) props);
        collect_type_deps case.gc_result_type)
      cases
  | OpaqueType _ -> RuntimeTypes.use_opaque ()
  | ExternalType {external_name; external_module; external_import_kind; _} -> (
    (* Track external type imports for types from external packages *)
    match external_module with
    | Some module_path ->
      let import_kind =
        match external_import_kind with
        | ExtNamed name -> ImportNamed name
        | ExtDefault -> ImportDefault external_name
        | ExtNamespace -> ImportNamespace external_name
      in
      ExternalTypeImports.add ~module_path ~import_kind
    | None ->
      (* Global TypeScript types don't need imports *)
      ())
  | ModuleDecl {mod_types; mod_values; mod_submodules; _} ->
    (* Collect dependencies from module contents *)
    List.iter collect_type_deps_decl mod_types;
    List.iter (fun v -> collect_type_deps v.mv_type) mod_values;
    List.iter
      (fun sub -> collect_type_deps_decl (ModuleDecl sub))
      mod_submodules

(** Legacy alias for collect_type_deps_decl *)
let collect_runtime_types_decl = collect_type_deps_decl

(** {1 Reused types from Js_op} *)

type mutable_flag = Js_op.mutable_flag
type binop = Js_op.binop
type int_op = Js_op.int_op
type kind = Js_op.kind
type property = Js_op.property
type number = Js_op.number
type ident_info = Js_op.ident_info
type exports = Js_op.exports
type tag_info = Js_op.tag_info
type property_name = Js_op.property_name

and ident = Ident.t

(** {1 Module References} *)

and module_id = {id: ident; kind: Js_op.kind; dynamic_import: bool}

and required_modules = module_id list

and vident = Id of ident | Qualified of module_id * string option

(** {1 Core IR Types} *)

and exception_ident = ident

and for_ident = ident
and for_direction = Js_op.direction_flag
and property_map = (property_name * expression) list
and length_object = Js_op.length_object
and delim = External_arg_spec.delim = DNone | DStarJ | DNoQuotes | DBackQuotes

(** {1 Expression Descriptors} *)

and expression_desc =
  | Length of expression * length_object
  | Is_null_or_undefined of expression
  | String_append of expression * expression
  | Bool of bool
  | Typeof of expression
  | In of expression * expression
  | Js_not of expression
  | Js_bnot of expression
  | Seq of expression * expression
  | Cond of expression * expression * expression
  | Bin of binop * expression * expression
  | FlatCall of expression * expression
  | Call of expression * expression list * Js_call_info.t
  | String_index of expression * expression
  | Array_index of expression * expression
  | Tagged_template of expression * expression list * expression list
  | Static_index of expression * string * int32 option
  | New of expression * expression list option
  | Var of vident
  | Fun of {
      is_method: bool;
      params: typed_ident list;
          (** Parameters with optional type annotations *)
      body: block;
      env: Js_fun_env.t;
      return_unit: bool;
      async: bool;
      directive: string option;
      fn_type: Types.type_expr option;  (** Original ML type for conversion *)
      return_type: ts_type option;  (** Explicit return type annotation *)
      type_params: type_param list;  (** Generic type parameters *)
    }
  | Str of {delim: delim; txt: string}
  | Raw_js_code of Js_raw_info.t
  | Array of expression list * mutable_flag
  | Optional_block of expression * bool
  | Caml_block of expression list * mutable_flag * expression * tag_info
  | Caml_block_tag of expression * string
  | Number of number
  | Object of expression option * property_map
  | Undefined of {is_unit: bool}
  | Null
  | Await of expression
  | Spread of expression
  | As of expression * ts_type  (** Type assertion: expr as Type *)

and typed_ident = {ident: ident; ident_type: ts_type option}
(** Identifier with optional type annotation *)

(** {1 Statements} *)

and for_ident_expression = expression

and finish_ident_expression = expression

and case_clause = {
  switch_body: block;
  should_break: bool;
  comment: string option;
}

and string_clause = Ast_untagged_variants.tag_type * case_clause
and int_clause = int * case_clause

and statement_desc =
  | Block of block
  | Variable of variable_declaration
  | Exp of expression
  | If of expression * block * block
  | While of expression * block
  | ForRange of
      for_ident_expression option
      * finish_ident_expression
      * for_ident
      * for_direction
      * block
  | Continue
  | Break
  | Return of expression
  | Int_switch of expression * int_clause list * block option
  | String_switch of expression * string_clause list * block option
  | Throw of expression
  | Try of block * (exception_ident * block) option * block option
  | Debugger
  | TypeDecl of type_decl  (** Type declaration statement *)

and expression = {expression_desc: expression_desc; comment: string option}

and statement = {statement_desc: statement_desc; comment: string option}

and variable_declaration = {
  ident: ident;
  value: expression option;
  property: property;
  ident_info: ident_info;
  var_type: ts_type option;  (** Optional type annotation *)
}

and block = statement list

(** {1 Program Structure} *)

and program = {
  block: block;
  exports: exports;
  export_set: Set_ident.t;
  type_exports: type_decl list;  (** Exported type declarations *)
}

and deps_program = {
  program: program;
  modules: required_modules;
  side_effect: string option;
  type_imports: type_import list;  (** Type-only imports *)
}

and type_import = {
  from_module: module_id;
  type_name: string;
  type_alias: string option;  (** Local alias if renamed *)
}
(** Type import for cross-module type references *)

(** {1 Conversion from Types.type_expr} *)

(** Extract @as string from attributes, returning the renamed name or None.
    Also marks the attribute as used to prevent "unused attribute" warnings. *)
let get_as_string (attrs : Parsetree.attributes) : string option =
  let result = ref None in
  List.iter
    (fun ((attr_name, payload) as attr : Parsetree.attribute) ->
      match attr_name.txt with
      | "as" -> (
        match payload with
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ({pexp_desc = Pexp_constant (Pconst_string (s, _)); _}, _);
                _;
              };
            ] ->
          Used_attributes.mark_used_attribute attr;
          result := Some s
        | _ -> ())
      | _ -> ())
    attrs;
  !result

(** Check if type declaration has @opaque attribute.
    Marks the attribute as used to prevent "unused attribute" warnings. *)
let has_opaque_attr (attrs : Parsetree.attributes) : bool =
  List.exists
    (fun (({txt}, _) as attr : Parsetree.attribute) ->
      if txt = "opaque" then (
        Used_attributes.mark_used_attribute attr;
        true)
      else false)
    attrs

type external_type_info = {
  ext_type_name: string;  (** The TypeScript type name (or "default"/"*") *)
  ext_module_path: string option;  (** Optional module to import from *)
  ext_use_external: bool;
      (** If true, use the external type instead of ReScript shape *)
  ext_import_kind: ext_import_kind;  (** How to import this type *)
}
(** External type info from @external attribute *)

(** Helper to determine import kind from type name *)
let import_kind_of_type_name (type_name : string) : ext_import_kind =
  match type_name with
  | "default" -> ExtDefault
  | "*" -> ExtNamespace
  | name -> ExtNamed name

(** Extract @external attribute from type declaration.
    Formats:
    - @external("TypeName") -> global TypeScript type
    - @external(("package", "TypeName")) -> named import from package
    - @external(("package", "default")) -> default import from package
    - @external(("package", "*")) -> namespace import from package
    - @external(("package", "TypeName", true)) -> use external type *)
let get_external_type (attrs : Parsetree.attributes) : external_type_info option
    =
  let result = ref None in
  List.iter
    (fun ((attr_name, payload) as attr : Parsetree.attribute) ->
      match attr_name.txt with
      | "external" -> (
        match payload with
        (* @external("TypeName") *)
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ({pexp_desc = Pexp_constant (Pconst_string (s, _)); _}, _);
                _;
              };
            ] ->
          Used_attributes.mark_used_attribute attr;
          result :=
            Some
              {
                ext_type_name = s;
                ext_module_path = None;
                ext_use_external = false;
                ext_import_kind = ExtNamed s;
              }
        (* @external(("package", "TypeName")) *)
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ( {
                        pexp_desc =
                          Pexp_tuple
                            [
                              {
                                pexp_desc =
                                  Pexp_constant (Pconst_string (pkg, _));
                                _;
                              };
                              {
                                pexp_desc =
                                  Pexp_constant (Pconst_string (type_name, _));
                                _;
                              };
                            ];
                        _;
                      },
                      _ );
                _;
              };
            ] ->
          Used_attributes.mark_used_attribute attr;
          result :=
            Some
              {
                ext_type_name = type_name;
                ext_module_path = Some pkg;
                ext_use_external = false;
                ext_import_kind = import_kind_of_type_name type_name;
              }
        (* @external(("package", "TypeName", true)) *)
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ( {
                        pexp_desc =
                          Pexp_tuple
                            [
                              {
                                pexp_desc =
                                  Pexp_constant (Pconst_string (pkg, _));
                                _;
                              };
                              {
                                pexp_desc =
                                  Pexp_constant (Pconst_string (type_name, _));
                                _;
                              };
                              {
                                pexp_desc =
                                  Pexp_construct ({txt = Lident "true"; _}, None);
                                _;
                              };
                            ];
                        _;
                      },
                      _ );
                _;
              };
            ] ->
          Used_attributes.mark_used_attribute attr;
          result :=
            Some
              {
                ext_type_name = type_name;
                ext_module_path = Some pkg;
                ext_use_external = true;
                ext_import_kind = import_kind_of_type_name type_name;
              }
        | _ -> ())
      | _ -> ())
    attrs;
  !result

(** Get the @as renamed name for a type from its declaration, or None if not renamed *)
let get_type_as_name (env : Env.t) (path : Path.t) : string option =
  try
    let decl = Env.find_type path env in
    get_as_string decl.type_attributes
  with Not_found -> None

(** Compute the display name for a type path, applying @as renaming if available.
    For local type references (Pident), tries to qualify using:
    1. current_module_path if set (for code generated within a module context)
    2. LocalTypeQualifier mapping by stamp (for hoisted implementation code)
    3. LocalTypeQualifier mapping by name (if unambiguous) *)
let type_display_name (path : Path.t) : string =
  match get_env () with
  | None -> Path.name path
  | Some env -> (
    (* Split path into module prefix and type name *)
    match path with
    | Path.Pdot (prefix, _type_name, _pos) -> (
      match get_type_as_name env path with
      | Some renamed -> Path.name prefix ^ "." ^ renamed
      | None -> Path.name path)
    | Path.Pident id -> (
      let base_name =
        match get_type_as_name env path with
        | Some renamed -> renamed
        | None -> Ident.name id
      in
      (* Try to qualify local type references *)
      match get_module_path () with
      | _ :: _ as path_parts ->
        (* If we have a current module path, use it *)
        String.concat "." path_parts ^ "." ^ base_name
      | [] -> (
        (* Try to look up from LocalTypeQualifier by stamp first *)
        let stamp = Ident.binding_time id in
        match LocalTypeQualifier.get_qualified_by_stamp stamp with
        | Some qualified -> qualified
        | None -> (
          (* Fall back to name-based lookup (only if unambiguous) *)
          match LocalTypeQualifier.get_qualified_by_name base_name with
          | Some qualified -> qualified
          | None -> base_name)))
    | Path.Papply _ -> Path.name path)

(** Mapping from anonymous type variable IDs to generated names.
    This is populated by collect_type_vars and used by ts_type_of_type_expr. *)
let anon_type_var_names : (int, string) Hashtbl.t = Hashtbl.create 16

(** Convert a Path.t to a list of strings for pattern matching.
    e.g., Stdlib.Dict.t -> ["t"; "Dict"; "Stdlib"] (reversed) *)
let rec path_to_list (path : Path.t) : string list =
  match path with
  | Path.Pident id -> [Ident.name id]
  | Path.Pdot (p, s, _) -> s :: path_to_list p
  | Path.Papply _ -> []

(** Check if a type is the unit type *)
let rec is_unit_type (ty : Types.type_expr) : bool =
  match ty.Types.desc with
  | Types.Tconstr (path, [], _) -> Path.name path = "unit"
  | Types.Tlink ty | Types.Tsubst ty -> is_unit_type ty
  | _ -> false

(** Remove option wrapper from optional labeled argument type.
    For ~name: option<string>=?, we want to emit name?: string, not name?: string | undefined *)
let rec remove_option_wrapper (lbl : Asttypes.arg_label) (ty : Types.type_expr)
    : Types.type_expr option =
  match (ty.Types.desc, lbl) with
  | Types.Tconstr (Path.Pident id, [inner], _), Asttypes.Optional _
    when Ident.name id = "option" ->
    Some inner
  | Types.Tlink t, _ | Types.Tsubst t, _ -> remove_option_wrapper lbl t
  | _ -> None

(** {1 Polymorphic Variant Processing} *)

type polyvar_field_info = {
  pv_nullary: string list;  (** Nullary constructors (no payload) *)
  pv_with_payload: (string * Types.type_expr) list;
      (** Constructors with payload *)
  pv_has_unknown: bool;  (** Has unknown/absent fields *)
}
(** Result of processing polymorphic variant row fields *)

(** Process polymorphic variant row fields into categorized lists *)
let process_polyvar_fields (row_fields : (string * Types.row_field) list) :
    polyvar_field_info =
  let rec loop ~nullary ~with_payload ~has_unknown fields =
    match fields with
    | (label, Types.Rpresent None) :: rest ->
      loop ~nullary:(label :: nullary) ~with_payload ~has_unknown rest
    | (label, Types.Reither (true, [], _, _)) :: rest ->
      loop ~nullary:(label :: nullary) ~with_payload ~has_unknown rest
    | (label, Types.Rpresent (Some payload)) :: rest ->
      loop ~nullary
        ~with_payload:((label, payload) :: with_payload)
        ~has_unknown rest
    | (_, (Types.Rabsent | Types.Reither _)) :: rest ->
      loop ~nullary ~with_payload ~has_unknown:true rest
    | [] ->
      {
        pv_nullary = List.rev nullary;
        pv_with_payload = List.rev with_payload;
        pv_has_unknown = has_unknown;
      }
  in
  loop ~nullary:[] ~with_payload:[] ~has_unknown:false row_fields

(** Convert ML type expression to TypeScript type.
    This is the core function for type annotation generation.
    Note: Set current_env before calling this for @as renaming support. *)
let rec ts_type_of_type_expr (ty : Types.type_expr) : ts_type =
  match ty.desc with
  | Tvar None -> (
    (* Look up the generated name for this anonymous type variable *)
    match Hashtbl.find_opt anon_type_var_names ty.id with
    | Some name -> TypeVar name
    | None -> Any (* Fallback if not collected *))
  | Tvar (Some name) -> TypeVar (String.capitalize_ascii name)
  | Tarrow ({lbl; typ = arg_type}, return_type, _, _) -> (
    (* Check if this is a unit parameter that should be skipped *)
    let is_unit_param = lbl = Nolabel && is_unit_type arg_type in
    (* For optional labeled params, unwrap option<T> to just T *)
    let actual_type =
      match lbl with
      | Optional _ -> (
        match remove_option_wrapper lbl arg_type with
        | Some inner -> inner
        | None -> arg_type)
      | _ -> arg_type
    in
    let param =
      if is_unit_param then None
      else
        Some
          {
            param_name =
              (match lbl with
              | Nolabel -> None
              | Labelled {txt} | Optional {txt} -> Some txt);
            param_type = ts_type_of_type_expr actual_type;
            param_optional =
              (match lbl with
              | Optional _ -> true
              | _ -> false);
          }
    in
    (* Flatten nested arrows into a single function type *)
    match ts_type_of_type_expr return_type with
    | Function {fn_params; fn_rest; fn_return; fn_type_params; fn_async} ->
      Function
        {
          fn_params =
            (match param with
            | Some p -> p :: fn_params
            | None -> fn_params);
          fn_rest;
          fn_return;
          fn_type_params;
          fn_async = false;
        }
    | return_ts ->
      Function
        {
          fn_params =
            (match param with
            | Some p -> [p]
            | None -> []);
          fn_rest = None;
          fn_return = return_ts;
          fn_type_params = [];
          fn_async = false;
        })
  | Ttuple types -> Tuple (List.map ts_type_of_type_expr types)
  | Tconstr (path, args, _) -> ts_type_of_constr path args
  | Tlink ty | Tsubst ty -> ts_type_of_type_expr ty
  | Tpoly (ty, type_vars) -> (
    (* Polymorphic type with locally abstract types: type a. t<a> => unit
       Extract type variables and add them to the function's type params *)
    match ts_type_of_type_expr ty with
    | Function fn when type_vars <> [] ->
      (* Extract type parameter names from the locally abstract types *)
      let extra_type_params =
        List.filter_map
          (fun tv ->
            match tv.Types.desc with
            | Types.Tvar (Some name) ->
              Some
                {
                  tp_name = String.capitalize_ascii name;
                  tp_constraint = None;
                  tp_default = None;
                }
            | Types.Tvar None ->
              (* Anonymous type var - generate a name *)
              Some {tp_name = "T"; tp_constraint = None; tp_default = None}
            | _ -> None)
          type_vars
      in
      Function {fn with fn_type_params = extra_type_params @ fn.fn_type_params}
    | other -> other)
  | Tvariant row_desc -> (
    (* Polymorphic variants *)
    let info = process_polyvar_fields row_desc.row_fields in
    if info.pv_has_unknown then Any
    else
      let nullary_types =
        List.map (fun label -> Literal (LitString label)) info.pv_nullary
      in
      let payload_types =
        List.map
          (fun (label, payload) ->
            (* Polymorphic variant with payload: { readonly NAME: "label"; readonly VAL: payload } *)
            (Object
               {
                 properties =
                   [
                     {
                       prop_name = Literals.polyvar_hash;
                       prop_type = Literal (LitString label);
                       prop_optional = false;
                       prop_readonly = true;
                     };
                     {
                       prop_name = Literals.polyvar_value;
                       prop_type = ts_type_of_type_expr payload;
                       prop_optional = false;
                       prop_readonly = true;
                     };
                   ];
                 index_sig = None;
                 call_sig = None;
               }
              : ts_type))
          info.pv_with_payload
      in
      match nullary_types @ payload_types with
      | [] -> Never
      | [single] -> single
      | types -> Union types)
  | Tobject (t_obj, _) ->
    (* Object types - extract fields from Tfield chain *)
    let rec get_fields (ty : Types.type_expr) : (string * ts_type) list =
      match ty.Types.desc with
      | Types.Tfield (name, _, field_type, rest) ->
        (* Skip internal fields starting with special characters *)
        if String.length name > 0 && name.[0] = '#' then get_fields rest
        else (name, ts_type_of_type_expr field_type) :: get_fields rest
      | Types.Tlink ty | Types.Tsubst ty -> get_fields ty
      | Types.Tnil -> []
      | _ -> [] (* Open object type - stop here *)
    in
    let fields = get_fields t_obj in
    Object
      {
        properties =
          List.map
            (fun (name, ty) ->
              {
                prop_name = name;
                prop_type = ty;
                prop_optional = false;
                prop_readonly = false;
              })
            fields;
        index_sig = None;
        call_sig = None;
      }
  | Tfield _ | Tnil -> Any
  | Tunivar None -> (
    (* Look up the generated name for this anonymous type variable *)
    match Hashtbl.find_opt anon_type_var_names ty.id with
    | Some name -> TypeVar name
    | None -> Any (* Fallback if not collected *))
  | Tunivar (Some name) -> TypeVar (String.capitalize_ascii name)
  | Tpackage _ -> Any

and ts_type_of_constr (path : Path.t) (args : Types.type_expr list) : ts_type =
  (* Handle built-in types specially using path list for better matching *)
  let path_list = path_to_list path in
  match (List.rev path_list, args) with
  | (["int"] | ["float"]), [] -> Number
  | ["bool"], [] -> Boolean
  | ( ( ["string"]
      | ["String"; "t"]
      | ["Js"; "String"; "t"]
      | ["Js"; "String2"; "t"] ),
      [] ) ->
    String
  | ["unit"], [] -> Void
  | (["bigint"] | ["BigInt"; "t"] | ["Js"; "Types"; "bigint_val"]), [] -> Bigint
  | (["symbol"] | ["Symbol"; "t"]), [] -> Symbol
  | (["Js"; "Date"; "t"] | ["Date"; "t"]), [] ->
    TypeRef {name = "Date"; args = []}
  | (["Js"; "Re"; "t"] | ["RegExp"; "t"]), [] ->
    TypeRef {name = "RegExp"; args = []}
  | (["Js"; "Exn"; "t"] | ["Error"; "t"] | ["JsError"; "t"]), [] ->
    TypeRef {name = "Error"; args = []}
  | (["array"] | ["Array"; "t"]), [elem] -> Array (ts_type_of_type_expr elem)
  | ( ( ["dict"]
      | ["Dict"; "t"]
      | ["Stdlib"; "Dict"; "t"]
      | ["Js"; "Dict"; "t"]
      | ["Js_dict"; "t"] ),
      [value] ) ->
    RuntimeType {rt_name = "dict"; rt_args = [ts_type_of_type_expr value]}
  | ["Map"; "t"], [key; value] ->
    TypeRef
      {
        name = "Map";
        args = [ts_type_of_type_expr key; ts_type_of_type_expr value];
      }
  | ["WeakMap"; "t"], [key; value] ->
    TypeRef
      {
        name = "WeakMap";
        args = [ts_type_of_type_expr key; ts_type_of_type_expr value];
      }
  | ["Set"; "t"], [elem] ->
    TypeRef {name = "Set"; args = [ts_type_of_type_expr elem]}
  | ["WeakSet"; "t"], [elem] ->
    TypeRef {name = "WeakSet"; args = [ts_type_of_type_expr elem]}
  | ( ( ["option"]
      | ["Option"; "t"]
      | ["Stdlib"; "Option"; "t"]
      | ["Js"; "undefined"]
      | ["Js"; "Undefined"; "t"]
      | ["Js_undefined"; "t"] ),
      [elem] ) ->
    RuntimeType {rt_name = "option"; rt_args = [ts_type_of_type_expr elem]}
  | ( ( ["null"]
      | ["Null"; "t"]
      | ["Stdlib"; "Null"; "t"]
      | ["Js"; "null"]
      | ["Js"; "Null"; "t"]
      | ["Js_null"; "t"] ),
      [elem] ) ->
    RuntimeType {rt_name = "null_"; rt_args = [ts_type_of_type_expr elem]}
  | ( ( ["nullable"]
      | ["Nullable"; "t"]
      | ["Stdlib"; "Nullable"; "t"]
      | ["Js"; "nullable"]
      | ["Js"; "null_undefined"]
      | ["Js"; "Nullable"; "t"]
      | ["Js_nullable"; "t"]
      | ["Js_null_undefined"; "t"] ),
      [elem] ) ->
    RuntimeType {rt_name = "nullable"; rt_args = [ts_type_of_type_expr elem]}
  | (["promise"] | ["Promise"; "t"] | ["Js"; "Promise"; "t"]), [elem] ->
    Promise (ts_type_of_type_expr elem)
  | (["promise"] | ["Promise"; "t"] | ["Js"; "Promise"; "t"]), [] -> Promise Any
  | ["Iterator"; "t"], [elem] ->
    TypeRef {name = "Iterator"; args = [ts_type_of_type_expr elem]}
  | ["AsyncIterator"; "t"], [elem] ->
    TypeRef {name = "AsyncIterator"; args = [ts_type_of_type_expr elem]}
  | (["result"] | ["Result"; "t"] | ["Stdlib"; "Result"; "t"]), [ok; err] ->
    RuntimeType
      {
        rt_name = "result";
        rt_args = [ts_type_of_type_expr ok; ts_type_of_type_expr err];
      }
  | ( (["ref"] | ["Ref"; "t"] | ["Stdlib"; "Ref"; "t"] | ["Pervasives"; "ref"]),
      [elem] ) ->
    RuntimeType {rt_name = "ref"; rt_args = [ts_type_of_type_expr elem]}
  | (["list"] | ["List"; "t"] | ["Stdlib"; "List"; "t"]), [elem] ->
    RuntimeType {rt_name = "list"; rt_args = [ts_type_of_type_expr elem]}
  | (["Js"; "Json"; "t"] | ["JSON"; "t"]), [] -> Unknown
  | (["Obj"; "t"] | ["Primitive_object"; "t"]), [] -> Any
  | ["Js"; "t"], [inner] -> ts_type_of_type_expr inner
  | _ ->
    (* Generic type reference with @as renaming applied *)
    TypeRef
      {name = type_display_name path; args = List.map ts_type_of_type_expr args}

(** {1 Utility Functions} *)

(** Create a typed identifier from an ident and optional ML type *)
let typed_ident_of_ident ?(typ : Types.type_expr option) (id : ident) :
    typed_ident =
  {ident = id; ident_type = Option.map ts_type_of_type_expr typ}

(** Create typed identifiers from a list of idents, extracting types from a function type *)
let typed_idents_of_params (params : ident list)
    (fn_type : Types.type_expr option) : typed_ident list =
  match fn_type with
  | None -> List.map (fun id -> {ident = id; ident_type = None}) params
  | Some ty ->
    let rec extract_param_types ty acc =
      match ty.Types.desc with
      | Types.Tarrow ({typ = arg_type; _}, return_type, _, _) ->
        extract_param_types return_type (ts_type_of_type_expr arg_type :: acc)
      | Types.Tlink ty | Types.Tsubst ty -> extract_param_types ty acc
      | _ -> List.rev acc
    in
    let param_types = extract_param_types ty [] in
    let num_params = List.length params in
    let num_types = List.length param_types in
    let adjusted_types =
      if num_types >= num_params then
        (* Take only as many types as we have params *)
        let rec take n lst =
          match (n, lst) with
          | 0, _ | _, [] -> []
          | n, x :: xs -> x :: take (n - 1) xs
        in
        take num_params param_types
      else
        (* Pad with Any if we have fewer types than params *)
        param_types @ List.init (num_params - num_types) (fun _ -> Any)
    in
    List.map2
      (fun id typ -> {ident = id; ident_type = Some typ})
      params adjusted_types

(** Extract return type from a function type expression *)
let return_type_of_fn_type (fn_type : Types.type_expr option) : ts_type option =
  match fn_type with
  | None -> None
  | Some ty ->
    let rec find_return ty =
      match ty.Types.desc with
      | Types.Tarrow (_, return_type, _, _) -> find_return return_type
      | Types.Tlink ty | Types.Tsubst ty | Types.Tpoly (ty, _) -> find_return ty
      | _ -> ts_type_of_type_expr ty
    in
    Some (find_return ty)

(** Extract the raw return type expression from a function type.
    Used for opaque type assertions on return statements. *)
let return_type_expr_of_fn_type (fn_type : Types.type_expr option) :
    Types.type_expr option =
  match fn_type with
  | None -> None
  | Some ty ->
    let rec find_return ty =
      match ty.Types.desc with
      | Types.Tarrow (_, return_type, _, _) -> find_return return_type
      | Types.Tlink ty | Types.Tsubst ty | Types.Tpoly (ty, _) -> find_return ty
      | _ -> ty
    in
    Some (find_return ty)

(** Generate a type variable name from an index (0 -> "A", 1 -> "B", etc.) *)
let type_var_name_of_index (i : int) : string =
  if i < 26 then String.make 1 (Char.chr (Char.code 'A' + i))
  else Printf.sprintf "T%d" (i - 26)

(** Collect all type variables from a type expression.
    Returns the list of type variable names (both named and anonymous).
    Also populates anon_type_var_names for anonymous type variables. *)
let collect_type_vars (fn_type : Types.type_expr option) : string list =
  Hashtbl.clear anon_type_var_names;
  match fn_type with
  | None -> []
  | Some ty ->
    let vars = ref [] in
    let seen_named = Hashtbl.create 16 in
    let seen_anon = Hashtbl.create 16 in
    let anon_counter = ref 0 in
    let rec collect ty =
      match ty.Types.desc with
      | Types.Tvar (Some name) ->
        if not (Hashtbl.mem seen_named name) then (
          Hashtbl.add seen_named name ();
          vars := String.capitalize_ascii name :: !vars)
      | Types.Tvar None ->
        if not (Hashtbl.mem seen_anon ty.Types.id) then (
          Hashtbl.add seen_anon ty.Types.id ();
          let name = type_var_name_of_index !anon_counter in
          incr anon_counter;
          Hashtbl.add anon_type_var_names ty.Types.id name;
          vars := name :: !vars)
      | Types.Tarrow ({typ = arg_type; _}, return_type, _, _) ->
        collect arg_type;
        collect return_type
      | Types.Ttuple types -> List.iter collect types
      | Types.Tconstr (_, args, _) -> List.iter collect args
      | Types.Tlink ty | Types.Tsubst ty -> collect ty
      | Types.Tpoly (ty, type_vars) ->
        (* Extract locally abstract type variables from Tpoly *)
        List.iter
          (fun tv ->
            match tv.Types.desc with
            | Types.Tunivar (Some name) | Types.Tvar (Some name) ->
              if not (Hashtbl.mem seen_named name) then (
                Hashtbl.add seen_named name ();
                vars := String.capitalize_ascii name :: !vars)
            | Types.Tunivar None | Types.Tvar None ->
              if not (Hashtbl.mem seen_anon tv.Types.id) then (
                Hashtbl.add seen_anon tv.Types.id ();
                let name = type_var_name_of_index !anon_counter in
                incr anon_counter;
                Hashtbl.add anon_type_var_names tv.Types.id name;
                vars := name :: !vars)
            | _ -> ())
          type_vars;
        collect ty
      | Types.Tunivar (Some name) ->
        (* Tunivar can appear in locally abstract types *)
        if not (Hashtbl.mem seen_named name) then (
          Hashtbl.add seen_named name ();
          vars := String.capitalize_ascii name :: !vars)
      | Types.Tunivar None ->
        if not (Hashtbl.mem seen_anon ty.Types.id) then (
          Hashtbl.add seen_anon ty.Types.id ();
          let name = type_var_name_of_index !anon_counter in
          incr anon_counter;
          Hashtbl.add anon_type_var_names ty.Types.id name;
          vars := name :: !vars)
      | _ -> ()
    in
    collect ty;
    List.rev !vars

(** Collect type variables with GADT constraints from a type expression.
    For each type variable, looks up constraints from GADT types it's used with.
    Returns list of type_param with constraints. *)
let collect_type_vars_with_constraints (fn_type : Types.type_expr option) :
    type_param list =
  Hashtbl.clear anon_type_var_names;
  match fn_type with
  | None -> []
  | Some ty ->
    (* Map from type var name to constraint *)
    let var_constraints : ts_type option StringMap.t ref =
      ref StringMap.empty
    in
    let vars = ref [] in
    let seen_named = Hashtbl.create 16 in
    let seen_anon = Hashtbl.create 16 in
    let anon_counter = ref 0 in
    (* Get var name for a type expression *)
    let get_var_name tv =
      match tv.Types.desc with
      | Types.Tvar (Some name) | Types.Tunivar (Some name) ->
        Some (String.capitalize_ascii name)
      | Types.Tvar None | Types.Tunivar None -> (
        match Hashtbl.find_opt anon_type_var_names tv.Types.id with
        | Some name -> Some name
        | None -> None)
      | _ -> None
    in
    let rec collect ty =
      match ty.Types.desc with
      | Types.Tvar (Some name) ->
        if not (Hashtbl.mem seen_named name) then (
          Hashtbl.add seen_named name ();
          vars := String.capitalize_ascii name :: !vars)
      | Types.Tvar None ->
        if not (Hashtbl.mem seen_anon ty.Types.id) then (
          Hashtbl.add seen_anon ty.Types.id ();
          let name = type_var_name_of_index !anon_counter in
          incr anon_counter;
          Hashtbl.add anon_type_var_names ty.Types.id name;
          vars := name :: !vars)
      | Types.Tarrow ({typ = arg_type; _}, return_type, _, _) ->
        collect arg_type;
        collect return_type
      | Types.Ttuple types -> List.iter collect types
      | Types.Tconstr (path, args, _) ->
        (* Check if any args are type vars and look up constraints *)
        let type_name = Path.last path in
        (match GadtConstraints.get type_name with
        | Some constraints ->
          List.iteri
            (fun i arg ->
              match get_var_name arg with
              | Some var_name -> (
                match List.nth_opt constraints i |> Option.join with
                | Some constraint_ty ->
                  (* Merge constraints: if var already has constraint, union them *)
                  let existing =
                    StringMap.find_opt var_name !var_constraints |> Option.join
                  in
                  let merged =
                    match existing with
                    | None -> constraint_ty
                    | Some (Union types) -> Union (constraint_ty :: types)
                    | Some existing_ty ->
                      if existing_ty = constraint_ty then existing_ty
                      else Union [existing_ty; constraint_ty]
                  in
                  var_constraints :=
                    StringMap.add var_name (Some merged) !var_constraints
                | None -> ())
              | None -> ())
            args
        | None -> ());
        List.iter collect args
      | Types.Tlink ty | Types.Tsubst ty -> collect ty
      | Types.Tpoly (ty, type_vars) ->
        (* Extract locally abstract type variables from Tpoly *)
        List.iter
          (fun tv ->
            match tv.Types.desc with
            | Types.Tunivar (Some name) | Types.Tvar (Some name) ->
              if not (Hashtbl.mem seen_named name) then (
                Hashtbl.add seen_named name ();
                vars := String.capitalize_ascii name :: !vars)
            | Types.Tunivar None | Types.Tvar None ->
              if not (Hashtbl.mem seen_anon tv.Types.id) then (
                Hashtbl.add seen_anon tv.Types.id ();
                let name = type_var_name_of_index !anon_counter in
                incr anon_counter;
                Hashtbl.add anon_type_var_names tv.Types.id name;
                vars := name :: !vars)
            | _ -> ())
          type_vars;
        collect ty
      | Types.Tunivar (Some name) ->
        if not (Hashtbl.mem seen_named name) then (
          Hashtbl.add seen_named name ();
          vars := String.capitalize_ascii name :: !vars)
      | Types.Tunivar None ->
        if not (Hashtbl.mem seen_anon ty.Types.id) then (
          Hashtbl.add seen_anon ty.Types.id ();
          let name = type_var_name_of_index !anon_counter in
          incr anon_counter;
          Hashtbl.add anon_type_var_names ty.Types.id name;
          vars := name :: !vars)
      | _ -> ()
    in
    collect ty;
    (* Convert to type_param list with constraints *)
    List.rev_map
      (fun name ->
        {
          tp_name = name;
          tp_constraint =
            StringMap.find_opt name !var_constraints |> Option.join;
          tp_default = None;
        })
      !vars

(** {1 Type Declaration Extraction} *)

(** Extract type arguments from a GADT constructor result type.
    For a result type like t<int, string>, returns [int_ts_type, string_ts_type].
    The result type is expected to be a Tconstr with the same path as the GADT itself. *)
let extract_gadt_type_args (res_ty : Types.type_expr) : ts_type list =
  let rec unwrap ty =
    match ty.Types.desc with
    | Types.Tlink t | Types.Tsubst t -> unwrap t
    | Types.Tconstr (_, args, _) -> List.map ts_type_of_type_expr args
    | _ -> []
  in
  unwrap res_ty

(** Compute constraints for GADT type parameters from constructor result types.
    For each type parameter position, collects all the types that appear at that position
    across all constructors, then creates a union constraint.
    E.g., for t<_> with Int: t<int> and Str: t<string>, returns [Some (number | string)] *)
let compute_gadt_constraints (constructors : Types.constructor_declaration list)
    (num_params : int) : ts_type option list =
  if num_params = 0 then []
  else
    (* Collect type arguments from each constructor *)
    let all_args =
      List.filter_map
        (fun (cd : Types.constructor_declaration) ->
          match cd.cd_res with
          | Some res_ty ->
            let args = extract_gadt_type_args res_ty in
            if List.length args = num_params then Some args else None
          | None -> None)
        constructors
    in
    (* For each parameter position, collect all types and create union *)
    List.init num_params (fun i ->
        let types_at_pos =
          List.filter_map (fun args -> List.nth_opt args i) all_args
        in
        (* Remove duplicates and create union if multiple types *)
        let unique_types =
          List.fold_left
            (fun acc ty ->
              if List.exists (fun t -> t = ty) acc then acc else ty :: acc)
            [] types_at_pos
          |> List.rev
        in
        match unique_types with
        | [] -> None
        | [single] -> Some single
        | multiple -> Some (Union multiple))

(** Convert type parameters from Types.type_expr list to type_param list *)
let type_params_of_type_exprs (params : Types.type_expr list) : type_param list
    =
  List.mapi
    (fun i param ->
      match param.Types.desc with
      | Types.Tvar (Some name) ->
        {
          tp_name = String.capitalize_ascii name;
          tp_constraint = None;
          tp_default = None;
        }
      | Types.Tvar None ->
        {
          tp_name = Printf.sprintf "T%d" i;
          tp_constraint = None;
          tp_default = None;
        }
      | _ ->
        {
          tp_name = Printf.sprintf "T%d" i;
          tp_constraint = None;
          tp_default = None;
        })
    params

(** Convert type parameters with GADT constraints *)
let type_params_of_type_exprs_with_constraints (params : Types.type_expr list)
    (constraints : ts_type option list) : type_param list =
  List.mapi
    (fun i param ->
      let constraint_opt =
        match List.nth_opt constraints i with
        | Some c -> c
        | None -> None
      in
      match param.Types.desc with
      | Types.Tvar (Some name) ->
        {
          tp_name = String.capitalize_ascii name;
          tp_constraint = constraint_opt;
          tp_default = None;
        }
      | Types.Tvar None ->
        {
          tp_name = Printf.sprintf "T%d" i;
          tp_constraint = constraint_opt;
          tp_default = None;
        }
      | _ ->
        {
          tp_name = Printf.sprintf "T%d" i;
          tp_constraint = constraint_opt;
          tp_default = None;
        })
    params

(** Extract a type declaration from Types.type_declaration *)
let type_decl_of_type_declaration (id : Ident.t) (decl : Types.type_declaration)
    : type_decl option =
  let type_params = type_params_of_type_exprs decl.type_params in
  let type_name =
    match get_as_string decl.type_attributes with
    | Some renamed -> renamed
    | None -> Ident.name id
  in
  (* Check for @external attribute upfront for types with shapes *)
  let external_type_info = get_external_type decl.type_attributes in
  match decl.type_kind with
  | Types.Type_record (labels, _) -> (
    (* Record type -> Interface or TypeAlias with external validation *)
    let properties =
      List.map
        (fun (ld : Types.label_declaration) ->
          let name =
            match get_as_string ld.ld_attributes with
            | Some renamed -> renamed
            | None -> Ident.name ld.ld_id
          in
          {
            prop_name = name;
            prop_type = ts_type_of_type_expr ld.ld_type;
            prop_optional = ld.ld_optional;
            prop_readonly = ld.ld_mutable = Asttypes.Immutable;
          })
        labels
    in
    let body = make_object_type ~properties ~index_sig:None ~call_sig:None in
    match external_type_info with
    | Some {ext_type_name; ext_module_path; ext_use_external; ext_import_kind}
      ->
      (* Record with @external - generate TypeAlias with validation *)
      (* For default/namespace imports, use mangled name with $ prefix *)
      let etc_name =
        match ext_import_kind with
        | ExtDefault | ExtNamespace -> "$" ^ type_name
        | ExtNamed _ -> ext_type_name
      in
      Some
        (TypeAlias
           {
             name = type_name;
             type_params;
             body;
             external_type =
               Some
                 {
                   etc_name;
                   etc_module = ext_module_path;
                   etc_use_external = ext_use_external;
                   etc_import_kind = ext_import_kind;
                 };
             is_opaque = false;
           })
    | None ->
      (* Regular record - generate Interface *)
      Some
        (Interface
           {
             name = type_name;
             type_params;
             extends = [];
             body = {properties; index_sig = None; call_sig = None};
           }))
  | Types.Type_variant constructors -> (
    (* Check if this variant has a manifest that maps to a runtime type.
       For example: type t<'a> = option<'a> = None | Some('a)
       In this case, we should generate a type alias to the runtime type
       instead of the full variant definition. *)
    match decl.type_manifest with
    | Some manifest_ty -> (
      let ts_ty = ts_type_of_type_expr manifest_ty in
      match ts_ty with
      | RuntimeType _ ->
        (* This variant re-exports a runtime type, generate alias instead *)
        Some
          (TypeAlias
             {
               name = type_name;
               type_params;
               body = ts_ty;
               external_type = None;
               is_opaque = false;
             })
      | _ ->
        (* Manifest exists but isn't a runtime type - generate full variant *)
        let is_unboxed =
          Ast_untagged_variants.has_untagged decl.type_attributes
        in
        let tag_name =
          match Ast_untagged_variants.process_tag_name decl.type_attributes with
          | Some custom_tag -> custom_tag
          | None -> Js_dump_lit.tag
        in
        let config = {vc_unboxed = is_unboxed; vc_tag_name = tag_name} in
        let cases =
          List.map
            (fun (cd : Types.constructor_declaration) ->
              let name = Ident.name cd.cd_id in
              let tag =
                match
                  Ast_untagged_variants.process_tag_type cd.cd_attributes
                with
                | Some (Ast_untagged_variants.String s) -> s
                | Some (Ast_untagged_variants.Int i) -> string_of_int i
                | _ -> (
                  match get_as_string cd.cd_attributes with
                  | Some renamed -> renamed
                  | None -> name)
              in
              let payload =
                match cd.cd_args with
                | Cstr_tuple [] -> NoPayload
                | Cstr_tuple [arg] -> TuplePayload [ts_type_of_type_expr arg]
                | Cstr_tuple args ->
                  TuplePayload (List.map ts_type_of_type_expr args)
                | Cstr_record labels ->
                  let properties =
                    List.map
                      (fun (ld : Types.label_declaration) ->
                        let field_name =
                          match get_as_string ld.ld_attributes with
                          | Some renamed -> renamed
                          | None -> Ident.name ld.ld_id
                        in
                        {
                          prop_name = field_name;
                          prop_type = ts_type_of_type_expr ld.ld_type;
                          prop_optional = ld.ld_optional;
                          prop_readonly = ld.ld_mutable = Asttypes.Immutable;
                        })
                      labels
                  in
                  InlineRecord properties
              in
              {vc_name = name; vc_tag = tag; vc_payload = payload})
            constructors
        in
        Some (VariantType {name = type_name; type_params; cases; config}))
    | None ->
      (* Check if this is a GADT - any constructor has a result type annotation *)
      let is_gadt =
        List.exists
          (fun (cd : Types.constructor_declaration) -> cd.cd_res <> None)
          constructors
      in
      let is_unboxed =
        Ast_untagged_variants.has_untagged decl.type_attributes
      in
      let tag_name =
        match Ast_untagged_variants.process_tag_name decl.type_attributes with
        | Some custom_tag -> custom_tag
        | None -> Js_dump_lit.tag
      in
      let config = {vc_unboxed = is_unboxed; vc_tag_name = tag_name} in
      if is_gadt then (
        (* Generate GADT type with constructor-specific result types *)
        (* First compute constraints for type parameters *)
        let num_params = List.length decl.type_params in
        let constraints = compute_gadt_constraints constructors num_params in
        (* Register constraints for function signature generation *)
        GadtConstraints.add type_name constraints;
        let type_params_with_constraints =
          type_params_of_type_exprs_with_constraints decl.type_params
            constraints
        in
        let cases =
          List.map
            (fun (cd : Types.constructor_declaration) ->
              let name = Ident.name cd.cd_id in
              let tag =
                match
                  Ast_untagged_variants.process_tag_type cd.cd_attributes
                with
                | Some (Ast_untagged_variants.String s) -> s
                | Some (Ast_untagged_variants.Int i) -> string_of_int i
                | _ -> (
                  match get_as_string cd.cd_attributes with
                  | Some renamed -> renamed
                  | None -> name)
              in
              let payload =
                match cd.cd_args with
                | Cstr_tuple [] -> NoPayload
                | Cstr_tuple [arg] -> TuplePayload [ts_type_of_type_expr arg]
                | Cstr_tuple args ->
                  TuplePayload (List.map ts_type_of_type_expr args)
                | Cstr_record labels ->
                  let properties =
                    List.map
                      (fun (ld : Types.label_declaration) ->
                        let field_name =
                          match get_as_string ld.ld_attributes with
                          | Some renamed -> renamed
                          | None -> Ident.name ld.ld_id
                        in
                        {
                          prop_name = field_name;
                          prop_type = ts_type_of_type_expr ld.ld_type;
                          prop_optional = ld.ld_optional;
                          prop_readonly = ld.ld_mutable = Asttypes.Immutable;
                        })
                      labels
                  in
                  InlineRecord properties
              in
              (* For GADT, get the result type from cd_res *)
              let result_type =
                match cd.cd_res with
                | Some res_ty -> ts_type_of_type_expr res_ty
                | None ->
                  (* Constructor without result type annotation - use base type *)
                  TypeRef
                    {
                      name = type_name;
                      args =
                        List.map
                          (fun tp -> TypeVar tp.tp_name)
                          type_params_with_constraints;
                    }
              in
              {
                gc_name = name;
                gc_tag = tag;
                gc_payload = payload;
                gc_result_type = result_type;
              })
            constructors
        in
        Some
          (GadtType
             {
               name = type_name;
               type_params = type_params_with_constraints;
               cases;
               config;
             }))
      else
        (* Regular variant type *)
        let cases =
          List.map
            (fun (cd : Types.constructor_declaration) ->
              let name = Ident.name cd.cd_id in
              (* Use @as name for the tag if present, otherwise use the constructor name *)
              let tag =
                match
                  Ast_untagged_variants.process_tag_type cd.cd_attributes
                with
                | Some (Ast_untagged_variants.String s) -> s
                | Some (Ast_untagged_variants.Int i) -> string_of_int i
                | _ -> (
                  (* Fall back to @as or constructor name *)
                  match get_as_string cd.cd_attributes with
                  | Some renamed -> renamed
                  | None -> name)
              in
              let payload =
                match cd.cd_args with
                | Cstr_tuple [] -> NoPayload
                | Cstr_tuple [arg] -> TuplePayload [ts_type_of_type_expr arg]
                | Cstr_tuple args ->
                  TuplePayload (List.map ts_type_of_type_expr args)
                | Cstr_record labels ->
                  (* Inline record - flatten fields into the variant object *)
                  let properties =
                    List.map
                      (fun (ld : Types.label_declaration) ->
                        let field_name =
                          match get_as_string ld.ld_attributes with
                          | Some renamed -> renamed
                          | None -> Ident.name ld.ld_id
                        in
                        {
                          prop_name = field_name;
                          prop_type = ts_type_of_type_expr ld.ld_type;
                          prop_optional = ld.ld_optional;
                          prop_readonly = ld.ld_mutable = Asttypes.Immutable;
                        })
                      labels
                  in
                  InlineRecord properties
              in
              {vc_name = name; vc_tag = tag; vc_payload = payload})
            constructors
        in
        Some (VariantType {name = type_name; type_params; cases; config}))
  | Types.Type_abstract -> (
    let external_type_info = get_external_type decl.type_attributes in
    let is_opaque_attr = has_opaque_attr decl.type_attributes in
    match decl.type_manifest with
    | Some ty when is_opaque_attr ->
      (* @opaque type t = string - branded opaque type with underlying type *)
      let body = ts_type_of_type_expr ty in
      Some (OpaqueType {name = type_name; type_params; underlying = Some body})
    | Some ty ->
      let external_type =
        match external_type_info with
        | Some
            {ext_type_name; ext_module_path; ext_use_external; ext_import_kind}
          ->
          let etc_name =
            match ext_import_kind with
            | ExtDefault | ExtNamespace -> "$" ^ type_name
            | ExtNamed _ -> ext_type_name
          in
          Some
            {
              etc_name;
              etc_module = ext_module_path;
              etc_use_external = ext_use_external;
              etc_import_kind = ext_import_kind;
            }
        | None -> None
      in
      let body = ts_type_of_type_expr ty in
      let is_opaque = has_phantom_params type_params body in
      Some
        (TypeAlias
           {name = type_name; type_params; body; external_type; is_opaque})
    | None -> (
      (* Abstract type without manifest *)
      match external_type_info with
      | Some {ext_type_name; ext_module_path; ext_import_kind; _} ->
        let external_name =
          match ext_import_kind with
          | ExtDefault | ExtNamespace -> "$" ^ type_name
          | ExtNamed _ -> ext_type_name
        in
        Some
          (ExternalType
             {
               name = type_name;
               type_params;
               external_name;
               external_module = ext_module_path;
               external_import_kind = ext_import_kind;
             })
      | None ->
        Some (OpaqueType {name = type_name; type_params; underlying = None})))
  | Types.Type_open -> None (* Open types not supported *)

(** Register a type in LocalTypeQualifier with its stamp and qualified path.
    Uses @as renamed name if available.
    @param id The type's ident
    @param decl The type declaration (to check for @as)
    @param prefix The module prefix (e.g., "Outer.Nested" or "") *)
let register_local_type ~(prefix : string) (id : Ident.t)
    (decl : Types.type_declaration) : unit =
  let name =
    match get_as_string decl.type_attributes with
    | Some renamed -> renamed
    | None -> Ident.name id
  in
  let stamp = Ident.binding_time id in
  let qualified = if prefix = "" then name else prefix ^ "." ^ name in
  LocalTypeQualifier.add ~stamp ~name ~qualified

(** Extract all type declarations from a Typedtree structure.
    Also populates LocalTypeQualifier for stamp-based type lookup. *)
let rec extract_type_decls (str : Typedtree.structure) : type_decl list =
  (* Reset LocalTypeQualifier at the start of extraction *)
  LocalTypeQualifier.reset ();
  let decls = ref [] in
  List.iter
    (fun (item : Typedtree.structure_item) ->
      match item.str_desc with
      | Typedtree.Tstr_type (_, type_decls) ->
        List.iter
          (fun (td : Typedtree.type_declaration) ->
            register_local_type ~prefix:"" td.typ_id td.typ_type;
            match type_decl_of_type_declaration td.typ_id td.typ_type with
            | Some decl -> decls := decl :: !decls
            | None -> ())
          type_decls
      | Typedtree.Tstr_module mb -> (
        (* Extract module as a type declaration *)
        match extract_module_decl ~prefix:"" mb with
        | Some mod_decl -> decls := ModuleDecl mod_decl :: !decls
        | None -> ())
      | _ -> ())
    str.str_items;
  List.rev !decls

(** Extract a module declaration from a module binding *)
and extract_module_decl ~(prefix : string) (mb : Typedtree.module_binding) :
    module_decl option =
  let name = Ident.name mb.mb_id in
  let module_prefix = if prefix = "" then name else prefix ^ "." ^ name in
  match mb.mb_expr.mod_type with
  | Types.Mty_signature sig_items ->
    let types = ref [] in
    let values = ref [] in
    let submodules = ref [] in
    List.iter
      (fun (sig_item : Types.signature_item) ->
        match sig_item with
        | Types.Sig_type (id, decl, _) -> (
          register_local_type ~prefix:module_prefix id decl;
          match type_decl_of_type_declaration id decl with
          | Some td -> types := td :: !types
          | None -> ())
        | Types.Sig_value (id, vd) ->
          values :=
            {
              mv_name = Ident.name id;
              mv_type = ts_type_of_type_expr vd.val_type;
            }
            :: !values
        | Types.Sig_module (id, md, _) -> (
          match extract_module_decl_from_sig ~prefix:module_prefix id md with
          | Some sub -> submodules := sub :: !submodules
          | None -> ())
        | _ -> ())
      sig_items;
    Some
      {
        mod_name = name;
        mod_types = List.rev !types;
        mod_values = List.rev !values;
        mod_submodules = List.rev !submodules;
      }
  | _ -> None

(** Extract a module declaration from a signature module declaration *)
and extract_module_decl_from_sig ~(prefix : string) (id : Ident.t)
    (md : Types.module_declaration) : module_decl option =
  let name = Ident.name id in
  let module_prefix = if prefix = "" then name else prefix ^ "." ^ name in
  match md.md_type with
  | Types.Mty_signature sig_items ->
    let types = ref [] in
    let values = ref [] in
    let submodules = ref [] in
    List.iter
      (fun (sig_item : Types.signature_item) ->
        match sig_item with
        | Types.Sig_type (id, decl, _) -> (
          register_local_type ~prefix:module_prefix id decl;
          match type_decl_of_type_declaration id decl with
          | Some td -> types := td :: !types
          | None -> ())
        | Types.Sig_value (id, vd) ->
          values :=
            {
              mv_name = Ident.name id;
              mv_type = ts_type_of_type_expr vd.val_type;
            }
            :: !values
        | Types.Sig_module (id, md, _) -> (
          match extract_module_decl_from_sig ~prefix:module_prefix id md with
          | Some sub -> submodules := sub :: !submodules
          | None -> ())
        | _ -> ())
      sig_items;
    Some
      {
        mod_name = name;
        mod_types = List.rev !types;
        mod_values = List.rev !values;
        mod_submodules = List.rev !submodules;
      }
  | _ -> None

type value_export = {
  ve_name: string;
  ve_type: Types.type_expr;
  ve_params: string list;
}
(** Value export with its type for .d.ts generation *)

(** Collect runtime types from value exports.
    This processes all exported value types to ensure runtime type imports 
    are generated for types like dict, list, option etc. used in variable declarations. *)
let collect_runtime_types_from_value_exports (exports : value_export list) :
    unit =
  List.iter
    (fun ve ->
      (* Convert to ts_type and collect runtime type dependencies *)
      let ts_ty = ts_type_of_type_expr ve.ve_type in
      collect_type_deps ts_ty)
    exports

(** Extract parameter names from a function expression *)
let rec extract_param_names (expr : Typedtree.expression) : string list =
  match expr.exp_desc with
  | Typedtree.Texp_function {param; case; _} ->
    Ident.name param :: extract_param_names case.c_rhs
  | _ -> []

(** Extract the identifier from a pattern, handling Tpat_var and Tpat_alias.
    Note: Tpat_constraint is in pat_extra, not pat_desc, so we just need to handle
    the core patterns. *)
let extract_pat_ident (pat : Typedtree.pattern) : Ident.t option =
  match pat.pat_desc with
  | Typedtree.Tpat_var (id, _) -> Some id
  | Typedtree.Tpat_alias (_, id, _) -> Some id
  | _ -> None

(** Extract constraint type from pattern's pat_extra if present.
    Returns the annotated type from Tpat_constraint, or falls back to pat_type. *)
let extract_constraint_type (pat : Typedtree.pattern) : Types.type_expr =
  let rec find_constraint = function
    | [] -> pat.pat_type
    | (Typedtree.Tpat_constraint cty, _, _) :: _ -> cty.ctyp_type
    | _ :: rest -> find_constraint rest
  in
  find_constraint pat.pat_extra

(** Extract all value exports from a Typedtree structure *)
let extract_value_exports (str : Typedtree.structure) : value_export list =
  let exports = ref [] in
  List.iter
    (fun (item : Typedtree.structure_item) ->
      match item.str_desc with
      | Typedtree.Tstr_value (_, bindings) ->
        List.iter
          (fun (vb : Typedtree.value_binding) ->
            match extract_pat_ident vb.vb_pat with
            | Some id ->
              let params = extract_param_names vb.vb_expr in
              (* Use constraint type from pat_extra if present (explicit type annotation),
                 otherwise fall back to pat_type. This ensures `let f: a => b = x => x`
                 gets type `a => b`, not the unified/inferred type. *)
              exports :=
                {
                  ve_name = Ident.name id;
                  ve_type = extract_constraint_type vb.vb_pat;
                  ve_params = params;
                }
                :: !exports
            | None -> ())
          bindings
      | Typedtree.Tstr_primitive _ ->
        (* Externals don't generate JS code, so skip them in .d.ts *)
        ()
      | _ -> ())
    str.str_items;
  List.rev !exports

(** {1 Exported Value Type Tracking} *)

(** Set exported value types from value_export list *)
let set_exported_types (exports : value_export list) : unit =
  ExportedTypes.reset ();
  List.iter (fun ve -> ExportedTypes.add ve.ve_name ve.ve_type) exports

(** Recursively register all modules with their qualified paths *)
let rec register_module_with_prefix prefix (mod_decl : module_decl) : unit =
  let qualified_path =
    if prefix = "" then mod_decl.mod_name else prefix ^ "." ^ mod_decl.mod_name
  in
  ExportedModules.add mod_decl.mod_name qualified_path;
  (* Register all submodules with updated prefix *)
  List.iter (register_module_with_prefix qualified_path) mod_decl.mod_submodules

(** Register module exports from type declarations *)
let set_exported_modules (decls : type_decl list) : unit =
  ExportedModules.reset ();
  List.iter
    (fun decl ->
      match decl with
      | ModuleDecl mod_decl -> register_module_with_prefix "" mod_decl
      | _ -> ())
    decls

(** Get the type for an exported value by name *)
let get_exported_type (name : string) : Types.type_expr option =
  ExportedTypes.find name

(** Get the qualified type path for a module name, if it's an exported module *)
let get_module_type_path (name : string) : string option =
  ExportedModules.get_type_path name

(** Check if a Types.type_expr represents an opaque type that needs 'as' assertion.
    This checks if the type is:
    1. A pure abstract type (type t)
    2. A type alias with phantom parameters (type t<'a> = string)
    We check if the type name is registered in OpaqueTypes. *)
let rec type_needs_as_assertion (ty : Types.type_expr) : bool =
  match ty.desc with
  | Types.Tconstr (path, _args, _) ->
    (* Check if this type constructor is registered as opaque *)
    let type_name = Path.last path in
    OpaqueTypes.has_any () && OpaqueTypes.is_opaque type_name
  | Types.Tlink ty | Types.Tsubst ty -> type_needs_as_assertion ty
  | _ -> false

(** {1 Type Printing} *)

module P = Ext_pp

(** TypeScript/JavaScript reserved words that cannot be used as identifiers.
    These need to be escaped when used as parameter names in .d.ts files. *)
let ts_reserved_words =
  [
    (* Keywords *)
    "break";
    "case";
    "catch";
    "class";
    "const";
    "continue";
    "debugger";
    "default";
    "delete";
    "do";
    "else";
    "enum";
    "export";
    "extends";
    "false";
    "finally";
    "for";
    "function";
    "if";
    "import";
    "in";
    "instanceof";
    "new";
    "null";
    "return";
    "super";
    "switch";
    "this";
    "throw";
    "true";
    "try";
    "typeof";
    "var";
    "void";
    "while";
    "with";
    (* Strict mode reserved words *)
    "implements";
    "interface";
    "let";
    "package";
    "private";
    "protected";
    "public";
    "static";
    "yield";
    (* TypeScript keywords *)
    "any";
    "boolean";
    "number";
    "string";
    "symbol";
    "abstract";
    "as";
    "async";
    "await";
    "declare";
    "from";
    "get";
    "is";
    "module";
    "namespace";
    "never";
    "readonly";
    "require";
    "set";
    "type";
    "undefined";
    "unique";
    "unknown";
  ]

(** Check if a name is a TypeScript reserved word *)
let is_ts_reserved_word name =
  Js_reserved_map.is_js_keyword name
  || Js_reserved_map.is_js_special_word name
  || Js_reserved_map.is_ts_keyword name

(** Escape a name if it's a TypeScript reserved word by adding underscore suffix *)
let escape_ts_reserved name =
  if is_ts_reserved_word name then name ^ "_" else name

(** Print a TypeScript type to the pretty printer *)
let rec pp_ts_type (f : P.t) (ty : ts_type) : unit =
  match ty with
  | Any -> P.string f "any"
  | Unknown -> P.string f "unknown"
  | Never -> P.string f "never"
  | Void -> P.string f "void"
  | Null -> P.string f "null"
  | Undefined -> P.string f "undefined"
  | Boolean -> P.string f "boolean"
  | Number -> P.string f "number"
  | Bigint -> P.string f "bigint"
  | String -> P.string f "string"
  | Symbol -> P.string f "symbol"
  | Array elem ->
    pp_ts_type f elem;
    P.string f "[]"
  | Tuple types ->
    P.string f "[";
    pp_ts_type_list f types;
    P.string f "]"
  | Object obj -> pp_object_type f obj
  | Function fn -> pp_fn_type f fn
  | Union types -> pp_union f types
  | Intersection types -> pp_intersection f types
  | TypeRef {name; args} ->
    P.string f name;
    if args <> [] then (
      P.string f "<";
      pp_ts_type_list f args;
      P.string f ">")
  | TypeVar name -> P.string f (String.capitalize_ascii name)
  | Literal lit -> pp_literal f lit
  | Readonly ty ->
    P.string f "Readonly<";
    pp_ts_type f ty;
    P.string f ">"
  | Promise ty ->
    P.string f "Promise<";
    pp_ts_type f ty;
    P.string f ">"
  | RuntimeType {rt_name; rt_args} ->
    P.string f runtime_types_namespace;
    P.string f ".";
    P.string f rt_name;
    if rt_args <> [] then (
      P.string f "<";
      pp_ts_type_list f rt_args;
      P.string f ">")

and pp_ts_type_list (f : P.t) (types : ts_type list) : unit =
  match types with
  | [] -> ()
  | [ty] -> pp_ts_type f ty
  | ty :: rest ->
    pp_ts_type f ty;
    P.string f ", ";
    pp_ts_type_list f rest

(** Print a type in union context, parenthesizing function types *)
and pp_ts_type_in_union (f : P.t) (ty : ts_type) : unit =
  match ty with
  | Function _ ->
    (* Function types must be parenthesized in union types *)
    P.string f "(";
    pp_ts_type f ty;
    P.string f ")"
  | _ -> pp_ts_type f ty

and pp_union (f : P.t) (types : ts_type list) : unit =
  match types with
  | [] -> P.string f "never"
  | [ty] -> pp_ts_type f ty
  | ty :: rest ->
    pp_ts_type_in_union f ty;
    P.string f " | ";
    pp_union f rest

and pp_intersection (f : P.t) (types : ts_type list) : unit =
  match types with
  | [] -> P.string f "unknown"
  | [ty] -> pp_ts_type f ty
  | ty :: rest ->
    pp_ts_type f ty;
    P.string f " & ";
    pp_intersection f rest

and pp_literal (f : P.t) (lit : literal_type) : unit =
  match lit with
  | LitString s ->
    P.string f "\"";
    P.string f s;
    P.string f "\""
  | LitNumber n -> P.string f (Printf.sprintf "%g" n)
  | LitBigint s ->
    P.string f s;
    P.string f "n"
  | LitBoolean b -> P.string f (if b then "true" else "false")

and pp_object_type (f : P.t) (obj : object_type) : unit =
  let has_index = obj.index_sig <> None in
  let prop_count = List.length obj.properties in
  let use_multiline = prop_count > 1 || has_index in
  if use_multiline then (
    P.string f "{";
    List.iter
      (fun prop ->
        P.newline f;
        P.string f "    ";
        if prop.prop_readonly then P.string f "readonly ";
        P.string f prop.prop_name;
        if prop.prop_optional then P.string f "?";
        P.string f ": ";
        pp_ts_type f prop.prop_type;
        P.string f ";")
      obj.properties;
    (match obj.index_sig with
    | Some {index_key; index_value} ->
      P.newline f;
      P.string f "    [key: ";
      pp_ts_type f index_key;
      P.string f "]: ";
      pp_ts_type f index_value;
      P.string f ";"
    | None -> ());
    P.newline f;
    P.string f "  }")
  else (
    P.string f "{ ";
    List.iter
      (fun prop ->
        if prop.prop_readonly then P.string f "readonly ";
        P.string f prop.prop_name;
        if prop.prop_optional then P.string f "?";
        P.string f ": ";
        pp_ts_type f prop.prop_type;
        P.string f "; ")
      obj.properties;
    (match obj.index_sig with
    | Some {index_key; index_value} ->
      P.string f "[key: ";
      pp_ts_type f index_key;
      P.string f "]: ";
      pp_ts_type f index_value;
      P.string f "; "
    | None -> ());
    P.string f "}")

and pp_fn_type (f : P.t) (fn : fn_type) : unit =
  if fn.fn_type_params <> [] then (
    P.string f "<";
    pp_type_params f fn.fn_type_params;
    P.string f ">");
  P.string f "(";
  pp_params f fn.fn_params;
  (match fn.fn_rest with
  | Some rest ->
    if fn.fn_params <> [] then P.string f ", ";
    P.string f "...";
    (match rest.param_name with
    | Some n -> P.string f n
    | None -> P.string f "args");
    P.string f ": ";
    pp_ts_type f rest.param_type
  | None -> ());
  P.string f ") => ";
  pp_ts_type f fn.fn_return

and pp_params (f : P.t) (params : param_type list) : unit =
  let unnamed_counter = ref 0 in
  let rec loop = function
    | [] -> ()
    | [p] -> pp_param f p ~unnamed_counter
    | p :: rest ->
      pp_param f p ~unnamed_counter;
      P.string f ", ";
      loop rest
  in
  loop params

and pp_param (f : P.t) (p : param_type) ~(unnamed_counter : int ref) : unit =
  (match p.param_name with
  | Some name -> P.string f name
  | None ->
    (* Use arg0, arg1, etc. for unnamed parameters to avoid duplicate identifiers *)
    P.string f "arg";
    P.string f (string_of_int !unnamed_counter);
    incr unnamed_counter);
  if p.param_optional then P.string f "?";
  P.string f ": ";
  pp_ts_type f p.param_type

and pp_type_params (f : P.t) (params : type_param list) : unit =
  match params with
  | [] -> ()
  | [p] -> pp_type_param f p
  | p :: rest ->
    pp_type_param f p;
    P.string f ", ";
    pp_type_params f rest

and pp_type_param (f : P.t) (p : type_param) : unit =
  P.string f p.tp_name;
  (match p.tp_constraint with
  | Some c ->
    P.string f " extends ";
    pp_ts_type f c
  | None -> ());
  match p.tp_default with
  | Some d ->
    P.string f " = ";
    pp_ts_type f d
  | None -> ()

(* Initialize the forward reference for pp_ts_type *)
let () = pp_ts_type_ref := pp_ts_type

(** Print type annotation (: Type) if type is present *)
let pp_type_annotation (f : P.t) (ty : ts_type option) : unit =
  match ty with
  | None -> ()
  | Some t ->
    P.string f ": ";
    pp_ts_type f t

(** Print type annotation from Types.type_expr *)
let pp_type_annotation_from_ml (f : P.t) (ty : Types.type_expr option) : unit =
  pp_type_annotation f (Option.map ts_type_of_type_expr ty)

(** Check if a type needs an 'as Type' assertion for opaque types. *)
let needs_as_assertion (ty : Types.type_expr option) : bool =
  match ty with
  | Some t -> type_needs_as_assertion t
  | None -> false

(** Print 'as Type' assertion if the type needs it (for opaque types).
    This is needed in combined .ts output where the value's runtime type
    differs from its declared opaque type. *)
let pp_as_assertion (f : P.t) (ty : Types.type_expr option) : unit =
  match ty with
  | Some t when type_needs_as_assertion t ->
    P.string f " as ";
    pp_ts_type f (ts_type_of_type_expr t)
  | _ -> ()

(** Find the branded opaque type for a return type, if any.
    For option/nullable wrappers, extracts the inner type since ReScript
    unboxes option (Some(x) becomes just x at runtime). *)
let find_branded_return_type (return_type : Types.type_expr option) :
    ts_type option =
  match return_type with
  | None -> None
  | Some ty ->
    if not (OpaqueTypes.has_any ()) then None
    else
      let ts_ty = ts_type_of_type_expr ty in
      let rec find_branded_inner t =
        match t with
        | TypeRef {name; _} when OpaqueTypes.is_branded name -> Some t
        | RuntimeType {rt_name = "option"; rt_args = [inner]}
        | RuntimeType {rt_name = "nullable"; rt_args = [inner]}
        | RuntimeType {rt_name = "null_"; rt_args = [inner]} ->
          find_branded_inner inner
        | _ -> None
      in
      find_branded_inner ts_ty

(** Check if return type needs an 'as Type' assertion for opaque types. *)
let needs_opaque_return_assertion (return_type : Types.type_expr option) : bool
    =
  find_branded_return_type return_type <> None

(** Print 'as Type' assertion for return statements if the return type
    is a branded opaque type (from @opaque attribute).
    This allows the underlying type to be returned while satisfying
    the opaque type constraint.
    
    For option/nullable wrappers, we extract the inner type since ReScript
    unboxes option (Some(x) becomes just x at runtime). *)
let pp_opaque_return_assertion (f : P.t) (return_type : Types.type_expr option)
    : unit =
  match find_branded_return_type return_type with
  | Some inner_ty ->
    P.string f " as ";
    pp_ts_type f inner_ty
  | None -> ()

(** Print generic type parameters <A, B, ...> if any type variables exist.
    Also includes constraints from GADT types. *)
let pp_type_params_from_ml (f : P.t) (fn_type : Types.type_expr option) : unit =
  let type_params = collect_type_vars_with_constraints fn_type in
  match type_params with
  | [] -> ()
  | _ ->
    P.string f "<";
    pp_type_params f type_params;
    P.string f ">"

(** {1 Type Declaration Printing} *)

(** Print a variant case for tagged unions.
    @param tag_name The tag field name (default Js_dump_lit.tag, can be customized with @tag)
    @param case The variant case to print *)
let pp_variant_case (f : P.t) ~(tag_name : string) (case : variant_case) : unit
    =
  match case.vc_payload with
  | NoPayload ->
    (* Nullary constructor - just the tag string literal *)
    P.string f "\"";
    P.string f case.vc_tag;
    P.string f "\""
  | TuplePayload types ->
    (* Tuple payload: { readonly TAG: "Name"; readonly _0: T0; ... } *)
    (* Use multiline when there's more than 1 payload field *)
    let use_multiline = List.length types > 1 in
    if use_multiline then (
      P.string f "{";
      P.newline f;
      P.string f "      readonly ";
      P.string f tag_name;
      P.string f ": \"";
      P.string f case.vc_tag;
      P.string f "\";";
      List.iteri
        (fun i ty ->
          P.newline f;
          P.string f "      readonly _";
          P.string f (string_of_int i);
          P.string f ": ";
          pp_ts_type f ty;
          P.string f ";")
        types;
      P.newline f;
      P.string f "    }")
    else (
      P.string f "{ readonly ";
      P.string f tag_name;
      P.string f ": \"";
      P.string f case.vc_tag;
      P.string f "\"";
      List.iteri
        (fun i ty ->
          P.string f "; readonly _";
          P.string f (string_of_int i);
          P.string f ": ";
          pp_ts_type f ty)
        types;
      P.string f " }")
  | InlineRecord props ->
    (* Inline record: { readonly TAG: "Name"; field1: T1; ... } *)
    (* Use multiline when there's more than 1 field *)
    let use_multiline = List.length props > 1 in
    if use_multiline then (
      P.string f "{";
      P.newline f;
      P.string f "      readonly ";
      P.string f tag_name;
      P.string f ": \"";
      P.string f case.vc_tag;
      P.string f "\";";
      List.iter
        (fun prop ->
          P.newline f;
          P.string f "      ";
          if prop.prop_readonly then P.string f "readonly ";
          P.string f prop.prop_name;
          if prop.prop_optional then P.string f "?";
          P.string f ": ";
          pp_ts_type f prop.prop_type;
          P.string f ";")
        props;
      P.newline f;
      P.string f "    }")
    else (
      P.string f "{ readonly ";
      P.string f tag_name;
      P.string f ": \"";
      P.string f case.vc_tag;
      P.string f "\"";
      List.iter
        (fun prop ->
          P.string f "; ";
          if prop.prop_readonly then P.string f "readonly ";
          P.string f prop.prop_name;
          if prop.prop_optional then P.string f "?";
          P.string f ": ";
          pp_ts_type f prop.prop_type)
        props;
      P.string f " }")

(** Print a variant case for @unboxed variants.
    Just prints the payload type directly.
    Uses pp_ts_type_in_union since unboxed variants are printed as union members. *)
let pp_unboxed_variant_case (f : P.t) (case : variant_case) : unit =
  match case.vc_payload with
  | NoPayload ->
    (* Nullary constructor - just the tag string literal *)
    P.string f "\"";
    P.string f case.vc_tag;
    P.string f "\""
  | TuplePayload [single_type] ->
    (* Single payload - just the type directly, parenthesizing if needed *)
    pp_ts_type_in_union f single_type
  | TuplePayload types ->
    (* Multiple payloads - shouldn't happen for valid @unboxed, but handle gracefully *)
    pp_ts_type f (Tuple types)
  | InlineRecord props ->
    (* Inline record - as object type *)
    pp_object_type f {properties = props; index_sig = None; call_sig = None}

(** Print a union type with each member on its own line (for type aliases) *)
let rec pp_union_multiline (f : P.t) (types : ts_type list) : unit =
  match types with
  | [] -> P.string f "never"
  | [ty] ->
    P.newline f;
    P.string f "  | ";
    pp_ts_type_in_union f ty
  | ty :: rest ->
    P.newline f;
    P.string f "  | ";
    pp_ts_type_in_union f ty;
    pp_union_multiline f rest

(** Helper to print the type body with optional external type wrapping *)
let pp_type_body_with_external (f : P.t) (type_params : type_param list)
    (body : ts_type) (external_type : external_type_constraint option) : unit =
  match external_type with
  | Some {etc_name; etc_use_external; _} ->
    (* Wrap with external<ExternalType, RescriptShape, UseExternal?>
       Format with newlines for readability:
       rescript.external<
         ExternalType,
         { ... },
         true
       >
    *)
    P.string f runtime_types_namespace;
    P.string f ".external<";
    P.newline f;
    P.string f "  ";
    P.string f etc_name;
    if type_params <> [] then (
      P.string f "<";
      let param_names =
        List.map (fun (tp : type_param) -> tp.tp_name) type_params
      in
      P.string f (String.concat ", " param_names);
      P.string f ">");
    P.string f ",";
    P.newline f;
    P.string f "  ";
    pp_ts_type f body;
    if etc_use_external then (
      P.string f ",";
      P.newline f;
      P.string f "  true");
    P.newline f;
    P.string f ">"
  | None -> pp_ts_type f body

(** Get type names defined in a module (for qualification) *)
let get_module_type_names (mod_decl : module_decl) : StringSet.t =
  List.fold_left
    (fun acc decl ->
      match decl with
      | TypeAlias {name; _}
      | Interface {name; _}
      | VariantType {name; _}
      | GadtType {name; _}
      | OpaqueType {name; _}
      | ExternalType {name; _} ->
        StringSet.add name acc
      | ModuleDecl _ -> acc)
    StringSet.empty mod_decl.mod_types

(** Print a TypeScript type with module path qualification for local types.
    @param module_path The current module namespace path (e.g., "Inner" or "Outer.Nested")
    @param local_types Set of type names defined in the current module that need qualification *)
let rec pp_ts_type_qualified (f : P.t) ~(module_path : string)
    ~(local_types : StringSet.t) (ty : ts_type) : unit =
  match ty with
  | TypeRef {name; args} ->
    (* If this is an unqualified type name that's defined locally, add the module path prefix *)
    let qualified_name =
      if (not (String.contains name '.')) && StringSet.mem name local_types then
        module_path ^ "." ^ name
      else name
    in
    P.string f qualified_name;
    if args <> [] then (
      P.string f "<";
      pp_ts_type_list_qualified f ~module_path ~local_types args;
      P.string f ">")
  | Array elem ->
    pp_ts_type_qualified f ~module_path ~local_types elem;
    P.string f "[]"
  | Tuple types ->
    P.string f "[";
    pp_ts_type_list_qualified f ~module_path ~local_types types;
    P.string f "]"
  | Object obj -> pp_object_type_qualified f ~module_path ~local_types obj
  | Function fn -> pp_fn_type_qualified f ~module_path ~local_types fn
  | Union types -> pp_union_qualified f ~module_path ~local_types types
  | Intersection types ->
    pp_intersection_qualified f ~module_path ~local_types types
  | Readonly ty ->
    P.string f "Readonly<";
    pp_ts_type_qualified f ~module_path ~local_types ty;
    P.string f ">"
  | Promise ty ->
    P.string f "Promise<";
    pp_ts_type_qualified f ~module_path ~local_types ty;
    P.string f ">"
  | RuntimeType {rt_name; rt_args} ->
    P.string f runtime_types_namespace;
    P.string f ".";
    P.string f rt_name;
    if rt_args <> [] then (
      P.string f "<";
      pp_ts_type_list_qualified f ~module_path ~local_types rt_args;
      P.string f ">")
  (* Non-recursive types don't need special handling *)
  | Any -> P.string f "any"
  | Unknown -> P.string f "unknown"
  | Never -> P.string f "never"
  | Void -> P.string f "void"
  | Null -> P.string f "null"
  | Undefined -> P.string f "undefined"
  | Boolean -> P.string f "boolean"
  | Number -> P.string f "number"
  | Bigint -> P.string f "bigint"
  | String -> P.string f "string"
  | Symbol -> P.string f "symbol"
  | TypeVar name -> P.string f (String.capitalize_ascii name)
  | Literal lit -> pp_literal f lit

and pp_ts_type_list_qualified (f : P.t) ~(module_path : string)
    ~(local_types : StringSet.t) (types : ts_type list) : unit =
  match types with
  | [] -> ()
  | [ty] -> pp_ts_type_qualified f ~module_path ~local_types ty
  | ty :: rest ->
    pp_ts_type_qualified f ~module_path ~local_types ty;
    P.string f ", ";
    pp_ts_type_list_qualified f ~module_path ~local_types rest

and pp_ts_type_in_union_qualified (f : P.t) ~(module_path : string)
    ~(local_types : StringSet.t) (ty : ts_type) : unit =
  match ty with
  | Function _ ->
    P.string f "(";
    pp_ts_type_qualified f ~module_path ~local_types ty;
    P.string f ")"
  | _ -> pp_ts_type_qualified f ~module_path ~local_types ty

and pp_union_qualified (f : P.t) ~(module_path : string)
    ~(local_types : StringSet.t) (types : ts_type list) : unit =
  match types with
  | [] -> P.string f "never"
  | [ty] -> pp_ts_type_qualified f ~module_path ~local_types ty
  | ty :: rest ->
    pp_ts_type_in_union_qualified f ~module_path ~local_types ty;
    P.string f " | ";
    pp_union_qualified f ~module_path ~local_types rest

and pp_intersection_qualified (f : P.t) ~(module_path : string)
    ~(local_types : StringSet.t) (types : ts_type list) : unit =
  match types with
  | [] -> P.string f "unknown"
  | [ty] -> pp_ts_type_qualified f ~module_path ~local_types ty
  | ty :: rest ->
    pp_ts_type_qualified f ~module_path ~local_types ty;
    P.string f " & ";
    pp_intersection_qualified f ~module_path ~local_types rest

and pp_object_type_qualified (f : P.t) ~(module_path : string)
    ~(local_types : StringSet.t) (obj : object_type) : unit =
  let has_index = obj.index_sig <> None in
  let prop_count = List.length obj.properties in
  let use_multiline = prop_count > 1 || has_index in
  if use_multiline then (
    P.string f "{";
    List.iter
      (fun prop ->
        P.newline f;
        P.string f "    ";
        if prop.prop_readonly then P.string f "readonly ";
        P.string f prop.prop_name;
        if prop.prop_optional then P.string f "?";
        P.string f ": ";
        pp_ts_type_qualified f ~module_path ~local_types prop.prop_type;
        P.string f ";")
      obj.properties;
    (match obj.index_sig with
    | Some {index_key; index_value} ->
      P.newline f;
      P.string f "    [key: ";
      pp_ts_type_qualified f ~module_path ~local_types index_key;
      P.string f "]: ";
      pp_ts_type_qualified f ~module_path ~local_types index_value;
      P.string f ";"
    | None -> ());
    P.newline f;
    P.string f "  }")
  else (
    P.string f "{ ";
    List.iter
      (fun prop ->
        if prop.prop_readonly then P.string f "readonly ";
        P.string f prop.prop_name;
        if prop.prop_optional then P.string f "?";
        P.string f ": ";
        pp_ts_type_qualified f ~module_path ~local_types prop.prop_type;
        P.string f "; ")
      obj.properties;
    (match obj.index_sig with
    | Some {index_key; index_value} ->
      P.string f "[key: ";
      pp_ts_type_qualified f ~module_path ~local_types index_key;
      P.string f "]: ";
      pp_ts_type_qualified f ~module_path ~local_types index_value;
      P.string f "; "
    | None -> ());
    P.string f "}")

and pp_fn_type_qualified (f : P.t) ~(module_path : string)
    ~(local_types : StringSet.t) (fn : fn_type) : unit =
  if fn.fn_type_params <> [] then (
    P.string f "<";
    pp_type_params f fn.fn_type_params;
    P.string f ">");
  P.string f "(";
  pp_params_qualified f ~module_path ~local_types fn.fn_params;
  (match fn.fn_rest with
  | Some rest ->
    if fn.fn_params <> [] then P.string f ", ";
    P.string f "...";
    (match rest.param_name with
    | Some n -> P.string f n
    | None -> P.string f "args");
    P.string f ": ";
    pp_ts_type_qualified f ~module_path ~local_types rest.param_type
  | None -> ());
  P.string f ") => ";
  pp_ts_type_qualified f ~module_path ~local_types fn.fn_return

and pp_params_qualified (f : P.t) ~(module_path : string)
    ~(local_types : StringSet.t) (params : param_type list) : unit =
  let unnamed_counter = ref 0 in
  let rec loop = function
    | [] -> ()
    | [p] -> pp_param_qualified f ~module_path ~local_types p ~unnamed_counter
    | p :: rest ->
      pp_param_qualified f ~module_path ~local_types p ~unnamed_counter;
      P.string f ", ";
      loop rest
  in
  loop params

and pp_param_qualified (f : P.t) ~(module_path : string)
    ~(local_types : StringSet.t) (p : param_type) ~(unnamed_counter : int ref) :
    unit =
  (match p.param_name with
  | Some name -> P.string f name
  | None ->
    P.string f "arg";
    P.string f (string_of_int !unnamed_counter);
    incr unnamed_counter);
  if p.param_optional then P.string f "?";
  P.string f ": ";
  pp_ts_type_qualified f ~module_path ~local_types p.param_type

(** Forward reference for module printing (to break mutual recursion) *)
let pp_module_decl_ref : (P.t -> module_decl -> unit) ref = ref (fun _ _ -> ())

(** Forward reference for module printing in combined .ts mode (no export const) *)
let pp_module_decl_ts_ref : (P.t -> module_decl -> unit) ref =
  ref (fun _ _ -> ())

(** Print a type declaration *)
let pp_type_decl (f : P.t) (decl : type_decl) : unit =
  match decl with
  | TypeAlias {name; type_params; body; external_type; is_opaque} ->
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    (if is_opaque then
       (* Opaque type: wrap with $res.opaque<"brand", params, body> *)
       let brand_name =
         match OpaqueTypes.get_full_name name with
         | Some full_name -> full_name
         | None -> name
       in
       pp_opaque_type f ~brand_name ~type_params ~underlying:(Some body)
     else
       match (body, external_type) with
       | Union types, None ->
         (* Print union types with newlines for readability *)
         pp_union_multiline f types
       | _ -> pp_type_body_with_external f type_params body external_type);
    P.string f ";"
  | Interface {name; type_params; extends; body} ->
    P.string f "export interface ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    (match extends with
    | [] -> ()
    | _ ->
      P.string f " extends ";
      let rec print_extends = function
        | [] -> ()
        | [{name; args}] ->
          P.string f name;
          if args <> [] then (
            P.string f "<";
            pp_ts_type_list f args;
            P.string f ">")
        | {name; args} :: rest ->
          P.string f name;
          if args <> [] then (
            P.string f "<";
            pp_ts_type_list f args;
            P.string f ">");
          P.string f ", ";
          print_extends rest
      in
      print_extends extends);
    P.string f " {";
    P.group f 1 (fun () ->
        List.iter
          (fun prop ->
            P.newline f;
            if prop.prop_readonly then P.string f "readonly ";
            P.string f prop.prop_name;
            if prop.prop_optional then P.string f "?";
            P.string f ": ";
            pp_ts_type f prop.prop_type;
            P.string f ";")
          body.properties;
        match body.index_sig with
        | Some {index_key; index_value} ->
          P.newline f;
          P.string f "[key: ";
          pp_ts_type f index_key;
          P.string f "]: ";
          pp_ts_type f index_value;
          P.string f ";"
        | None -> ());
    P.newline f;
    P.string f "}"
  | VariantType {name; type_params; cases; config} ->
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " =";
    List.iter
      (fun case ->
        P.newline f;
        P.string f "  | ";
        if config.vc_unboxed then pp_unboxed_variant_case f case
        else pp_variant_case f ~tag_name:config.vc_tag_name case)
      cases;
    P.string f ";"
  | GadtType {name; type_params; cases; config} ->
    (* GADT generates separate types for each constructor, then a union.
       For: type t<_> = Int(int): t<int> | Float(float): t<float>
       Generates:
         export type t$Int = { TAG: "Int"; _0: number };
         export type t$Float = { TAG: "Float"; _0: number };
         export type t<A> = t$Int | t$Float; *)
    (* First, generate a type for each GADT constructor *)
    List.iter
      (fun (case : gadt_case) ->
        P.string f "export type ";
        P.string f name;
        P.string f "$";
        P.string f case.gc_name;
        P.string f " = ";
        if config.vc_unboxed then
          pp_unboxed_variant_case f
            {
              vc_name = case.gc_name;
              vc_tag = case.gc_tag;
              vc_payload = case.gc_payload;
            }
        else
          pp_variant_case f ~tag_name:config.vc_tag_name
            {
              vc_name = case.gc_name;
              vc_tag = case.gc_tag;
              vc_payload = case.gc_payload;
            };
        P.string f ";";
        P.newline f)
      cases;
    (* Then generate the union type *)
    P.newline f;
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " =";
    List.iter
      (fun (case : gadt_case) ->
        P.newline f;
        P.string f "  | ";
        P.string f name;
        P.string f "$";
        P.string f case.gc_name)
      cases;
    P.string f ";"
  | OpaqueType {name; type_params; underlying} ->
    (* For .ts files, generate: export type t = $res.opaque<"Brand", underlying> *)
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    (* Use full brand name from OpaqueTypes if available *)
    let brand_name =
      match OpaqueTypes.get_full_name name with
      | Some full_name -> full_name
      | None -> name
    in
    pp_opaque_type f ~brand_name ~type_params ~underlying;
    P.string f ";"
  | ExternalType {name; type_params; external_name; external_module = _} ->
    (* Abstract external type: export type t<A> = Set<A>; *)
    (* For abstract types, just alias to the external type directly *)
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    P.string f external_name;
    if type_params <> [] then (
      P.string f "<";
      let param_names =
        List.map (fun (tp : type_param) -> tp.tp_name) type_params
      in
      P.string f (String.concat ", " param_names);
      P.string f ">");
    P.string f ";"
  | ModuleDecl mod_decl -> !pp_module_decl_ref f mod_decl

(** Print a type declaration for combined .ts files.
    Same as pp_type_decl but doesn't emit 'export const' for modules
    since the implementation will provide that. *)
let pp_type_decl_ts (f : P.t) (decl : type_decl) : unit =
  match decl with
  | ModuleDecl mod_decl -> !pp_module_decl_ts_ref f mod_decl
  | _ -> pp_type_decl f decl

(** Print type declarations for combined .ts output (no const for modules) *)
let pp_type_decls_ts (f : P.t) (decls : type_decl list) : unit =
  List.iter
    (fun decl ->
      pp_type_decl_ts f decl;
      P.at_least_two_lines f)
    decls

(** Print runtime type import based on collected types *)
let pp_runtime_type_import (f : P.t) : unit =
  if RuntimeTypes.has_any () then (
    P.string f "import type * as ";
    P.string f runtime_types_namespace;
    P.string f " from \"@rescript/runtime/types\";";
    P.at_least_two_lines f)

(** Print type declarations only (without collecting runtime types).
    Uses pp_type_decl_ts which doesn't emit const for modules. *)
let pp_type_decls_only (f : P.t) (decls : type_decl list) : unit =
  List.iter
    (fun decl ->
      pp_type_decl_ts f decl;
      P.at_least_two_lines f)
    decls

(** Collect opaque types from type declarations for $res.opaque brand generation.
    Recursively processes module declarations to find nested opaque types.
    For branded opaque types (@opaque), also registers the underlying type.
    @param prefix Optional module path prefix *)
let rec collect_opaque_types_with_prefix ~(prefix : string option)
    (decls : type_decl list) : unit =
  List.iter
    (fun decl ->
      match decl with
      | OpaqueType {name; type_params; underlying} -> (
        let full_name =
          match prefix with
          | Some p -> p ^ "." ^ name
          | None -> name
        in
        match underlying with
        | Some ty ->
          (* Branded opaque type from @opaque - register with underlying type *)
          OpaqueTypes.add_branded full_name (List.length type_params) ty
        | None ->
          (* Pure abstract type *)
          OpaqueTypes.add full_name (List.length type_params))
      | TypeAlias {name; type_params; is_opaque = true; _} ->
        let full_name =
          match prefix with
          | Some p -> p ^ "." ^ name
          | None -> name
        in
        OpaqueTypes.add full_name (List.length type_params)
      | ModuleDecl {mod_name; mod_types; mod_submodules; _} ->
        (* Recursively collect from module types and submodules with updated prefix *)
        let new_prefix =
          match prefix with
          | Some p -> Some (p ^ "." ^ mod_name)
          | None -> Some mod_name
        in
        collect_opaque_types_with_prefix ~prefix:new_prefix mod_types;
        List.iter
          (fun sub ->
            collect_opaque_types_with_prefix ~prefix:new_prefix [ModuleDecl sub])
          mod_submodules
      | _ -> ())
    decls

(** Collect opaque types from type declarations for $res.opaque brand generation.
    @param module_name The file module name (e.g., "Modules") to prefix all brands *)
let collect_opaque_types ~(module_name : string) (decls : type_decl list) : unit
    =
  collect_opaque_types_with_prefix ~prefix:(Some module_name) decls

(** Initialize state and collect type information without printing.
    Call this before printing runtime import and type declarations separately. *)
let init_type_decls ~(module_name : string) (decls : type_decl list) : unit =
  reset_state ();
  set_module_name module_name;
  List.iter collect_runtime_types_decl decls;
  collect_opaque_types ~module_name decls

(** Print all type declarations with runtime type imports.
    @param module_name The module name for opaque type brand prefixing *)
let pp_type_decls ~(module_name : string) (f : P.t) (decls : type_decl list) :
    unit =
  init_type_decls ~module_name decls;
  (* Print import and declarations *)
  pp_runtime_type_import f;
  pp_type_decls_only f decls

(** {1 Declaration File (.d.ts) Generation} *)

(** Check if a type is a function type *)
let is_function_type (ty : Types.type_expr) : bool =
  let rec check ty =
    match ty.Types.desc with
    | Types.Tarrow _ -> true
    | Types.Tlink ty | Types.Tsubst ty | Types.Tpoly (ty, _) -> check ty
    | _ -> false
  in
  check ty

(** Threshold for multi-line function formatting *)
let function_line_width_threshold = 80

(** Estimate the length of a ts_type when printed *)
let rec estimate_ts_type_length (ty : ts_type) : int =
  match ty with
  | Any -> 3
  | Unknown -> 7
  | Never -> 5
  | Void -> 4
  | Null -> 4
  | Undefined -> 9
  | Boolean -> 7
  | Number -> 6
  | Bigint -> 6
  | String -> 6
  | Symbol -> 6
  | Array elem -> estimate_ts_type_length elem + 2 (* [] *)
  | Tuple types ->
    List.fold_left ( + ) 2
      (List.map (fun t -> estimate_ts_type_length t + 2) types)
  | Object {properties; _} ->
    List.fold_left ( + ) 4
      (List.map
         (fun p ->
           String.length p.prop_name + estimate_ts_type_length p.prop_type + 4)
         properties)
  | Function {fn_params; fn_return; _} ->
    let params_len =
      List.fold_left ( + ) 0
        (List.map (fun p -> estimate_ts_type_length p.param_type + 4) fn_params)
    in
    params_len + estimate_ts_type_length fn_return + 6 (* () =>  *)
  | Union types | Intersection types ->
    List.fold_left ( + ) 0
      (List.map (fun t -> estimate_ts_type_length t + 3) types)
  | TypeRef {name; args} ->
    String.length name
    + List.fold_left ( + ) 0
        (List.map (fun t -> estimate_ts_type_length t + 2) args)
  | TypeVar name -> String.length name
  | Literal (LitString s) -> String.length s + 2
  | Literal (LitNumber n) -> String.length (string_of_float n)
  | Literal (LitBigint s) -> String.length s + 1
  | Literal (LitBoolean _) -> 5
  | Readonly ty -> estimate_ts_type_length ty + 9
  | Promise ty -> estimate_ts_type_length ty + 8
  | RuntimeType {rt_name; rt_args} ->
    String.length rt_name + 5
    + List.fold_left ( + ) 0
        (List.map (fun t -> estimate_ts_type_length t + 2) rt_args)

(** Estimate the total length of a function declaration *)
let estimate_function_decl_length (name : string) (param_names : string list)
    (fn_type : Types.type_expr option) : int =
  let base_len = 16 + String.length name + 4 in
  (* "export function " + name + "(): " *)
  match fn_type with
  | None -> base_len
  | Some ty ->
    let param_names_ref = ref param_names in
    let unnamed_counter = ref 0 in
    let get_next_param_name () =
      match !param_names_ref with
      | pname :: rest ->
        param_names_ref := rest;
        if pname = "_" then (
          let n = !unnamed_counter in
          incr unnamed_counter;
          "arg" ^ string_of_int n)
        else pname
      | [] ->
        let n = !unnamed_counter in
        incr unnamed_counter;
        "arg" ^ string_of_int n
    in
    let rec count_params_length ty =
      match ty.Types.desc with
      | Types.Tarrow ({lbl; typ = arg_type}, return_type, _, _) ->
        if lbl = Nolabel && is_unit_type arg_type then (
          ignore (get_next_param_name ());
          count_params_length return_type)
        else
          let param_name =
            match lbl with
            | Asttypes.Nolabel -> get_next_param_name ()
            | Asttypes.Labelled {txt} | Asttypes.Optional {txt} ->
              ignore (get_next_param_name ());
              txt
          in
          let actual_type =
            match lbl with
            | Asttypes.Optional _ -> (
              match remove_option_wrapper lbl arg_type with
              | Some inner -> inner
              | None -> arg_type)
            | _ -> arg_type
          in
          String.length param_name + 2
          + estimate_ts_type_length (ts_type_of_type_expr actual_type)
          + 2 (* ", " *)
          + count_params_length return_type
      | Types.Tlink ty | Types.Tsubst ty -> count_params_length ty
      | _ -> 0
    in
    let params_len = count_params_length ty in
    let return_len =
      match return_type_of_fn_type (Some ty) with
      | Some ret_ty -> estimate_ts_type_length ret_ty
      | None -> 0
    in
    base_len + params_len + return_len

(** Print a function declaration for .d.ts *)
let pp_dts_function_decl (f : P.t) (name : string) (param_names : string list)
    (fn_type : Types.type_expr option) : unit =
  let use_multiline =
    estimate_function_decl_length name param_names fn_type
    > function_line_width_threshold
  in
  P.string f "export function ";
  (* Escape function name if it's a reserved word *)
  P.string f (escape_ts_reserved name);
  (* Print type parameters *)
  pp_type_params_from_ml f fn_type;
  (* Print parameters *)
  P.string f "(";
  (match fn_type with
  | None -> ()
  | Some ty ->
    let param_names_ref = ref param_names in
    let unnamed_counter = ref 0 in
    let get_next_param_name () =
      match !param_names_ref with
      | pname :: rest ->
        param_names_ref := rest;
        (* If name is "_", generate a unique arg name *)
        if pname = "_" then (
          let n = !unnamed_counter in
          incr unnamed_counter;
          "arg" ^ string_of_int n)
        else escape_ts_reserved pname
      | [] ->
        let n = !unnamed_counter in
        incr unnamed_counter;
        "arg" ^ string_of_int n
    in
    let rec print_params first ty =
      match ty.Types.desc with
      | Types.Tarrow ({lbl; typ = arg_type}, return_type, _, _) ->
        (* Skip unit parameters *)
        if lbl = Nolabel && is_unit_type arg_type then (
          ignore (get_next_param_name ());
          print_params first return_type)
        else (
          if use_multiline then (
            P.newline f;
            P.string f "  ")
          else if not first then P.string f ", ";
          (match lbl with
          | Asttypes.Nolabel -> P.string f (get_next_param_name ())
          | Asttypes.Labelled {txt} | Asttypes.Optional {txt} ->
            ignore (get_next_param_name ());
            P.string f (escape_ts_reserved txt));
          (match lbl with
          | Asttypes.Optional _ -> P.string f "?"
          | _ -> ());
          P.string f ": ";
          (* For optional params, unwrap option<T> to just T *)
          let actual_type =
            match lbl with
            | Asttypes.Optional _ -> (
              match remove_option_wrapper lbl arg_type with
              | Some inner -> inner
              | None -> arg_type)
            | _ -> arg_type
          in
          pp_ts_type f (ts_type_of_type_expr actual_type);
          if use_multiline then P.string f ",";
          print_params false return_type)
      | Types.Tlink ty | Types.Tsubst ty | Types.Tpoly (ty, _) ->
        print_params first ty
      | _ -> ()
    in
    print_params true ty);
  if use_multiline then P.newline f;
  P.string f ")";
  (* Print return type *)
  (match return_type_of_fn_type fn_type with
  | Some ret_ty ->
    P.string f ": ";
    pp_ts_type f ret_ty
  | None -> ());
  P.string f ";"

(** Print a value declaration for .d.ts *)
let pp_dts_value_decl (f : P.t) (name : string) (ty : ts_type) : unit =
  P.string f "export const ";
  P.string f (escape_ts_reserved name);
  P.string f ": ";
  pp_ts_type f ty;
  P.string f ";"

(** Print a single value export as either function or const declaration *)
let pp_dts_value_export (f : P.t) (ve : value_export) : unit =
  if is_function_type ve.ve_type then
    pp_dts_function_decl f ve.ve_name ve.ve_params (Some ve.ve_type)
  else pp_dts_value_decl f ve.ve_name (ts_type_of_type_expr ve.ve_type)

(** Print a type declaration for .d.ts *)
let rec pp_dts_type_decl (f : P.t) (decl : type_decl) : unit =
  match decl with
  | TypeAlias {name; type_params; body; external_type; is_opaque} ->
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    (if is_opaque then
       (* Opaque type: wrap with $res.opaque<"brand", params, body> *)
       let brand_name =
         match OpaqueTypes.get_full_name name with
         | Some full_name -> full_name
         | None -> name
       in
       pp_opaque_type f ~brand_name ~type_params ~underlying:(Some body)
     else
       match (body, external_type) with
       | Union types, None ->
         (* Print union types with newlines for readability *)
         pp_union_multiline f types
       | _ -> pp_type_body_with_external f type_params body external_type);
    P.string f ";"
  | Interface {name; type_params; extends; body} ->
    P.string f "export interface ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    (match extends with
    | [] -> ()
    | _ ->
      P.string f " extends ";
      let rec print_extends = function
        | [] -> ()
        | [{name; args}] ->
          P.string f name;
          if args <> [] then (
            P.string f "<";
            pp_ts_type_list f args;
            P.string f ">")
        | {name; args} :: rest ->
          P.string f name;
          if args <> [] then (
            P.string f "<";
            pp_ts_type_list f args;
            P.string f ">");
          P.string f ", ";
          print_extends rest
      in
      print_extends extends);
    P.string f " {";
    P.group f 1 (fun () ->
        List.iter
          (fun prop ->
            P.newline f;
            if prop.prop_readonly then P.string f "readonly ";
            P.string f prop.prop_name;
            if prop.prop_optional then P.string f "?";
            P.string f ": ";
            pp_ts_type f prop.prop_type;
            P.string f ";")
          body.properties;
        match body.index_sig with
        | Some {index_key; index_value} ->
          P.newline f;
          P.string f "[key: ";
          pp_ts_type f index_key;
          P.string f "]: ";
          pp_ts_type f index_value;
          P.string f ";"
        | None -> ());
    P.newline f;
    P.string f "}"
  | VariantType {name; type_params; cases; config} ->
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " =";
    List.iter
      (fun case ->
        P.newline f;
        P.string f "  | ";
        if config.vc_unboxed then pp_unboxed_variant_case f case
        else pp_variant_case f ~tag_name:config.vc_tag_name case)
      cases;
    P.string f ";"
  | GadtType {name; type_params; cases; config} ->
    (* GADT: generate separate types for each constructor, then a union *)
    List.iter
      (fun (case : gadt_case) ->
        P.string f "export type ";
        P.string f name;
        P.string f "$";
        P.string f case.gc_name;
        P.string f " = ";
        if config.vc_unboxed then
          pp_unboxed_variant_case f
            {
              vc_name = case.gc_name;
              vc_tag = case.gc_tag;
              vc_payload = case.gc_payload;
            }
        else
          pp_variant_case f ~tag_name:config.vc_tag_name
            {
              vc_name = case.gc_name;
              vc_tag = case.gc_tag;
              vc_payload = case.gc_payload;
            };
        P.string f ";";
        P.newline f)
      cases;
    P.newline f;
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " =";
    List.iter
      (fun (case : gadt_case) ->
        P.newline f;
        P.string f "  | ";
        P.string f name;
        P.string f "$";
        P.string f case.gc_name)
      cases;
    P.string f ";"
  | OpaqueType {name; type_params; underlying} ->
    (* Generate: export type name<params> = $res.opaque<"name", params, underlying> *)
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    let brand_name =
      match OpaqueTypes.get_full_name name with
      | Some full_name -> full_name
      | None -> name
    in
    pp_opaque_type f ~brand_name ~type_params ~underlying;
    P.string f ";"
  | ExternalType {name; type_params; external_name; external_import_kind; _} ->
    (* Abstract external type: export type t<A> = Set<A>; *)
    (* For abstract types, just alias to the external type directly *)
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    (* Default and namespace imports need typeof since they import values/modules *)
    (match external_import_kind with
    | ExtDefault | ExtNamespace -> P.string f "typeof "
    | ExtNamed _ -> ());
    P.string f external_name;
    if type_params <> [] then (
      P.string f "<";
      let param_names =
        List.map (fun (tp : type_param) -> tp.tp_name) type_params
      in
      P.string f (String.concat ", " param_names);
      P.string f ">");
    P.string f ";"
  | ModuleDecl mod_decl -> pp_module_decl f mod_decl

(** Print a module declaration as namespace + type + const.
    Takes an optional parent_path for nested module qualification.
    @param emit_const If true, emit 'export const' for the module value.
                      Set to false for combined .ts output where implementation provides this. *)
and pp_module_decl_with_path (f : P.t) ~(parent_path : string option)
    ~(emit_const : bool) (mod_decl : module_decl) : unit =
  let {mod_name; mod_types; mod_values; mod_submodules} = mod_decl in
  (* Compute the full module path for type qualification *)
  let module_path =
    match parent_path with
    | Some parent -> parent ^ "." ^ mod_name
    | None ->
      (* Use file module name as prefix for top-level modules *)
      let file_module = get_module_name () in
      if file_module = "" then mod_name else file_module ^ "." ^ mod_name
  in
  (* Collect type names defined in this module for qualification *)
  let local_types = get_module_type_names mod_decl in
  (* Also include submodule names as they can be referenced as types *)
  let local_types_with_submodules =
    List.fold_left
      (fun acc sub -> StringSet.add sub.mod_name acc)
      local_types mod_submodules
  in
  (* Print namespace with types *)
  let has_namespace_content = mod_types <> [] || mod_submodules <> [] in
  if has_namespace_content then (
    P.string f "declare namespace ";
    P.string f mod_name;
    P.string f " {";
    (* Print types in namespace *)
    List.iter
      (fun decl ->
        P.newline f;
        P.string f "  ";
        pp_namespace_type_decl_with_path f ~module_path ~emit_const decl)
      mod_types;
    (* Print nested submodules *)
    List.iter
      (fun sub ->
        P.newline f;
        pp_nested_module_decl_with_path f sub ~indent:2 ~parent_path:module_path
          ~emit_const)
      mod_submodules;
    P.newline f;
    P.string f "}";
    P.newline f);
  (* Print module type (interface for the module value) *)
  P.string f "export type ";
  P.string f mod_name;
  P.string f " = {";
  let all_members =
    List.map (fun v -> (v.mv_name, v.mv_type)) mod_values
    @ List.map
        (fun sub ->
          ( sub.mod_name,
            TypeRef {name = mod_name ^ "." ^ sub.mod_name; args = []} ))
        mod_submodules
  in
  List.iter
    (fun (name, ty) ->
      P.newline f;
      P.string f "  ";
      P.string f name;
      P.string f ": ";
      pp_ts_type_qualified f ~module_path
        ~local_types:local_types_with_submodules ty;
      P.string f ";")
    all_members;
  P.newline f;
  P.string f "};";
  (* Print module value export only if emit_const is true *)
  if emit_const then (
    P.newline f;
    P.string f "export const ";
    P.string f mod_name;
    P.string f ": ";
    P.string f mod_name;
    P.string f ";")

(** Wrapper for pp_module_decl_with_path for .d.ts output (emit const) *)
and pp_module_decl (f : P.t) (mod_decl : module_decl) : unit =
  pp_module_decl_with_path f ~parent_path:None ~emit_const:true mod_decl

(** Wrapper for pp_module_decl_with_path for combined .ts output (no const) *)
and pp_module_decl_ts (f : P.t) (mod_decl : module_decl) : unit =
  pp_module_decl_with_path f ~parent_path:None ~emit_const:false mod_decl

(** Print a type declaration inside a namespace with path context *)
and pp_namespace_type_decl_with_path (f : P.t) ~(module_path : string)
    ~(emit_const : bool) (decl : type_decl) : unit =
  match decl with
  | TypeAlias {name; type_params; body; external_type = _; is_opaque = _} ->
    P.string f "type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    pp_ts_type f body;
    P.string f ";"
  | Interface {name; type_params; body; _} ->
    P.string f "interface ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " { ";
    List.iter
      (fun prop ->
        if prop.prop_readonly then P.string f "readonly ";
        P.string f prop.prop_name;
        if prop.prop_optional then P.string f "?";
        P.string f ": ";
        pp_ts_type f prop.prop_type;
        P.string f "; ")
      body.properties;
    P.string f "}"
  | VariantType {name; type_params; cases; config} ->
    P.string f "type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " =";
    List.iter
      (fun case ->
        P.string f " | ";
        if config.vc_unboxed then pp_unboxed_variant_case f case
        else pp_variant_case f ~tag_name:config.vc_tag_name case)
      cases;
    P.string f ";"
  | GadtType {name; type_params; cases; config} ->
    (* GADT inside module namespace: generate inline *)
    List.iter
      (fun (case : gadt_case) ->
        P.string f "type ";
        P.string f name;
        P.string f "$";
        P.string f case.gc_name;
        P.string f " = ";
        if config.vc_unboxed then
          pp_unboxed_variant_case f
            {
              vc_name = case.gc_name;
              vc_tag = case.gc_tag;
              vc_payload = case.gc_payload;
            }
        else
          pp_variant_case f ~tag_name:config.vc_tag_name
            {
              vc_name = case.gc_name;
              vc_tag = case.gc_tag;
              vc_payload = case.gc_payload;
            };
        P.string f "; ")
      cases;
    P.string f "type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " =";
    List.iter
      (fun (case : gadt_case) ->
        P.string f " | ";
        P.string f name;
        P.string f "$";
        P.string f case.gc_name)
      cases;
    P.string f ";"
  | OpaqueType {name; type_params; underlying} ->
    (* Use full path for opaque brand to avoid conflicts between modules *)
    let brand_name = module_path ^ "." ^ name in
    P.string f "type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    pp_opaque_type f ~brand_name ~type_params ~underlying;
    P.string f ";"
  | ExternalType {name; type_params; external_name; _} ->
    P.string f "type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    P.string f external_name;
    if type_params <> [] then (
      P.string f "<";
      let param_names =
        List.map (fun (tp : type_param) -> tp.tp_name) type_params
      in
      P.string f (String.concat ", " param_names);
      P.string f ">");
    P.string f ";"
  | ModuleDecl sub ->
    pp_nested_module_decl_with_path f sub ~indent:0 ~parent_path:module_path
      ~emit_const

(** Print a nested module declaration with indentation and path context *)
and pp_nested_module_decl_with_path (f : P.t) (mod_decl : module_decl)
    ~(indent : int) ~(parent_path : string) ~(emit_const : bool) : unit =
  let {mod_name; mod_types; mod_values; mod_submodules} = mod_decl in
  let indent_str = String.make indent ' ' in
  (* Compute the full module path for type qualification *)
  let module_path = parent_path ^ "." ^ mod_name in
  (* Collect type names defined in this module for qualification *)
  let local_types = get_module_type_names mod_decl in
  (* Also include submodule names as they can be referenced as types *)
  let local_types_with_submodules =
    List.fold_left
      (fun acc sub -> StringSet.add sub.mod_name acc)
      local_types mod_submodules
  in
  (* Print namespace if there are types or submodules *)
  let has_namespace_content = mod_types <> [] || mod_submodules <> [] in
  if has_namespace_content then (
    P.string f indent_str;
    P.string f "namespace ";
    P.string f mod_name;
    P.string f " {";
    List.iter
      (fun decl ->
        P.newline f;
        P.string f indent_str;
        P.string f "  ";
        pp_namespace_type_decl_with_path f ~module_path ~emit_const decl)
      mod_types;
    List.iter
      (fun sub ->
        P.newline f;
        pp_nested_module_decl_with_path f sub ~indent:(indent + 2)
          ~parent_path:module_path ~emit_const)
      mod_submodules;
    P.newline f;
    P.string f indent_str;
    P.string f "}";
    P.newline f);
  (* Print module type *)
  P.string f indent_str;
  P.string f "type ";
  P.string f mod_name;
  P.string f " = {";
  let all_members =
    List.map (fun v -> (v.mv_name, v.mv_type)) mod_values
    @ List.map
        (fun sub ->
          ( sub.mod_name,
            TypeRef {name = mod_name ^ "." ^ sub.mod_name; args = []} ))
        mod_submodules
  in
  List.iter
    (fun (name, ty) ->
      P.newline f;
      P.string f indent_str;
      P.string f "  ";
      P.string f name;
      P.string f ": ";
      pp_ts_type_qualified f ~module_path
        ~local_types:local_types_with_submodules ty;
      P.string f ";")
    all_members;
  P.newline f;
  P.string f indent_str;
  P.string f "};"

(** Print a type declaration inside a namespace (no export keyword) *)
and pp_namespace_type_decl (f : P.t) (decl : type_decl) : unit =
  match decl with
  | TypeAlias {name; type_params; body; external_type = _; is_opaque = _} ->
    P.string f "type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    pp_ts_type f body;
    P.string f ";"
  | Interface {name; type_params; body; _} ->
    P.string f "interface ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " { ";
    List.iter
      (fun prop ->
        if prop.prop_readonly then P.string f "readonly ";
        P.string f prop.prop_name;
        if prop.prop_optional then P.string f "?";
        P.string f ": ";
        pp_ts_type f prop.prop_type;
        P.string f "; ")
      body.properties;
    P.string f "}"
  | VariantType {name; type_params; cases; config} ->
    P.string f "type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " =";
    List.iter
      (fun case ->
        P.string f " | ";
        if config.vc_unboxed then pp_unboxed_variant_case f case
        else pp_variant_case f ~tag_name:config.vc_tag_name case)
      cases;
    P.string f ";"
  | GadtType {name; type_params; cases; config} ->
    (* GADT in namespace: generate inline *)
    List.iter
      (fun (case : gadt_case) ->
        P.string f "type ";
        P.string f name;
        P.string f "$";
        P.string f case.gc_name;
        P.string f " = ";
        if config.vc_unboxed then
          pp_unboxed_variant_case f
            {
              vc_name = case.gc_name;
              vc_tag = case.gc_tag;
              vc_payload = case.gc_payload;
            }
        else
          pp_variant_case f ~tag_name:config.vc_tag_name
            {
              vc_name = case.gc_name;
              vc_tag = case.gc_tag;
              vc_payload = case.gc_payload;
            };
        P.string f "; ")
      cases;
    P.string f "type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " =";
    List.iter
      (fun (case : gadt_case) ->
        P.string f " | ";
        P.string f name;
        P.string f "$";
        P.string f case.gc_name)
      cases;
    P.string f ";"
  | OpaqueType {name; type_params; underlying} ->
    (* Note: pp_namespace_type_decl without path is only used for non-module contexts.
       For module-nested opaque types, pp_namespace_type_decl_with_path should be used. *)
    P.string f "type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    let brand_name =
      match OpaqueTypes.get_full_name name with
      | Some full_name -> full_name
      | None -> name
    in
    pp_opaque_type f ~brand_name ~type_params ~underlying;
    P.string f ";"
  | ExternalType {name; type_params; external_name; _} ->
    P.string f "type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    P.string f external_name;
    if type_params <> [] then (
      P.string f "<";
      let param_names =
        List.map (fun (tp : type_param) -> tp.tp_name) type_params
      in
      P.string f (String.concat ", " param_names);
      P.string f ">");
    P.string f ";"
  | ModuleDecl sub -> pp_nested_module_decl f sub ~indent:0

(** Print a nested module declaration with indentation *)
and pp_nested_module_decl (f : P.t) (mod_decl : module_decl) ~(indent : int) :
    unit =
  let {mod_name; mod_types; mod_values; mod_submodules} = mod_decl in
  let indent_str = String.make indent ' ' in
  (* Print namespace if there are types or submodules *)
  let has_namespace_content = mod_types <> [] || mod_submodules <> [] in
  if has_namespace_content then (
    P.string f indent_str;
    P.string f "namespace ";
    P.string f mod_name;
    P.string f " {";
    List.iter
      (fun decl ->
        P.newline f;
        P.string f indent_str;
        P.string f "  ";
        pp_namespace_type_decl f decl)
      mod_types;
    List.iter
      (fun sub ->
        P.newline f;
        pp_nested_module_decl f sub ~indent:(indent + 2))
      mod_submodules;
    P.newline f;
    P.string f indent_str;
    P.string f "}";
    P.newline f);
  (* Print module type *)
  P.string f indent_str;
  P.string f "type ";
  P.string f mod_name;
  P.string f " = {";
  let all_members =
    List.map (fun v -> (v.mv_name, v.mv_type)) mod_values
    @ List.map
        (fun sub ->
          ( sub.mod_name,
            TypeRef {name = mod_name ^ "." ^ sub.mod_name; args = []} ))
        mod_submodules
  in
  List.iter
    (fun (name, ty) ->
      P.newline f;
      P.string f indent_str;
      P.string f "  ";
      P.string f name;
      P.string f ": ";
      pp_ts_type f ty;
      P.string f ";")
    all_members;
  P.newline f;
  P.string f indent_str;
  P.string f "};"

(* Initialize the forward references for pp_module_decl *)
let () = pp_module_decl_ref := pp_module_decl
let () = pp_module_decl_ts_ref := pp_module_decl_ts

(** Generate type-only import for .d.ts files *)
let pp_dts_import (f : P.t) (module_name : string) (module_path : string) : unit
    =
  P.string f "import type * as ";
  P.string f module_name;
  P.string f " from \"";
  P.string f module_path;
  P.string f "\";"

(** Generate namespace import for runtime types (for .d.ts files) *)
let pp_runtime_types_import_dts (f : P.t) : unit =
  if RuntimeTypes.has_any () then (
    P.string f "import type * as ";
    P.string f runtime_types_namespace;
    P.string f " from \"@rescript/runtime/types\";";
    P.newline f)

type dts_import = {
  module_name: string;
  module_path: string;  (** Path with .d.ts/.d.mts/.d.cts extension *)
}
(** Type import info for .d.ts generation *)

(** Print type imports from external packages *)
let pp_external_type_imports (f : P.t) : unit =
  let all_imports = ExternalTypeImports.get_all () in
  List.iter
    (fun (module_path, import_kinds) ->
      let named_imports =
        List.filter_map
          (function
            | ImportNamed name -> Some name
            | _ -> None)
          import_kinds
      in
      let default_import =
        List.find_map
          (function
            | ImportDefault name -> Some name
            | _ -> None)
          import_kinds
      in
      let namespace_import =
        List.find_map
          (function
            | ImportNamespace name -> Some name
            | _ -> None)
          import_kinds
      in
      (* Emit default import: import type LocalName from "module" *)
      (match default_import with
      | Some name ->
        P.string f "import type ";
        P.string f name;
        P.string f " from \"";
        P.string f module_path;
        P.string f "\";";
        P.newline f
      | None -> ());
      (* Emit namespace import: import type * as LocalName from "module" *)
      (match namespace_import with
      | Some name ->
        P.string f "import type * as ";
        P.string f name;
        P.string f " from \"";
        P.string f module_path;
        P.string f "\";";
        P.newline f
      | None -> ());
      (* Emit named imports: import type { A, B } from "module" *)
      if named_imports <> [] then (
        P.string f "import type { ";
        P.string f (String.concat ", " named_imports);
        P.string f " } from \"";
        P.string f module_path;
        P.string f "\";";
        P.newline f))
    all_imports

(** Collect names of locally defined modules from type declarations *)
let rec collect_local_module_names (decls : type_decl list) : StringSet.t =
  List.fold_left
    (fun acc decl ->
      match decl with
      | ModuleDecl {mod_name; mod_submodules; _} ->
        (* Add this module and recursively add submodules *)
        let with_this = StringSet.add mod_name acc in
        let sub_decls = List.map (fun m -> ModuleDecl m) mod_submodules in
        StringSet.union with_this (collect_local_module_names sub_decls)
      | _ -> acc)
    StringSet.empty decls

(** Generate the complete .d.ts file content
    TODO(refactor): Move it to a part of dump program *)
let pp_dts_file ~(module_name : string) (f : P.t) (imports : dts_import list)
    (type_decls : type_decl list) (value_exports : value_export list) : unit =
  reset_state ();
  List.iter collect_type_deps_decl type_decls;
  List.iter
    (fun ve -> collect_type_deps (ts_type_of_type_expr ve.ve_type))
    value_exports;
  let has_runtime_types = RuntimeTypes.has_any () in
  let has_external_type_imports = ExternalTypeImports.has_any () in
  let required_modules = TypeModuleDeps.get_used () in
  let local_modules = collect_local_module_names type_decls in
  let provided_imports =
    List.fold_left
      (fun acc imp -> StringSet.add imp.module_name acc)
      StringSet.empty imports
  in
  (* Filter imports to only those that are actually used in types,
     and add any missing module imports.
     Exclude locally defined modules - they should not be imported. *)
  let type_imports =
    let used_provided =
      List.filter
        (fun imp ->
          List.mem imp.module_name required_modules
          && not (StringSet.mem imp.module_name local_modules))
        imports
    in
    (* Add missing imports for modules that are required but not provided,
       excluding local modules *)
    let missing_modules =
      List.filter
        (fun m ->
          (not (StringSet.mem m provided_imports))
          && not (StringSet.mem m local_modules))
        required_modules
    in
    let missing_imports =
      List.map
        (fun module_name ->
          (* Generate import path: ./ModuleName.js *)
          {module_name; module_path = "./" ^ module_name ^ ".js"})
        missing_modules
    in
    used_provided @ missing_imports
  in
  P.string f Bs_version.header;
  P.at_least_two_lines f;
  pp_runtime_types_import_dts f;
  pp_external_type_imports f;
  List.iter
    (fun imp ->
      pp_dts_import f imp.module_name imp.module_path;
      P.newline f)
    type_imports;
  if type_imports <> [] || has_runtime_types || has_external_type_imports then
    P.at_least_two_lines f;
  let all_items =
    List.map (fun decl -> `Type decl) type_decls
    @ List.map (fun ve -> `Value ve) value_exports
  in
  collect_opaque_types ~module_name type_decls;
  set_module_name module_name;
  let rec print_items = function
    | [] -> ()
    | [`Type decl] -> pp_dts_type_decl f decl
    | [`Value ve] -> pp_dts_value_export f ve
    | `Type decl :: rest ->
      pp_dts_type_decl f decl;
      P.at_least_two_lines f;
      print_items rest
    | `Value ve :: rest ->
      pp_dts_value_export f ve;
      P.at_least_two_lines f;
      print_items rest
  in
  print_items all_items;
  P.flush f ()
