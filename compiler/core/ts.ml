(** 
  TypeScript Codegen IR

  A typed intermediate representation for generating
  TypeScript/JavaScript with type annotations and JSDoc.
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

(** {1 Type Declarations} *)

type variant_case = {
  vc_name: string;
  vc_tag: string;  (** The TAG value, e.g. "Ok", "Error" *)
  vc_payload: ts_type option;  (** None for nullary constructors *)
}
(** Variant constructor for tagged unions *)

type type_decl =
  | TypeAlias of {name: string; type_params: type_param list; body: ts_type}
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
    }

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

(** Current environment for type lookups during conversion.
    This is set before calling ts_type_of_type_expr. *)
let current_env : Env.t option ref = ref None

(** Get the @as renamed name for a type from its declaration, or None if not renamed *)
let get_type_as_name (env : Env.t) (path : Path.t) : string option =
  try
    let decl = Env.find_type path env in
    get_as_string decl.type_attributes
  with Not_found -> None

(** Compute the display name for a type path, applying @as renaming if available *)
let type_display_name (path : Path.t) : string =
  match !current_env with
  | None -> Path.name path
  | Some env -> (
    (* Split path into module prefix and type name *)
    match path with
    | Path.Pdot (prefix, _type_name, _pos) -> (
      match get_type_as_name env path with
      | Some renamed -> Path.name prefix ^ "." ^ renamed
      | None -> Path.name path)
    | Path.Pident id -> (
      match get_type_as_name env path with
      | Some renamed -> renamed
      | None -> Ident.name id)
    | Path.Papply _ -> Path.name path)

(** Mapping from anonymous type variable IDs to generated names.
    This is populated by collect_type_vars and used by ts_type_of_type_expr. *)
let anon_type_var_names : (int, string) Hashtbl.t = Hashtbl.create 16

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
  | Tvar (Some name) -> TypeVar name
  | Tarrow ({lbl; typ = arg_type}, return_type, _, _) -> (
    let param =
      {
        param_name =
          (match lbl with
          | Nolabel -> None
          | Labelled {txt} | Optional {txt} -> Some txt);
        param_type = ts_type_of_type_expr arg_type;
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
          fn_params = param :: fn_params;
          fn_rest;
          fn_return;
          fn_type_params;
          fn_async = false;
        }
    | return_ts ->
      Function
        {
          fn_params = [param];
          fn_rest = None;
          fn_return = return_ts;
          fn_type_params = [];
          fn_async = false;
        })
  | Ttuple types -> Tuple (List.map ts_type_of_type_expr types)
  | Tconstr (path, args, _) -> ts_type_of_constr path args
  | Tlink ty | Tsubst ty -> ts_type_of_type_expr ty
  | Tpoly (ty, _) -> ts_type_of_type_expr ty
  | Tvariant _ ->
    (* Polymorphic variants - simplified to union of strings for now *)
    Any
  | Tobject _ ->
    (* Object types - need more work *)
    Any
  | Tfield _ | Tnil -> Any
  | Tunivar _ -> Any
  | Tpackage _ -> Any

and ts_type_of_constr (path : Path.t) (args : Types.type_expr list) : ts_type =
  (* Handle built-in types specially *)
  match Path.name path with
  | "int" | "float" -> Number
  | "bool" -> Boolean
  | "string" -> String
  | "unit" -> Void
  | "array" -> (
    match args with
    | [elem] -> Array (ts_type_of_type_expr elem)
    | _ -> Any)
  | "option" -> (
    match args with
    | [elem] -> Union [ts_type_of_type_expr elem; Undefined]
    | _ -> Any)
  | "null" -> (
    match args with
    | [elem] -> Union [ts_type_of_type_expr elem; Null]
    | _ -> Any)
  | "nullable" -> (
    match args with
    | [elem] -> Union [ts_type_of_type_expr elem; Null; Undefined]
    | _ -> Any)
  | "promise" | "Promise.t" -> (
    match args with
    | [elem] -> Promise (ts_type_of_type_expr elem)
    | _ -> Promise Any)
  | "bigint" -> Bigint
  | "symbol" -> Symbol
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
    (* Extract parameter types from the function type *)
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
      | Types.Tlink ty | Types.Tsubst ty -> find_return ty
      | _ -> ts_type_of_type_expr ty
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
      | Types.Tpoly (ty, _) -> collect ty
      | _ -> ()
    in
    collect ty;
    List.rev !vars

(** {1 Type Declaration Extraction} *)

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

(** Extract a type declaration from Types.type_declaration *)
let type_decl_of_type_declaration (id : Ident.t) (decl : Types.type_declaration)
    : type_decl option =
  let type_params = type_params_of_type_exprs decl.type_params in
  (* Use @as name for the type if present, otherwise use the original type name *)
  let type_name =
    match get_as_string decl.type_attributes with
    | Some renamed -> renamed
    | None -> Ident.name id
  in
  match decl.type_kind with
  | Types.Type_record (labels, _) ->
    (* Record type -> Interface *)
    let properties =
      List.map
        (fun (ld : Types.label_declaration) ->
          (* Use @as name if present, otherwise use the original field name *)
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
    Some
      (Interface
         {
           name = type_name;
           type_params;
           extends = [];
           body = {properties; index_sig = None; call_sig = None};
         })
  | Types.Type_variant constructors ->
    (* Variant type -> Tagged union *)
    let cases =
      List.map
        (fun (cd : Types.constructor_declaration) ->
          let name = Ident.name cd.cd_id in
          (* Use @as name for the tag if present, otherwise use the constructor name *)
          let tag =
            match get_as_string cd.cd_attributes with
            | Some renamed -> renamed
            | None -> name
          in
          let payload =
            match cd.cd_args with
            | Cstr_tuple [] -> None
            | Cstr_tuple [arg] -> Some (ts_type_of_type_expr arg)
            | Cstr_tuple args ->
              Some (Tuple (List.map ts_type_of_type_expr args))
            | Cstr_record labels ->
              let properties =
                List.map
                  (fun (ld : Types.label_declaration) ->
                    (* Use @as name for record fields if present *)
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
              Some (Object {properties; index_sig = None; call_sig = None})
          in
          {vc_name = name; vc_tag = tag; vc_payload = payload})
        constructors
    in
    Some (VariantType {name = type_name; type_params; cases})
  | Types.Type_abstract -> (
    (* Abstract type with manifest -> type alias *)
    match decl.type_manifest with
    | Some ty ->
      Some
        (TypeAlias
           {name = type_name; type_params; body = ts_type_of_type_expr ty})
    | None -> None (* Opaque abstract types can't be represented *))
  | Types.Type_open -> None (* Open types not supported *)

(** Extract all type declarations from a Typedtree structure *)
let extract_type_decls (str : Typedtree.structure) : type_decl list =
  let decls = ref [] in
  List.iter
    (fun (item : Typedtree.structure_item) ->
      match item.str_desc with
      | Typedtree.Tstr_type (_, type_decls) ->
        List.iter
          (fun (td : Typedtree.type_declaration) ->
            match type_decl_of_type_declaration td.typ_id td.typ_type with
            | Some decl -> decls := decl :: !decls
            | None -> ())
          type_decls
      | _ -> ())
    str.str_items;
  List.rev !decls

type value_export = {
  ve_name: string;
  ve_type: Types.type_expr;
  ve_params: string list;  (** Parameter names for functions *)
}
(** Value export with its type for .d.ts generation *)

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
              exports :=
                {
                  ve_name = Ident.name id;
                  ve_type = vb.vb_expr.exp_type;
                  ve_params = params;
                }
                :: !exports
            | None -> ())
          bindings
      | Typedtree.Tstr_primitive vd ->
        exports :=
          {
            ve_name = Ident.name vd.val_id;
            ve_type = vd.val_val.val_type;
            ve_params = [];
          }
          :: !exports
      | _ -> ())
    str.str_items;
  List.rev !exports

(** Set the environment for @as type renaming lookups.
    Call this before generating TypeScript output that uses ts_type_of_type_expr. *)
let set_env (env : Env.t) : unit = current_env := Some env

(** Clear the environment after generating TypeScript output. *)
let clear_env () : unit = current_env := None

(** {1 Type Printing} *)

module P = Ext_pp

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

and pp_ts_type_list (f : P.t) (types : ts_type list) : unit =
  match types with
  | [] -> ()
  | [ty] -> pp_ts_type f ty
  | ty :: rest ->
    pp_ts_type f ty;
    P.string f ", ";
    pp_ts_type_list f rest

and pp_union (f : P.t) (types : ts_type list) : unit =
  match types with
  | [] -> P.string f "never"
  | [ty] -> pp_ts_type f ty
  | ty :: rest ->
    pp_ts_type f ty;
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
  P.string f "}"

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
  match params with
  | [] -> ()
  | [p] -> pp_param f p
  | p :: rest ->
    pp_param f p;
    P.string f ", ";
    pp_params f rest

and pp_param (f : P.t) (p : param_type) : unit =
  (match p.param_name with
  | Some name -> P.string f name
  | None -> P.string f "_");
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

(** Print generic type parameters <A, B, ...> if any type variables exist *)
let pp_type_params_from_ml (f : P.t) (fn_type : Types.type_expr option) : unit =
  let vars = collect_type_vars fn_type in
  match vars with
  | [] -> ()
  | _ ->
    P.string f "<";
    let rec print_vars = function
      | [] -> ()
      | [v] -> P.string f (String.capitalize_ascii v)
      | v :: rest ->
        P.string f (String.capitalize_ascii v);
        P.string f ", ";
        print_vars rest
    in
    print_vars vars;
    P.string f ">"

(** {1 Type Declaration Printing} *)

(** Print a variant case for tagged unions *)
let pp_variant_case (f : P.t) (case : variant_case) : unit =
  P.string f "{ TAG: \"";
  P.string f case.vc_tag;
  P.string f "\"";
  (match case.vc_payload with
  | None -> ()
  | Some payload_ty ->
    P.string f "; _0: ";
    pp_ts_type f payload_ty);
  P.string f " }"

(** Print a type declaration *)
let pp_type_decl (f : P.t) (decl : type_decl) : unit =
  match decl with
  | TypeAlias {name; type_params; body} ->
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    pp_ts_type f body;
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
  | VariantType {name; type_params; cases} ->
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
        pp_variant_case f case)
      cases;
    P.string f ";"

(** Print all type declarations *)
let pp_type_decls (f : P.t) (decls : type_decl list) : unit =
  List.iter
    (fun decl ->
      pp_type_decl f decl;
      P.at_least_two_lines f)
    decls

(** {1 Declaration File (.d.ts) Generation} *)

(** Check if a type is a function type *)
let is_function_type (ty : Types.type_expr) : bool =
  let rec check ty =
    match ty.Types.desc with
    | Types.Tarrow _ -> true
    | Types.Tlink ty | Types.Tsubst ty -> check ty
    | _ -> false
  in
  check ty

(** Print a function declaration for .d.ts *)
let pp_dts_function_decl (f : P.t) (name : string) (param_names : string list)
    (fn_type : Types.type_expr option) : unit =
  P.string f "export function ";
  P.string f name;
  (* Print type parameters *)
  pp_type_params_from_ml f fn_type;
  (* Print parameters *)
  P.string f "(";
  (match fn_type with
  | None -> ()
  | Some ty ->
    let param_names_ref = ref param_names in
    let get_next_param_name () =
      match !param_names_ref with
      | name :: rest ->
        param_names_ref := rest;
        name
      | [] -> "_"
    in
    let rec print_params first ty =
      match ty.Types.desc with
      | Types.Tarrow ({lbl; typ = arg_type}, return_type, _, _) ->
        if not first then P.string f ", ";
        (match lbl with
        | Asttypes.Nolabel -> P.string f (get_next_param_name ())
        | Asttypes.Labelled {txt} | Asttypes.Optional {txt} ->
          ignore (get_next_param_name ());
          P.string f txt);
        (match lbl with
        | Asttypes.Optional _ -> P.string f "?"
        | _ -> ());
        P.string f ": ";
        pp_ts_type f (ts_type_of_type_expr arg_type);
        print_params false return_type
      | Types.Tlink ty | Types.Tsubst ty -> print_params first ty
      | _ -> ()
    in
    print_params true ty);
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
  P.string f name;
  P.string f ": ";
  pp_ts_type f ty;
  P.string f ";"

(** Print a single value export as either function or const declaration *)
let pp_dts_value_export (f : P.t) (ve : value_export) : unit =
  if is_function_type ve.ve_type then
    pp_dts_function_decl f ve.ve_name ve.ve_params (Some ve.ve_type)
  else pp_dts_value_decl f ve.ve_name (ts_type_of_type_expr ve.ve_type)

(** Print a type declaration for .d.ts *)
let pp_dts_type_decl (f : P.t) (decl : type_decl) : unit =
  match decl with
  | TypeAlias {name; type_params; body} ->
    P.string f "export type ";
    P.string f name;
    if type_params <> [] then (
      P.string f "<";
      pp_type_params f type_params;
      P.string f ">");
    P.string f " = ";
    pp_ts_type f body;
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
  | VariantType {name; type_params; cases} ->
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
        pp_variant_case f case)
      cases;
    P.string f ";"

(** Generate type-only import for .d.ts files *)
let pp_dts_import (f : P.t) (module_name : string) (module_path : string) : unit
    =
  P.string f "import type * as ";
  P.string f module_name;
  P.string f " from \"";
  P.string f module_path;
  P.string f "\";"

type dts_import = {
  module_name: string;
  module_path: string;  (** Path with .d.ts/.d.mts/.d.cts extension *)
}
(** Type import info for .d.ts generation *)

(** Generate the complete .d.ts file content *)
let pp_dts_file (f : P.t) (imports : dts_import list)
    (type_decls : type_decl list) (value_exports : value_export list) : unit =
  (* Print header comment *)
  P.string f "// Generated by ReScript, PLEASE EDIT WITH CARE";
  P.at_least_two_lines f;
  (* Print type imports *)
  List.iter
    (fun imp ->
      pp_dts_import f imp.module_name imp.module_path;
      P.newline f)
    imports;
  if imports <> [] then P.at_least_two_lines f;
  (* Print type declarations with blank lines between them *)
  let all_items =
    List.map (fun decl -> `Type decl) type_decls
    @ List.map (fun ve -> `Value ve) value_exports
  in
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
