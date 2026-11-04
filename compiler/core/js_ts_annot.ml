(* Experimental TypeScript emission ([-bs-emit-ts]).

   The typed tree gives each top-level function's parameters and result a
   [Types.type_expr]; genType's translation turns each into TypeScript. The
   printer looks the parameters and the function up by their [Ident.t]
   (name and stamp), so an annotation reaches the output only when the
   identifier the J IR prints is the one the typed tree bound. *)

type key = string * int

type param = {
  ty: string;
  optional: bool;
      (** printed [p?: ty]: an optional labelled argument followed only by
          optional labelled arguments, with [ty] the option's element type *)
}

type state = {
  params: (key, param) Hashtbl.t;  (** parameter ident -> its annotation *)
  results: (key, string) Hashtbl.t;  (** top-level function -> result type *)
  generics: (key, string list) Hashtbl.t;
      (** top-level function -> its type parameters, in order of occurrence,
          each with its bound if it has one *)
  names: (string, int) Hashtbl.t;
      (** top-level function name -> its stamp, to report lost stamps *)
  mutable header: string;  (** the module's type declarations *)
  mutable exports: Ident.t list;  (** the program's exports *)
  inline_exports: (key, unit) Hashtbl.t;
      (** exports printed as [export function] *)
}

let fresh () =
  {
    params = Hashtbl.create 16;
    results = Hashtbl.create 16;
    generics = Hashtbl.create 16;
    names = Hashtbl.create 16;
    header = "";
    exports = [];
    inline_exports = Hashtbl.create 16;
  }

let key = Request_local.new_key fresh
let current () = Domain.DLS.get key
let enabled () = !((Js_config.current ()).emit_ts)
let key_of (id : Ident.t) : key = (id.name, id.stamp)

(* [.res.js] -> [.res.ts], [.mjs] -> [.mts], [.cjs] -> [.cts] *)
let ts_suffix suffix =
  if Filename.check_suffix suffix ".mjs" then
    Filename.chop_suffix suffix ".mjs" ^ ".mts"
  else if Filename.check_suffix suffix ".cjs" then
    Filename.chop_suffix suffix ".cjs" ^ ".cts"
  else if Filename.check_suffix suffix ".js" then
    Filename.chop_suffix suffix ".js" ^ ".ts"
  else suffix ^ ".ts"

let config () = {Gentype_config.default with everything = true}

(* [option<t>] holds [undefined] for [None], and for [Some(v)] the value
   [Primitive_option.some(v)]: [v] itself, or a frozen marker object
   [{BS_PRIVATE_NESTED_SOME_NONE: depth}] when [v] is [undefined] or a marker.
   The compiler unwraps and wraps with the identity where the payload type
   cannot hold [undefined] ([Typeopt.type_cannot_contain_undefined]); there
   [option<t>] prints [undefined | t]. Elsewhere it prints
   [undefined | t | {readonly BS_PRIVATE_NESTED_SOME_NONE: number}], except
   at a function's type parameter, which is bounded instead (below). *)
let payload_may_nest ~env (t : Types.type_expr) =
  not (Typeopt.type_cannot_contain_undefined t env)

let signature_payload_may_nest ~env (t : Types.type_expr) =
  match (Ctype.repr t).desc with
  | Tvar _ -> false
  | _ -> payload_may_nest ~env t

(* The bound of a function's type parameter that is an option's payload: the
   types none of whose values is [undefined] or a marker, so that
   [Primitive_option.some] and [valFromOption] return such a value unchanged
   and [option<a>] is exactly [undefined | a]. A primitive owns no key
   [BS_PRIVATE_NESTED_SOME_NONE], and its prototypes hold none. A record type
   does not exclude the key: TypeScript's object types admit further keys. *)
let payload_bound = "string | number | boolean | bigint"

(* The type variables that are the payload of an option in [t]. *)
let rec payload_vars (t : Gentype_common.type_) =
  match t with
  | Option (TypeVar v) -> [v]
  | Array (t, _) | Dict t | Null t | Nullable t | Option t | Promise t ->
    payload_vars t
  | Function {arg_types; ret_type} ->
    List.concat_map
      (fun (a : Gentype_common.arg_type) -> payload_vars a.a_type)
      arg_types
    @ payload_vars ret_type
  | Ident {type_args} -> List.concat_map payload_vars type_args
  | Object (_, fields) ->
    List.concat_map
      (fun (f : Gentype_common.field) ->
        match (f.optional, f.type_) with
        | Optional, TypeVar v -> [v]
        | _ -> payload_vars f.type_)
      fields
  | Tuple ts -> List.concat_map payload_vars ts
  | TypeVar _ -> []
  | Variant {inherits; payloads} ->
    List.concat_map payload_vars inherits
    @ List.concat_map
        (fun (p : Gentype_common.payload) -> payload_vars p.t)
        payloads

let type_to_string type_ =
  Emit_type.type_to_string ~config:(config ())
    ~type_name_is_interface:(fun _ -> false)
    type_

(* The types of one function's parameters and result, translated with one
   namer for anonymous type variables, so a variable shared between them gets
   one name. Also the type variables, in order of first occurrence. *)
let ts_of_signature ~env type_env (type_exprs : Types.type_expr list) =
  let config =
    {(config ()) with option_payload_may_nest = signature_payload_may_nest ~env}
  in
  let translations =
    Translate_type_expr_from_types.translate_type_exprs_from_types ~config
      ~type_env type_exprs
  in
  let type_vars =
    List.fold_left
      (fun acc (t : Translate_type_expr_from_types.translation) ->
        List.fold_left
          (fun acc v -> if List.mem v acc then acc else acc @ [v])
          acc (Type_vars.free t.type_))
      [] translations
  in
  ( Ext_list.map translations (fun t -> type_to_string t.type_),
    type_vars,
    Ext_list.map translations (fun t -> t.type_) )

let debug = Sys.getenv_opt "RESCRIPT_EMIT_TS_DEBUG" <> None

let is_optional (p : Typedtree.function_param) =
  match p.fp_lbl with
  | Optional _ -> true
  | Nolabel | Labelled _ -> false

(* The element type of [option<t>]. *)
let option_element (t : Types.type_expr) =
  match (Ctype.repr t).desc with
  | Tconstr (path, [elt], _) when Path.same path Predef.path_option -> Some elt
  | _ -> None

(* The parameters printed [p?: t]: the optional labelled arguments after the
   last mandatory one, because TypeScript requires every parameter after an
   optional one to be optional, whose element type [t] holds no marker. An
   optional argument earlier in the list prints [p: undefined | t]; a call
   from ReScript passes [undefined] for an omitted argument in either
   position. *)
let rec trailing_optionals ~env (params : Typedtree.function_param list) =
  match params with
  | [] -> ([], true)
  | p :: rest ->
    let flags, all_optional = trailing_optionals ~env rest in
    let optional =
      all_optional && is_optional p
      &&
      match option_element p.fp_pat.pat_type with
      | Some elt -> not (signature_payload_may_nest ~env elt)
      | None -> false
    in
    (optional :: flags, optional)

let collect_function st type_env (id : Ident.t) (e : Typedtree.expression) =
  match e.exp_desc with
  | Texp_function {params; body} ->
    Hashtbl.replace st.names id.name id.stamp;
    let env = e.exp_env in
    let optionals, _ = trailing_optionals ~env params in
    let param_types =
      List.map2
        (fun (p : Typedtree.function_param) optional ->
          match option_element p.fp_pat.pat_type with
          | Some elt when optional -> elt
          | _ -> p.fp_pat.pat_type)
        params optionals
    in
    let types, type_vars, translated =
      ts_of_signature ~env type_env (param_types @ [body.exp_type])
    in
    let bounded =
      List.concat
        (List.map2
           (fun optional (t : Gentype_common.type_) ->
             match (optional, t) with
             | true, TypeVar v -> [v]
             | _ -> payload_vars t)
           (optionals @ [false]) translated)
    in
    if type_vars <> [] then
      Hashtbl.replace st.generics (key_of id)
        (Ext_list.map type_vars (fun v ->
             if List.mem v bounded then v ^ " extends " ^ payload_bound else v));
    let rec bind params optionals types =
      match (params, optionals, types) with
      | [], [], [result] -> Hashtbl.replace st.results (key_of id) result
      | ( (p : Typedtree.function_param) :: params,
          optional :: optionals,
          ty :: types ) ->
        Hashtbl.replace st.params (key_of p.fp_param) {ty; optional};
        bind params optionals types
      | _ -> assert false
    in
    bind params optionals types
  | _ -> ()

let is_type_declaration (item : Typedtree.structure_item) =
  match item.str_desc with
  | Tstr_type _ -> true
  | _ -> false

let header_of_structure ~module_name (str : Typedtree.structure) =
  let config =
    {
      (config ()) with
      option_payload_may_nest = payload_may_nest ~env:str.str_final_env;
    }
  in
  let type_env = Type_env.root () in
  let output_file_relative = module_name ^ ".res.ts" in
  let resolver =
    Module_resolver.create_lazy_resolver ~config
      ~extensions:[".res"; ".shim.ts"] ~exclude_file:(fun _ -> false)
  in
  let translation =
    {str with str_items = Ext_list.filter str.str_items is_type_declaration}
    |> Translate_structure.translate_structure ~config ~output_file_relative
         ~resolver ~type_env
    |> Translation.combine
    |> Translation.add_type_declarations_from_module_equations ~type_env
  in
  Emit_js.emit_translation_as_string ~config
    ~file_name:(Module_name.from_string_unsafe module_name)
    ~output_file_relative ~resolver
    ~input_cmt_translate_type_declarations:
      Gentype_main.input_cmt_translate_type_declarations translation

let collect ~module_name (str : Typedtree.structure) =
  let st = current () in
  Hashtbl.reset st.params;
  Hashtbl.reset st.results;
  Hashtbl.reset st.generics;
  Hashtbl.reset st.names;
  let type_env = Type_env.root () in
  (* Functions of nested modules are printed at the top level too. *)
  let rec items (str : Typedtree.structure) =
    Ext_list.iter str.str_items (fun item ->
        match item.str_desc with
        | Tstr_value (_, vbs) ->
          Ext_list.iter vbs (fun (vb : Typedtree.value_binding) ->
              match vb.vb_pat.pat_desc with
              | Tpat_var (id, _) -> collect_function st type_env id vb.vb_expr
              | _ -> ())
        | Tstr_module {mb_expr = {mod_desc = Tmod_structure s}} -> items s
        | _ -> ())
  in
  items str;
  st.header <- header_of_structure ~module_name str

let header () = (current ()).header

(* Report, once per top-level function the printer names, whether its stamp
   and its parameters' stamps are the typed tree's. *)
let report_function (id : Ident.t) (params : Ident.t list) =
  if debug then
    let st = current () in
    let fn =
      if Hashtbl.mem st.results (key_of id) then "stamp kept"
      else
        match Hashtbl.find_opt st.names id.name with
        | Some s -> Printf.sprintf "stamp lost (typed %d, J %d)" s id.stamp
        | None -> "not a typed top-level function"
    in
    let ps =
      Ext_list.map params (fun p ->
          Printf.sprintf "%s/%d:%s" p.Ident.name p.stamp
            (if Hashtbl.mem st.params (key_of p) then "kept" else "lost"))
    in
    Printf.eprintf "emit-ts: %s/%d %s; params %s\n" id.name id.stamp fn
      (String.concat ", " ps)

let param_type (id : Ident.t) = Hashtbl.find_opt (current ()).params (key_of id)

let result_type (id : Ident.t) =
  Hashtbl.find_opt (current ()).results (key_of id)

let type_params (id : Ident.t) =
  match Hashtbl.find_opt (current ()).generics (key_of id) with
  | Some vars -> "<" ^ String.concat ", " vars ^ ">"
  | None -> ""

let set_exports exports =
  let st = current () in
  st.exports <- exports;
  Hashtbl.reset st.inline_exports

(* A top-level function is printed [export function f] when it is exported
   under its own name. The printer is assumed to print a top-level function
   under its source name. *)
let export_inline (id : Ident.t) =
  enabled ()
  &&
  let st = current () in
  Ext_list.exists st.exports (fun e -> Ident.same e id)
  && Ext_ident.convert id.name = id.name
  &&
  (Hashtbl.replace st.inline_exports (key_of id) ();
   true)

let exported_inline (id : Ident.t) =
  enabled () && Hashtbl.mem (current ()).inline_exports (key_of id)
