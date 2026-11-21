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
  overloads: (key, ((key * string) list * string) list) Hashtbl.t;
      (** top-level function -> its overloads, each its parameters' types by
          their idents and its result type *)
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
    overloads = Hashtbl.create 16;
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
   [Primitive_option.some(v)]: the frozen marker
   [{BS_PRIVATE_NESTED_SOME_NONE: 0}] when [v] is [undefined], the marker of
   depth [d + 1] when [v] is the marker of depth [d], and [v] itself
   otherwise. The compiler wraps and unwraps with the identity where the
   payload type cannot hold [undefined]
   ([Typeopt.type_cannot_contain_undefined]); there [option<t>] prints
   [undefined | t]. Elsewhere it prints [t] beside the marker of each depth
   [Some(v)] can be, read off the encoding of [t]:
   - the values of [option<u>] are [undefined] and [Some(v)] for [v] in [u];
   - the value of [unit] is [undefined];
   - a value of a type that cannot hold [undefined] and owns no field
     [BS_PRIVATE_NESTED_SOME_NONE] is no marker either; a record or an
     inline record naming that field at run time ([@as]), or an object type,
     may hold any depth there, which [Primitive_option.some] reads.
   The depth of a marker is a literal type where the encoding is known, and
   [number] elsewhere: at an abstract type, at a type that may own the key,
   and at a type declaration's parameter. A function's type parameter that
   is an option's payload is bounded instead (below), so its values are
   neither [undefined] nor markers. *)
type encoding =
  | Known of {undefined: bool; markers: int list}
      (** whether a value can be [undefined], and the depths of the markers
          it can be *)
  | Unknown

let some_markers ~undefined ~markers =
  (if undefined then [0] else []) @ List.map succ markers

let marker_key = "BS_PRIVATE_NESTED_SOME_NONE"

(* Whether a value of [t] may own the marker key: a record or an inline
   record with a field of that runtime name, an object type, or a variant
   case's payload that may; a declaration not found may. *)
let rec may_own_marker_key ~env ~visiting (t : Types.type_expr) =
  let owns (labels : Types.label_declaration list) =
    List.exists (fun l -> Record_runtime.declaration_name l = marker_key) labels
  in
  match (Ctype.expand_head_opt env t).desc with
  | Tobject _ -> true
  | Tconstr (path, _, _) when List.exists (Path.same path) visiting -> false
  | Tconstr (path, _, _) -> (
    match Env.find_type path env with
    | {type_kind = Type_record (labels, _)} -> owns labels
    | {type_kind = Type_variant (cases, _)} ->
      let visiting = path :: visiting in
      List.exists
        (fun (c : Types.constructor_declaration) ->
          match c.cd_args with
          | Cstr_record labels -> owns labels
          | Cstr_tuple args ->
            List.exists (may_own_marker_key ~env ~visiting) args)
        cases
    | _ -> false
    | exception Not_found -> true)
  | _ -> false

let rec encoding ~env ~var (t : Types.type_expr) =
  match (Ctype.expand_head_opt env t).desc with
  | Tconstr (path, [u], _) when Path.same path Predef.path_option -> (
    match encoding ~env ~var u with
    | Known {undefined; markers} ->
      Known {undefined = true; markers = some_markers ~undefined ~markers}
    | Unknown -> Unknown)
  | Tconstr (path, [], _) when Path.same path Predef.path_unit ->
    Known {undefined = true; markers = []}
  | Tvar _ -> var t
  | _ ->
    if
      Typeopt.type_cannot_contain_undefined t env
      && not (may_own_marker_key ~env ~visiting:[] t)
    then Known {undefined = false; markers = []}
    else Unknown

(* The markers [option<t>] can hold at the payload [t]; [var] gives the
   encoding of a type variable. *)
let payload_markers ~env ~var (t : Types.type_expr) :
    Gentype_config.option_markers =
  match encoding ~env ~var t with
  | Known {undefined; markers} -> (
    match some_markers ~undefined ~markers with
    | [] -> No_markers
    | depths -> Depths depths)
  | Unknown -> Any_depth

(* A type declaration's parameter: any value, a marker of any depth among
   them. *)
let declaration_var _ = Unknown

(* The bound of a function's type parameter that is an option's payload: the
   types none of whose values is [undefined] or a marker, so that
   [Primitive_option.some] and [valFromOption] return such a value unchanged
   and [option<a>] is exactly [undefined | a]. A primitive owns no key
   [BS_PRIVATE_NESTED_SOME_NONE], and its prototypes hold none. A record type
   does not exclude the key: TypeScript's object types admit further keys. *)
let payload_bound = "string | number | boolean | bigint"

(* The deepest nesting a function's overloads state for each bounded type
   parameter (below): [a], [option<a>], ..., [option^n<a>]. *)
let overload_depth = 1

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
let ts_of_signature ~env ~var type_env (type_exprs : Types.type_expr list) =
  let config =
    {(config ()) with option_payload_markers = payload_markers ~env ~var}
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
let rec trailing_optionals ~env ~var (params : Typedtree.function_param list) =
  match params with
  | [] -> ([], true)
  | p :: rest ->
    let flags, all_optional = trailing_optionals ~env ~var rest in
    let optional =
      all_optional && is_optional p
      &&
      match option_element p.fp_pat.pat_type with
      | Some elt -> payload_markers ~env ~var elt = No_markers
      | None -> false
    in
    (optional :: flags, optional)

(* A signature's types with type variables replaced, through type
   constructors, arrows and tuples; any other type naming a replaced variable
   has no such copy here. *)
exception Unsubstitutable

let mentions vars (t : Types.type_expr) =
  let visited = ref [] in
  let rec go (t : Types.type_expr) =
    let t = Ctype.repr t in
    if not (List.memq t !visited) then (
      visited := t :: !visited;
      if List.memq t vars then raise Exit;
      Btype.iter_type_expr go t)
  in
  match go t with
  | () -> false
  | exception Exit -> true

let substitute pairs (t : Types.type_expr) =
  let rec go visiting (t : Types.type_expr) =
    let t = Ctype.repr t in
    match List.assq_opt t pairs with
    | Some r -> r
    | None -> (
      if List.memq t visiting then raise Unsubstitutable;
      let visiting = t :: visiting in
      match t.desc with
      | Tconstr (p, args, _) ->
        Btype.newgenty
          (Tconstr (p, List.map (go visiting) args, ref Types.Mnil))
      | Tarrow (params, ret) ->
        Btype.newgenty
          (Tarrow
             ( List.map
                 (fun (a : Types.arg) -> {a with typ = go visiting a.typ})
                 params,
               go visiting ret ))
      | Ttuple ts -> Btype.newgenty (Ttuple (List.map (go visiting) ts))
      | _ ->
        if mentions (List.map fst pairs) t then raise Unsubstitutable else t)
  in
  go [] t

(* [option^depth<t>] *)
let rec nested depth (t : Types.type_expr) =
  if depth = 0 then t
  else
    Btype.newgenty
      (Tconstr (Predef.path_option, [nested (depth - 1) t], ref Types.Mnil))

(* Every assignment of a depth from [0] to [overload_depth] to each of [n]
   variables, the shallowest first. *)
let rec depth_assignments n =
  if n = 0 then [[]]
  else
    List.concat_map
      (fun rest -> List.init (overload_depth + 1) (fun d -> d :: rest))
      (depth_assignments (n - 1))

(* A function whose type parameter is an option's payload is printed with
   one overload per nesting of that parameter, [a], [option<a>], up to
   [overload_depth] options, each the function's signature at that instance
   with [a] bounded as above: each holds of the function's code, which is
   the code of every instance. Its own signature, which no caller sees, is
   the deepest instance's, whose parameters admit every overload's. An
   optional argument then prints [p: undefined | t], one form for every
   overload. *)
let collect_function st type_env (id : Ident.t) (e : Typedtree.expression) =
  match e.exp_desc with
  | Texp_function {params; body} ->
    Hashtbl.replace st.names id.name id.stamp;
    let env = e.exp_env in
    (* The type variables an option's encoding reaches: each is bounded. *)
    let reached = ref [] in
    let var t =
      let t = Ctype.repr t in
      if not (List.memq t !reached) then reached := !reached @ [t];
      Known {undefined = false; markers = []}
    in
    let signature optionals =
      List.map2
        (fun (p : Typedtree.function_param) optional ->
          match option_element p.fp_pat.pat_type with
          | Some elt when optional -> elt
          | _ -> p.fp_pat.pat_type)
        params optionals
      @ [body.exp_type]
    in
    let optionals, _ = trailing_optionals ~env ~var params in
    let _ = ts_of_signature ~env ~var type_env (signature optionals) in
    let payload = !reached in
    let optionals =
      if payload = [] then optionals else List.map (fun _ -> false) optionals
    in
    let base = signature optionals in
    (* Every type variable of the signature, each with its name. *)
    let vars =
      List.fold_left
        (fun acc t ->
          List.fold_left
            (fun acc v ->
              let v = Ctype.repr v in
              if List.memq v acc then acc else acc @ [v])
            acc (Ctype.free_variables t))
        [] base
    in
    let strings, type_vars, translated =
      ts_of_signature ~env ~var type_env (base @ vars)
    in
    let arity = List.length base in
    let types = List.filteri (fun i _ -> i < arity) strings in
    let translated_signature = List.filteri (fun i _ -> i < arity) translated in
    let names =
      List.filteri (fun i _ -> i >= arity) translated
      |> List.map (fun (t : Gentype_common.type_) ->
          match t with
          | TypeVar v -> v
          | _ -> assert false)
    in
    let named = List.combine vars names in
    let bounded =
      List.filter_map (fun v -> List.assq_opt v named) payload
      @ List.concat
          (List.map2
             (fun optional (t : Gentype_common.type_) ->
               match (optional, t) with
               | true, TypeVar v -> [v]
               | _ -> payload_vars t)
             (optionals @ [false]) translated_signature)
    in
    if type_vars <> [] then
      Hashtbl.replace st.generics (key_of id)
        (Ext_list.map type_vars (fun v ->
             if List.mem v bounded then v ^ " extends " ^ payload_bound else v));
    let at_depths depths =
      let pairs =
        List.map
          (fun (v, name) ->
            let fresh = Btype.newgenty (Tvar (Some name)) in
            match List.assq_opt v (List.combine payload depths) with
            | Some d -> (v, nested d fresh)
            | None -> (v, fresh))
          named
      in
      let strings, _, _ =
        ts_of_signature ~env ~var type_env (List.map (substitute pairs) base)
      in
      strings
    in
    let types =
      if payload = [] then types
      else
        match
          ( List.map at_depths (depth_assignments (List.length payload)),
            at_depths (List.map (fun _ -> overload_depth) payload) )
        with
        | overloads, deepest ->
          Hashtbl.replace st.overloads (key_of id)
            (Ext_list.map overloads (fun strings ->
                 let rec split params strings =
                   match (params, strings) with
                   | [], [result] -> ([], result)
                   | (p : Typedtree.function_param) :: params, ty :: strings ->
                     let typed, result = split params strings in
                     ((key_of p.fp_param, ty) :: typed, result)
                   | _ -> assert false
                 in
                 split params strings));
          deepest
        | exception Unsubstitutable -> types
    in
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
      option_payload_markers =
        payload_markers ~env:str.str_final_env ~var:declaration_var;
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
  Hashtbl.reset st.overloads;
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

(* A top-level function's overloads: each its parameters' types by their
   idents and its result type. *)
let overloads (id : Ident.t) =
  match Hashtbl.find_opt (current ()).overloads (key_of id) with
  | Some overloads -> overloads
  | None -> []

let overload_param_type typed (id : Ident.t) = List.assoc_opt (key_of id) typed

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
