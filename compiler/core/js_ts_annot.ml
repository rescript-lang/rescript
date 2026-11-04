(* Experimental TypeScript emission ([-bs-emit-ts]).

   The typed tree gives each top-level function's parameters and result a
   [Types.type_expr]; genType's translation turns each into TypeScript. The
   printer looks the parameters and the function up by their [Ident.t]
   (name and stamp), so an annotation reaches the output only when the
   identifier the J IR prints is the one the typed tree bound. *)

type key = string * int

type state = {
  params: (key, string) Hashtbl.t;  (** parameter ident -> its type *)
  results: (key, string) Hashtbl.t;  (** top-level function -> result type *)
  names: (string, int) Hashtbl.t;
      (** top-level function name -> its stamp, to report lost stamps *)
  mutable header: string;  (** the module's type declarations *)
}

let fresh () =
  {
    params = Hashtbl.create 16;
    results = Hashtbl.create 16;
    names = Hashtbl.create 16;
    header = "";
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

let ts_of_type_expr type_env (type_expr : Types.type_expr) =
  let config = config () in
  let translation =
    Translate_type_expr_from_types.translate_type_expr_from_types ~config
      ~type_env type_expr
  in
  Emit_type.type_to_string ~config
    ~type_name_is_interface:(fun _ -> false)
    translation.type_

let debug = Sys.getenv_opt "RESCRIPT_EMIT_TS_DEBUG" <> None

let collect_function st type_env (id : Ident.t) (e : Typedtree.expression) =
  match e.exp_desc with
  | Texp_function {params; body} ->
    Hashtbl.replace st.names id.name id.stamp;
    Ext_list.iter params (fun (p : Typedtree.function_param) ->
        Hashtbl.replace st.params (key_of p.fp_param)
          (ts_of_type_expr type_env p.fp_pat.pat_type));
    Hashtbl.replace st.results (key_of id)
      (ts_of_type_expr type_env body.exp_type)
  | _ -> ()

let is_type_declaration (item : Typedtree.structure_item) =
  match item.str_desc with
  | Tstr_type _ -> true
  | _ -> false

let header_of_structure ~module_name (str : Typedtree.structure) =
  let config = config () in
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
