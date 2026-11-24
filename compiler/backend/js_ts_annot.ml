(* Experimental TypeScript emission ([-bs-emit-ts]): per-request annotation
   table populated by [Js_ts_collect] in the driver and read by the JS
   printer in the backend.

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

let debug = Sys.getenv_opt "RESCRIPT_EMIT_TS_DEBUG" <> None

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
      List.map
        (fun p ->
          Printf.sprintf "%s/%d:%s" p.Ident.name p.stamp
            (if Hashtbl.mem st.params (key_of p) then "kept" else "lost"))
        params
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
  List.exists (fun e -> Ident.same e id) st.exports
  && Ext_ident.convert id.name = id.name
  &&
  (Hashtbl.replace st.inline_exports (key_of id) ();
   true)

let exported_inline (id : Ident.t) =
  enabled () && Hashtbl.mem (current ()).inline_exports (key_of id)
