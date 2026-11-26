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
  values: (key, string) Hashtbl.t;
      (** top-level non-function binding -> its type *)
  locals: (key, string) Hashtbl.t;
      (** local binding -> its type, emitted when initialized to [undefined]
          or declared without an initializer *)
  sroa_fields: (key, string array) Hashtbl.t;
      (** local record/tuple binding -> its field types by index, propagated
          to the scalar variables when [Lam_pass_sroa] replaces the block *)
  generics: (key, string list) Hashtbl.t;
      (** top-level function -> its type parameters, in order of occurrence,
          each with its bound if it has one *)
  overloads: (key, ((key * string) list * string) list) Hashtbl.t;
      (** top-level function -> its overloads, each its parameters' types by
          their idents and its result type *)
  param_origin: (key, key) Hashtbl.t;
      (** renamed parameter ident -> original typedtree parameter ident *)
  names: (string, int) Hashtbl.t;
      (** top-level function name -> its stamp, to report lost stamps *)
  mutable header: string;  (** the module's type declarations *)
  mutable exports: Ident.t list;  (** the program's exports *)
  inline_exports: (key, unit) Hashtbl.t;
      (** exports printed as [export function] or [export const] *)
}

let fresh () =
  {
    params = Hashtbl.create 16;
    results = Hashtbl.create 16;
    values = Hashtbl.create 16;
    locals = Hashtbl.create 16;
    sroa_fields = Hashtbl.create 16;
    generics = Hashtbl.create 16;
    overloads = Hashtbl.create 16;
    param_origin = Hashtbl.create 16;
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

let overload_param_type typed (id : Ident.t) =
  let k = key_of id in
  match List.assoc_opt k typed with
  | Some _ as res -> res
  | None -> (
    match Hashtbl.find_opt (current ()).param_origin k with
    | Some orig -> List.assoc_opt orig typed
    | None -> None)

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

let value_type (id : Ident.t) = Hashtbl.find_opt (current ()).values (key_of id)

let local_type (id : Ident.t) = Hashtbl.find_opt (current ()).locals (key_of id)

let type_params (id : Ident.t) =
  match Hashtbl.find_opt (current ()).generics (key_of id) with
  | Some vars -> "<" ^ String.concat ", " vars ^ ">"
  | None -> ""

let set_exports exports =
  let st = current () in
  st.exports <- exports;
  Hashtbl.reset st.inline_exports

let rename ~(from : Ident.t) ~(to_ : Ident.t) =
  if enabled () then (
    let st = current () in
    let src = key_of from in
    let dst = key_of to_ in
    let copy tbl =
      match Hashtbl.find_opt tbl src with
      | Some v -> Hashtbl.replace tbl dst v
      | None -> ()
    in
    copy st.params;
    copy st.results;
    copy st.values;
    copy st.locals;
    copy st.sroa_fields;
    copy st.generics;
    copy st.overloads;
    let orig =
      match Hashtbl.find_opt st.param_origin src with
      | Some orig -> orig
      | None -> src
    in
    if Hashtbl.mem st.params src then Hashtbl.replace st.param_origin dst orig)

let sroa_replace ~(block : Ident.t) ~(fields : Ident.t array) =
  if enabled () then
    let st = current () in
    match Hashtbl.find_opt st.sroa_fields (key_of block) with
    | Some field_tys when Array.length field_tys = Array.length fields ->
      Array.iteri
        (fun i id -> Hashtbl.replace st.locals (key_of id) field_tys.(i))
        fields
    | _ -> ()

(* A top-level binding is printed [export function f] or [export const x]
   when it is exported under its own printed name. *)
let export_inline (id : Ident.t) ~(printed : string) =
  enabled ()
  &&
  let st = current () in
  List.exists (fun e -> Ident.same e id) st.exports
  && id.name <> "default"
  && String.equal printed (Ext_ident.convert id.name)
  &&
  (Hashtbl.replace st.inline_exports (key_of id) ();
   true)

let exported_inline (id : Ident.t) =
  enabled () && Hashtbl.mem (current ()).inline_exports (key_of id)
