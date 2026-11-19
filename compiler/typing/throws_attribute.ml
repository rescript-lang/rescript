(* A [@throws([E, M.F])] attribute names exceptions as written, so only the
   type checker knows which exceptions they are: the names depend on the opens,
   aliases and includes in scope at the declaration. [resolve] looks each name
   up there and records the exception in a generated [@res.throws] attribute,
   which reanalyze's exception analysis compares with the exceptions a call
   site catches. The attribute goes into the .cmi with the declaration, so it
   reaches callers in other modules and packages. *)

let generated = "res.throws"

(* An exception's identity: the path of the source file that defines it, as
   the compiler was given it, without the extension, and its name, as
   [src/Lib#E]. A module's .res and .resi share it, and it does not change when
   the definition moves within the file. Exceptions of the same name in one
   file share it, as do the exceptions of two applications of one functor.
   Rebinding with [exception E = Lib.F] defines a new identity. A file
   compiled with [-no-keep-locs], as the runtime and Belt are, keeps no
   locations in its .cmi, so its exceptions are identified by their name
   alone, as [#Undefined], also while that file itself is compiled. *)
let identity (cstr : Types.constructor_description) =
  match cstr.cstr_kind with
  | Extension_constructor (Pident id) when Ident.is_predef_exn id ->
    Some (Ident.name id)
  | Extension_constructor _ ->
    let file = cstr.cstr_loc.loc_start.pos_fname in
    let file =
      if
        file = Location.none.loc_start.pos_fname
        || not !((Clflags.current ()).keep_locs)
      then ""
      else Filename.remove_extension file
    in
    Some (file ^ "#" ^ cstr.cstr_name)
  | Ordinary_constructor _ -> None

(* [Lib-Ns] is the module [Lib] of namespace [Ns]. *)
let without_namespace name =
  match String.index_opt name '-' with
  | Some i -> String.sub name 0 i
  | None -> name

(* Where the current file defines its modules and exceptions, as module paths
   within the file ([M; N; E] for [E] in [M.N]), and the module being typed.
   [Typemod] records them, so messages can name a file's own exceptions
   exactly. *)
type definitions = {
  mutable current: string list;  (** The module being typed, outermost first *)
  defined: (Ident.t, string list) Hashtbl.t;
  warned: (Location.t, unit) Hashtbl.t;
      (** Names already warned about: a [let] that destructures a tuple
          carries its attributes on every binding *)
}

let definitions =
  Request_local.new_key (fun () ->
      {current = []; defined = Hashtbl.create 16; warned = Hashtbl.create 1})

(* Type the module [name] with [f], recording what it defines under [name]. *)
let in_module name f =
  let d = Request_local.get definitions in
  let outer = d.current in
  d.current <- outer @ [name];
  Fun.protect ~finally:(fun () -> d.current <- outer) f

(* Record that the module being typed defines [id]. *)
let define id =
  let d = Request_local.get definitions in
  Hashtbl.replace d.defined id (d.current @ [Ident.name id])

(* A readable name for messages: [Lib.E] for another module's exception,
   aliases resolved, and [Lib.M.E] for one that the current file [Lib]
   defines in its module [M]. *)
let display_name env (cstr : Types.constructor_description) =
  let unit = without_namespace (Env.get_unit_name ()) in
  let local id =
    match Hashtbl.find_opt (Request_local.get definitions).defined id with
    | Some path -> unit :: path
    | None -> [unit; Ident.name id]
  in
  match cstr.cstr_kind with
  | Extension_constructor (Pident id) when Ident.is_predef_exn id ->
    Ident.name id
  | Extension_constructor (Pident id) -> String.concat "." (local id)
  | Extension_constructor (Pdot (prefix, name, _)) -> (
    let prefix = Env.normalize_path None env prefix in
    match Path.flatten prefix with
    | `Ok (head, rest) ->
      let head =
        if Ident.persistent head then [without_namespace (Ident.name head)]
        else local head
      in
      String.concat "." (head @ rest @ [name])
    | `Contains_apply -> Path.name prefix ^ "." ^ name)
  | Extension_constructor (Papply _ as path) -> Path.name path
  | Ordinary_constructor _ -> cstr.cstr_name

(* The names a [@throws] payload lists: one name, as [E] or [M.E(_)], or
   several in an array, a tuple or a [list{...}]. A payload-less [@throws]
   lists none. *)
let names (payload : Parsetree.payload) =
  let rec of_expr (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_construct ({txt = Lident "[]"}, {txt = []}) -> []
    | Pexp_construct ({txt = Lident "::"}, {txt = [head; tail]}) ->
      of_expr head @ of_expr tail
    | Pexp_construct ({txt; loc}, _) | Pexp_ident {txt; loc} -> [(txt, loc)]
    | Pexp_constant (Pconst_string s) ->
      [(Longident.parse (String_literal.string_semantic s), e.pexp_loc)]
    | Pexp_tuple es | Pexp_array es -> List.concat_map of_expr es
    | _ -> []
  in
  match payload with
  | PStr [{pstr_desc = Pstr_eval (e, _)}] -> of_expr e
  | _ -> []

(* Add [@res.throws] after a [@throws], listing each named exception as its
   readable name and identity. A name that is not an exception in [env] gets a
   warning and is listed as written, as its own identity. *)
let resolve env (attributes : Parsetree.attributes) =
  match
    List.find_opt
      (fun (({txt}, _) : Parsetree.attribute) -> txt = "throws")
      attributes
  with
  | None -> attributes
  | Some (_, payload) -> (
    let resolved =
      names payload
      |> List.map (fun (lid, loc) ->
          let found =
            match Env.lookup_constructor ~loc lid env with
            | cstr -> (
              match identity cstr with
              | Some id -> Some (display_name env cstr, id)
              | None -> None)
            | exception (Not_found | Env.Error _) -> None
          in
          match found with
          | Some entry -> entry
          | None ->
            let written = String.concat "." (Longident.flatten lid) in
            let warned = (Request_local.get definitions).warned in
            if not (Hashtbl.mem warned loc) then (
              Hashtbl.replace warned loc ();
              Loc_report.prerr_warning loc (Warnings.Bs_throws_unbound written));
            (written, written))
    in
    match resolved with
    | [] -> attributes
    | _ ->
      let loc = Location.none in
      let string s = Ast_helper.Exp.constant (Ast_helper.Const.string s) in
      let entries =
        resolved
        |> List.map (fun (name, id) ->
            Ast_helper.Exp.tuple [string name; string id])
      in
      attributes
      @ [
          ( {txt = generated; loc},
            PStr [Ast_helper.Str.eval (Ast_helper.Exp.array entries)] );
        ])

(* Read [@res.throws] back as (name, identity) pairs. *)
let read (attributes : Parsetree.attributes) =
  match
    List.find_opt
      (fun (({txt}, _) : Parsetree.attribute) -> txt = generated)
      attributes
  with
  | Some (_, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_array es}, _)}]) ->
    Some
      (es
      |> List.filter_map (fun (e : Parsetree.expression) ->
          match e.pexp_desc with
          | Pexp_tuple
              [
                {pexp_desc = Pexp_constant (Pconst_string name)};
                {pexp_desc = Pexp_constant (Pconst_string id)};
              ] ->
            Some
              ( String_literal.string_semantic name,
                String_literal.string_semantic id )
          | _ -> None))
  | _ -> None
