(* A [@throws([E, M.F])] attribute names exceptions as written, so only the
   type checker knows which exceptions they are: the names depend on the opens,
   aliases and includes in scope at the declaration. [resolve] looks each name
   up there and records the exception in a generated [@res.throws] attribute,
   which reanalyze's exception analysis compares with the exceptions a call
   site catches. The attribute goes into the .cmi with the declaration, so it
   reaches callers in other modules and packages. *)

let generated = "res.throws"

(* An exception's identity: the source file that defines it, without its
   extension, and its name, as [src/Lib#E]. A module's .res and .resi share it,
   and it does not change when the definition moves within the file. Two
   exceptions of the same name in one file share it. Rebinding with
   [exception E = Lib.F] defines a new identity. A .cmi built with
   [-no-keep-locs], as the runtime's and Belt's are, has no locations, so
   its exceptions are identified by their name alone, as [#Undefined]. *)
let identity (cstr : Types.constructor_description) =
  match cstr.cstr_kind with
  | Extension_constructor (Pident id) when Ident.is_predef_exn id ->
    Some (Ident.name id)
  | Extension_constructor _ ->
    let file = cstr.cstr_loc.loc_start.pos_fname in
    let file =
      if file = Location.none.loc_start.pos_fname then ""
      else Filename.remove_extension file
    in
    Some (file ^ "#" ^ cstr.cstr_name)
  | Ordinary_constructor _ -> None

(* [Lib-Ns] is the module [Lib] of namespace [Ns]. *)
let without_namespace name =
  match String.index_opt name '-' with
  | Some i -> String.sub name 0 i
  | None -> name

(* A readable name for messages: [Lib.E] for another module's exception,
   aliases resolved, and the current module's name in front of its own. *)
let display_name env (cstr : Types.constructor_description) =
  match cstr.cstr_kind with
  | Extension_constructor (Pident id) when Ident.is_predef_exn id ->
    Ident.name id
  | Extension_constructor (Pdot (prefix, name, _)) -> (
    let prefix = Env.normalize_path None env prefix in
    match Path.flatten prefix with
    | `Ok (head, rest) ->
      let names = (without_namespace (Ident.name head) :: rest) @ [name] in
      let names =
        if Ident.persistent head then names
        else without_namespace (Env.get_unit_name ()) :: names
      in
      String.concat "." names
    | `Contains_apply -> Path.name prefix ^ "." ^ name)
  | Extension_constructor (Pident id) ->
    without_namespace (Env.get_unit_name ()) ^ "." ^ Ident.name id
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
            Location.prerr_warning loc (Warnings.Bs_throws_unbound written);
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
