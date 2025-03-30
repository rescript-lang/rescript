(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Environment handling *)

open Cmi_format
open Config
open Misc
open Asttypes
open Longident
open Path
open Types
open Btype

let value_declarations : (string * Location.t, unit -> unit) Hashtbl.t =
  Hashtbl.create 16
(* This table is used to usage of value declarations.  A declaration is
   identified with its name and location.  The callback attached to a
   declaration is called whenever the value is used explicitly
   (lookup_value) or implicitly (inclusion test between signatures,
   cf Includemod.value_descriptions). *)

let type_declarations = Hashtbl.create 16
let module_declarations = Hashtbl.create 16

type constructor_usage = Positive | Pattern | Privatize
type constructor_usages = {
  mutable cu_positive: bool;
  mutable cu_pattern: bool;
  mutable cu_privatize: bool;
}
let add_constructor_usage cu = function
  | Positive -> cu.cu_positive <- true
  | Pattern -> cu.cu_pattern <- true
  | Privatize -> cu.cu_privatize <- true
let constructor_usages () =
  {cu_positive = false; cu_pattern = false; cu_privatize = false}

let used_constructors :
    (string * Location.t * string, constructor_usage -> unit) Hashtbl.t =
  Hashtbl.create 16

let prefixed_sg = Hashtbl.create 113

type error =
  | Illegal_renaming of string * string * string
  | Inconsistent_import of string * string * string
  | Missing_module of Location.t * Path.t * Path.t
  | Illegal_value_name of Location.t * string

exception Error of error

let error err = raise (Error err)

module EnvLazy : sig
  type ('a, 'b) t

  type log

  val force : ('a -> 'b) -> ('a, 'b) t -> 'b
  val create : 'a -> ('a, 'b) t
  val get_arg : ('a, 'b) t -> 'a option

  (* [force_logged log f t] is equivalent to [force f t] but if [f] returns [None] then
     [t] is recorded in [log]. [backtrack log] will then reset all the recorded [t]s back
     to their original state. *)
  val log : unit -> log
  val force_logged : log -> ('a -> 'b option) -> ('a, 'b option) t -> 'b option
  val backtrack : log -> unit
end = struct
  type ('a, 'b) t = ('a, 'b) eval ref

  and ('a, 'b) eval = Done of 'b | Raise of exn | Thunk of 'a

  type undo = Nil | Cons : ('a, 'b) t * 'a * undo -> undo

  type log = undo ref

  let force f x =
    match !x with
    | Done x -> x
    | Raise e -> raise e
    | Thunk e -> (
      match f e with
      | y ->
        x := Done y;
        y
      | exception e ->
        x := Raise e;
        raise e)

  let get_arg x =
    match !x with
    | Thunk a -> Some a
    | _ -> None

  let create x = ref (Thunk x)

  let log () = ref Nil

  let force_logged log f x =
    match !x with
    | Done x -> x
    | Raise e -> raise e
    | Thunk e -> (
      match f e with
      | None ->
        x := Done None;
        log := Cons (x, e, !log);
        None
      | Some _ as y ->
        x := Done y;
        y
      | exception e ->
        x := Raise e;
        raise e)

  let backtrack log =
    let rec loop = function
      | Nil -> ()
      | Cons (x, e, rest) ->
        x := Thunk e;
        loop rest
    in
    loop !log
end

module PathMap = Map.Make (Path)

type summary =
  | Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_extension of summary * Ident.t * extension_constructor
  | Env_module of summary * Ident.t * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_open of summary * Path.t
  | Env_functor_arg of summary * Ident.t
  | Env_constraints of summary * type_declaration PathMap.t
  | Env_copy_types of summary * string list

module TycompTbl = struct
  (** This module is used to store components of types (i.e. labels
        and constructors).  We keep a representation of each nested
        "open" and the set of local bindings between each of them. *)

  type 'a t = {
    current: 'a Ident.tbl;  (** Local bindings since the last open. *)
    opened: 'a opened option;
        (** Symbolic representation of the last (innermost) open, if any. *)
  }

  and 'a opened = {
    components: (string, 'a list) Tbl.t;
        (** Components from the opened module. We keep a list of
          bindings for each name, as in comp_labels and
          comp_constrs. *)
    using: (string -> ('a * 'a) option -> unit) option;
        (** A callback to be applied when a component is used from this
          "open".  This is used to detect unused "opens".  The
          arguments are used to detect shadowing. *)
    next: 'a t;  (** The table before opening the module. *)
  }

  let empty = {current = Ident.empty; opened = None}

  let add id x tbl = {tbl with current = Ident.add id x tbl.current}

  let add_open slot wrap components next =
    let using =
      match slot with
      | None -> None
      | Some f -> Some (fun s x -> f s (wrap x))
    in
    {current = Ident.empty; opened = Some {using; components; next}}

  let rec find_same id tbl =
    try Ident.find_same id tbl.current
    with Not_found as exn -> (
      match tbl.opened with
      | Some {next; _} -> find_same id next
      | None -> raise exn)

  let nothing () = ()

  let mk_callback rest name desc = function
    | None -> nothing
    | Some f -> (
      fun () ->
        match rest with
        | [] -> f name None
        | (hidden, _) :: _ -> f name (Some (desc, hidden)))

  let rec find_all name tbl =
    List.map
      (fun (_id, desc) -> (desc, nothing))
      (Ident.find_all name tbl.current)
    @
    match tbl.opened with
    | None -> []
    | Some {using; next; components} -> (
      let rest = find_all name next in
      match Tbl.find_str name components with
      | exception Not_found -> rest
      | opened ->
        List.map (fun desc -> (desc, mk_callback rest name desc using)) opened
        @ rest)

  let rec fold_name f tbl acc =
    let acc = Ident.fold_name (fun _id d -> f d) tbl.current acc in
    match tbl.opened with
    | Some {using = _; next; components} ->
      acc
      |> Tbl.fold (fun _name -> List.fold_right (fun desc -> f desc)) components
      |> fold_name f next
    | None -> acc

  let rec local_keys tbl acc =
    let acc = Ident.fold_all (fun k _ accu -> k :: accu) tbl.current acc in
    match tbl.opened with
    | Some o -> local_keys o.next acc
    | None -> acc

  let diff_keys is_local tbl1 tbl2 =
    let keys2 = local_keys tbl2 [] in
    Ext_list.filter keys2 (fun id ->
        is_local (find_same id tbl2)
        &&
        try
          ignore (find_same id tbl1);
          false
        with Not_found -> true)
end

module IdTbl = struct
  (** This module is used to store all kinds of components except
        (labels and constructors) in environments.  We keep a
        representation of each nested "open" and the set of local
        bindings between each of them. *)

  type 'a t = {
    current: 'a Ident.tbl;  (** Local bindings since the last open *)
    opened: 'a opened option;
        (** Symbolic representation of the last (innermost) open, if any. *)
  }

  and 'a opened = {
    root: Path.t;
        (** The path of the opened module, to be prefixed in front of
          its local names to produce a valid path in the current
          environment. *)
    components: (string, 'a * int) Tbl.t;
        (** Components from the opened module. *)
    using: (string -> ('a * 'a) option -> unit) option;
        (** A callback to be applied when a component is used from this
          "open".  This is used to detect unused "opens".  The
          arguments are used to detect shadowing. *)
    next: 'a t;  (** The table before opening the module. *)
  }

  let empty = {current = Ident.empty; opened = None}

  let add id x tbl = {tbl with current = Ident.add id x tbl.current}

  let add_open slot wrap root components next =
    let using =
      match slot with
      | None -> None
      | Some f -> Some (fun s x -> f s (wrap x))
    in
    {current = Ident.empty; opened = Some {using; root; components; next}}

  let rec find_same id tbl =
    try Ident.find_same id tbl.current
    with Not_found as exn -> (
      match tbl.opened with
      | Some {next; _} -> find_same id next
      | None -> raise exn)

  let rec find_name mark name tbl =
    try
      let id, desc = Ident.find_name name tbl.current in
      (Pident id, desc)
    with Not_found as exn -> (
      match tbl.opened with
      | Some {using; root; next; components} -> (
        try
          let descr, pos = Tbl.find_str name components in
          let res = (Pdot (root, name, pos), descr) in
          (if mark then
             match using with
             | None -> ()
             | Some f -> (
               try f name (Some (snd (find_name false name next), snd res))
               with Not_found -> f name None));
          res
        with Not_found -> find_name mark name next)
      | None -> raise exn)

  let find_name name tbl = find_name true name tbl

  let rec update name f tbl =
    try
      let id, desc = Ident.find_name name tbl.current in
      let new_desc = f desc in
      {tbl with current = Ident.add id new_desc tbl.current}
    with Not_found -> (
      match tbl.opened with
      | Some {root; using; next; components} -> (
        try
          let desc, pos = Tbl.find_str name components in
          let new_desc = f desc in
          let components = Tbl.add name (new_desc, pos) components in
          {tbl with opened = Some {root; using; next; components}}
        with Not_found ->
          let next = update name f next in
          {tbl with opened = Some {root; using; next; components}})
      | None -> tbl)

  let rec find_all name tbl =
    List.map
      (fun (id, desc) -> (Pident id, desc))
      (Ident.find_all name tbl.current)
    @
    match tbl.opened with
    | None -> []
    | Some {root; using = _; next; components} -> (
      try
        let desc, pos = Tbl.find_str name components in
        (Pdot (root, name, pos), desc) :: find_all name next
      with Not_found -> find_all name next)

  let rec fold_name f tbl acc =
    let acc =
      Ident.fold_name
        (fun id d -> f (Ident.name id) (Pident id, d))
        tbl.current acc
    in
    match tbl.opened with
    | Some {root; using = _; next; components} ->
      acc
      |> Tbl.fold
           (fun name (desc, pos) -> f name (Pdot (root, name, pos), desc))
           components
      |> fold_name f next
    | None -> acc

  let rec local_keys tbl acc =
    let acc = Ident.fold_all (fun k _ accu -> k :: accu) tbl.current acc in
    match tbl.opened with
    | Some o -> local_keys o.next acc
    | None -> acc

  let rec iter f tbl =
    Ident.iter (fun id desc -> f id (Pident id, desc)) tbl.current;
    match tbl.opened with
    | Some {root; using = _; next; components} ->
      Tbl.iter
        (fun s (x, pos) ->
          f (Ident.hide (Ident.create s) (* ??? *)) (Pdot (root, s, pos), x))
        components;
      iter f next
    | None -> ()

  let diff_keys tbl1 tbl2 =
    let keys2 = local_keys tbl2 [] in
    Ext_list.filter keys2 (fun id ->
        try
          ignore (find_same id tbl1);
          false
        with Not_found -> true)
end

type type_descriptions = constructor_description list * label_description list

let in_signature_flag = 0x01
let implicit_coercion_flag = 0x02

type t = {
  values: value_description IdTbl.t;
  constrs: constructor_description TycompTbl.t;
  labels: label_description TycompTbl.t;
  types: (type_declaration * type_descriptions) IdTbl.t;
  modules: (Subst.t * module_declaration, module_declaration) EnvLazy.t IdTbl.t;
  modtypes: modtype_declaration IdTbl.t;
  components: module_components IdTbl.t;
  functor_args: unit Ident.tbl;
  summary: summary;
  local_constraints: type_declaration PathMap.t;
  gadt_instances: (int * TypeSet.t ref) list;
  flags: int;
}

and module_components = {
  deprecated: string option;
  loc: Location.t;
  comps:
    ( t * Subst.t * Path.t * Types.module_type,
      module_components_repr option )
    EnvLazy.t;
}

and module_components_repr =
  | Structure_comps of structure_components
  | Functor_comps of functor_components

and 'a comp_tbl = (string, 'a * int) Tbl.t

and structure_components = {
  mutable comp_values: value_description comp_tbl;
  mutable comp_constrs: (string, constructor_description list) Tbl.t;
  mutable comp_labels: (string, label_description list) Tbl.t;
  mutable comp_types: (type_declaration * type_descriptions) comp_tbl;
  mutable comp_modules:
    (Subst.t * module_declaration, module_declaration) EnvLazy.t comp_tbl;
  mutable comp_modtypes: modtype_declaration comp_tbl;
  mutable comp_components: module_components comp_tbl; (* warning -69*)
}

and functor_components = {
  fcomp_param: Ident.t; (* Formal parameter *)
  fcomp_arg: module_type option; (* Argument signature *)
  fcomp_res: module_type; (* Result signature *)
  fcomp_cache: (Path.t, module_components) Hashtbl.t; (* For memoization *)
  fcomp_subst_cache: (Path.t, module_type) Hashtbl.t;
}

let copy_local ~from env =
  {
    env with
    local_constraints = from.local_constraints;
    gadt_instances = from.gadt_instances;
    flags = from.flags;
  }

let same_constr = ref (fun _ _ _ -> assert false)

(* Helper to decide whether to report an identifier shadowing
   by some 'open'. For labels and constructors, we do not report
   if the two elements are from the same re-exported declaration.

   Later, one could also interpret some attributes on value and
   type declarations to silence the shadowing warnings. *)

let check_shadowing env = function
  | `Constructor (Some (c1, c2))
    when not (!same_constr env c1.cstr_res c2.cstr_res) ->
    Some "constructor"
  | `Label (Some (l1, l2)) when not (!same_constr env l1.lbl_res l2.lbl_res) ->
    Some "label"
  | `Value (Some _) -> Some "value"
  | `Type (Some _) -> Some "type"
  | `Module (Some _) | `Component (Some _) -> Some "module"
  | `Module_type (Some _) -> Some "module type"
  | `Constructor _ | `Label _
  | `Value None
  | `Type None
  | `Module None
  | `Module_type None
  | `Component None ->
    None

let subst_modtype_maker (subst, md) =
  if subst == Subst.identity then md
  else {md with md_type = Subst.modtype subst md.md_type}

let empty =
  {
    values = IdTbl.empty;
    constrs = TycompTbl.empty;
    labels = TycompTbl.empty;
    types = IdTbl.empty;
    modules = IdTbl.empty;
    modtypes = IdTbl.empty;
    components = IdTbl.empty;
    summary = Env_empty;
    local_constraints = PathMap.empty;
    gadt_instances = [];
    flags = 0;
    functor_args = Ident.empty;
  }

let in_signature b env =
  let flags =
    if b then env.flags lor in_signature_flag
    else env.flags land lnot in_signature_flag
  in
  {env with flags}

let implicit_coercion env =
  {env with flags = env.flags lor implicit_coercion_flag}

let is_in_signature env = env.flags land in_signature_flag <> 0
let is_implicit_coercion env = env.flags land implicit_coercion_flag <> 0

let is_ident = function
  | Pident _ -> true
  | Pdot _ | Papply _ -> false

let is_local_ext = function
  | {cstr_tag = Cstr_extension p} -> is_ident p
  | _ -> false

let diff env1 env2 =
  IdTbl.diff_keys env1.values env2.values
  @ TycompTbl.diff_keys is_local_ext env1.constrs env2.constrs
  @ IdTbl.diff_keys env1.modules env2.modules

type can_load_cmis = Can_load_cmis | Cannot_load_cmis of EnvLazy.log

let can_load_cmis = ref Can_load_cmis

let without_cmis f x =
  let log = EnvLazy.log () in
  let res =
    Misc.(
      protect_refs [R (can_load_cmis, Cannot_load_cmis log)] (fun () -> f x))
  in
  EnvLazy.backtrack log;
  res

(* Forward declarations *)

let components_of_module' =
  ref
    (fun ~deprecated:_ ~loc:_ _env _sub _path _mty -> assert false
      : deprecated:string option ->
        loc:Location.t ->
        t ->
        Subst.t ->
        Path.t ->
        module_type ->
        module_components)
let components_of_module_maker' =
  ref
    (fun (_env, _sub, _path, _mty) -> assert false
      : t * Subst.t * Path.t * module_type -> module_components_repr option)
let components_of_functor_appl' =
  ref
    (fun _f _env _p1 _p2 -> assert false
      : functor_components -> t -> Path.t -> Path.t -> module_components)
let check_modtype_inclusion =
  (* to be filled with Includemod.check_modtype_inclusion *)
  ref
    (fun ~loc:_ _env _mty1 _path1 _mty2 -> assert false
      : loc:Location.t -> t -> module_type -> Path.t -> module_type -> unit)
let strengthen =
  (* to be filled with Mtype.strengthen *)
  ref
    (fun ~aliasable:_ _env _mty _path -> assert false
      : aliasable:bool -> t -> module_type -> Path.t -> module_type)

let md md_type = {md_type; md_attributes = []; md_loc = Location.none}

let get_components_opt c =
  match !can_load_cmis with
  | Can_load_cmis -> EnvLazy.force !components_of_module_maker' c.comps
  | Cannot_load_cmis log ->
    EnvLazy.force_logged log !components_of_module_maker' c.comps

let empty_structure =
  Structure_comps
    {
      comp_values = Tbl.empty;
      comp_constrs = Tbl.empty;
      comp_labels = Tbl.empty;
      comp_types = Tbl.empty;
      comp_modules = Tbl.empty;
      comp_modtypes = Tbl.empty;
      comp_components = Tbl.empty;
    }

let get_components c =
  match get_components_opt c with
  | None -> empty_structure
  | Some c -> c

(* The name of the compilation unit currently compiled.
   "" if outside a compilation unit. *)

let current_unit = ref ""

(* Persistent structure descriptions *)

type pers_struct = {
  ps_name: string;
  ps_sig: signature Lazy.t;
  ps_comps: module_components;
  ps_crcs: (string * Digest.t option) list;
  ps_filename: string;
  ps_flags: pers_flags list;
}
[@@warning "-69"]

let persistent_structures =
  (Hashtbl.create 17 : (string, pers_struct option) Hashtbl.t)

(* Consistency between persistent structures *)

let crc_units = Consistbl.create ()

module StringSet = Set.Make (struct
  type t = string
  let compare = String.compare
end)

let imported_units = ref StringSet.empty

let add_import s = imported_units := StringSet.add s !imported_units

let clear_imports () =
  Consistbl.clear crc_units;
  imported_units := StringSet.empty

let check_consistency ps =
  try
    List.iter
      (fun (name, crco) ->
        match crco with
        | None -> ()
        | Some crc ->
          add_import name;
          Consistbl.check crc_units name crc ps.ps_filename)
      ps.ps_crcs
  with Consistbl.Inconsistency (name, source, auth) ->
    error (Inconsistent_import (name, auth, source))

(* Reading persistent structures from .cmi files *)

let save_pers_struct crc ps =
  let modname = ps.ps_name in
  Hashtbl.add persistent_structures modname (Some ps);
  Consistbl.set crc_units modname crc ps.ps_filename;
  add_import modname

module Persistent_signature = struct
  type t = {filename: string; cmi: Cmi_format.cmi_infos}

  let load =
    ref (fun ~unit_name ->
        match find_in_path_uncap !load_path (unit_name ^ ".cmi") with
        | filename -> Some {filename; cmi = read_cmi filename}
        | exception Not_found -> None)
end

let acknowledge_pers_struct check modname {Persistent_signature.filename; cmi} =
  let name = cmi.cmi_name in
  let sign = cmi.cmi_sign in
  let crcs = cmi.cmi_crcs in
  let flags = cmi.cmi_flags in
  let deprecated =
    List.fold_left
      (fun _ -> function
        | Deprecated s -> Some s)
      None flags
  in
  let comps =
    !components_of_module' ~deprecated ~loc:Location.none empty Subst.identity
      (Pident (Ident.create_persistent name))
      (Mty_signature sign)
  in
  let ps =
    {
      ps_name = name;
      ps_sig = lazy (Subst.signature Subst.identity sign);
      ps_comps = comps;
      ps_crcs = crcs;
      ps_filename = filename;
      ps_flags = flags;
    }
  in
  if ps.ps_name <> modname then
    error (Illegal_renaming (modname, ps.ps_name, filename));
  if check then check_consistency ps;
  Hashtbl.add persistent_structures modname (Some ps);
  ps

let read_pers_struct check modname filename =
  add_import modname;
  let cmi = read_cmi filename in
  acknowledge_pers_struct check modname {Persistent_signature.filename; cmi}

let find_pers_struct check name =
  if name = "*predef*" then raise Not_found;
  match Hashtbl.find persistent_structures name with
  | Some ps -> ps
  | None -> raise Not_found
  | exception Not_found -> (
    match !can_load_cmis with
    | Cannot_load_cmis _ -> raise Not_found
    | Can_load_cmis ->
      let ps =
        match !Persistent_signature.load ~unit_name:name with
        | Some ps -> ps
        | None ->
          Hashtbl.add persistent_structures name None;
          raise Not_found
      in
      add_import name;
      acknowledge_pers_struct check name ps)

(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct name =
  try ignore (find_pers_struct false name) with
  | Not_found ->
    let warn = Warnings.No_cmi_file (name, None) in
    Location.prerr_warning Location.none warn
  | Cmi_format.Error err ->
    let msg = Format.asprintf "%a" Cmi_format.report_error err in
    let warn = Warnings.No_cmi_file (name, Some msg) in
    Location.prerr_warning Location.none warn
  | Error err ->
    let msg =
      match err with
      | Illegal_renaming (name, ps_name, filename) ->
        Format.asprintf
          " %a@ contains the compiled interface for @ %s when %s was expected"
          Location.print_filename filename ps_name name
      | Inconsistent_import _ -> assert false
      | Missing_module _ -> assert false
      | Illegal_value_name _ -> assert false
    in
    let warn = Warnings.No_cmi_file (name, Some msg) in
    Location.prerr_warning Location.none warn

let read_pers_struct modname filename = read_pers_struct true modname filename

let find_pers_struct name = find_pers_struct true name

let check_pers_struct name =
  if not (Hashtbl.mem persistent_structures name) then (
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. *)
    add_import name;
    if Warnings.is_active (Warnings.No_cmi_file ("", None)) then
      Delayed_checks.add_delayed_check (fun () -> check_pers_struct name))

let reset_cache () =
  current_unit := "";
  Hashtbl.clear persistent_structures;
  clear_imports ();
  Hashtbl.clear value_declarations;
  Hashtbl.clear type_declarations;
  Hashtbl.clear module_declarations;
  Hashtbl.clear used_constructors;
  Hashtbl.clear prefixed_sg

let reset_cache_toplevel () =
  (* Delete 'missing cmi' entries from the cache. *)
  let l =
    Hashtbl.fold
      (fun name r acc -> if r = None then name :: acc else acc)
      persistent_structures []
  in
  List.iter (Hashtbl.remove persistent_structures) l;
  Hashtbl.clear value_declarations;
  Hashtbl.clear type_declarations;
  Hashtbl.clear module_declarations;
  Hashtbl.clear used_constructors;
  Hashtbl.clear prefixed_sg

let set_unit_name name = current_unit := name

let get_unit_name () = !current_unit

(* Lookup by identifier *)

let rec find_module_descr path env =
  match path with
  | Pident id -> (
    try IdTbl.find_same id env.components
    with Not_found ->
      if Ident.persistent id && not (Ident.name id = !current_unit) then
        (find_pers_struct (Ident.name id)).ps_comps
      else raise Not_found)
  | Pdot (p, s, _pos) -> (
    match get_components (find_module_descr p env) with
    | Structure_comps c ->
      let descr, _pos = Tbl.find_str s c.comp_components in
      descr
    | Functor_comps _ -> raise Not_found)
  | Papply (p1, p2) -> (
    match get_components (find_module_descr p1 env) with
    | Functor_comps f -> !components_of_functor_appl' f env p1 p2
    | Structure_comps _ -> raise Not_found)

let find proj1 proj2 path env =
  match path with
  | Pident id -> IdTbl.find_same id (proj1 env)
  | Pdot (p, s, _pos) -> (
    match get_components (find_module_descr p env) with
    | Structure_comps c ->
      let data, _pos = Tbl.find_str s (proj2 c) in
      data
    | Functor_comps _ -> raise Not_found)
  | Papply _ -> raise Not_found

let find_value = find (fun env -> env.values) (fun sc -> sc.comp_values)

and find_type_full = find (fun env -> env.types) (fun sc -> sc.comp_types)

and find_modtype = find (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)

let type_of_cstr path = function
  | {cstr_inlined = Some d; _} ->
    (d, ([], List.map snd (Datarepr.labels_of_type path d)))
  | _ -> assert false

let find_type_full path env =
  match Path.constructor_typath path with
  | Regular p -> (
    try (PathMap.find p env.local_constraints, ([], []))
    with Not_found -> find_type_full p env)
  | Cstr (ty_path, s) ->
    let _, (cstrs, _) =
      try find_type_full ty_path env with Not_found -> assert false
    in
    let cstr =
      try List.find (fun cstr -> cstr.cstr_name = s) cstrs
      with Not_found -> assert false
    in
    type_of_cstr path cstr
  | LocalExt id ->
    let cstr =
      try TycompTbl.find_same id env.constrs with Not_found -> assert false
    in
    type_of_cstr path cstr
  | Ext (mod_path, s) -> (
    let comps =
      try find_module_descr mod_path env with Not_found -> assert false
    in
    let comps =
      match get_components comps with
      | Structure_comps c -> c
      | Functor_comps _ -> assert false
    in
    let exts =
      Ext_list.filter
        (try Tbl.find_str s comps.comp_constrs with Not_found -> assert false)
        (function
          | {cstr_tag = Cstr_extension _} -> true
          | _ -> false)
    in

    match exts with
    | [cstr] -> type_of_cstr path cstr
    | _ -> assert false)

let find_type p env = fst (find_type_full p env)
let find_type_descrs p env = snd (find_type_full p env)

let find_module ~alias path env =
  match path with
  | Pident id -> (
    try
      let data = IdTbl.find_same id env.modules in
      EnvLazy.force subst_modtype_maker data
    with Not_found ->
      if Ident.persistent id && not (Ident.name id = !current_unit) then
        let ps = find_pers_struct (Ident.name id) in
        md (Mty_signature (Lazy.force ps.ps_sig))
      else raise Not_found)
  | Pdot (p, s, _pos) -> (
    match get_components (find_module_descr p env) with
    | Structure_comps c ->
      let data, _pos = Tbl.find_str s c.comp_modules in
      EnvLazy.force subst_modtype_maker data
    | Functor_comps _ -> raise Not_found)
  | Papply (p1, p2) -> (
    let desc1 = find_module_descr p1 env in
    match get_components desc1 with
    | Functor_comps f ->
      md
        (match f.fcomp_res with
        | Mty_alias _ as mty -> mty
        | mty -> (
          if alias then mty
          else
            try Hashtbl.find f.fcomp_subst_cache p2
            with Not_found ->
              let mty =
                Subst.modtype
                  (Subst.add_module f.fcomp_param p2 Subst.identity)
                  f.fcomp_res
              in
              Hashtbl.add f.fcomp_subst_cache p2 mty;
              mty))
    | Structure_comps _ -> raise Not_found)

let rec normalize_path lax env path =
  let path =
    match path with
    | Pdot (p, s, pos) -> Pdot (normalize_path lax env p, s, pos)
    | Papply (p1, p2) ->
      Papply (normalize_path lax env p1, normalize_path true env p2)
    | _ -> path
  in
  try
    match find_module ~alias:true path env with
    | {md_type = Mty_alias (_, path1)} -> normalize_path lax env path1
    | _ -> path
  with
  | Not_found
  when lax
       ||
       match path with
       | Pident id -> not (Ident.persistent id)
       | _ -> true
  ->
    path

let normalize_path oloc env path =
  try normalize_path (oloc = None) env path
  with Not_found -> (
    match oloc with
    | None -> assert false
    | Some loc ->
      raise (Error (Missing_module (loc, path, normalize_path true env path))))

let normalize_path_prefix oloc env path =
  match path with
  | Pdot (p, s, pos) -> Pdot (normalize_path oloc env p, s, pos)
  | Pident _ -> path
  | Papply _ -> assert false

let find_module = find_module ~alias:false

(* Find the manifest type associated to a type when appropriate:
   - the type should be public or should have a private row,
   - the type should have an associated manifest type. *)
let find_type_expansion path env =
  let decl = find_type path env in
  match decl.type_manifest with
  | Some body
    when decl.type_private = Public
         || decl.type_kind <> Type_abstract
         || Btype.has_constr_row body ->
    (decl.type_params, body, may_map snd decl.type_newtype_level)
  (* The manifest type of Private abstract data types without
     private row are still considered unknown to the type system.
     Hence, this case is caught by the following clause that also handles
     purely abstract data types without manifest type definition. *)
  | _ -> raise Not_found

(* Find the manifest type information associated to a type, i.e.
   the necessary information for the compiler's type-based optimisations.
   In particular, the manifest type associated to a private abstract type
   is revealed for the sake of compiler's type-based optimisations. *)
let find_type_expansion_opt path env =
  let decl = find_type path env in
  match decl.type_manifest with
  (* The manifest type of Private abstract data types can still get
     an approximation using their manifest type. *)
  | Some body -> (decl.type_params, body, may_map snd decl.type_newtype_level)
  | _ -> raise Not_found

let find_modtype_expansion path env =
  match (find_modtype path env).mtd_type with
  | None -> raise Not_found
  | Some mty -> mty

let rec is_functor_arg path env =
  match path with
  | Pident id -> (
    try
      Ident.find_same id env.functor_args;
      true
    with Not_found -> false)
  | Pdot (p, _s, _) -> is_functor_arg p env
  | Papply _ -> true

(* Lookup by name *)

exception Recmodule

let report_deprecated ?loc p deprecated =
  match (loc, deprecated) with
  | Some loc, Some txt ->
    let txt = if txt = "" then "" else "\n" ^ txt in
    Location.deprecated loc (Printf.sprintf "module %s%s" (Path.name p) txt)
  | _ -> ()

let mark_module_used env name loc =
  if not (is_implicit_coercion env) then
    try Hashtbl.find module_declarations (name, loc) () with Not_found -> ()

let rec lookup_module_descr_aux ?loc lid env =
  match lid with
  | Lident s -> (
    try IdTbl.find_name s env.components
    with Not_found ->
      if s = !current_unit then raise Not_found;
      let ps = find_pers_struct s in
      (Pident (Ident.create_persistent s), ps.ps_comps))
  | Ldot (l, s) -> (
    let p, descr = lookup_module_descr ?loc l env in
    match get_components descr with
    | Structure_comps c ->
      let descr, pos = Tbl.find_str s c.comp_components in
      (Pdot (p, s, pos), descr)
    | Functor_comps _ -> raise Not_found)
  | Lapply (l1, l2) -> (
    let p1, desc1 = lookup_module_descr ?loc l1 env in
    let p2 = lookup_module ~load:true ?loc l2 env in
    let {md_type = mty2} = find_module p2 env in
    match get_components desc1 with
    | Functor_comps f ->
      let loc =
        match loc with
        | Some l -> l
        | None -> Location.none
      in
      Misc.may (!check_modtype_inclusion ~loc env mty2 p2) f.fcomp_arg;
      (Papply (p1, p2), !components_of_functor_appl' f env p1 p2)
    | Structure_comps _ -> raise Not_found)

and lookup_module_descr ?loc lid env =
  let ((p, comps) as res) = lookup_module_descr_aux ?loc lid env in
  mark_module_used env (Path.last p) comps.loc;
  report_deprecated ?loc p comps.deprecated;
  res

and lookup_module ~load ?loc lid env : Path.t =
  match lid with
  | Lident s -> (
    try
      let p, data = IdTbl.find_name s env.modules in
      let {md_loc; md_attributes; md_type} =
        EnvLazy.force subst_modtype_maker data
      in
      mark_module_used env s md_loc;
      (match md_type with
      | Mty_ident (Path.Pident id) when Ident.name id = "#recmod#" ->
        (* see #5965 *)
        raise Recmodule
      | Mty_alias (_, Path.Pident id) ->
        if (not !Clflags.transparent_modules) && Ident.persistent id then
          find_pers_struct (Ident.name id) |> ignore
      | _ -> ());
      report_deprecated ?loc p
        (Builtin_attributes.deprecated_of_attrs md_attributes);
      p
    with Not_found ->
      if s = !current_unit then raise Not_found;
      let p = Pident (Ident.create_persistent s) in
      (if !Clflags.transparent_modules && not load then check_pers_struct s
       else
         let ps = find_pers_struct s in
         report_deprecated ?loc p ps.ps_comps.deprecated);
      p)
  | Ldot (l, s) -> (
    let p, descr = lookup_module_descr ?loc l env in
    match get_components descr with
    | Structure_comps c ->
      let _data, pos = Tbl.find_str s c.comp_modules in
      let comps, _ = Tbl.find_str s c.comp_components in
      mark_module_used env s comps.loc;
      let p = Pdot (p, s, pos) in
      report_deprecated ?loc p comps.deprecated;
      p
    | Functor_comps _ -> raise Not_found)
  | Lapply (l1, l2) -> (
    let p1, desc1 = lookup_module_descr ?loc l1 env in
    let p2 = lookup_module ~load:true ?loc l2 env in
    let {md_type = mty2} = find_module p2 env in
    let p = Papply (p1, p2) in
    match get_components desc1 with
    | Functor_comps f ->
      let loc =
        match loc with
        | Some l -> l
        | None -> Location.none
      in
      Misc.may (!check_modtype_inclusion ~loc env mty2 p2) f.fcomp_arg;
      p
    | Structure_comps _ -> raise Not_found)

let lookup proj1 proj2 ?loc lid env =
  match lid with
  | Lident s -> IdTbl.find_name s (proj1 env)
  | Ldot (l, s) -> (
    let p, desc = lookup_module_descr ?loc l env in
    match get_components desc with
    | Structure_comps c ->
      let data, pos = Tbl.find_str s (proj2 c) in
      (Pdot (p, s, pos), data)
    | Functor_comps _ -> raise Not_found)
  | Lapply _ -> raise Not_found

let lookup_all_simple proj1 proj2 shadow ?loc lid env =
  match lid with
  | Lident s ->
    let xl = TycompTbl.find_all s (proj1 env) in
    let rec do_shadow = function
      | [] -> []
      | (x, f) :: xs ->
        (x, f)
        :: do_shadow (Ext_list.filter xs (fun (y, _) -> not (shadow x y)))
    in
    do_shadow xl
  | Ldot (l, s) -> (
    let _p, desc = lookup_module_descr ?loc l env in
    match get_components desc with
    | Structure_comps c ->
      let comps = try Tbl.find_str s (proj2 c) with Not_found -> [] in
      List.map (fun data -> (data, fun () -> ())) comps
    | Functor_comps _ -> raise Not_found)
  | Lapply _ -> raise Not_found

let has_local_constraints env = not (PathMap.is_empty env.local_constraints)

let cstr_shadow cstr1 cstr2 =
  match (cstr1.cstr_tag, cstr2.cstr_tag) with
  | Cstr_extension _, Cstr_extension _ -> true
  | _ -> false

let lbl_shadow _lbl1 _lbl2 = false

let lookup_value = lookup (fun env -> env.values) (fun sc -> sc.comp_values)
let lookup_all_constructors =
  lookup_all_simple
    (fun env -> env.constrs)
    (fun sc -> sc.comp_constrs)
    cstr_shadow
let lookup_all_labels =
  lookup_all_simple
    (fun env -> env.labels)
    (fun sc -> sc.comp_labels)
    lbl_shadow
let lookup_type = lookup (fun env -> env.types) (fun sc -> sc.comp_types)
let lookup_modtype =
  lookup (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes)

let copy_types l env =
  let f desc =
    {desc with val_type = Subst.type_expr Subst.identity desc.val_type}
  in
  let values =
    List.fold_left (fun env s -> IdTbl.update s f env) env.values l
  in
  {env with values; summary = Env_copy_types (env.summary, l)}

let mark_value_used env name vd =
  if not (is_implicit_coercion env) then
    try Hashtbl.find value_declarations (name, vd.val_loc) ()
    with Not_found -> ()

let mark_type_used env name vd =
  if not (is_implicit_coercion env) then
    try Hashtbl.find type_declarations (name, vd.type_loc) ()
    with Not_found -> ()

let mark_constructor_used usage env name vd constr =
  if not (is_implicit_coercion env) then
    try Hashtbl.find used_constructors (name, vd.type_loc, constr) usage
    with Not_found -> ()

let mark_extension_used usage env ext name =
  if not (is_implicit_coercion env) then
    let ty_name = Path.last ext.ext_type_path in
    try Hashtbl.find used_constructors (ty_name, ext.ext_loc, name) usage
    with Not_found -> ()

let set_value_used_callback name vd callback =
  let key = (name, vd.val_loc) in
  try
    let old = Hashtbl.find value_declarations key in
    Hashtbl.replace value_declarations key (fun () ->
        old ();
        callback ())
    (* this is to support cases like:
             let x = let x = 1 in x in x
       where the two declarations have the same location
       (e.g. resulting from Camlp4 expansion of grammar entries) *)
  with Not_found -> Hashtbl.add value_declarations key callback

let set_type_used_callback name td callback =
  let loc = td.type_loc in
  if loc.Location.loc_ghost then ()
  else
    let key = (name, loc) in
    let old =
      try Hashtbl.find type_declarations key with Not_found -> assert false
    in
    Hashtbl.replace type_declarations key (fun () -> callback old)

let lookup_value ?loc lid env =
  let ((_, desc) as r) = lookup_value ?loc lid env in
  mark_value_used env (Longident.last lid) desc;
  r

let lookup_type ?loc lid env =
  let path, (decl, _) = lookup_type ?loc lid env in
  mark_type_used env (Longident.last lid) decl;
  path

let mark_type_path env path =
  try
    let decl = find_type path env in
    mark_type_used env (Path.last path) decl
  with Not_found -> ()

let ty_path t =
  match repr t with
  | {desc = Tconstr (path, _, _)} -> path
  | _ -> assert false

let lookup_constructor ?loc lid env =
  match lookup_all_constructors ?loc lid env with
  | [] -> raise Not_found
  | (desc, use) :: _ ->
    mark_type_path env (ty_path desc.cstr_res);
    use ();
    desc

let is_lident = function
  | Lident _ -> true
  | _ -> false

let lookup_all_constructors ?loc lid env =
  try
    let cstrs = lookup_all_constructors ?loc lid env in
    let wrap_use desc use () =
      mark_type_path env (ty_path desc.cstr_res);
      use ()
    in
    List.map (fun (cstr, use) -> (cstr, wrap_use cstr use)) cstrs
  with Not_found when is_lident lid -> []

let mark_constructor usage env name desc =
  if not (is_implicit_coercion env) then
    match desc.cstr_tag with
    | Cstr_extension _ -> (
      let ty_path = ty_path desc.cstr_res in
      let ty_name = Path.last ty_path in
      try Hashtbl.find used_constructors (ty_name, desc.cstr_loc, name) usage
      with Not_found -> ())
    | _ ->
      let ty_path = ty_path desc.cstr_res in
      let ty_decl =
        try find_type ty_path env with Not_found -> assert false
      in
      let ty_name = Path.last ty_path in
      mark_constructor_used usage env ty_name ty_decl name

let lookup_label ?loc lid env =
  match lookup_all_labels ?loc lid env with
  | [] -> raise Not_found
  | (desc, use) :: _ ->
    mark_type_path env (ty_path desc.lbl_res);
    use ();
    desc

let lookup_all_labels ?loc lid env =
  try
    let lbls = lookup_all_labels ?loc lid env in
    let wrap_use desc use () =
      mark_type_path env (ty_path desc.lbl_res);
      use ()
    in
    List.map (fun (lbl, use) -> (lbl, wrap_use lbl use)) lbls
  with Not_found when is_lident lid -> []

(* Iter on an environment (ignoring the body of functors and
   not yet evaluated structures) *)

type iter_cont = unit -> unit
let iter_env_cont = ref []

let rec scrape_alias_for_visit env mty =
  match mty with
  | Mty_alias (_, Pident id)
    when Ident.persistent id
         && not (Hashtbl.mem persistent_structures (Ident.name id)) ->
    false
  | Mty_alias (_, path) -> (
    (* PR#6600: find_module may raise Not_found *)
    try scrape_alias_for_visit env (find_module path env).md_type
    with Not_found -> false)
  | _ -> true

let iter_env proj1 proj2 f env () =
  IdTbl.iter (fun id x -> f (Pident id) x) (proj1 env);
  let rec iter_components path path' mcomps =
    let cont () =
      let visit =
        match EnvLazy.get_arg mcomps.comps with
        | None -> true
        | Some (env, _sub, _path, mty) -> scrape_alias_for_visit env mty
      in
      if not visit then ()
      else
        match get_components mcomps with
        | Structure_comps comps ->
          Tbl.iter
            (fun s (d, n) -> f (Pdot (path, s, n)) (Pdot (path', s, n), d))
            (proj2 comps);
          Tbl.iter
            (fun s (c, n) ->
              iter_components (Pdot (path, s, n)) (Pdot (path', s, n)) c)
            comps.comp_components
        | Functor_comps _ -> ()
    in
    iter_env_cont := (path, cont) :: !iter_env_cont
  in
  Hashtbl.iter
    (fun s pso ->
      match pso with
      | None -> ()
      | Some ps ->
        let id = Pident (Ident.create_persistent s) in
        iter_components id id ps.ps_comps)
    persistent_structures;
  IdTbl.iter
    (fun id (path, comps) -> iter_components (Pident id) path comps)
    env.components

let run_iter_cont l =
  iter_env_cont := [];
  List.iter (fun c -> c ()) l;
  let cont = List.rev !iter_env_cont in
  iter_env_cont := [];
  cont

let iter_types f = iter_env (fun env -> env.types) (fun sc -> sc.comp_types) f

let same_types env1 env2 =
  env1.types == env2.types && env1.components == env2.components

let used_persistent () =
  let r = ref Concr.empty in
  Hashtbl.iter
    (fun s pso -> if pso != None then r := Concr.add s !r)
    persistent_structures;
  !r

let find_all_comps proj s (p, mcomps) =
  match get_components mcomps with
  | Functor_comps _ -> []
  | Structure_comps comps -> (
    try
      let c, n = Tbl.find_str s (proj comps) in
      [(Pdot (p, s, n), c)]
    with Not_found -> [])

let rec find_shadowed_comps path env =
  match path with
  | Pident id -> IdTbl.find_all (Ident.name id) env.components
  | Pdot (p, s, _) ->
    let l = find_shadowed_comps p env in
    let l' =
      List.map (find_all_comps (fun comps -> comps.comp_components) s) l
    in
    List.flatten l'
  | Papply _ -> []

let find_shadowed proj1 proj2 path env =
  match path with
  | Pident id -> IdTbl.find_all (Ident.name id) (proj1 env)
  | Pdot (p, s, _) ->
    let l = find_shadowed_comps p env in
    let l' = List.map (find_all_comps proj2 s) l in
    List.flatten l'
  | Papply _ -> []

let find_shadowed_types path env =
  List.map fst
    (find_shadowed
       (fun env -> env.types)
       (fun comps -> comps.comp_types)
       path env)

(* GADT instance tracking *)

let add_gadt_instance_level lv env =
  {env with gadt_instances = (lv, ref TypeSet.empty) :: env.gadt_instances}

let is_Tlink = function
  | {desc = Tlink _} -> true
  | _ -> false

let gadt_instance_level env t =
  let rec find_instance = function
    | [] -> None
    | (lv, r) :: rem ->
      if TypeSet.exists is_Tlink !r then
        (* Should we use set_typeset ? *)
        r := TypeSet.fold (fun ty -> TypeSet.add (repr ty)) !r TypeSet.empty;
      if TypeSet.mem t !r then Some lv else find_instance rem
  in
  find_instance env.gadt_instances

let add_gadt_instances env lv tl =
  let r =
    try List.assoc lv env.gadt_instances with Not_found -> assert false
  in
  (* Format.eprintf "Added";
     List.iter (fun ty -> Format.eprintf "@ %a" !Btype.print_raw ty) tl;
     Format.eprintf "@."; *)
  set_typeset r (List.fold_right TypeSet.add tl !r)

(* Only use this after expand_head! *)
let add_gadt_instance_chain env lv t =
  let r =
    try List.assoc lv env.gadt_instances with Not_found -> assert false
  in
  let rec add_instance t =
    let t = repr t in
    if not (TypeSet.mem t !r) then (
      (* Format.eprintf "@ %a" !Btype.print_raw t; *)
      set_typeset r (TypeSet.add t !r);
      match t.desc with
      | Tconstr (p, _, memo) -> may add_instance (find_expans Private p !memo)
      | _ -> ())
  in
  (* Format.eprintf "Added chain"; *)
  add_instance t
(* Format.eprintf "@." *)

(* Expand manifest module type names at the top of the given module type *)

let rec scrape_alias env ?path mty =
  match (mty, path) with
  | Mty_ident p, _ -> (
    try scrape_alias env (find_modtype_expansion p env) ?path
    with Not_found -> mty)
  | Mty_alias (_, path), _ -> (
    try scrape_alias env (find_module path env).md_type ~path
    with Not_found ->
      (*Location.prerr_warning Location.none
        (Warnings.No_cmi_file (Path.name path));*)
      mty)
  | mty, Some path -> !strengthen ~aliasable:true env mty path
  | _ -> mty

let scrape_alias env mty = scrape_alias env mty

(* Given a signature and a root path, prefix all idents in the signature
   by the root path and build the corresponding substitution. *)

let rec prefix_idents root pos sub = function
  | [] -> ([], sub)
  | Sig_value (id, decl) :: rem ->
    let p = Pdot (root, Ident.name id, pos) in
    let nextpos =
      match decl.val_kind with
      | Val_prim _ -> pos
      | _ -> pos + 1
    in
    let pl, final_sub = prefix_idents root nextpos sub rem in
    (p :: pl, final_sub)
  | Sig_type (id, _, _) :: rem ->
    let p = Pdot (root, Ident.name id, nopos) in
    let pl, final_sub = prefix_idents root pos (Subst.add_type id p sub) rem in
    (p :: pl, final_sub)
  | Sig_typext (id, _, _) :: rem ->
    let p = Pdot (root, Ident.name id, pos) in
    (* we extend the substitution in case of an inlined record *)
    let pl, final_sub =
      prefix_idents root (pos + 1) (Subst.add_type id p sub) rem
    in
    (p :: pl, final_sub)
  | Sig_module (id, _, _) :: rem ->
    let p = Pdot (root, Ident.name id, pos) in
    let pl, final_sub =
      prefix_idents root (pos + 1) (Subst.add_module id p sub) rem
    in
    (p :: pl, final_sub)
  | Sig_modtype (id, _) :: rem ->
    let p = Pdot (root, Ident.name id, nopos) in
    let pl, final_sub =
      prefix_idents root pos (Subst.add_modtype id (Mty_ident p) sub) rem
    in
    (p :: pl, final_sub)
  | Sig_class _ :: _ -> assert false
  | Sig_class_type _ :: _ -> assert false

let prefix_idents root sub sg =
  if sub = Subst.identity then (
    let sgs =
      try Hashtbl.find prefixed_sg root
      with Not_found ->
        let sgs = ref [] in
        Hashtbl.add prefixed_sg root sgs;
        sgs
    in
    try List.assq sg !sgs
    with Not_found ->
      let r = prefix_idents root 0 sub sg in
      sgs := (sg, r) :: !sgs;
      r)
  else prefix_idents root 0 sub sg

(* Compute structure descriptions *)

let add_to_tbl id decl tbl =
  let decls = try Tbl.find_str id tbl with Not_found -> [] in
  Tbl.add id (decl :: decls) tbl

let rec components_of_module ~deprecated ~loc env sub path mty =
  {deprecated; loc; comps = EnvLazy.create (env, sub, path, mty)}

and components_of_module_maker (env, sub, path, mty) =
  match scrape_alias env mty with
  | Mty_signature sg ->
    let c =
      {
        comp_values = Tbl.empty;
        comp_constrs = Tbl.empty;
        comp_labels = Tbl.empty;
        comp_types = Tbl.empty;
        comp_modules = Tbl.empty;
        comp_modtypes = Tbl.empty;
        comp_components = Tbl.empty;
      }
    in
    let pl, sub = prefix_idents path sub sg in
    let env = ref env in
    let pos = ref 0 in
    List.iter2
      (fun item path ->
        match item with
        | Sig_value (id, decl) -> (
          let decl' = Subst.value_description sub decl in
          c.comp_values <- Tbl.add (Ident.name id) (decl', !pos) c.comp_values;
          match decl.val_kind with
          | Val_prim _ -> ()
          | _ -> incr pos)
        | Sig_type (id, decl, _) ->
          let decl' = Subst.type_declaration sub decl in
          Datarepr.set_row_name decl' (Subst.type_path sub (Path.Pident id));
          let constructors =
            List.map snd (Datarepr.constructors_of_type path decl')
          in
          let labels = List.map snd (Datarepr.labels_of_type path decl') in
          c.comp_types <-
            Tbl.add (Ident.name id)
              ((decl', (constructors, labels)), nopos)
              c.comp_types;
          List.iter
            (fun descr ->
              c.comp_constrs <- add_to_tbl descr.cstr_name descr c.comp_constrs)
            constructors;
          List.iter
            (fun descr ->
              c.comp_labels <- add_to_tbl descr.lbl_name descr c.comp_labels)
            labels;
          env := store_type_infos id decl !env
        | Sig_typext (id, ext, _) ->
          let ext' = Subst.extension_constructor sub ext in
          let descr = Datarepr.extension_descr path ext' in
          c.comp_constrs <- add_to_tbl (Ident.name id) descr c.comp_constrs;
          incr pos
        | Sig_module (id, md, _) ->
          let md' = EnvLazy.create (sub, md) in
          c.comp_modules <- Tbl.add (Ident.name id) (md', !pos) c.comp_modules;
          let deprecated =
            Builtin_attributes.deprecated_of_attrs md.md_attributes
          in
          let comps =
            components_of_module ~deprecated ~loc:md.md_loc !env sub path
              md.md_type
          in
          c.comp_components <-
            Tbl.add (Ident.name id) (comps, !pos) c.comp_components;
          env := store_module ~check:false id md !env;
          incr pos
        | Sig_modtype (id, decl) ->
          let decl' = Subst.modtype_declaration sub decl in
          c.comp_modtypes <-
            Tbl.add (Ident.name id) (decl', nopos) c.comp_modtypes;
          env := store_modtype id decl !env
        | Sig_class () -> assert false
        | Sig_class_type () -> assert false)
      sg pl;
    Some (Structure_comps c)
  | Mty_functor (param, ty_arg, ty_res) ->
    Some
      (Functor_comps
         {
           fcomp_param = param;
           (* fcomp_arg and fcomp_res must be prefixed eagerly, because
              they are interpreted in the outer environment *)
           fcomp_arg = may_map (Subst.modtype sub) ty_arg;
           fcomp_res = Subst.modtype sub ty_res;
           fcomp_cache = Hashtbl.create 17;
           fcomp_subst_cache = Hashtbl.create 17;
         })
  | Mty_ident _ | Mty_alias _ -> None

(* Insertion of bindings by identifier + path *)

and check_usage loc id warn tbl =
  if (not loc.Location.loc_ghost) && Warnings.is_active (warn "") then (
    let name = Ident.name id in
    let key = (name, loc) in
    if Hashtbl.mem tbl key then ()
    else
      let used = ref false in
      Hashtbl.add tbl key (fun () -> used := true);
      if not (name = "" || name.[0] = '_' || name.[0] = '#') then
        Delayed_checks.add_delayed_check (fun () ->
            if not !used then Location.prerr_warning loc (warn name)))

and check_value_name name loc =
  (* Note: we could also check here general validity of the
     identifier, to protect against bad identifiers forged by -pp or
     -ppx preprocessors. *)
  if name = "->" then raise (Error (Illegal_value_name (loc, name)))
  else if String.length name > 0 && name.[0] = '#' then
    for i = 1 to String.length name - 1 do
      if name.[i] = '#' then raise (Error (Illegal_value_name (loc, name)))
    done

and store_value ?check id decl env =
  check_value_name (Ident.name id) decl.val_loc;
  may (fun f -> check_usage decl.val_loc id f value_declarations) check;
  {
    env with
    values = IdTbl.add id decl env.values;
    summary = Env_value (env.summary, id, decl);
  }

and store_type ~check id info env =
  let loc = info.type_loc in
  if check then
    check_usage loc id
      (fun s -> Warnings.Unused_type_declaration s)
      type_declarations;
  let path = Pident id in
  let constructors = Datarepr.constructors_of_type path info in
  let labels = Datarepr.labels_of_type path info in
  let descrs = (List.map snd constructors, List.map snd labels) in

  (if
     check
     && (not loc.Location.loc_ghost)
     && Warnings.is_active (Warnings.Unused_constructor ("", false, false))
   then
     let ty = Ident.name id in
     List.iter
       (fun (_, {cstr_name = c; _}) ->
         let k = (ty, loc, c) in
         if not (Hashtbl.mem used_constructors k) then (
           let used = constructor_usages () in
           Hashtbl.add used_constructors k (add_constructor_usage used);
           if not (ty = "" || ty.[0] = '_') then
             Delayed_checks.add_delayed_check (fun () ->
                 if (not (is_in_signature env)) && not used.cu_positive then
                   Location.prerr_warning loc
                     (Warnings.Unused_constructor
                        (c, used.cu_pattern, used.cu_privatize)))))
       constructors);
  {
    env with
    constrs =
      List.fold_right
        (fun (id, descr) constrs -> TycompTbl.add id descr constrs)
        constructors env.constrs;
    labels =
      List.fold_right
        (fun (id, descr) labels -> TycompTbl.add id descr labels)
        labels env.labels;
    types = IdTbl.add id (info, descrs) env.types;
    summary = Env_type (env.summary, id, info);
  }

and store_type_infos id info env =
  (* Simplified version of store_type that doesn't compute and store
     constructor and label infos, but simply record the arity and
     manifest-ness of the type.  Used in components_of_module to
     keep track of type abbreviations (e.g. type t = float) in the
     computation of label representations. *)
  {
    env with
    types = IdTbl.add id (info, ([], [])) env.types;
    summary = Env_type (env.summary, id, info);
  }

and store_extension ~check id ext env =
  let loc = ext.ext_loc in
  (if
     check
     && (not loc.Location.loc_ghost)
     && Warnings.is_active (Warnings.Unused_extension ("", false, false, false))
   then
     let ty = Path.last ext.ext_type_path in
     let n = Ident.name id in
     let k = (ty, loc, n) in
     if not (Hashtbl.mem used_constructors k) then (
       let used = constructor_usages () in
       Hashtbl.add used_constructors k (add_constructor_usage used);
       Delayed_checks.add_delayed_check (fun () ->
           if (not (is_in_signature env)) && not used.cu_positive then
             Location.prerr_warning loc
               (Warnings.Unused_extension
                  (n, ext.ext_is_exception, used.cu_pattern, used.cu_privatize)))));
  {
    env with
    constrs =
      TycompTbl.add id (Datarepr.extension_descr (Pident id) ext) env.constrs;
    summary = Env_extension (env.summary, id, ext);
  }

and store_module ~check id md env =
  let loc = md.md_loc in
  if check then
    check_usage loc id (fun s -> Warnings.Unused_module s) module_declarations;

  let deprecated = Builtin_attributes.deprecated_of_attrs md.md_attributes in
  {
    env with
    modules = IdTbl.add id (EnvLazy.create (Subst.identity, md)) env.modules;
    components =
      IdTbl.add id
        (components_of_module ~deprecated ~loc:md.md_loc env Subst.identity
           (Pident id) md.md_type)
        env.components;
    summary = Env_module (env.summary, id, md);
  }

and store_modtype id info env =
  {
    env with
    modtypes = IdTbl.add id info env.modtypes;
    summary = Env_modtype (env.summary, id, info);
  }

(* Compute the components of a functor application in a path. *)

let components_of_functor_appl f env p1 p2 =
  try Hashtbl.find f.fcomp_cache p2
  with Not_found ->
    let p = Papply (p1, p2) in
    let sub = Subst.add_module f.fcomp_param p2 Subst.identity in
    let mty = Subst.modtype sub f.fcomp_res in
    let comps =
      components_of_module ~deprecated:None ~loc:Location.none (*???*)
        env Subst.identity p mty
    in
    Hashtbl.add f.fcomp_cache p2 comps;
    comps

(* Define forward functions *)

let _ =
  components_of_module' := components_of_module;
  components_of_functor_appl' := components_of_functor_appl;
  components_of_module_maker' := components_of_module_maker

(* Insertion of bindings by identifier *)

let add_functor_arg id env =
  {
    env with
    functor_args = Ident.add id () env.functor_args;
    summary = Env_functor_arg (env.summary, id);
  }

let add_value ?check id desc env = store_value ?check id desc env

let add_type ~check id info env = store_type ~check id info env

and add_extension ~check id ext env = store_extension ~check id ext env

and add_module_declaration ?(arg = false) ~check id md env =
  let env = store_module ~check id md env in
  if arg then add_functor_arg id env else env

and add_modtype id info env = store_modtype id info env

let add_module ?arg id mty env =
  add_module_declaration ~check:false ?arg id (md mty) env

let add_local_type path info env =
  {env with local_constraints = PathMap.add path info env.local_constraints}

let add_local_constraint path info elv env =
  match info with
  | {type_manifest = Some _; type_newtype_level = Some (lv, _)} ->
    (* elv is the expansion level, lv is the definition level *)
    let info = {info with type_newtype_level = Some (lv, elv)} in
    add_local_type path info env
  | _ -> assert false

(* Insertion of bindings by name *)

let enter store_fun name data env =
  let id = Ident.create name in
  (id, store_fun id data env)

let enter_value ?check = enter (store_value ?check)

and enter_type = enter (store_type ~check:true)

and enter_extension = enter (store_extension ~check:true)

and enter_module_declaration ?arg id md env =
  add_module_declaration ?arg ~check:true id md env
(* let (id, env) = enter store_module name md env in
   (id, add_functor_arg ?arg id env) *)

and enter_modtype = enter store_modtype

let enter_module ?arg s mty env =
  let id = Ident.create s in
  (id, enter_module_declaration ?arg id (md mty) env)

(* Insertion of all components of a signature *)

let add_item comp env =
  match comp with
  | Sig_value (id, decl) -> add_value id decl env
  | Sig_type (id, decl, _) -> add_type ~check:false id decl env
  | Sig_typext (id, ext, _) -> add_extension ~check:false id ext env
  | Sig_module (id, md, _) -> add_module_declaration ~check:false id md env
  | Sig_modtype (id, decl) -> add_modtype id decl env
  | Sig_class () -> env
  | Sig_class_type () -> env

let rec add_signature sg env =
  match sg with
  | [] -> env
  | comp :: rem -> add_signature rem (add_item comp env)

(* Open a signature path *)

let add_components slot root env0 comps =
  let add_l w comps env0 = TycompTbl.add_open slot w comps env0 in

  let add w comps env0 = IdTbl.add_open slot w root comps env0 in

  let constrs =
    add_l (fun x -> `Constructor x) comps.comp_constrs env0.constrs
  in
  let labels = add_l (fun x -> `Label x) comps.comp_labels env0.labels in

  let values = add (fun x -> `Value x) comps.comp_values env0.values in
  let types = add (fun x -> `Type x) comps.comp_types env0.types in
  let modtypes =
    add (fun x -> `Module_type x) comps.comp_modtypes env0.modtypes
  in
  let components =
    add (fun x -> `Component x) comps.comp_components env0.components
  in

  let modules = add (fun x -> `Module x) comps.comp_modules env0.modules in

  {
    env0 with
    summary = Env_open (env0.summary, root);
    constrs;
    labels;
    values;
    types;
    modtypes;
    components;
    modules;
  }

let open_signature slot root env0 =
  match get_components (find_module_descr root env0) with
  | Functor_comps _ -> None
  | Structure_comps comps -> Some (add_components slot root env0 comps)

(* Open a signature from a file *)

let open_pers_signature name env =
  match open_signature None (Pident (Ident.create_persistent name)) env with
  | Some env -> env
  | None -> assert false (* a compilation unit cannot refer to a functor *)

let open_signature ?(used_slot = ref false) ?(loc = Location.none)
    ?(toplevel = false) ovf root env =
  if
    (not toplevel) && ovf = Asttypes.Fresh
    && (not loc.Location.loc_ghost)
    && (Warnings.is_active (Warnings.Unused_open "")
       || Warnings.is_active (Warnings.Open_shadow_identifier ("", ""))
       || Warnings.is_active (Warnings.Open_shadow_label_constructor ("", "")))
  then (
    let used = used_slot in
    Delayed_checks.add_delayed_check (fun () ->
        if not !used then (
          used := true;
          Location.prerr_warning loc (Warnings.Unused_open (Path.name root))));
    let shadowed = ref [] in
    let slot s b =
      (match check_shadowing env b with
      | Some kind when not (List.mem (kind, s) !shadowed) ->
        shadowed := (kind, s) :: !shadowed;
        let w =
          match kind with
          | "label" | "constructor" ->
            Warnings.Open_shadow_label_constructor (kind, s)
          | _ -> Warnings.Open_shadow_identifier (kind, s)
        in
        Location.prerr_warning loc w
      | _ -> ());
      used := true
    in
    open_signature (Some slot) root env)
  else open_signature None root env

(* Read a signature from a file *)

let read_signature modname filename =
  let ps = read_pers_struct modname filename in
  Lazy.force ps.ps_sig

(* Return the CRC of the interface of the given compilation unit *)

let crc_of_unit name =
  let ps = find_pers_struct name in
  let crco = try List.assoc name ps.ps_crcs with Not_found -> assert false in
  match crco with
  | None -> assert false
  | Some crc -> crc

(* Return the list of imported interfaces with their CRCs *)

let imports () =
  let dont_record_crc_unit = !Clflags.dont_record_crc_unit in
  match dont_record_crc_unit with
  | None -> Consistbl.extract (StringSet.elements !imported_units) crc_units
  | Some x ->
    Consistbl.extract
      (StringSet.fold
         (fun m acc -> if m = x then acc else m :: acc)
         !imported_units [])
      crc_units

(* Save a signature to a file *)

let save_signature_with_imports ?check_exists ~deprecated sg modname filename
    imports =
  (*prerr_endline filename;
    List.iter (fun (name, crc) -> prerr_endline name) imports;*)
  Btype.cleanup_abbrev ();
  Subst.reset_for_saving ();
  let sg = Subst.signature (Subst.for_saving Subst.identity) sg in
  let flags =
    match deprecated with
    | Some s -> [Deprecated s]
    | None -> []
  in
  try
    let cmi =
      {cmi_name = modname; cmi_sign = sg; cmi_crcs = imports; cmi_flags = flags}
    in
    let crc = create_cmi ?check_exists filename cmi in
    (* Enter signature in persistent table so that imported_unit()
       will also return its crc *)
    let comps =
      components_of_module ~deprecated ~loc:Location.none empty Subst.identity
        (Pident (Ident.create_persistent modname))
        (Mty_signature sg)
    in
    let ps =
      {
        ps_name = modname;
        ps_sig = lazy (Subst.signature Subst.identity sg);
        ps_comps = comps;
        ps_crcs = (cmi.cmi_name, Some crc) :: imports;
        ps_filename = filename;
        ps_flags = cmi.cmi_flags;
      }
    in
    save_pers_struct crc ps;
    cmi
  with exn ->
    remove_file filename;
    raise exn

let save_signature ?check_exists ~deprecated sg modname filename =
  save_signature_with_imports ?check_exists ~deprecated sg modname filename
    (imports ())

(* Folding on environments *)

let find_all proj1 proj2 f lid env acc =
  match lid with
  | None ->
    IdTbl.fold_name
      (fun name (p, data) acc -> f name p data acc)
      (proj1 env) acc
  | Some l -> (
    let p, desc = lookup_module_descr l env in
    match get_components desc with
    | Structure_comps c ->
      Tbl.fold
        (fun s (data, pos) acc -> f s (Pdot (p, s, pos)) data acc)
        (proj2 c) acc
    | Functor_comps _ -> acc)

let find_all_simple_list proj1 proj2 f lid env acc =
  match lid with
  | None -> TycompTbl.fold_name (fun data acc -> f data acc) (proj1 env) acc
  | Some l -> (
    let _p, desc = lookup_module_descr l env in
    match get_components desc with
    | Structure_comps c ->
      Tbl.fold
        (fun _s comps acc ->
          match comps with
          | [] -> acc
          | data :: _ -> f data acc)
        (proj2 c) acc
    | Functor_comps _ -> acc)

let fold_modules f lid env acc =
  match lid with
  | None ->
    let acc =
      IdTbl.fold_name
        (fun name (p, data) acc ->
          let data = EnvLazy.force subst_modtype_maker data in
          f name p data acc)
        env.modules acc
    in
    Hashtbl.fold
      (fun name ps acc ->
        match ps with
        | None -> acc
        | Some ps ->
          f name
            (Pident (Ident.create_persistent name))
            (md (Mty_signature (Lazy.force ps.ps_sig)))
            acc)
      persistent_structures acc
  | Some l -> (
    let p, desc = lookup_module_descr l env in
    match get_components desc with
    | Structure_comps c ->
      Tbl.fold
        (fun s (data, pos) acc ->
          f s (Pdot (p, s, pos)) (EnvLazy.force subst_modtype_maker data) acc)
        c.comp_modules acc
    | Functor_comps _ -> acc)

let fold_values f =
  find_all (fun env -> env.values) (fun sc -> sc.comp_values) f

and fold_constructors f =
  find_all_simple_list (fun env -> env.constrs) (fun sc -> sc.comp_constrs) f

and fold_labels f =
  find_all_simple_list (fun env -> env.labels) (fun sc -> sc.comp_labels) f

and fold_types f = find_all (fun env -> env.types) (fun sc -> sc.comp_types) f

and fold_modtypes f =
  find_all (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes) f

(* Make the initial environment *)
let initial_safe_string =
  Predef.build_initial_env (add_type ~check:false)
    (add_extension ~check:false)
    empty

(* Return the environment summary *)

let summary env =
  if PathMap.is_empty env.local_constraints then env.summary
  else Env_constraints (env.summary, env.local_constraints)

let last_env = ref empty
let last_reduced_env = ref empty

let keep_only_summary env =
  if !last_env == env then !last_reduced_env
  else
    let new_env =
      {
        empty with
        summary = env.summary;
        local_constraints = env.local_constraints;
        flags = env.flags;
      }
    in
    last_env := env;
    last_reduced_env := new_env;
    new_env

let env_of_only_summary env_from_summary env =
  let new_env = env_from_summary env.summary Subst.identity in
  {new_env with local_constraints = env.local_constraints; flags = env.flags}

(* Error report *)

open Format

(* taken from https://github.com/rescript-lang/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/env.ml#L1842 *)
(* modified branches are commented *)
let report_error ppf = function
  | Illegal_renaming (name, modname, _filename) ->
    (* modified *)
    fprintf ppf
      "@[You referred to the module %s, but we've found one called %s \
       instead.@ Is the name's casing right?@]"
      name modname
  | Inconsistent_import (name, source1, source2) ->
    (* modified *)
    fprintf ppf
      "@[<v>@[@{<info>It's possible that your build is stale.@}@ Try to clean \
       the artifacts and build again?@]@,\
       @,\
       @[@{<info>Here's the original error message@}@]@,\
       @]";
    fprintf ppf
      "@[<hov>The files %a@ and %a@ make inconsistent assumptions@ over \
       interface %s@]"
      Location.print_filename source1 Location.print_filename source2 name
  | Missing_module (_, path1, path2) ->
    fprintf ppf "@[@[<hov>";
    if Path.same path1 path2 then
      fprintf ppf "Internal path@ %s@ is dangling." (Path.name path1)
    else
      fprintf ppf "Internal path@ %s@ expands to@ %s@ which is dangling."
        (Path.name path1) (Path.name path2);
    fprintf ppf "@]@ @[%s@ %s@ %s.@]@]" "The compiled interface for module"
      (Ident.name (Path.head path2))
      "was not found"
  | Illegal_value_name (_loc, name) ->
    fprintf ppf "'%s' is not a valid value identifier." name

let () =
  Location.register_error_of_exn (function
    | Error ((Missing_module (loc, _, _) | Illegal_value_name (loc, _)) as err)
      when loc <> Location.none ->
      Some (Location.error_of_printer loc report_error err)
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
