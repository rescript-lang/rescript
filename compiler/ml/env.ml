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
open Misc
open Asttypes
open Longident
open Path
open Types
open Btype

(* This table is used to usage of value declarations.  A declaration is
   identified with its name and location.  The callback attached to a
   declaration is called whenever the value is used explicitly
   (lookup_value) or implicitly (inclusion test between signatures,
   cf Includemod.value_descriptions). *)

let value_declarations_key =
  Domain.DLS.new_key (fun () ->
      (Hashtbl.create 16 : (string * Location.t, unit -> unit) Hashtbl.t))
let value_declarations () = Domain.DLS.get value_declarations_key

let type_declarations_key = Domain.DLS.new_key (fun () -> Hashtbl.create 16)
let type_declarations () = Domain.DLS.get type_declarations_key
let module_declarations_key = Domain.DLS.new_key (fun () -> Hashtbl.create 16)
let module_declarations () = Domain.DLS.get module_declarations_key

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

let used_constructors_key =
  Domain.DLS.new_key (fun () ->
      (Hashtbl.create 16
        : (string * Location.t * string, constructor_usage -> unit) Hashtbl.t))
let used_constructors () = Domain.DLS.get used_constructors_key

let prefixed_sg_key = Domain.DLS.new_key (fun () -> Hashtbl.create 113)
let prefixed_sg () = Domain.DLS.get prefixed_sg_key

type error =
  | Illegal_renaming of string * string * string
  | Inconsistent_import of string * string * string
  | Missing_module of Location.t * Path.t * Path.t
  | Illegal_value_name of Location.t * string

exception Error of error

let error err = raise (Error err)

module Env_lazy : sig
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

module Path_map = Map.Make (Path)

type summary =
  | Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_extension of summary * Ident.t * extension_constructor
  | Env_module of summary * Ident.t * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_open of summary * Path.t
  | Env_functor_arg of summary * Ident.t
  | Env_constraints of summary * type_declaration Path_map.t
  | Env_copy_types of summary * string list

module Tycomp_tbl = struct
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

module Id_tbl = struct
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
  values: value_description Id_tbl.t;
  constrs: constructor_description Tycomp_tbl.t;
  labels: label_description Tycomp_tbl.t;
  types: (type_declaration * type_descriptions) Id_tbl.t;
  modules:
    (Subst.t * module_declaration, module_declaration) Env_lazy.t Id_tbl.t;
  modtypes: modtype_declaration Id_tbl.t;
  components: module_components Id_tbl.t;
  functor_args: unit Ident.tbl;
  summary: summary;
  local_constraints: type_declaration Path_map.t;
  gadt_instances: (int * Type_set.t ref) list;
  flags: int;
}

and module_components = {
  deprecated: string option;
  loc: Location.t;
  comps:
    ( t * Subst.t * Path.t * Types.module_type,
      module_components_repr option )
    Env_lazy.t;
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
    (Subst.t * module_declaration, module_declaration) Env_lazy.t comp_tbl;
  mutable comp_modtypes: modtype_declaration comp_tbl;
  mutable comp_components: module_components comp_tbl; (* warning -69*)
}

and functor_components = {
  fcomp_param: Ident.t; (* Formal parameter *)
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

(* Ctype installs this once during module initialization. Requests only read it. *)
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
    values = Id_tbl.empty;
    constrs = Tycomp_tbl.empty;
    labels = Tycomp_tbl.empty;
    types = Id_tbl.empty;
    modules = Id_tbl.empty;
    modtypes = Id_tbl.empty;
    components = Id_tbl.empty;
    summary = Env_empty;
    local_constraints = Path_map.empty;
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
  | {cstr_kind = Extension_constructor p} -> is_ident p
  | _ -> false

let diff env1 env2 =
  Id_tbl.diff_keys env1.values env2.values
  @ Tycomp_tbl.diff_keys is_local_ext env1.constrs env2.constrs
  @ Id_tbl.diff_keys env1.modules env2.modules

type can_load_cmis = Can_load_cmis | Cannot_load_cmis of Env_lazy.log

let can_load_cmis_key = Domain.DLS.new_key (fun () -> ref Can_load_cmis)
let can_load_cmis () = Domain.DLS.get can_load_cmis_key

let without_cmis f x =
  let log = Env_lazy.log () in
  let res =
    Misc.(
      protect_refs [R (can_load_cmis (), Cannot_load_cmis log)] (fun () -> f x))
  in
  Env_lazy.backtrack log;
  res

(* Forward declarations installed below during module initialization. They are
   shared read-only once compilation requests can run. *)

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
  match !(can_load_cmis ()) with
  | Can_load_cmis -> Env_lazy.force !components_of_module_maker' c.comps
  | Cannot_load_cmis log ->
    Env_lazy.force_logged log !components_of_module_maker' c.comps

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

let current_unit_key = Domain.DLS.new_key (fun () -> ref "")
let current_unit () = Domain.DLS.get current_unit_key

(* Persistent structure descriptions *)

(* The three lazy expansion stages allocate identifiers at different points in
   a request. Capture their nodes in allocation order so each stage can take
   fresh request-local IDs without walking the large signature again. *)
type allocation_stage = {
  first_type_id: int;
  allocated_type_ids: int;
  type_nodes: type_expr array;
  first_ident_stamp: int;
  allocated_ident_stamps: int;
  identifiers: Ident.t array;
}

type alias_key = {
  target_name: string;
  namespace_name: string;
  alias_name: string;
}

type expanded_snapshot = {
  raw_signature: signature;
  expanded_signature: signature;
  target_components: module_components_repr option;
  alias_components: module_components_repr option;
  target_ids: allocation_stage;
  signature_ids: allocation_stage;
  alias_ids: allocation_stage;
  crcs: (string * Digest.t option) list;
  flags: pers_flags list;
}

type request_snapshot = {
  key: alias_key;
  graph: expanded_snapshot;
  mutable target_relocated: bool;
  mutable signature_relocated: bool;
  mutable alias_relocated: bool;
}

let capture_allocation_stage action =
  let state = Compiler_request_state.current () in
  let first_type_id = state.type_node_id in
  let first_ident_stamp = Ident.current_time () in
  let (result, identifiers), type_nodes =
    Btype.with_allocation_capture (fun () ->
        Ident.with_allocation_capture action)
  in
  let allocated_type_ids = state.type_node_id - first_type_id in
  let allocated_ident_stamps = Ident.current_time () - first_ident_stamp in
  if
    Array.length type_nodes <> allocated_type_ids
    || Array.length identifiers <> allocated_ident_stamps
  then invalid_arg "incomplete dependency allocation capture";
  ( result,
    {
      first_type_id;
      allocated_type_ids;
      type_nodes;
      first_ident_stamp;
      allocated_ident_stamps;
      identifiers;
    } )

(* A cache entry is exclusive to one compiler domain. The graph is visible to
   only one request at a time, and its IDs are reset before it can be reused. *)
let relocate_allocation_stage stage =
  let state = Compiler_request_state.current () in
  let first_type_id = state.type_node_id in
  let first_ident_stamp = Ident.current_time () in
  Array.iteri
    (fun index ty -> ty.id <- first_type_id + index + 1)
    stage.type_nodes;
  Array.iteri
    (fun index id -> id.Ident.stamp <- first_ident_stamp + index + 1)
    stage.identifiers;
  state.type_node_id <- first_type_id + stage.allocated_type_ids;
  Ident.set_current_time (first_ident_stamp + stage.allocated_ident_stamps)

let reset_allocation_stage stage =
  Array.iteri
    (fun index ty -> ty.id <- stage.first_type_id + index + 1)
    stage.type_nodes;
  Array.iteri
    (fun index id -> id.Ident.stamp <- stage.first_ident_stamp + index + 1)
    stage.identifiers

type pers_struct = {
  ps_name: string;
  ps_sig: signature Lazy.t;
  ps_comps: module_components;
  ps_crcs: (string * Digest.t option) list;
  ps_filename: string;
  ps_flags: pers_flags list;
  ps_snapshot: request_snapshot option;
}
[@@warning "-69"]

let persistent_structures_key =
  Domain.DLS.new_key (fun () ->
      (Hashtbl.create 17 : (string, pers_struct option) Hashtbl.t))
let persistent_structures () = Domain.DLS.get persistent_structures_key

(* Consistency between persistent structures *)

let crc_units_key = Domain.DLS.new_key Consistbl.create
let crc_units () = Domain.DLS.get crc_units_key

module String_set = Set.Make (struct
  type t = string
  let compare = String.compare
end)

let imported_units_key = Domain.DLS.new_key (fun () -> ref String_set.empty)
let imported_units () = Domain.DLS.get imported_units_key

let add_import s = imported_units () := String_set.add s !(imported_units ())

let clear_imports () =
  Consistbl.clear (crc_units ());
  imported_units () := String_set.empty

let check_consistency ps =
  Compiler_phase_trace.dependency "dependency.consistency" (fun () ->
      try
        List.iter
          (fun (name, crco) ->
            match crco with
            | None -> ()
            | Some crc ->
              add_import name;
              Consistbl.check (crc_units ()) name crc ps.ps_filename)
          ps.ps_crcs
      with Consistbl.Inconsistency (name, source, auth) ->
        error (Inconsistent_import (name, auth, source)))

(* Reading persistent structures from .cmi files *)

let save_pers_struct crc ps =
  let modname = ps.ps_name in
  Hashtbl.add (persistent_structures ()) modname (Some ps);
  Consistbl.set (crc_units ()) modname crc ps.ps_filename;
  add_import modname

module Persistent_signature = struct
  type t = {filename: string; cmi: Cmi_format.cmi_infos}

  (* Bs_conditional_initial installs the native loader once at startup. *)
  let load =
    ref (fun ~unit_name ->
        match
          find_in_path_uncap (Config.get_load_path ()) (unit_name ^ ".cmi")
        with
        | filename -> Some {filename; cmi = read_cmi filename}
        | exception Not_found -> None)
end

let cached_pers_struct_loader :
    (check:bool -> name:string -> pers_struct option) ref =
  ref (fun ~check:_ ~name:_ -> None)

let cached_cmi_loader :
    (name:string -> Persistent_signature.t option option) ref =
  ref (fun ~name:_ -> None)

let acknowledge_pers_struct check modname {Persistent_signature.filename; cmi} =
  Compiler_phase_trace.dependency "dependency.make_available" (fun () ->
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
        !components_of_module' ~deprecated ~loc:Location.none empty
          Subst.identity
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
          ps_snapshot = None;
        }
      in
      if ps.ps_name <> modname then
        error (Illegal_renaming (modname, ps.ps_name, filename));
      if check then check_consistency ps;
      Hashtbl.add (persistent_structures ()) modname (Some ps);
      ps)

let read_pers_struct check modname filename =
  add_import modname;
  let cmi = read_cmi filename in
  acknowledge_pers_struct check modname {Persistent_signature.filename; cmi}

let find_pers_struct check name =
  if name = "*predef*" then raise Not_found;
  match Hashtbl.find (persistent_structures ()) name with
  | Some ps -> ps
  | None -> raise Not_found
  | exception Not_found -> (
    match !(can_load_cmis ()) with
    | Cannot_load_cmis _ -> raise Not_found
    | Can_load_cmis -> (
      match !cached_pers_struct_loader ~check ~name with
      | Some ps ->
        add_import name;
        if check then check_consistency ps;
        Hashtbl.add (persistent_structures ()) name (Some ps);
        ps
      | None ->
        let ps =
          match
            match !cached_cmi_loader ~name with
            | Some cached -> cached
            | None -> !Persistent_signature.load ~unit_name:name
          with
          | Some ps -> ps
          | None ->
            Hashtbl.add (persistent_structures ()) name None;
            raise Not_found
        in
        add_import name;
        acknowledge_pers_struct check name ps))

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
  if not (Hashtbl.mem (persistent_structures ()) name) then (
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. *)
    add_import name;
    if Warnings.is_active (Warnings.No_cmi_file ("", None)) then
      Delayed_checks.add_delayed_check (fun () -> check_pers_struct name))

let reset_cache () =
  current_unit () := "";
  Hashtbl.clear (persistent_structures ());
  clear_imports ();
  Hashtbl.clear (value_declarations ());
  Hashtbl.clear (type_declarations ());
  Hashtbl.clear (module_declarations ());
  Hashtbl.clear (used_constructors ());
  Hashtbl.clear (prefixed_sg ())

let reset_cache_toplevel () =
  (* Delete 'missing cmi' entries from the cache. *)
  let l =
    Hashtbl.fold
      (fun name r acc -> if r = None then name :: acc else acc)
      (persistent_structures ()) []
  in
  List.iter (Hashtbl.remove (persistent_structures ())) l;
  Hashtbl.clear (value_declarations ());
  Hashtbl.clear (type_declarations ());
  Hashtbl.clear (module_declarations ());
  Hashtbl.clear (used_constructors ());
  Hashtbl.clear (prefixed_sg ())

let set_unit_name name = current_unit () := name

let get_unit_name () = !(current_unit ())

(* Lookup by identifier *)

let rec find_module_descr path env =
  match path with
  | Pident id -> (
    try Id_tbl.find_same id env.components
    with Not_found ->
      if Ident.persistent id && not (Ident.name id = !(current_unit ())) then
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
  | Pident id -> Id_tbl.find_same id (proj1 env)
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
    try (Path_map.find p env.local_constraints, ([], []))
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
      try Tycomp_tbl.find_same id env.constrs with Not_found -> assert false
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
          | {cstr_kind = Extension_constructor _} -> true
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
      let data = Id_tbl.find_same id env.modules in
      Env_lazy.force subst_modtype_maker data
    with Not_found ->
      if Ident.persistent id && not (Ident.name id = !(current_unit ())) then
        let ps = find_pers_struct (Ident.name id) in
        md (Mty_signature (Lazy.force ps.ps_sig))
      else raise Not_found)
  | Pdot (p, s, _pos) -> (
    match get_components (find_module_descr p env) with
    | Structure_comps c ->
      let data, _pos = Tbl.find_str s c.comp_modules in
      Env_lazy.force subst_modtype_maker data
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
    try Hashtbl.find (module_declarations ()) (name, loc) ()
    with Not_found -> ()

let rec lookup_module_descr_aux ?loc lid env =
  match lid with
  | Lident s -> (
    try Id_tbl.find_name s env.components
    with Not_found ->
      if s = !(current_unit ()) then raise Not_found;
      let ps = find_pers_struct s in
      (Pident (Ident.create_persistent s), ps.ps_comps))
  | Ldot (l, s) -> (
    let p, descr = lookup_module_descr ?loc l env in
    match get_components descr with
    | Structure_comps c ->
      let descr, pos = Tbl.find_str s c.comp_components in
      (Pdot (p, s, pos), descr)
    | Functor_comps _ -> raise Not_found)

and lookup_module_descr ?loc lid env =
  let ((p, comps) as res) = lookup_module_descr_aux ?loc lid env in
  mark_module_used env (Path.last p) comps.loc;
  report_deprecated ?loc p comps.deprecated;
  res

and lookup_module ~load ?loc lid env : Path.t =
  match lid with
  | Lident s -> (
    try
      let p, data = Id_tbl.find_name s env.modules in
      let {md_loc; md_attributes; md_type} =
        Env_lazy.force subst_modtype_maker data
      in
      mark_module_used env s md_loc;
      (match md_type with
      | Mty_ident (Path.Pident id) when Ident.name id = "#recmod#" ->
        (* see #5965 *)
        raise Recmodule
      | Mty_alias (_, Path.Pident id) ->
        if
          (not !((Clflags.current ()).transparent_modules))
          && Ident.persistent id
        then find_pers_struct (Ident.name id) |> ignore
      | _ -> ());
      report_deprecated ?loc p
        (Builtin_attributes.deprecated_of_attrs md_attributes);
      p
    with Not_found ->
      if s = !(current_unit ()) then raise Not_found;
      let p = Pident (Ident.create_persistent s) in
      (if !((Clflags.current ()).transparent_modules) && not load then
         check_pers_struct s
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

let lookup proj1 proj2 ?loc lid env =
  match lid with
  | Lident s -> Id_tbl.find_name s (proj1 env)
  | Ldot (l, s) -> (
    let p, desc = lookup_module_descr ?loc l env in
    match get_components desc with
    | Structure_comps c ->
      let data, pos = Tbl.find_str s (proj2 c) in
      (Pdot (p, s, pos), data)
    | Functor_comps _ -> raise Not_found)

let lookup_all_simple proj1 proj2 shadow ?loc lid env =
  match lid with
  | Lident s ->
    let xl = Tycomp_tbl.find_all s (proj1 env) in
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

let has_local_constraints env = not (Path_map.is_empty env.local_constraints)

let cstr_shadow cstr1 cstr2 =
  match (cstr1.cstr_kind, cstr2.cstr_kind) with
  | Extension_constructor _, Extension_constructor _ -> true
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
    List.fold_left (fun env s -> Id_tbl.update s f env) env.values l
  in
  {env with values; summary = Env_copy_types (env.summary, l)}

let mark_value_used env name vd =
  if not (is_implicit_coercion env) then
    try Hashtbl.find (value_declarations ()) (name, vd.val_loc) ()
    with Not_found -> ()

let mark_type_used env name vd =
  if not (is_implicit_coercion env) then
    try Hashtbl.find (type_declarations ()) (name, vd.type_loc) ()
    with Not_found -> ()

let mark_constructor_used usage env name vd constr =
  if not (is_implicit_coercion env) then
    try Hashtbl.find (used_constructors ()) (name, vd.type_loc, constr) usage
    with Not_found -> ()

let mark_extension_used usage env ext name =
  if not (is_implicit_coercion env) then
    let ty_name = Path.last ext.ext_type_path in
    try Hashtbl.find (used_constructors ()) (ty_name, ext.ext_loc, name) usage
    with Not_found -> ()

let set_value_used_callback name vd callback =
  let key = (name, vd.val_loc) in
  try
    let old = Hashtbl.find (value_declarations ()) key in
    Hashtbl.replace (value_declarations ()) key (fun () ->
        old ();
        callback ())
    (* this is to support cases like:
             let x = let x = 1 in x in x
       where the two declarations have the same location
       (e.g. resulting from Camlp4 expansion of grammar entries) *)
  with Not_found -> Hashtbl.add (value_declarations ()) key callback

let set_type_used_callback name td callback =
  let loc = td.type_loc in
  if loc.Location.loc_ghost then ()
  else
    let key = (name, loc) in
    let old =
      try Hashtbl.find (type_declarations ()) key
      with Not_found -> assert false
    in
    Hashtbl.replace (type_declarations ()) key (fun () -> callback old)

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
    match desc.cstr_kind with
    | Extension_constructor _ -> (
      let ty_path = ty_path desc.cstr_res in
      let ty_name = Path.last ty_path in
      try
        Hashtbl.find (used_constructors ()) (ty_name, desc.cstr_loc, name) usage
      with Not_found -> ())
    | _ ->
      let ty_path = ty_path desc.cstr_res in
      let ty_decl =
        try find_type ty_path env with Not_found -> assert false
      in
      let ty_name = Path.last ty_path in
      mark_constructor_used usage env ty_name ty_decl name

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
let iter_env_cont_key = Domain.DLS.new_key (fun () -> ref [])
let iter_env_cont () = Domain.DLS.get iter_env_cont_key

let rec scrape_alias_for_visit env mty =
  match mty with
  | Mty_alias (_, Pident id)
    when Ident.persistent id
         && not (Hashtbl.mem (persistent_structures ()) (Ident.name id)) ->
    false
  | Mty_alias (_, path) -> (
    (* PR#6600: find_module may raise Not_found *)
    try scrape_alias_for_visit env (find_module path env).md_type
    with Not_found -> false)
  | _ -> true

let iter_env proj1 proj2 f env () =
  Id_tbl.iter (fun id x -> f (Pident id) x) (proj1 env);
  let rec iter_components path path' mcomps =
    let cont () =
      let visit =
        match Env_lazy.get_arg mcomps.comps with
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
    iter_env_cont () := (path, cont) :: !(iter_env_cont ())
  in
  Hashtbl.iter
    (fun s pso ->
      match pso with
      | None -> ()
      | Some ps ->
        let id = Pident (Ident.create_persistent s) in
        iter_components id id ps.ps_comps)
    (persistent_structures ());
  Id_tbl.iter
    (fun id (path, comps) -> iter_components (Pident id) path comps)
    env.components

let run_iter_cont l =
  iter_env_cont () := [];
  List.iter (fun c -> c ()) l;
  let cont = List.rev !(iter_env_cont ()) in
  iter_env_cont () := [];
  cont

let iter_types f = iter_env (fun env -> env.types) (fun sc -> sc.comp_types) f

let same_types env1 env2 =
  env1.types == env2.types && env1.components == env2.components

let used_persistent () =
  let r = ref Concr.empty in
  Hashtbl.iter
    (fun s pso -> if pso != None then r := Concr.add s !r)
    (persistent_structures ());
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
  | Pident id -> Id_tbl.find_all (Ident.name id) env.components
  | Pdot (p, s, _) ->
    let l = find_shadowed_comps p env in
    let l' =
      List.map (find_all_comps (fun comps -> comps.comp_components) s) l
    in
    List.flatten l'
  | Papply _ -> []

let find_shadowed proj1 proj2 path env =
  match path with
  | Pident id -> Id_tbl.find_all (Ident.name id) (proj1 env)
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
  {env with gadt_instances = (lv, ref Type_set.empty) :: env.gadt_instances}

let is_Tlink = function
  | {desc = Tlink _} -> true
  | _ -> false

let gadt_instance_level env t =
  let rec find_instance = function
    | [] -> None
    | (lv, r) :: rem ->
      if Type_set.exists is_Tlink !r then
        (* Should we use set_typeset ? *)
        r := Type_set.fold (fun ty -> Type_set.add (repr ty)) !r Type_set.empty;
      if Type_set.mem t !r then Some lv else find_instance rem
  in
  find_instance env.gadt_instances

let add_gadt_instances env lv tl =
  let r =
    try List.assoc lv env.gadt_instances with Not_found -> assert false
  in
  (* Format.eprintf "Added";
     List.iter (fun ty -> Format.eprintf "@ %a" !Btype.print_raw ty) tl;
     Format.eprintf "@."; *)
  set_typeset r (List.fold_right Type_set.add tl !r)

(* Only use this after expand_head! *)
let add_gadt_instance_chain env lv t =
  let r =
    try List.assoc lv env.gadt_instances with Not_found -> assert false
  in
  let rec add_instance t =
    let t = repr t in
    if not (Type_set.mem t !r) then (
      (* Format.eprintf "@ %a" !Btype.print_raw t; *)
      set_typeset r (Type_set.add t !r);
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

let prefix_idents root sub sg =
  if sub = Subst.identity then (
    let sgs =
      try Hashtbl.find (prefixed_sg ()) root
      with Not_found ->
        let sgs = ref [] in
        Hashtbl.add (prefixed_sg ()) root sgs;
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

module Physical_type_table = Hashtbl.Make (struct
  type t = type_expr

  let equal first second = first == second
  let hash ty = ty.id
end)

module Physical_ident_table = Hashtbl.Make (struct
  type t = Ident.t

  let equal first second = first == second
  let hash id = Hashtbl.hash (id.Ident.stamp, id.Ident.name)
end)

module Physical_label_table = Hashtbl.Make (struct
  type t = label_description

  let equal first second = first == second
  let hash label = Hashtbl.hash (label.lbl_name, label.lbl_res.id)
end)

type type_snapshot = {
  nodes: (type_expr * type_desc * int * int) array;
  identifiers: (Ident.t * int * int) array;
  abbrevs: (abbrev_memo ref * abbrev_memo) array;
  mutabilities: (field_mutability ref * field_mutability) array;
  row_fields: (row_field option ref * row_field option) array;
  label_links: (label_description * label_description array) array;
  label_arrays: (label_description array * label_description array) array;
  layouts: (Variant_runtime.layout_ref * Variant_runtime.layout) array;
  component_checks: (unit -> bool) array;
  unsupported: bool;
}

(* Track the mutable fields reachable from the cached signature and component
   tables. Unsupported memo shapes make the entry ineligible for direct reuse. *)
let snapshot_type_graph graph =
  let seen = Physical_type_table.create 32768 in
  let seen_identifiers = Physical_ident_table.create 8192 in
  let seen_label_arrays = Physical_label_table.create 1024 in
  let abbrevs = ref [] in
  let mutabilities = ref [] in
  let row_fields = ref [] in
  let label_links = ref [] in
  let label_arrays = ref [] in
  let layouts = ref [] in
  let component_checks = ref [] in
  let unsupported = ref false in
  let visit_ident id = Physical_ident_table.replace seen_identifiers id () in
  let rec visit_path = function
    | Pident id -> visit_ident id
    | Pdot (path, _, _) -> visit_path path
    | Papply (first, second) ->
      visit_path first;
      visit_path second
  in
  let visit_abbrev = function
    | Mnil -> ()
    | Mcons _ | Mlink _ -> unsupported := true
  in
  let rec visit_mutability depth reference =
    if depth > 128 then unsupported := true
    else (
      mutabilities := (reference, !reference) :: !mutabilities;
      match !reference with
      | Mutability_value _ -> ()
      | Mutability_link next -> visit_mutability (depth + 1) next)
  in
  let rec visit_row_field depth field =
    if depth > 128 then unsupported := true
    else
      match field with
      | Reither (_, _, _, reference) ->
        row_fields := (reference, !reference) :: !row_fields;
        Option.iter (visit_row_field (depth + 1)) !reference
      | Rpresent _ | Rabsent -> ()
  in
  let visit_layout reference =
    try layouts := (reference, Variant_runtime.get_layout reference) :: !layouts
    with Failure _ -> unsupported := true
  in
  let visit_record_representation = function
    | Record_inlined {representation} -> visit_layout representation.variant
    | Record_regular | Record_float_unused | Record_unboxed _ | Record_extension
      ->
      ()
  in
  let rec visit ty =
    if not (Physical_type_table.mem seen ty) then (
      Physical_type_table.add seen ty ();
      (match ty.desc with
      | Tconstr (path, _, reference) ->
        visit_path path;
        abbrevs := (reference, !reference) :: !abbrevs;
        visit_abbrev !reference
      | Tfield {mutability} -> visit_mutability 0 mutability
      | Tvariant row ->
        List.iter (fun (_, field) -> visit_row_field 0 field) row.row_fields;
        Option.iter (fun (path, _) -> visit_path path) row.row_name
      | Tpackage (path, _, _) -> visit_path path
      | Tvar _ | Tarrow _ | Ttuple _ | Tobject _ | Tnil | Tlink _ | Tsubst _
      | Tunivar _ | Tpoly _ ->
        ());
      Btype.iter_type_expr visit ty)
  in
  let original = Btype.type_iterators in
  let iterator =
    {
      original with
      it_type_expr = (fun _ ty -> visit ty);
      it_type_declaration =
        (fun iterator declaration ->
          (match declaration.type_kind with
          | Type_variant (_, reference) -> visit_layout reference
          | Type_abstract | Type_record _ | Type_open -> ());
          original.it_type_declaration iterator declaration);
    }
  in
  let visit_label_declaration declaration = visit_ident declaration.ld_id in
  let visit_constructor_declaration declaration =
    visit_ident declaration.cd_id;
    match declaration.cd_args with
    | Cstr_tuple _ -> ()
    | Cstr_record labels -> List.iter visit_label_declaration labels
  in
  let visit_type_declaration declaration =
    (match declaration.type_kind with
    | Type_variant (constructors, _) ->
      List.iter visit_constructor_declaration constructors
    | Type_record (labels, representation) ->
      List.iter visit_label_declaration labels;
      visit_record_representation representation
    | Type_abstract | Type_open -> ());
    List.iter
      (function
        | Record {labels} -> List.iter visit_label_declaration labels)
      declaration.type_inlined_types
  in
  let rec visit_module_type = function
    | Mty_ident path | Mty_alias (_, path) -> visit_path path
    | Mty_signature signature -> List.iter visit_signature_item signature
    | Mty_functor (id, argument, result) ->
      visit_ident id;
      Option.iter visit_module_type argument;
      visit_module_type result
  and visit_signature_item = function
    | Sig_value (id, _) -> visit_ident id
    | Sig_type (id, declaration, _) ->
      visit_ident id;
      visit_type_declaration declaration
    | Sig_typext (id, extension, _) -> (
      visit_ident id;
      visit_path extension.ext_type_path;
      match extension.ext_args with
      | Cstr_tuple _ -> ()
      | Cstr_record labels -> List.iter visit_label_declaration labels)
    | Sig_module (id, declaration, _) ->
      visit_ident id;
      visit_module_type declaration.md_type
    | Sig_modtype (id, declaration) ->
      visit_ident id;
      Option.iter visit_module_type declaration.mtd_type
  in
  iterator.it_signature iterator graph.raw_signature;
  iterator.it_signature iterator graph.expanded_signature;
  List.iter visit_signature_item graph.raw_signature;
  List.iter visit_signature_item graph.expanded_signature;
  Array.iter visit graph.target_ids.type_nodes;
  Array.iter visit graph.signature_ids.type_nodes;
  Array.iter visit graph.alias_ids.type_nodes;
  let capture_components = function
    | Some (Structure_comps components) ->
      let values = components.comp_values in
      let constrs = components.comp_constrs in
      let labels_table = components.comp_labels in
      let types = components.comp_types in
      let modules = components.comp_modules in
      let modtypes = components.comp_modtypes in
      let nested = components.comp_components in
      component_checks :=
        (fun () ->
          components.comp_values == values
          && components.comp_constrs == constrs
          && components.comp_labels == labels_table
          && components.comp_types == types
          && components.comp_modules == modules
          && components.comp_modtypes == modtypes
          && components.comp_components == nested)
        :: !component_checks;
      Tbl.iter (fun _ (description, _) -> visit description.val_type) values;
      Tbl.iter
        (fun _ descriptions ->
          List.iter
            (fun label ->
              visit label.lbl_res;
              visit label.lbl_arg;
              let all = label.lbl_all in
              visit_record_representation label.lbl_repres;
              if Array.length all = 0 then
                label_links := (label, all) :: !label_links
              else
                let first = all.(0) in
                if not (Physical_label_table.mem seen_label_arrays first) then (
                  Physical_label_table.add seen_label_arrays first ();
                  label_arrays := (all, Array.copy all) :: !label_arrays;
                  Array.iter
                    (fun member ->
                      label_links := (member, member.lbl_all) :: !label_links)
                    all))
            descriptions)
        labels_table;
      Tbl.iter
        (fun _ ((declaration, (constructors, labels)), _) ->
          visit_type_declaration declaration;
          iterator.it_type_declaration iterator declaration;
          List.iter (fun description -> visit description.cstr_res) constructors;
          List.iter
            (fun description ->
              visit description.lbl_res;
              visit description.lbl_arg)
            labels;
          match declaration.type_kind with
          | Type_variant (_, reference) -> visit_layout reference
          | Type_abstract | Type_record _ | Type_open -> ())
        types;
      Tbl.iter
        (fun _ descriptions ->
          List.iter
            (fun description ->
              visit description.cstr_res;
              List.iter visit description.cstr_existentials;
              List.iter visit description.cstr_args;
              Option.iter
                (fun declaration ->
                  iterator.it_type_declaration iterator declaration)
                description.cstr_inlined;
              match description.cstr_kind with
              | Ordinary_constructor reference -> visit_layout reference.variant
              | Extension_constructor path -> visit_path path)
            descriptions)
        constrs;
      Tbl.iter
        (fun _ (declaration, _) ->
          Option.iter visit_module_type declaration.mtd_type;
          Option.iter
            (fun module_type -> iterator.it_module_type iterator module_type)
            declaration.mtd_type)
        modtypes
    | Some (Functor_comps _) | None -> unsupported := true
  in
  capture_components graph.target_components;
  capture_components graph.alias_components;
  Array.iter visit_ident graph.target_ids.identifiers;
  Array.iter visit_ident graph.signature_ids.identifiers;
  Array.iter visit_ident graph.alias_ids.identifiers;
  let identifiers =
    Physical_ident_table.to_seq_keys seen_identifiers
    |> Seq.map (fun id -> (id, id.Ident.stamp, id.Ident.flags))
    |> Array.of_seq
  in
  {
    nodes =
      Physical_type_table.to_seq_keys seen
      |> Seq.map (fun ty -> (ty, ty.desc, ty.level, ty.id))
      |> Array.of_seq;
    identifiers;
    abbrevs = Array.of_list !abbrevs;
    mutabilities = Array.of_list !mutabilities;
    row_fields = Array.of_list !row_fields;
    label_links = Array.of_list !label_links;
    label_arrays = Array.of_list !label_arrays;
    layouts = Array.of_list !layouts;
    component_checks = Array.of_list !component_checks;
    unsupported = !unsupported;
  }

let type_graph_unchanged snapshot =
  (not snapshot.unsupported)
  && Array.for_all
       (fun (ty, desc, level, id) ->
         ty.desc == desc && ty.level = level && ty.id = id)
       snapshot.nodes
  && Array.for_all
       (fun (id, stamp, flags) ->
         id.Ident.stamp = stamp && id.Ident.flags = flags)
       snapshot.identifiers
  && Array.for_all
       (fun (reference, value) -> !reference == value)
       snapshot.abbrevs
  && Array.for_all
       (fun (reference, value) -> !reference == value)
       snapshot.mutabilities
  && Array.for_all
       (fun (reference, value) -> !reference == value)
       snapshot.row_fields
  && Array.for_all
       (fun (label, array) -> label.lbl_all == array)
       snapshot.label_links
  && Array.for_all
       (fun (array, contents) ->
         Array.length array = Array.length contents
         && Array.for_all2 ( == ) array contents)
       snapshot.label_arrays
  && Array.for_all
       (fun (reference, layout) ->
         Variant_runtime.get_layout reference == layout)
       snapshot.layouts
  && Array.for_all (fun check -> check ()) snapshot.component_checks

type expanded_snapshot_cache_entry = {
  key: alias_key;
  target_filename: string;
  namespace_filename: string;
  resolved_load_path: string list;
  target_stats: Unix.stats;
  namespace_stats: Unix.stats;
  bytes: string;
  mutable graph: expanded_snapshot option;
  mutable typed_integrity: type_snapshot option;
  mutable in_use: bool;
}

let expanded_snapshot_cache_key = Domain.DLS.new_key (fun () -> ref None)
let expanded_snapshot_cache () = Domain.DLS.get expanded_snapshot_cache_key

(* Only the marshaled image crosses domain boundaries. Each compiler domain
   decodes its own graph, so type inference never mutates another worker's
   imported types. The lock also lets one worker prepare a shared image while
   other workers wait to decode it. *)
type shared_expanded_snapshot = {
  key: alias_key;
  target_filename: string;
  namespace_filename: string;
  resolved_load_path: string list;
  target_stats: Unix.stats;
  namespace_stats: Unix.stats;
  bytes: string;
}

let shared_expanded_snapshot = ref None
let shared_expanded_snapshot_lock = Mutex.create ()

(* Preparing a large graph costs more than one ordinary alias expansion. Wait
   for a second compiler request across the process so one-off edits stay cheap.
   The expanded graphs themselves remain exclusive to their compiler domains. *)
let expanded_snapshot_candidates = Hashtbl.create 8
let expanded_snapshot_candidates_lock = Mutex.create ()

let candidate_seen_in_previous_request key filename request =
  Mutex.lock expanded_snapshot_candidates_lock;
  Fun.protect
    (fun () ->
      let candidate = (key, filename) in
      let seen =
        match Hashtbl.find_opt expanded_snapshot_candidates candidate with
        | Some previous -> previous != request
        | None -> false
      in
      Hashtbl.replace expanded_snapshot_candidates candidate request;
      seen)
    ~finally:(fun () -> Mutex.unlock expanded_snapshot_candidates_lock)

let forget_snapshot_candidate key filename =
  Mutex.lock expanded_snapshot_candidates_lock;
  Fun.protect
    (fun () -> Hashtbl.remove expanded_snapshot_candidates (key, filename))
    ~finally:(fun () -> Mutex.unlock expanded_snapshot_candidates_lock)

let expanded_snapshot_enabled_key = Domain.DLS.new_key (fun () -> false)

let with_expanded_snapshot_cache action =
  let previous = Domain.DLS.get expanded_snapshot_enabled_key in
  Domain.DLS.set expanded_snapshot_enabled_key true;
  Fun.protect action ~finally:(fun () ->
      Domain.DLS.set expanded_snapshot_enabled_key previous)

let preparing_expanded_snapshot = Domain.DLS.new_key (fun () -> false)
let prepare_expanded_snapshot : (alias_key -> unit) ref = ref (fun _ -> ())

let expanded_snapshot_enabled () =
  match Sys.getenv_opt "REWATCH_COMBINED_SIGNATURE_CACHE" with
  | Some "0" -> false
  | Some ("force" | "force_typed" | "typed" | "audit") -> true
  | _ -> Domain.DLS.get expanded_snapshot_enabled_key

let typed_expanded_snapshot_reuse () =
  not (Sys.getenv_opt "REWATCH_COMBINED_SIGNATURE_CACHE" = Some "force")

let audit_expanded_snapshot_reuse () =
  Sys.getenv_opt "REWATCH_COMBINED_SIGNATURE_CACHE" = Some "audit"

let force_fresh_expanded_snapshot () =
  Sys.getenv_opt "REWATCH_COMBINED_SIGNATURE_CACHE" = Some "force"

let same_file_stats first second =
  first.Unix.st_dev = second.Unix.st_dev
  && first.Unix.st_ino = second.Unix.st_ino
  && first.Unix.st_size = second.Unix.st_size
  && first.Unix.st_mtime = second.Unix.st_mtime
  && first.Unix.st_ctime = second.Unix.st_ctime

type cmi_cache_entry = {
  resolved_filename: string;
  stats: Unix.stats;
  bytes: bytes;
  mutable cmi: Cmi_format.cmi_infos;
  mutable used: bool;
}

let cmi_cache_key = Domain.DLS.new_key (fun () -> Hashtbl.create 2)
let cmi_cache () = Domain.DLS.get cmi_cache_key

(* These two runtime interfaces are loaded by nearly every compile request.
   Their decoded graphs stay private to one compiler domain. A request may
   mutate them, so [finalize_cmi_cache] restores the saved image if needed.
   Resolve the path on every hit to notice newly shadowing or replaced CMIs. *)
let load_cached_cmi ~name =
  if
    (not (expanded_snapshot_enabled ()))
    || Domain.DLS.get preparing_expanded_snapshot
    || (name <> "Stdlib" && name <> "Pervasives")
  then None
  else
    let cache = cmi_cache () in
    let load_fresh () =
      let loaded = !Persistent_signature.load ~unit_name:name in
      (match loaded with
      | None -> ()
      | Some {filename; cmi} -> (
        let resolved_filename = Compiler_request_state.resolve_path filename in
        try
          let stats = Unix.stat resolved_filename in
          let bytes = Marshal.to_bytes cmi [] in
          if same_file_stats (Unix.stat resolved_filename) stats then
            Hashtbl.replace cache name
              {resolved_filename; stats; bytes; cmi; used = true}
        with Sys_error _ | Unix.Unix_error _ | Invalid_argument _ -> ()));
      Some loaded
    in
    match Hashtbl.find_opt cache name with
    | Some entry -> (
      try
        let filename =
          Compiler_phase_trace.dependency "dependency.cmi_cache_validate"
            (fun () ->
              find_in_path_uncap (Config.get_load_path ()) (name ^ ".cmi"))
        in
        if
          Compiler_request_state.resolve_path filename = entry.resolved_filename
          && same_file_stats (Unix.stat entry.resolved_filename) entry.stats
        then (
          entry.used <- true;
          Some (Some Persistent_signature.{filename; cmi = entry.cmi}))
        else (
          Hashtbl.remove cache name;
          load_fresh ())
      with Not_found | Sys_error _ | Unix.Unix_error _ ->
        Hashtbl.remove cache name;
        load_fresh ())
    | None -> load_fresh ()

let finalize_cmi_cache () =
  Hashtbl.iter
    (fun _ entry ->
      if entry.used then (
        entry.used <- false;
        let pristine =
          Compiler_phase_trace.dependency "dependency.cmi_cache_verify"
            (fun () ->
              try Bytes.equal (Marshal.to_bytes entry.cmi []) entry.bytes
              with Invalid_argument _ -> false)
        in
        if not pristine then entry.cmi <- Marshal.from_bytes entry.bytes 0))
    (cmi_cache ())

let () = cached_cmi_loader := load_cached_cmi

let is_target_path name = function
  | Pident id -> Ident.persistent id && Ident.name id = name
  | Pdot _ | Papply _ -> false

let alias_key_of_module path mty =
  match (path, mty) with
  | Pdot (Pident root, alias_name, _), Mty_alias (_, Pident target)
    when Ident.persistent root && Ident.persistent target ->
    Some
      {
        target_name = Ident.name target;
        namespace_name = Ident.name root;
        alias_name;
      }
  | _ -> None

let is_alias_path key path mty = alias_key_of_module path mty = Some key

let rec components_of_module ~deprecated ~loc env sub path mty =
  {deprecated; loc; comps = Env_lazy.create (env, sub, path, mty)}

and components_of_module_maker (env, sub, path, mty) =
  Compiler_phase_trace.dependency_lazy
    (fun () ->
      let origin =
        match mty with
        | Mty_alias (_, target) -> ":alias=" ^ Path.name target
        | Mty_ident target -> ":ident=" ^ Path.name target
        | Mty_signature _ -> ":signature"
        | Mty_functor _ -> ":functor"
      in
      "dependency.expand_components:" ^ Path.name path ^ origin)
    (fun () ->
      if not (expanded_snapshot_enabled ()) then
        components_of_module_maker_uncached (env, sub, path, mty)
      else
        let alias_key = alias_key_of_module path mty in
        let target_name =
          match path with
          | Pident id when Ident.persistent id -> Some (Ident.name id)
          | _ -> Option.map (fun key -> key.target_name) alias_key
        in
        let cached =
          match target_name with
          | Some target_name -> (
            try (find_pers_struct target_name).ps_snapshot
            with Not_found -> None)
          | None -> None
        in
        match cached with
        | Some snapshot when is_target_path snapshot.key.target_name path ->
          if not snapshot.target_relocated then (
            relocate_allocation_stage snapshot.graph.target_ids;
            snapshot.target_relocated <- true);
          snapshot.graph.target_components
        | Some snapshot when is_alias_path snapshot.key path mty ->
          ignore (Lazy.force (find_pers_struct snapshot.key.target_name).ps_sig);
          if not snapshot.alias_relocated then (
            relocate_allocation_stage snapshot.graph.alias_ids;
            snapshot.alias_relocated <- true);
          snapshot.graph.alias_components
        | _ ->
          let result =
            components_of_module_maker_uncached (env, sub, path, mty)
          in
          (match alias_key with
          | Some key when not (Domain.DLS.get preparing_expanded_snapshot) -> (
            try !prepare_expanded_snapshot key
            with
            | Not_found | Sys_error _ | Unix.Unix_error _ | Cmi_format.Error _
            | Error _ | Invalid_argument _
            ->
              ())
          | _ -> ());
          result)

and components_of_module_maker_uncached (env, sub, path, mty) =
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
    let labels_by_name = Hashtbl.create 127 in
    let label_names_rev = ref [] in
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
              let name = descr.lbl_name in
              match Hashtbl.find labels_by_name name with
              | _, previous ->
                Hashtbl.replace labels_by_name name (name, descr :: previous)
              | exception Not_found ->
                Hashtbl.add labels_by_name name (name, [descr]);
                label_names_rev := name :: !label_names_rev)
            labels;
          env := store_type_infos id decl !env
        | Sig_typext (id, ext, _) ->
          let ext' = Subst.extension_constructor sub ext in
          let descr = Datarepr.extension_descr path ext' in
          c.comp_constrs <- add_to_tbl (Ident.name id) descr c.comp_constrs;
          incr pos
        | Sig_module (id, md, _) ->
          let md' = Env_lazy.create (sub, md) in
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
          env := store_modtype id decl !env)
      sg pl;
    (* Large signatures often repeat label names. Keep first appearance order
       to preserve Tbl's shape and the latest key and declarations to preserve
       its contents. *)
    c.comp_labels <-
      List.fold_left
        (fun table name ->
          let latest_name, declarations = Hashtbl.find labels_by_name name in
          Tbl.add latest_name declarations table)
        Tbl.empty
        (List.rev !label_names_rev);
    Some (Structure_comps c)
  | Mty_functor (param, _ty_arg, ty_res) ->
    Some
      (Functor_comps
         {
           fcomp_param = param;
           (* fcomp_res must be prefixed eagerly, because it is interpreted
              in the outer environment *)
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
  may (fun f -> check_usage decl.val_loc id f (value_declarations ())) check;
  {
    env with
    values = Id_tbl.add id decl env.values;
    summary = Env_value (env.summary, id, decl);
  }

and store_type ~check id info env =
  let loc = info.type_loc in
  if check then
    check_usage loc id
      (fun s -> Warnings.Unused_type_declaration s)
      (type_declarations ());
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
         if not (Hashtbl.mem (used_constructors ()) k) then (
           let used = constructor_usages () in
           Hashtbl.add (used_constructors ()) k (add_constructor_usage used);
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
        (fun (id, descr) constrs -> Tycomp_tbl.add id descr constrs)
        constructors env.constrs;
    labels =
      List.fold_right
        (fun (id, descr) labels -> Tycomp_tbl.add id descr labels)
        labels env.labels;
    types = Id_tbl.add id (info, descrs) env.types;
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
    types = Id_tbl.add id (info, ([], [])) env.types;
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
     if not (Hashtbl.mem (used_constructors ()) k) then (
       let used = constructor_usages () in
       Hashtbl.add (used_constructors ()) k (add_constructor_usage used);
       Delayed_checks.add_delayed_check (fun () ->
           if (not (is_in_signature env)) && not used.cu_positive then
             Location.prerr_warning loc
               (Warnings.Unused_extension
                  (n, ext.ext_is_exception, used.cu_pattern, used.cu_privatize)))));
  {
    env with
    constrs =
      Tycomp_tbl.add id (Datarepr.extension_descr (Pident id) ext) env.constrs;
    summary = Env_extension (env.summary, id, ext);
  }

and store_module ~check id md env =
  let loc = md.md_loc in
  if check then
    check_usage loc id
      (fun s -> Warnings.Unused_module s)
      (module_declarations ());

  let deprecated = Builtin_attributes.deprecated_of_attrs md.md_attributes in
  {
    env with
    modules = Id_tbl.add id (Env_lazy.create (Subst.identity, md)) env.modules;
    components =
      Id_tbl.add id
        (components_of_module ~deprecated ~loc:md.md_loc env Subst.identity
           (Pident id) md.md_type)
        env.components;
    summary = Env_module (env.summary, id, md);
  }

and store_modtype id info env =
  {
    env with
    modtypes = Id_tbl.add id info env.modtypes;
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
      components_of_module ~deprecated:None ~loc:Location.none (*???*) env
        Subst.identity p mty
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
  {env with local_constraints = Path_map.add path info env.local_constraints}

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

let rec add_signature sg env =
  match sg with
  | [] -> env
  | comp :: rem -> add_signature rem (add_item comp env)

(* Open a signature path *)

let add_components slot root env0 comps =
  let add_l w comps env0 = Tycomp_tbl.add_open slot w comps env0 in

  let add w comps env0 = Id_tbl.add_open slot w root comps env0 in

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

let imports () =
  let dont_record_crc_unit = !((Clflags.current ()).dont_record_crc_unit) in
  match dont_record_crc_unit with
  | None ->
    Consistbl.extract (String_set.elements !(imported_units ())) (crc_units ())
  | Some x ->
    Consistbl.extract
      (String_set.fold
         (fun m acc -> if m = x then acc else m :: acc)
         !(imported_units ())
         [])
      (crc_units ())

(* Save a signature to a file *)

let save_signature_with_imports ?check_exists ~deprecated sg modname filename
    imports =
  Compiler_phase_trace.section "artifact.cmi_prep" (fun () ->
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
          {
            cmi_name = modname;
            cmi_sign = sg;
            cmi_crcs = imports;
            cmi_flags = flags;
          }
        in
        let crc = create_cmi ?check_exists filename cmi in
        (* Enter signature in persistent table so that imported_unit()
       will also return its crc *)
        let comps =
          Compiler_phase_trace.section "artifact.cmi_register" (fun () ->
              components_of_module ~deprecated ~loc:Location.none empty
                Subst.identity
                (Pident (Ident.create_persistent modname))
                (Mty_signature sg))
        in
        let ps =
          {
            ps_name = modname;
            ps_sig = lazy (Subst.signature Subst.identity sg);
            ps_comps = comps;
            ps_crcs = (cmi.cmi_name, Some crc) :: imports;
            ps_filename = filename;
            ps_flags = cmi.cmi_flags;
            ps_snapshot = None;
          }
        in
        save_pers_struct crc ps;
        cmi
      with exn ->
        remove_file filename;
        raise exn)

let save_signature ?check_exists ~deprecated sg modname filename =
  save_signature_with_imports ?check_exists ~deprecated sg modname filename
    (imports ())

(* Folding on environments *)

let find_all proj1 proj2 f lid env acc =
  match lid with
  | None ->
    Id_tbl.fold_name
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
  | None -> Tycomp_tbl.fold_name (fun data acc -> f data acc) (proj1 env) acc
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
      Id_tbl.fold_name
        (fun name (p, data) acc ->
          let data = Env_lazy.force subst_modtype_maker data in
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
      (persistent_structures ()) acc
  | Some l -> (
    let p, desc = lookup_module_descr l env in
    match get_components desc with
    | Structure_comps c ->
      Tbl.fold
        (fun s (data, pos) acc ->
          f s (Pdot (p, s, pos)) (Env_lazy.force subst_modtype_maker data) acc)
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
(* The initial environment contains mutable type graphs. Each compiler domain
   needs its own copy because type instantiation marks source graph nodes. *)
let create_initial_safe_string () =
  Predef.build_initial_env (add_type ~check:false)
    (add_extension ~check:false)
    empty

let initial_safe_string_key = Domain.DLS.new_key create_initial_safe_string

let initial_safe_string () = Domain.DLS.get initial_safe_string_key

let reset_initial_for_request () =
  Domain.DLS.set initial_safe_string_key (create_initial_safe_string ())

let with_fresh_initial action =
  let previous = initial_safe_string () in
  Fun.protect action ~finally:(fun () ->
      Domain.DLS.set initial_safe_string_key previous)

(* Return the environment summary *)

let summary env =
  if Path_map.is_empty env.local_constraints then env.summary
  else Env_constraints (env.summary, env.local_constraints)

let last_env_key = Domain.DLS.new_key (fun () -> ref empty)
let last_env () = Domain.DLS.get last_env_key
let last_reduced_env_key = Domain.DLS.new_key (fun () -> ref empty)
let last_reduced_env () = Domain.DLS.get last_reduced_env_key

let with_fresh_key key create action =
  let previous = Domain.DLS.get key in
  Domain.DLS.set key (create ());
  Fun.protect action ~finally:(fun () -> Domain.DLS.set key previous)

(* The persistent CMI cache, import consistency table, declaration usage
   callbacks, and summary memo all belong to one compilation request. *)
let with_fresh action =
  with_fresh_key value_declarations_key
    (fun () -> Hashtbl.create 16)
    (fun () ->
      with_fresh_key type_declarations_key
        (fun () -> Hashtbl.create 16)
        (fun () ->
          with_fresh_key module_declarations_key
            (fun () -> Hashtbl.create 16)
            (fun () ->
              with_fresh_key used_constructors_key
                (fun () -> Hashtbl.create 16)
                (fun () ->
                  with_fresh_key prefixed_sg_key
                    (fun () -> Hashtbl.create 113)
                    (fun () ->
                      with_fresh_key can_load_cmis_key
                        (fun () -> ref Can_load_cmis)
                        (fun () ->
                          with_fresh_key current_unit_key
                            (fun () -> ref "")
                            (fun () ->
                              with_fresh_key persistent_structures_key
                                (fun () -> Hashtbl.create 17)
                                (fun () ->
                                  with_fresh_key crc_units_key Consistbl.create
                                    (fun () ->
                                      with_fresh_key imported_units_key
                                        (fun () -> ref String_set.empty)
                                        (fun () ->
                                          with_fresh_key iter_env_cont_key
                                            (fun () -> ref [])
                                            (fun () ->
                                              with_fresh_key last_env_key
                                                (fun () -> ref empty)
                                                (fun () ->
                                                  with_fresh_key
                                                    last_reduced_env_key
                                                    (fun () -> ref empty)
                                                    action))))))))))))

let snapshot_graph_from_cmis key =
  let namespace = find_pers_struct key.namespace_name in
  let dependency = find_pers_struct key.target_name in
  let raw_signature =
    match Env_lazy.get_arg dependency.ps_comps.comps with
    | Some (_, _, _, Mty_signature signature) -> signature
    | _ -> raise Not_found
  in
  let alias_component =
    match get_components namespace.ps_comps with
    | Structure_comps components ->
      fst (Tbl.find_str key.alias_name components.comp_components)
    | Functor_comps _ -> raise Not_found
  in
  let env, sub, path, mty =
    match Env_lazy.get_arg alias_component.comps with
    | Some context -> context
    | None -> raise Not_found
  in
  if not (is_alias_path key path mty) then raise Not_found;
  let target_components, target_ids =
    capture_allocation_stage (fun () -> get_components_opt dependency.ps_comps)
  in
  let expanded_signature, signature_ids =
    capture_allocation_stage (fun () -> Lazy.force dependency.ps_sig)
  in
  let alias_components, alias_ids =
    capture_allocation_stage (fun () ->
        components_of_module_maker_uncached (env, sub, path, mty))
  in
  (match alias_components with
  | Some (Structure_comps components) ->
    if
      Tbl.fold (fun _ _ _ -> true) components.comp_modules false
      || Tbl.fold (fun _ _ _ -> true) components.comp_components false
    then raise Not_found
  | Some (Functor_comps _) | None -> raise Not_found);
  {
    raw_signature;
    expanded_signature;
    target_components;
    alias_components;
    target_ids;
    signature_ids;
    alias_ids;
    crcs = dependency.ps_crcs;
    flags = dependency.ps_flags;
  }

let prepare_expanded_snapshot_now key =
  let cache = expanded_snapshot_cache () in
  if !cache = None then
    let namespace = find_pers_struct key.namespace_name in
    let dependency = find_pers_struct key.target_name in
    let namespace_filename =
      Compiler_request_state.resolve_path namespace.ps_filename
    in
    let target_filename =
      Compiler_request_state.resolve_path dependency.ps_filename
    in
    let resolved_load_path =
      List.map Compiler_request_state.resolve_path (Config.get_load_path ())
    in
    let namespace_stats = Unix.stat namespace_filename in
    let target_stats = Unix.stat target_filename in
    let forced =
      match Sys.getenv_opt "REWATCH_COMBINED_SIGNATURE_CACHE" with
      | Some ("force" | "force_typed") -> true
      | _ -> false
    in
    if target_stats.Unix.st_size >= 256 * 1024 || forced then
      let request = Compiler_request_state.current () in
      let seen_in_previous_request =
        candidate_seen_in_previous_request key target_filename request
      in
      let prepared =
        Mutex.lock shared_expanded_snapshot_lock;
        Fun.protect
          (fun () ->
            match !shared_expanded_snapshot with
            | Some shared
              when shared.key = key
                   && shared.target_filename = target_filename
                   && shared.namespace_filename = namespace_filename
                   && shared.resolved_load_path = resolved_load_path
                   && same_file_stats shared.target_stats target_stats
                   && same_file_stats shared.namespace_stats namespace_stats ->
              Some (shared.bytes, None)
            | _ when forced || seen_in_previous_request ->
              let cwd = Compiler_request_state.cwd () in
              let load_path = Config.get_load_path () in
              let previous = Domain.DLS.get preparing_expanded_snapshot in
              Domain.DLS.set preparing_expanded_snapshot true;
              let graph =
                Fun.protect
                  (fun () ->
                    Ident.with_fresh (fun () ->
                        with_fresh (fun () ->
                            Btype.with_fresh (fun () ->
                                Compiler_request_state.with_fresh ~cwd
                                  (fun () ->
                                    Config.set_load_path load_path;
                                    snapshot_graph_from_cmis key)))))
                  ~finally:(fun () ->
                    Domain.DLS.set preparing_expanded_snapshot previous)
              in
              let bytes = Marshal.to_string graph [] in
              if
                String.length bytes <= 8 * 1024 * 1024
                && same_file_stats
                     (Unix.stat namespace_filename)
                     namespace_stats
                && same_file_stats (Unix.stat target_filename) target_stats
              then (
                shared_expanded_snapshot :=
                  Some
                    {
                      key;
                      target_filename;
                      namespace_filename;
                      resolved_load_path;
                      target_stats;
                      namespace_stats;
                      bytes;
                    };
                Some (bytes, Some graph))
              else None
            | _ -> None)
          ~finally:(fun () -> Mutex.unlock shared_expanded_snapshot_lock)
      in
      match prepared with
      | None -> ()
      | Some (bytes, prepared_graph) ->
        let graph =
          match prepared_graph with
          | Some graph -> graph
          | None ->
            Compiler_phase_trace.dependency "dependency.snapshot_shared_restore"
              (fun () -> Marshal.from_string bytes 0)
        in
        cache :=
          Some
            {
              key;
              target_filename;
              namespace_filename;
              resolved_load_path;
              target_stats;
              namespace_stats;
              bytes;
              graph = Some graph;
              typed_integrity =
                (if typed_expanded_snapshot_reuse () then
                   Some
                     (Compiler_phase_trace.dependency
                        "dependency.snapshot_capture" (fun () ->
                          snapshot_type_graph graph))
                 else None);
              in_use = false;
            }

let load_expanded_snapshot ~check:_ ~name =
  if
    (not (expanded_snapshot_enabled ()))
    || Domain.DLS.get preparing_expanded_snapshot
  then None
  else
    let cached = !(expanded_snapshot_cache ()) in
    match cached with
    | Some entry when name = entry.key.target_name ->
      let valid =
        Compiler_phase_trace.dependency "dependency.snapshot_validate"
          (fun () ->
            try
              let path name =
                find_in_path_uncap (Config.get_load_path ()) (name ^ ".cmi")
                |> Compiler_request_state.resolve_path
              in
              path entry.key.target_name = entry.target_filename
              && path entry.key.namespace_name = entry.namespace_filename
              && List.map Compiler_request_state.resolve_path
                   (Config.get_load_path ())
                 = entry.resolved_load_path
              && same_file_stats
                   (Unix.stat entry.target_filename)
                   entry.target_stats
              && same_file_stats
                   (Unix.stat entry.namespace_filename)
                   entry.namespace_stats
            with Not_found | Sys_error _ | Unix.Unix_error _ -> false)
      in
      if not valid then (
        forget_snapshot_candidate entry.key entry.target_filename;
        expanded_snapshot_cache () := None;
        None)
      else
        Some
          (Compiler_phase_trace.dependency "dependency.snapshot_reuse"
             (fun () ->
               let graph : expanded_snapshot =
                 match entry.graph with
                 | Some graph when not (force_fresh_expanded_snapshot ()) ->
                   graph
                 | Some _ | None ->
                   let graph =
                     Compiler_phase_trace.dependency
                       "dependency.snapshot_restore" (fun () ->
                         Marshal.from_string entry.bytes 0)
                   in
                   entry.graph <- Some graph;
                   entry.typed_integrity <-
                     (if typed_expanded_snapshot_reuse () then
                        Some
                          (Compiler_phase_trace.dependency
                             "dependency.snapshot_capture" (fun () ->
                               snapshot_type_graph graph))
                      else None);
                   graph
               in
               entry.in_use <- true;
               let snapshot =
                 {
                   key = entry.key;
                   graph;
                   target_relocated = false;
                   signature_relocated = false;
                   alias_relocated = false;
                 }
               in
               let deprecated =
                 List.fold_left
                   (fun _ -> function
                     | Deprecated s -> Some s)
                   None graph.flags
               in
               let ps_comps =
                 components_of_module ~deprecated ~loc:Location.none empty
                   Subst.identity
                   (Pident (Ident.create_persistent name))
                   (Mty_signature graph.raw_signature)
               in
               let ps_sig =
                 lazy
                   (if not snapshot.signature_relocated then (
                      relocate_allocation_stage graph.signature_ids;
                      snapshot.signature_relocated <- true);
                    graph.expanded_signature)
               in
               {
                 ps_name = name;
                 ps_sig;
                 ps_comps;
                 ps_crcs = graph.crcs;
                 ps_filename = entry.target_filename;
                 ps_flags = graph.flags;
                 ps_snapshot = Some snapshot;
               }))
    | _ -> None

let finalize_expanded_snapshot_cache () =
  finalize_cmi_cache ();
  match !(expanded_snapshot_cache ()) with
  | Some entry when entry.in_use -> (
    entry.in_use <- false;
    match entry.graph with
    | Some _ when force_fresh_expanded_snapshot () ->
      entry.graph <- None;
      entry.typed_integrity <- None
    | Some graph ->
      let pristine =
        Compiler_phase_trace.dependency "dependency.snapshot_verify" (fun () ->
            reset_allocation_stage graph.target_ids;
            reset_allocation_stage graph.signature_ids;
            reset_allocation_stage graph.alias_ids;
            let typed =
              match entry.typed_integrity with
              | Some snapshot -> type_graph_unchanged snapshot
              | None -> false
            in
            (if audit_expanded_snapshot_reuse () then
               let full = Marshal.to_string graph [] = entry.bytes in
               if typed && not full then
                 failwith "typed dependency integrity check missed mutation");
            typed)
      in
      if not pristine then (
        Compiler_phase_trace.dependency "dependency.snapshot_dirty" (fun () ->
            ());
        entry.graph <- None;
        entry.typed_integrity <- None)
    | None -> ())
  | _ -> ()

let () =
  prepare_expanded_snapshot := prepare_expanded_snapshot_now;
  cached_pers_struct_loader := load_expanded_snapshot

let keep_only_summary env =
  if !(last_env ()) == env then !(last_reduced_env ())
  else
    let new_env =
      {
        empty with
        summary = env.summary;
        local_constraints = env.local_constraints;
        flags = env.flags;
      }
    in
    last_env () := env;
    last_reduced_env () := new_env;
    new_env

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
