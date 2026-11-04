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

(* Representation of types and declarations *)

open Asttypes

(* Type expressions for the core language *)

(* An index into a store, see [Store] below and types.mli. *)
type type_expr = int

and arg = {lbl: arg_label; typ: type_expr}

and type_desc =
  | Tvar of string option
  | Tarrow of arg list * type_expr
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr
  | Tfield of {
      name: string;
      mutability: field_mutability ref;
      typ: type_expr;
      rest: type_expr;
    }
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr (* for copying *)
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * Longident.t list * type_expr list

and row_desc = {
  row_fields: (label * row_field) list;
  row_more: type_expr;
  row_closed: bool;
  row_fixed: bool;
  row_name: (Path.t * type_expr list) option;
}

and row_field =
  | Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
  (* 1st true denotes a constant constructor *)
  (* 2nd true denotes a tag in a pattern matching, and
     is erased later *)
  | Rabsent

and abbrev_memo =
  | Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and field_mutability =
  | Mutability_value of Asttypes.mutable_flag
  | Mutability_link of field_mutability ref

(**** Stores ****)

(* One store per slot. A node is [generation lsl gen_shift lor slot lsl
   index_bits lor index], so every access finds its store through the global
   [slots] table: no [Domain.DLS] lookup. Only the domain that owns a store
   writes it; [slot_lock] orders a store's reuse by another domain after its
   release.

   Columns: [descs] holds the descriptor; [meta] holds, for node [i], at
   [4 * i] the link code, at [4 * i + 1] the level and at [4 * i + 2] the id
   (interleaved, so one node's integers share a cache line; the fourth word is
   padding). The link code keeps [Tlink] and [Tsubst] as integers, so that
   linking and copy marks are integer stores: -1, the descriptor is
   [descs.(i)]; t >= 0, [Tlink t]; t <= -2, [Tsubst (-2 - t)]. When the code
   is not -1, [descs.(i)] is the stale descriptor from before the link, kept
   so restoring it is free.

   With [check_generations], a store's generation changes each time it is
   released, and every access checks that the node's generation is its
   store's: a node used after its store was freed raises instead of reading
   another request's node. Off by default (it costs a compare per access). *)
type store = {
  slot: int;
  mutable generation: int;
  mutable base: int;
  mutable descs: type_desc array;
  mutable meta: int array;
  mutable length: int;
}

let check_generations = false
let index_bits = if Sys.int_size >= 63 then 32 else 22
let index_mask = (1 lsl index_bits) - 1
let slot_bits = if Sys.int_size >= 63 then 12 else Sys.int_size - 1 - index_bits
let max_slots = 1 lsl slot_bits
let slot_mask = max_slots - 1
let gen_shift = index_bits + slot_bits
let gen_mask =
  if check_generations then (1 lsl (Sys.int_size - 1 - gen_shift)) - 1 else 0
let max_index = index_mask

let make_meta capacity =
  let meta = Array.make (4 * capacity) 0 in
  for i = 0 to capacity - 1 do
    meta.(4 * i) <- -1
  done;
  meta

let base_of slot generation =
  (generation lsl gen_shift) lor (slot lsl index_bits)

let make_store slot generation capacity =
  let capacity = max 16 capacity in
  {
    slot;
    generation;
    base = base_of slot generation;
    descs = Array.make capacity Tnil;
    meta = make_meta capacity;
    length = 0;
  }

let unused = make_store 0 0 16
let slots = Array.make max_slots unused
let slot_lock = Mutex.create ()
let free_slots = ref []
let next_slot = ref 0
let default_capacity = 1024

(* Capacity kept by a released store for its next user; larger columns are
   dropped so one huge request does not pin memory forever. *)
let retained_capacity = 1 lsl 20

let acquire capacity =
  Mutex.lock slot_lock;
  let slot =
    match !free_slots with
    | slot :: rest ->
      free_slots := rest;
      Some slot
    | [] ->
      if !next_slot < max_slots then (
        let slot = !next_slot in
        incr next_slot;
        Some slot)
      else None
  in
  Mutex.unlock slot_lock;
  match slot with
  | None -> failwith "Types: too many type stores alive"
  | Some slot ->
    let s = Array.unsafe_get slots slot in
    if s == unused || Array.length s.descs < capacity then (
      let s = make_store slot (s.generation land gen_mask) capacity in
      slots.(slot) <- s;
      s)
    else s

let release s =
  let generation = (s.generation + 1) land gen_mask in
  if Array.length s.descs > retained_capacity then
    slots.(s.slot) <- make_store s.slot generation default_capacity
  else (
    Array.fill s.descs 0 s.length Tnil;
    for i = 0 to s.length - 1 do
      s.meta.(4 * i) <- -1
    done;
    s.length <- 0;
    s.generation <- generation;
    s.base <- base_of s.slot generation);
  Mutex.lock slot_lock;
  free_slots := s.slot :: !free_slots;
  Mutex.unlock slot_lock

let current_key =
  Domain.DLS.new_key (fun () ->
      let s = acquire default_capacity in
      Domain.at_exit (fun () -> release s);
      s)

let[@inline] stale t = failwith (Printf.sprintf "Types: stale type node %x" t)

let[@inline] store_of (t : type_expr) =
  let s = Array.unsafe_get slots ((t lsr index_bits) land slot_mask) in
  if check_generations && (t lsr gen_shift) land gen_mask <> s.generation then
    stale t;
  s

let[@inline] index (t : type_expr) = t land index_mask

let grow s =
  let n = s.length in
  let capacity = 2 * Array.length s.descs in
  if capacity - 1 > max_index then failwith "Types: type store is full";
  let descs = Array.make capacity Tnil in
  Array.blit s.descs 0 descs 0 n;
  s.descs <- descs;
  let meta = make_meta capacity in
  Array.blit s.meta 0 meta 0 (4 * n);
  s.meta <- meta

let[@inline] link_code = function
  | Tlink t -> t
  | Tsubst t -> -2 - t
  | _ -> -1

let newty_in s desc ~level ~id =
  if s.length = Array.length s.descs then grow s;
  let i = s.length in
  s.length <- i + 1;
  (match link_code desc with
  | -1 -> s.descs.(i) <- desc
  | code -> s.meta.(4 * i) <- code);
  s.meta.((4 * i) + 1) <- level;
  s.meta.((4 * i) + 2) <- id;
  s.base lor i

(* Column reads are bounds-checked: a stale node from a freed store must not
   read outside the arrays. *)
let[@inline] get_desc t =
  let s = store_of t and i = index t in
  let code = s.meta.(4 * i) in
  if code = -1 then s.descs.(i)
  else if code >= 0 then Tlink code
  else Tsubst (-2 - code)

let[@inline] get_level t = (store_of t).meta.((4 * index t) + 1)
let[@inline] get_id t = (store_of t).meta.((4 * index t) + 2)

let[@inline] link_code_of t = (store_of t).meta.(4 * index t)

let[@inline] link_target t =
  let code = link_code_of t in
  if code >= 0 then code else t

let[@inline] subst_target t =
  let code = link_code_of t in
  if code <= -2 then -2 - code else t

module Store = struct
  type t = store

  let current () = Domain.DLS.get current_key

  let with_store ?(capacity = default_capacity) action =
    let previous = current () in
    let s = acquire capacity in
    Domain.DLS.set current_key s;
    Fun.protect action ~finally:(fun () ->
        Domain.DLS.set current_key previous;
        release s)

  let create ?(capacity = default_capacity) () = acquire capacity
  let release = release
  let length s = s.length
  let of_node = store_of

  let nth s i =
    if i < 0 || i >= s.length then invalid_arg "Types.Store.nth";
    s.base lor i

  let index = index
  let index_bits = index_bits
  let max_slots = max_slots
  let newty_in = newty_in
end

module Transient_expr = struct
  let create desc ~level ~id = newty_in (Store.current ()) desc ~level ~id

  let[@inline] set_desc t desc =
    let s = store_of t and i = index t in
    match link_code desc with
    | -1 ->
      s.meta.(4 * i) <- -1;
      (* Restoring the descriptor saved before a link or copy mark writes the
         value that is already there; skip the write barrier then. *)
      if s.descs.(i) != desc then s.descs.(i) <- desc
    | code -> s.meta.(4 * i) <- code

  let[@inline] set_level t level =
    (store_of t).meta.((4 * index t) + 1) <- level
end

module Type_ops = struct
  type t = type_expr
  let compare t1 t2 = get_id t1 - get_id t2
  let hash t = get_id t
  let equal t1 t2 = t1 == t2
end

(* Maps of methods and instance variables *)

module Ordered_string = struct
  type t = string
  let compare (x : t) y = compare x y
end
module Vars = Map.Make (Ordered_string)

(* Value descriptions *)

type value_description = {
  val_type: type_expr; (* Type of the value *)
  val_kind: value_kind;
  val_loc: Location.t;
  val_attributes: Parsetree.attributes;
}

and value_kind =
  | Val_reg (* Regular value *)
  | Val_prim of Primitive.description (* Primitive *)

(* Variance *)

module Variance = struct
  type t = int
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  let single = function
    | May_pos -> 1
    | May_neg -> 2
    | May_weak -> 4
    | Inj -> 8
    | Pos -> 16
    | Neg -> 32
    | Inv -> 64
  let union v1 v2 = v1 lor v2
  let inter v1 v2 = v1 land v2
  let subset v1 v2 = v1 land v2 = v1
  let set x b v = if b then v lor single x else v land lnot (single x)
  let mem x = subset (single x)
  let null = 0
  let may_inv = 7
  let full = 127
  let covariant = single May_pos lor single Pos lor single Inj
  let swap f1 f2 v =
    let v' = set f1 (mem f2 v) v in
    set f2 (mem f1 v) v'
  let conjugate v = swap May_pos May_neg (swap Pos Neg v)
  let get_upper v = (mem May_pos v, mem May_neg v)
  let get_lower v = (mem Pos v, mem Neg v, mem Inv v, mem Inj v)
end

(* Type definitions *)

type type_declaration = {
  type_params: type_expr list;
  type_arity: int;
  type_kind: type_kind;
  type_private: private_flag;
  type_manifest: type_expr option;
  type_variance: Variance.t list;
  type_newtype_level: (int * int) option;
  type_loc: Location.t;
  type_attributes: Parsetree.attributes;
  type_immediate: bool;
  type_representation: type_representation;
  type_shape: Variant_runtime.value_shape;
      (** Resolved from the declaration's annotations in its own
          environment, when it is declared *)
  type_inlined_types: type_inlined_type list;
}

and type_inlined_type =
  | Record of {type_name: string; labels: label_declaration list}

and type_kind =
  | Type_abstract
  | Type_record of label_declaration list * record_representation
  | Type_variant of constructor_declaration list * Variant_runtime.layout_ref
  | Type_open

and record_representation =
  | Record_regular (* All fields are boxed / tagged *)
  | Record_unboxed of bool (* Unboxed single-field record, inlined or not *)
  | Record_inlined of
      (* Inlined record *)
       {
      name: string;
      representation: Variant_runtime.constructor_reference;
    }
  | Record_extension (* Inlined record under extension *)

and label_declaration = {
  ld_id: Ident.t;
  ld_runtime_name: string option;
      (* The name the field has at run time, when it differs from [ld_id].
         Comes from [@as] on the declaration, as its decoded value: two
         spellings of one name are one name, so this compares by [=] and is
         stable in the cmi. Note the asymmetry with [ld_attributes] below,
         which still carries whole parsetree attributes, source spelling
         included. That is safe only because every reader of an attribute
         payload goes through a semantic accessor; anything comparing
         attributes structurally would be comparing keystrokes. *)
  ld_mutable: mutable_flag;
  ld_optional: bool;
  ld_type: type_expr;
  ld_loc: Location.t;
  ld_attributes: Parsetree.attributes;
}

and constructor_declaration = {
  cd_id: Ident.t;
  cd_runtime_tag: Variant_runtime.literal_tag option;
      (* The decoded [@as] value of the constructor. Its canonical runtime
         layout is derived from this field rather than from attributes. *)
  cd_args: constructor_arguments;
  cd_res: type_expr option;
  cd_loc: Location.t;
  cd_attributes: Parsetree.attributes;
}

and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

and type_representation = Boxed | Unboxed
(* Single-payload @unboxed type: the payload is the whole runtime
         value. Multi-constructor unboxed variants are tracked by their layout,
         not here. *)

type extension_constructor = {
  ext_type_path: Path.t;
  ext_type_params: type_expr list;
  ext_args: constructor_arguments;
  ext_ret_type: type_expr option;
  ext_private: private_flag;
  ext_loc: Location.t;
  ext_attributes: Parsetree.attributes;
  ext_is_exception: bool;
}

(* Type expressions for the class language *)

module Concr = Set.Make (Ordered_string)

(* Type expressions for the module language *)

type module_type =
  | Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of Ident.t * module_type option * module_type
  | Mty_alias of alias_presence * Path.t

and alias_presence = Mta_present | Mta_absent

and signature = signature_item list

and signature_item =
  | Sig_value of Ident.t * value_description
  | Sig_type of Ident.t * type_declaration * rec_status
  | Sig_typext of Ident.t * extension_constructor * ext_status
  | Sig_module of Ident.t * module_declaration * rec_status
  | Sig_modtype of Ident.t * modtype_declaration

and module_declaration = {
  md_type: module_type;
  md_attributes: Parsetree.attributes;
  md_loc: Location.t;
}

and modtype_declaration = {
  mtd_type: module_type option; (* Note: abstract *)
  mtd_attributes: Parsetree.attributes;
  mtd_loc: Location.t;
}

and rec_status =
  | Trec_not (* first in a nonrecursive group *)
  | Trec_first (* first in a recursive group *)
  | Trec_next (* not first in a recursive/nonrecursive group *)

and ext_status =
  | Text_first (* first constructor of an extension *)
  | Text_next (* not first constructor of an extension *)
  | Text_exception (* an exception *)

(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description = {
  cstr_name: string; (* Constructor name *)
  cstr_res: type_expr; (* Type of the result *)
  cstr_existentials: type_expr list; (* list of existentials *)
  cstr_args: type_expr list; (* Type of the arguments *)
  cstr_arity: int; (* Number of arguments *)
  cstr_kind: constructor_kind;
  cstr_generalized: bool; (* Constrained return type? *)
  cstr_private: private_flag; (* Read-only constructor? *)
  cstr_loc: Location.t;
  cstr_attributes: Parsetree.attributes;
  cstr_inlined: type_declaration option;
}

and constructor_kind =
  | Ordinary_constructor of Variant_runtime.constructor_reference
    (* Constructor introduced by a variant type declaration. This stable
       reference addresses its runtime representation directly; the shared
       variant value also supplies declaration-level matching facts. *)
  | Extension_constructor of Path.t
(* Extension constructor, identified by its own path since extension
         constructors can be rebound *)

(* Whether two constructor descriptions denote the same constructor of a
   common scrutinee type. Because a re-exported variant mints descriptions
   with a different type path, ordinary constructors are compared by name
   only; the shared scrutinee type makes the name unambiguous. Not a
   general-purpose identity test across unrelated types. *)
let same_constructor c1 c2 =
  match (c1.cstr_kind, c2.cstr_kind) with
  | Ordinary_constructor _, Ordinary_constructor _ ->
    c1.cstr_name = c2.cstr_name
  | Extension_constructor p1, Extension_constructor p2 -> Path.same p1 p2
  | (Ordinary_constructor _ | Extension_constructor _), _ -> false

let may_equal_constr c1 c2 =
  match (c1.cstr_kind, c2.cstr_kind) with
  | Extension_constructor _, Extension_constructor _ ->
    (* extension constructors may be rebound, so paths cannot disprove
       equality; arity can *)
    c1.cstr_arity = c2.cstr_arity
  | _ -> same_constructor c1 c2

type label_description = {
  lbl_name: string; (* Short name *)
  lbl_runtime_name: string;
      (* The name the field has at run time: its [@as] payload when it has
         one, otherwise [lbl_name]. Decided once when the declaration is
         typed, so no consumer re-reads the attribute. *)
  lbl_res: type_expr; (* Type of the result *)
  lbl_arg: type_expr; (* Type of the argument *)
  lbl_mut: mutable_flag; (* Is this a mutable field? *)
  lbl_optional: bool; (* Is this an optional field? *)
  lbl_pos: int; (* Position in block *)
  mutable lbl_all: label_description array;
      (* All the labels in this type. This is mutable only because of a specific feature related to dicts, and should not be mutated elsewhere. *)
  lbl_repres: record_representation; (* Representation for this record *)
  lbl_private: private_flag; (* Read-only field? *)
  lbl_loc: Location.t;
  lbl_attributes: Parsetree.attributes;
}
let same_record_representation x y =
  match x with
  | Record_regular -> y = Record_regular
  | Record_inlined {name} -> (
    match y with
    | Record_inlined y -> name = y.name
    | _ -> false)
  | Record_extension -> y = Record_extension
  | Record_unboxed x -> (
    match y with
    | Record_unboxed y -> x = y
    | _ -> false)
