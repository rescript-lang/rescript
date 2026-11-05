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

   The descriptors are encoded as integers (rescript-perf-investigation
   results/arena/encoded/README.md). [meta] holds [stride] integers per node,
   in one cache line:
   - [f_link]: -1, the node's descriptor is the encoded one; t >= 0,
     [Tlink t]; t <= -2, [Tsubst (-2 - t)]. The encoded descriptor stays in
     place under a link, so removing the link restores it for free.
   - [f_level], [f_id].
   - [f_kind]: the encoded descriptor's kind ([k_*] below).
   - [f_a], [f_b], [f_c]: payload, by kind:
     [k_var]/[k_univar] a = -1 for an unnamed variable, else the index in
       [boxed] of the descriptor block itself (so views and exports reuse
       it, keeping the sharing of named variables);
     [k_arrow] a = start in [pool], b = number of parameters, c = result;
       the pool holds, per parameter, the label code (-1 [Nolabel], else
       index in [labels]) and the parameter type;
     [k_tuple] b = start, c = length;
     [k_constr] a = path (index in [paths]), b = start, c = length;
     [k_object] a = fields; [k_poly] a = body, b = start, c = length;
     [k_boxed] a = index in [boxed] ([Tfield], [Tvariant], [Tpackage]).
   - [f_memo]: [Tconstr]'s abbreviation cell: -1 for a cell holding [Mnil]
     that nobody has asked for yet, else the index of the cell in [memos].
   Pool slices are never written after they are filled: setting a descriptor
   allocates a new slice, so slices may be shared between nodes.

   With [check_generations], a store's generation changes each time it is
   released, and every access checks that the node's generation is its
   store's: a node used after its store was freed raises instead of reading
   another request's node. Off by default (it costs a compare per access). *)

module Vec = struct
  type 'a t = {mutable data: 'a array; mutable len: int; dummy: 'a}

  let make n dummy = {data = Array.make n dummy; len = 0; dummy}

  let push v x =
    let n = v.len in
    if n = Array.length v.data then (
      (* Filled with the (old) dummy: [Array.make] of a large array with a
         young value forces a minor collection first. *)
      let a = Array.make (2 * n) v.dummy in
      Array.blit v.data 0 a 0 n;
      v.data <- a);
    Array.unsafe_set v.data n x;
    v.len <- n + 1;
    n

  let[@inline] get v i = v.data.(i)

  let reset v retained =
    if Array.length v.data > retained then v.data <- Array.make 64 v.dummy
    else Array.fill v.data 0 v.len v.dummy;
    v.len <- 0
end

let stride = 8
let f_link = 0
let f_level = 1
let f_id = 2
let f_kind = 3
let f_a = 4
let f_b = 5
let f_c = 6
let f_memo = 7

let k_var = 0
let k_arrow = 1
let k_tuple = 2
let k_constr = 3
let k_object = 4
let k_nil = 5
let k_univar = 6
let k_poly = 7
let k_boxed = 8

type store = {
  slot: int;
  mutable generation: int;
  mutable base: int;
  mutable meta: int array;
  mutable length: int;
  mutable pool: int array;
  mutable pool_len: int;
  paths: Path.t Vec.t;
  labels: arg_label Vec.t;
  boxed: type_desc Vec.t;
  memos: abbrev_memo ref Vec.t;
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

let dummy_path = Path.Pident (Ident.create_persistent "*dummy*")
let dummy_memo = ref Mnil

let make_meta capacity =
  let meta = Array.make (stride * capacity) 0 in
  for i = 0 to capacity - 1 do
    meta.(stride * i) <- -1
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
    meta = make_meta capacity;
    length = 0;
    pool = Array.make (2 * capacity) 0;
    pool_len = 0;
    paths = Vec.make 64 dummy_path;
    labels = Vec.make 16 Nolabel;
    boxed = Vec.make 16 Tnil;
    memos = Vec.make 16 dummy_memo;
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
    if s == unused || Array.length s.meta < stride * capacity then (
      let s = make_store slot (s.generation land gen_mask) capacity in
      slots.(slot) <- s;
      s)
    else s

let release s =
  let generation = (s.generation + 1) land gen_mask in
  if Array.length s.meta > stride * retained_capacity then
    slots.(s.slot) <- make_store s.slot generation default_capacity
  else (
    for i = 0 to s.length - 1 do
      s.meta.(stride * i) <- -1
    done;
    s.length <- 0;
    if Array.length s.pool > 4 * retained_capacity then
      s.pool <- Array.make (2 * default_capacity) 0;
    s.pool_len <- 0;
    let r = retained_capacity in
    Vec.reset s.paths r;
    Vec.reset s.labels r;
    Vec.reset s.boxed r;
    Vec.reset s.memos r;
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
  let capacity = 2 * (Array.length s.meta / stride) in
  if capacity - 1 > max_index then failwith "Types: type store is full";
  let meta = make_meta capacity in
  Array.blit s.meta 0 meta 0 (stride * n);
  s.meta <- meta

(* Reserve [n] pool words; returns the start. *)
let[@inline] pool_alloc s n =
  let start = s.pool_len in
  let need = start + n in
  if need > Array.length s.pool then (
    let a = Array.make (max need (2 * Array.length s.pool)) 0 in
    Array.blit s.pool 0 a 0 start;
    s.pool <- a);
  s.pool_len <- need;
  start

(* A fresh node holding [Tnil]. *)
let[@inline] alloc s ~level ~id =
  if s.length = Array.length s.meta / stride then grow s;
  let i = s.length in
  s.length <- i + 1;
  let m = s.meta and o = stride * i in
  m.(o + f_link) <- -1;
  m.(o + f_level) <- level;
  m.(o + f_id) <- id;
  m.(o + f_kind) <- k_nil;
  m.(o + f_memo) <- -1;
  s.base lor i

let[@inline] label_code s (l : arg_label) =
  match l with
  | Nolabel -> -1
  | _ -> Vec.push s.labels l

(* The last path pushed is often pushed again (a re-description of the same
   constructor): reuse its entry. *)
let[@inline] path_code s p =
  let n = s.paths.len in
  if n > 0 && Vec.get s.paths (n - 1) == p then n - 1 else Vec.push s.paths p

let rec push_list pool start l =
  match l with
  | [] -> ()
  | t :: rest ->
    Array.unsafe_set pool start t;
    push_list pool (start + 1) rest

let[@inline] set_enc s i kind a b c =
  let m = s.meta and o = stride * i in
  m.(o + f_link) <- -1;
  m.(o + f_kind) <- kind;
  m.(o + f_a) <- a;
  m.(o + f_b) <- b;
  m.(o + f_c) <- c

let encode_list s l =
  let n = List.length l in
  let start = pool_alloc s n in
  push_list s.pool start l;
  start

(* Encode [d] as the descriptor of node [i] of [s]. *)
let encode s i (d : type_desc) =
  match d with
  | Tvar None -> set_enc s i k_var (-1) 0 0
  | Tvar _ -> set_enc s i k_var (Vec.push s.boxed d) 0 0
  | Tunivar None -> set_enc s i k_univar (-1) 0 0
  | Tunivar _ -> set_enc s i k_univar (Vec.push s.boxed d) 0 0
  | Tnil -> set_enc s i k_nil 0 0 0
  | Tlink t -> s.meta.((stride * i) + f_link) <- t
  | Tsubst t -> s.meta.((stride * i) + f_link) <- -2 - t
  | Tobject t -> set_enc s i k_object t 0 0
  | Ttuple l ->
    let start = encode_list s l in
    set_enc s i k_tuple 0 start (s.pool_len - start)
  | Tconstr (p, l, memo) ->
    let start = encode_list s l in
    set_enc s i k_constr (path_code s p) start (s.pool_len - start);
    s.meta.((stride * i) + f_memo) <- Vec.push s.memos memo
  | Tpoly (t, l) ->
    let start = encode_list s l in
    set_enc s i k_poly t start (s.pool_len - start)
  | Tarrow (args, ret) ->
    let n = List.length args in
    let start = pool_alloc s (2 * n) in
    List.iteri
      (fun k (a : arg) ->
        let code = label_code s a.lbl in
        s.pool.(start + (2 * k)) <- code;
        s.pool.(start + (2 * k) + 1) <- a.typ)
      args;
    set_enc s i k_arrow start n ret
  | Tfield _ | Tvariant _ | Tpackage _ ->
    set_enc s i k_boxed (Vec.push s.boxed d) 0 0

let newty_in s desc ~level ~id =
  let t = alloc s ~level ~id in
  encode s (index t) desc;
  t

(**** Reading ****)

let tvar_none = Tvar None
let tunivar_none = Tunivar None

let[@inline] label_of s code : arg_label =
  if code < 0 then Nolabel else Vec.get s.labels code

let rec pool_list pool start n acc =
  if n = 0 then acc
  else pool_list pool start (n - 1) (pool.(start + n - 1) :: acc)

let abbrev_cell_in s o =
  let k = s.meta.(o + f_memo) in
  if k >= 0 then Vec.get s.memos k
  else
    let cell = ref Mnil in
    s.meta.(o + f_memo) <- Vec.push s.memos cell;
    cell

(* The encoded descriptor, ignoring the link column. *)
let decode s o : type_desc =
  let m = s.meta in
  let a = m.(o + f_a) and b = m.(o + f_b) and c = m.(o + f_c) in
  match m.(o + f_kind) with
  | 0 (* k_var *) -> if a < 0 then tvar_none else Vec.get s.boxed a
  | 1 (* k_arrow *) ->
    let pool = s.pool in
    let rec args k acc =
      if k < 0 then acc
      else
        args (k - 1)
          ({lbl = label_of s pool.(a + (2 * k)); typ = pool.(a + (2 * k) + 1)}
          :: acc)
    in
    Tarrow (args (b - 1) [], c)
  | 2 (* k_tuple *) -> Ttuple (pool_list s.pool b c [])
  | 3 (* k_constr *) ->
    Tconstr (Vec.get s.paths a, pool_list s.pool b c [], abbrev_cell_in s o)
  | 4 (* k_object *) -> Tobject a
  | 5 (* k_nil *) -> Tnil
  | 6 (* k_univar *) -> if a < 0 then tunivar_none else Vec.get s.boxed a
  | 7 (* k_poly *) -> Tpoly (a, pool_list s.pool b c [])
  | _ (* k_boxed *) -> Vec.get s.boxed a

(* Column reads are bounds-checked: a stale node from a freed store must not
   read outside the arrays. *)
let get_desc t =
  let s = store_of t and o = stride * index t in
  let code = s.meta.(o + f_link) in
  if code = -1 then decode s o
  else if code >= 0 then Tlink code
  else Tsubst (-2 - code)

(* The descriptor under the node's link or copy mark, if any. *)
let stored_desc t = decode (store_of t) (stride * index t)

let[@inline] get_level t = (store_of t).meta.((stride * index t) + f_level)
let[@inline] get_id t = (store_of t).meta.((stride * index t) + f_id)

let[@inline] link_code_of t = (store_of t).meta.((stride * index t) + f_link)

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

  let[@inline] set_desc t desc = encode (store_of t) (index t) desc

  let[@inline] set_level t level =
    (store_of t).meta.((stride * index t) + f_level) <- level
end

(**** Kind-specific accessors ****)

type kind =
  | Kvar
  | Karrow
  | Ktuple
  | Kconstr
  | Kobject
  | Knil
  | Kunivar
  | Kpoly
  | Kboxed
  | Klink
  | Ksubst

(* [kind] is a constant-only variant: its values are the immediates 0..10 in
   declaration order, the [k_*] codes for the encoded kinds. *)
let[@inline] kind_of_int (k : int) : kind = Obj.magic k

let[@inline] kind t : kind =
  let s = store_of t and o = stride * index t in
  let code = s.meta.(o + f_link) in
  if code = -1 then kind_of_int s.meta.(o + f_kind)
  else if code >= 0 then Klink
  else Ksubst

let[@inline] stored_kind t : kind =
  kind_of_int (store_of t).meta.((stride * index t) + f_kind)

let[@inline] payload_a t = (store_of t).meta.((stride * index t) + f_a)
let[@inline] payload_b t = (store_of t).meta.((stride * index t) + f_b)
let[@inline] payload_c t = (store_of t).meta.((stride * index t) + f_c)

let var_name t =
  let s = store_of t in
  let a = s.meta.((stride * index t) + f_a) in
  if a < 0 then None
  else
    match Vec.get s.boxed a with
    | Tvar name | Tunivar name -> name
    | _ -> None

let[@inline] arrow_arity t = payload_b t
let[@inline] arrow_arg t k = (store_of t).pool.(payload_a t + (2 * k) + 1)

let arrow_label t k =
  let s = store_of t in
  label_of s s.pool.(payload_a t + (2 * k))

let[@inline] arrow_label_is_nolabel t k =
  (store_of t).pool.(payload_a t + (2 * k)) < 0

let[@inline] arrow_ret t = payload_c t
let[@inline] tuple_length t = payload_c t
let[@inline] tuple_elem t k = (store_of t).pool.(payload_b t + k)
let[@inline] constr_path t = Vec.get (store_of t).paths (payload_a t)
let[@inline] constr_length t = payload_c t
let[@inline] constr_arg t k = (store_of t).pool.(payload_b t + k)

let same_constr_path t1 t2 =
  let s1 = store_of t1 and s2 = store_of t2 in
  let a1 = payload_a t1 and a2 = payload_a t2 in
  (s1 == s2 && a1 = a2)
  ||
  let p1 = Vec.get s1.paths a1 and p2 = Vec.get s2.paths a2 in
  p1 == p2 || Path.same p1 p2

let[@inline] object_fields t = payload_a t
let[@inline] poly_body t = payload_a t
let[@inline] poly_length t = payload_c t
let[@inline] poly_var t k = (store_of t).pool.(payload_b t + k)
let boxed_desc t = Vec.get (store_of t).boxed (payload_a t)

let abbrev_cell t =
  let s = store_of t in
  abbrev_cell_in s (stride * index t)

(* The contents of the abbreviation cell, without creating it. *)
let abbrev_memo t =
  let s = store_of t in
  let k = s.meta.((stride * index t) + f_memo) in
  if k >= 0 then !(Vec.get s.memos k) else Mnil

let clear_abbrev_memo t =
  let s = store_of t in
  let k = s.meta.((stride * index t) + f_memo) in
  if k >= 0 then Vec.get s.memos k := Mnil

let set_abbrev_cell t cell =
  let s = store_of t in
  s.meta.((stride * index t) + f_memo) <- Vec.push s.memos cell

let[@inline] link_code t = link_code_of t

let[@inline] set_link_code t code =
  (store_of t).meta.((stride * index t) + f_link) <- code

let[@inline] set_link t target = set_link_code t target
let[@inline] set_subst t target = set_link_code t (-2 - target)

(* The children of the node, in [Btype.iter_type_expr]'s order, except for
   [Kboxed] nodes, which [f_boxed] is given. A link (or copy mark) has its
   target as only child. [f] may allocate nodes: the pool is read afresh
   after each call. *)
let iter_children f t =
  let s = store_of t and o = stride * index t in
  let m = s.meta in
  let code = m.(o + f_link) in
  if code >= 0 then f code
  else if code <= -2 then f (-2 - code)
  else
    let a = m.(o + f_a) and b = m.(o + f_b) and c = m.(o + f_c) in
    match m.(o + f_kind) with
    | 0 (* k_var *) | 5 (* k_nil *) | 6 (* k_univar *) -> ()
    | 1 (* k_arrow *) ->
      for k = 0 to b - 1 do
        f s.pool.(a + (2 * k) + 1)
      done;
      f c
    | 2 (* k_tuple *) | 3 (* k_constr *) ->
      for k = b to b + c - 1 do
        f s.pool.(k)
      done
    | 4 (* k_object *) -> f a
    | 7 (* k_poly *) ->
      f a;
      for k = b to b + c - 1 do
        f s.pool.(k)
      done
    | _ -> invalid_arg "Types.iter_children: boxed descriptor"

(* Give the fresh node [dst] the encoded descriptor of [src] with each child
   [c] replaced by [f c], evaluating [f] in the order [Btype.copy_type_desc]
   does (an arrow's result first, then its parameters; a [Tpoly]'s variables,
   through [fvar], then its body). Variables lose their names unless
   [keep_names]; universal variables keep them. A [Tconstr] gets a fresh
   abbreviation cell. [src] must not be [Kboxed], nor carry a link. *)
let copy_into ~keep_names ~fvar ~dst src f =
  let ss = store_of src and so = stride * index src in
  let ds = store_of dst and di = index dst in
  let sm = ss.meta in
  let a = sm.(so + f_a) and b = sm.(so + f_b) and c = sm.(so + f_c) in
  let same = ss == ds in
  let map_slice f start n =
    let d = pool_alloc ds n in
    for k = 0 to n - 1 do
      let v = f ss.pool.(start + k) in
      ds.pool.(d + k) <- v
    done;
    d
  in
  let boxed_entry a =
    if same || a < 0 then a else Vec.push ds.boxed (Vec.get ss.boxed a)
  in
  match sm.(so + f_kind) with
  | 0 (* k_var *) ->
    set_enc ds di k_var (if keep_names then boxed_entry a else -1) 0 0
  | 1 (* k_arrow *) ->
    let ret = f c in
    let d = pool_alloc ds (2 * b) in
    for k = 0 to b - 1 do
      let code = ss.pool.(a + (2 * k)) in
      let code =
        if same || code < 0 then code
        else Vec.push ds.labels (Vec.get ss.labels code)
      in
      ds.pool.(d + (2 * k)) <- code;
      let v = f ss.pool.(a + (2 * k) + 1) in
      ds.pool.(d + (2 * k) + 1) <- v
    done;
    set_enc ds di k_arrow d b ret
  | 2 (* k_tuple *) -> set_enc ds di k_tuple 0 (map_slice f b c) c
  | 3 (* k_constr *) ->
    let p = if same then a else path_code ds (Vec.get ss.paths a) in
    let d = map_slice f b c in
    set_enc ds di k_constr p d c;
    ds.meta.((stride * di) + f_memo) <- -1
  | 4 (* k_object *) -> set_enc ds di k_object (f a) 0 0
  | 5 (* k_nil *) -> set_enc ds di k_nil 0 0 0
  | 6 (* k_univar *) -> set_enc ds di k_univar (boxed_entry a) 0 0
  | 7 (* k_poly *) ->
    let d = map_slice fvar b c in
    let body = f a in
    set_enc ds di k_poly body d c
  | _ -> invalid_arg "Types.copy_into: boxed descriptor"

(* A [Tconstr] node whose abbreviation cell is created when asked for. *)
let new_constr_in s ~level ~id p args =
  let t = alloc s ~level ~id in
  let n = Array.length args in
  let start = pool_alloc s n in
  Array.blit args 0 s.pool start n;
  set_enc s (index t) k_constr (path_code s p) start n;
  t

let new_constr ~level ~id p args =
  new_constr_in (Store.current ()) ~level ~id p args

(* Give [t] the descriptor [Tconstr (p, args, ref Mnil)] without allocating
   the cell. *)
let set_constr t p args =
  let s = store_of t in
  let start = encode_list s args in
  set_enc s (index t) k_constr (path_code s p) start (s.pool_len - start);
  s.meta.((stride * index t) + f_memo) <- -1

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
