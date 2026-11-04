(* See type_arena.mli and rescript-perf-investigation results/arena/DESIGN.md. *)

type node = int

type arg = {lbl: Asttypes.arg_label; typ: node}

and desc =
  | Tvar of string option
  | Tarrow of arg list * node
  | Ttuple of node list
  | Tconstr of Path.t * node list * abbrev_memo ref
  | Tobject of node
  | Tfield of {
      name: string;
      mutability: Types.field_mutability ref;
      typ: node;
      rest: node;
    }
  | Tnil
  | Tlink of node
  | Tsubst of node
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of node * node list
  | Tpackage of Path.t * Longident.t list * node list

and row_desc = {
  row_fields: (Asttypes.label * row_field) list;
  row_more: node;
  row_closed: bool;
  row_fixed: bool;
  row_name: (Path.t * node list) option;
}

and row_field =
  | Rpresent of node option
  | Reither of bool * node list * bool * row_field option ref
  | Rabsent

and abbrev_memo =
  | Mnil
  | Mcons of Asttypes.private_flag * Path.t * node * node * abbrev_memo
  | Mlink of abbrev_memo ref

(**** Stores ****)

(* One store per slot. A node is [slot lsl index_bits lor index], so every
   access finds its store through the global [slots] table: two loads and no
   [Domain.DLS] lookup. Only the domain that owns a store reads or writes it;
   [slot_lock] orders a store's reuse by another domain after its release.

   Columns: [descs] holds the boxed descriptor; [meta] holds, for node [i],
   at [4 * i] the link code, at [4 * i + 1] the level and at [4 * i + 2] the
   id (interleaved, so one node's integers share a cache line; the fourth
   word is padding). The link code keeps [Tlink] and [Tsubst] as integers,
   so that linking and copy marks are integer stores: -1, the descriptor is
   [descs.(i)]; t >= 0, [Tlink t]; t <= -2, [Tsubst (-2 - t)]. When the code
   is not -1, [descs.(i)] is the stale descriptor from before the link, kept
   so restoring it is free. *)
type store = {
  slot: int;
  base: int;
  mutable descs: desc array;
  mutable meta: int array;
  mutable length: int;
}

let index_bits = if Sys.int_size >= 63 then 32 else 22
let index_mask = (1 lsl index_bits) - 1
let max_slots = min 4096 (1 lsl (Sys.int_size - 1 - index_bits))
let max_index = index_mask

let make_meta capacity =
  let meta = Array.make (4 * capacity) 0 in
  for i = 0 to capacity - 1 do
    meta.(4 * i) <- -1
  done;
  meta

let make_store slot capacity =
  let capacity = max 16 capacity in
  {
    slot;
    base = slot lsl index_bits;
    descs = Array.make capacity Tnil;
    meta = make_meta capacity;
    length = 0;
  }

let unused = make_store 0 16
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
  | None -> failwith "Type_arena: too many stores alive"
  | Some slot ->
    let s = Array.unsafe_get slots slot in
    if s == unused || Array.length s.descs < capacity then (
      let s = make_store slot capacity in
      slots.(slot) <- s;
      s)
    else s

let release s =
  if Array.length s.descs > retained_capacity then
    slots.(s.slot) <- make_store s.slot default_capacity
  else (
    Array.fill s.descs 0 s.length Tnil;
    for i = 0 to s.length - 1 do
      s.meta.(4 * i) <- -1
    done;
    s.length <- 0);
  Mutex.lock slot_lock;
  free_slots := s.slot :: !free_slots;
  Mutex.unlock slot_lock

let current_key =
  Domain.DLS.new_key (fun () ->
      let s = acquire default_capacity in
      Domain.at_exit (fun () -> release s);
      s)

let current () = Domain.DLS.get current_key

let with_store ?(capacity = default_capacity) action =
  let previous = current () in
  let s = acquire capacity in
  Domain.DLS.set current_key s;
  Fun.protect action ~finally:(fun () ->
      Domain.DLS.set current_key previous;
      release s)

let length s = s.length
let[@inline] store_of (t : node) = Array.unsafe_get slots (t lsr index_bits)
let[@inline] index (t : node) = t land index_mask

let nth s i =
  if i < 0 || i >= s.length then invalid_arg "Type_arena.nth";
  s.base lor i

let grow s =
  let n = s.length in
  let capacity = 2 * Array.length s.descs in
  if capacity - 1 > max_index then failwith "Type_arena: store is full";
  let extend a fill =
    let a' = Array.make capacity fill in
    Array.blit a 0 a' 0 n;
    a'
  in
  s.descs <- extend s.descs Tnil;
  let meta = make_meta capacity in
  Array.blit s.meta 0 meta 0 (4 * n);
  s.meta <- meta

let[@inline] link_code = function
  | Tlink t -> t
  | Tsubst t -> -2 - t
  | _ -> -1

let newty_in s ~level ~id desc =
  if s.length = Array.length s.descs then grow s;
  let i = s.length in
  s.length <- i + 1;
  (match link_code desc with
  | -1 -> s.descs.(i) <- desc
  | code -> s.meta.(4 * i) <- code);
  s.meta.((4 * i) + 1) <- level;
  s.meta.((4 * i) + 2) <- id;
  s.base lor i

let newty ~level ~id desc = newty_in (current ()) ~level ~id desc

(* Column reads are bounds-checked: a stale node from a freed store must not
   read outside the arrays. *)
let[@inline] get_desc t =
  let s = store_of t and i = index t in
  let code = s.meta.(4 * i) in
  if code = -1 then s.descs.(i)
  else if code >= 0 then Tlink code
  else Tsubst (-2 - code)

let[@inline] set_desc t desc =
  let s = store_of t and i = index t in
  match link_code desc with
  | -1 ->
    s.meta.(4 * i) <- -1;
    (* Restoring the descriptor saved before a link or copy mark writes the
       value that is already there; skip the write barrier then. *)
    if s.descs.(i) != desc then s.descs.(i) <- desc
  | code -> s.meta.(4 * i) <- code

let[@inline] link_code_of t = (store_of t).meta.(4 * index t)

let[@inline] link_of t =
  let code = link_code_of t in
  if code >= 0 then code else -1

let[@inline] subst_of t =
  let code = link_code_of t in
  if code <= -2 then -2 - code else -1

let[@inline] link_target t =
  let code = link_code_of t in
  if code >= 0 then code else t

let[@inline] subst_target t =
  let code = link_code_of t in
  if code <= -2 then -2 - code else t

let[@inline] get_level t = (store_of t).meta.((4 * index t) + 1)
let[@inline] set_level t level = (store_of t).meta.((4 * index t) + 1) <- level
let[@inline] get_id t = (store_of t).meta.((4 * index t) + 2)
let equal (t1 : node) t2 = t1 == t2
let compare t1 t2 = Int.compare (get_id t1) (get_id t2)

(**** Signatures ****)

module Sig = struct
  type value_description = {
    val_type: node;
    val_kind: Types.value_kind;
    val_loc: Location.t;
    val_attributes: Parsetree.attributes;
  }

  type type_declaration = {
    type_params: node list;
    type_arity: int;
    type_kind: type_kind;
    type_private: Asttypes.private_flag;
    type_manifest: node option;
    type_variance: Types.Variance.t list;
    type_newtype_level: (int * int) option;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_immediate: bool;
    type_representation: Types.type_representation;
    type_shape: Variant_runtime.value_shape;
    type_inlined_types: type_inlined_type list;
  }

  and type_inlined_type =
    | Record of {type_name: string; labels: label_declaration list}

  and type_kind =
    | Type_abstract
    | Type_record of label_declaration list * Types.record_representation
    | Type_variant of constructor_declaration list * Variant_runtime.layout_ref
    | Type_open

  and label_declaration = {
    ld_id: Ident.t;
    ld_runtime_name: string option;
    ld_mutable: Asttypes.mutable_flag;
    ld_optional: bool;
    ld_type: node;
    ld_loc: Location.t;
    ld_attributes: Parsetree.attributes;
  }

  and constructor_declaration = {
    cd_id: Ident.t;
    cd_runtime_tag: Variant_runtime.literal_tag option;
    cd_args: constructor_arguments;
    cd_res: node option;
    cd_loc: Location.t;
    cd_attributes: Parsetree.attributes;
  }

  and constructor_arguments =
    | Cstr_tuple of node list
    | Cstr_record of label_declaration list

  type extension_constructor = {
    ext_type_path: Path.t;
    ext_type_params: node list;
    ext_args: constructor_arguments;
    ext_ret_type: node option;
    ext_private: Asttypes.private_flag;
    ext_loc: Location.t;
    ext_attributes: Parsetree.attributes;
    ext_is_exception: bool;
  }

  type module_type =
    | Mty_ident of Path.t
    | Mty_signature of signature
    | Mty_functor of Ident.t * module_type option * module_type
    | Mty_alias of Types.alias_presence * Path.t

  and signature = signature_item list

  and signature_item =
    | Sig_value of Ident.t * value_description
    | Sig_type of Ident.t * type_declaration * Types.rec_status
    | Sig_typext of Ident.t * extension_constructor * Types.ext_status
    | Sig_module of Ident.t * module_declaration * Types.rec_status
    | Sig_modtype of Ident.t * modtype_declaration

  and module_declaration = {
    md_type: module_type;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
  }

  and modtype_declaration = {
    mtd_type: module_type option;
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
  }
end

(**** Boundaries ****)

(* Physical-identity memo tables. OCaml 5 cannot hash addresses (objects
   move), so blocks are hashed by their contents and compared with [==]:
   correct, but structurally equal distinct blocks share a bucket. The hash
   reads at most three levels and four fields per block: enough to reach the
   immutable [id] of the nodes in a list or an argument, and not far enough
   to reach the contents of an abbreviation cell below a node, which change
   while a conversion marks them (see [cell_mark]). Each table has one key
   type, enforced by the phantom parameters. Immediate values ([[]], [None],
   constant constructors) have no identity and are not memoized. *)
let rec phys_hash depth (r : Obj.t) =
  if Obj.is_int r then (Obj.obj r : int)
  else
    let tag = Obj.tag r in
    if tag >= Obj.no_scan_tag then Hashtbl.hash r
    else
      let size = Obj.size r in
      let h = ref ((tag * 31) + size) in
      if depth > 0 then
        for i = 0 to min size 4 - 1 do
          h := (!h * 65599) + phys_hash (depth - 1) (Obj.field r i)
        done;
      !h land max_int

module Phys = Hashtbl.Make (struct
  type t = Obj.t
  let equal = ( == )
  let hash r = phys_hash 3 r
end)

type ('a, 'b) memo = Obj.t Phys.t

let new_memo () : ('a, 'b) memo = Phys.create 256

let memo (m : ('a, 'b) memo) (f : 'a -> 'b) (x : 'a) : 'b =
  let k = Obj.repr x in
  if Obj.is_int k then f x
  else
    match Phys.find_opt m k with
    | Some v -> Obj.obj v
    | None ->
      let v = f x in
      Phys.add m k (Obj.repr v);
      v

(* Mutable cells may close cycles: register the new cell before filling it. *)
let memo_ref (m : ('a ref, 'b ref) memo) (dummy : 'b) (f : 'a -> 'b)
    (r : 'a ref) : 'b ref =
  let k = Obj.repr r in
  match Phys.find_opt m k with
  | Some v -> Obj.obj v
  | None ->
    let r' = ref dummy in
    Phys.add m k (Obj.repr r');
    r' := f !r;
    r'

(* A list is memoized cell by cell, so shared tails stay shared. *)
let rec memo_list (m : ('a list, 'b list) memo) (f : 'a -> 'b) (l : 'a list) :
    'b list =
  match l with
  | [] -> []
  | _ ->
    memo m
      (function
        | [] -> []
        | x :: rest ->
          let y = f x in
          y :: memo_list m f rest)
      l

(* Same as [memo_list], for the [option] blocks holding converted values. *)
let memo_option (m : ('a option, 'b option) memo) (f : 'a -> 'b) (o : 'a option)
    : 'b option =
  memo m (Option.map f) o

(* Abbreviation cells ([Tconstr]'s [abbrev_memo ref]) are distinct mutable
   blocks which all hash alike (almost all hold [Mnil]), so memoizing them,
   and the [Tconstr] descriptors holding them, by structural hash degenerates
   into long bucket scans. Instead, while a conversion runs, each converted
   cell temporarily holds the marker [Mlink k], where [k] is an immediate
   integer (no real cell is immediate) naming the cell's entry; when the
   conversion session ends, the cells get their contents back. The marked
   graph must not be read by anyone else meanwhile, as with the [Tsubst]
   marks of a copy. *)
type ('d, 'd2, 'm, 'm2) cell_mark = {
  owner: 'd;
  mutable mirror: 'd2;
  mutable finished: bool;
  cell: 'm ref;
  saved: 'm;
  mirror_cell: 'm2 ref;
}

type ('d, 'd2, 'm, 'm2) cell_marks = {
  mutable marks: ('d, 'd2, 'm, 'm2) cell_mark array;
  mutable n_marks: int;
  mutable closed: bool;
}

let new_cell_marks () = {marks = [||]; n_marks = 0; closed = false}

let push_mark ms m =
  let n = ms.n_marks in
  if n = Array.length ms.marks then (
    let a = Array.make (max 64 (2 * n)) m in
    Array.blit ms.marks 0 a 0 n;
    ms.marks <- a);
  ms.marks.(n) <- m;
  ms.n_marks <- n + 1;
  n

(* The marker for entry [k] ([Mlink] of an immediate), and the entry a
   cell's contents name, or -1. *)
let boxed_marker (k : int) = Types.Mlink (Obj.magic k : Types.abbrev_memo ref)

let boxed_marked = function
  | Types.Mlink r when Obj.is_int (Obj.repr r) -> (Obj.magic r : int)
  | _ -> -1

let arena_marker (k : int) = Mlink (Obj.magic k : abbrev_memo ref)

let arena_marked = function
  | Mlink r when Obj.is_int (Obj.repr r) -> (Obj.magic r : int)
  | _ -> -1

(* Give every marked cell its contents back, at the end of a session. *)
let restore_cells ms =
  for i = 0 to ms.n_marks - 1 do
    let m = ms.marks.(i) in
    m.cell := m.saved
  done;
  ms.marks <- [||];
  ms.n_marks <- 0;
  ms.closed <- true

module Type_hash = Hashtbl.Make (struct
  type t = Types.type_expr
  let equal = ( == )
  let hash = Types.get_id
end)

module Import = struct
  type t = {
    store: store;
    nodes: node Type_hash.t;
    mutable count: int;
    descs: (Types.type_desc, desc) memo;
    lists: (Types.type_expr list, node list) memo;
    args: (Types.arg, arg) memo;
    arg_lists: (Types.arg list, arg list) memo;
    options: (Types.type_expr option, node option) memo;
    abbrevs: (Types.abbrev_memo ref, abbrev_memo ref) memo;
    abbrev_values: (Types.abbrev_memo, abbrev_memo) memo;
    rows: (Types.row_desc, row_desc) memo;
    row_fields: (Types.row_field, row_field) memo;
    row_field_refs: (Types.row_field option ref, row_field option ref) memo;
    row_field_options: (Types.row_field option, row_field option) memo;
    row_pairs: (string * Types.row_field, string * row_field) memo;
    row_pair_lists:
      ((string * Types.row_field) list, (string * row_field) list) memo;
    row_names:
      ((Path.t * Types.type_expr list) option, (Path.t * node list) option) memo;
    row_name_pairs: (Path.t * Types.type_expr list, Path.t * node list) memo;
    (* signatures *)
    sig_items: (Types.signature_item, Sig.signature_item) memo;
    sig_lists: (Types.signature, Sig.signature) memo;
    values: (Types.value_description, Sig.value_description) memo;
    decls: (Types.type_declaration, Sig.type_declaration) memo;
    kinds: (Types.type_kind, Sig.type_kind) memo;
    labels: (Types.label_declaration, Sig.label_declaration) memo;
    label_lists:
      (Types.label_declaration list, Sig.label_declaration list) memo;
    cstrs: (Types.constructor_declaration, Sig.constructor_declaration) memo;
    cstr_lists:
      ( Types.constructor_declaration list,
        Sig.constructor_declaration list )
      memo;
    cstr_args: (Types.constructor_arguments, Sig.constructor_arguments) memo;
    inlined: (Types.type_inlined_type, Sig.type_inlined_type) memo;
    inlined_lists:
      (Types.type_inlined_type list, Sig.type_inlined_type list) memo;
    exts: (Types.extension_constructor, Sig.extension_constructor) memo;
    mtys: (Types.module_type, Sig.module_type) memo;
    mty_options: (Types.module_type option, Sig.module_type option) memo;
    mds: (Types.module_declaration, Sig.module_declaration) memo;
    mtds: (Types.modtype_declaration, Sig.modtype_declaration) memo;
    cells: (Types.type_desc, desc, Types.abbrev_memo, abbrev_memo) cell_marks;
  }

  let create ?store () =
    let store =
      match store with
      | Some s -> s
      | None -> current ()
    in
    {
      store;
      nodes = Type_hash.create 1024;
      count = 0;
      descs = new_memo ();
      lists = new_memo ();
      args = new_memo ();
      arg_lists = new_memo ();
      options = new_memo ();
      abbrevs = new_memo ();
      abbrev_values = new_memo ();
      rows = new_memo ();
      row_fields = new_memo ();
      row_field_refs = new_memo ();
      row_field_options = new_memo ();
      row_pairs = new_memo ();
      row_pair_lists = new_memo ();
      row_names = new_memo ();
      row_name_pairs = new_memo ();
      sig_items = new_memo ();
      sig_lists = new_memo ();
      values = new_memo ();
      decls = new_memo ();
      kinds = new_memo ();
      labels = new_memo ();
      label_lists = new_memo ();
      cstrs = new_memo ();
      cstr_lists = new_memo ();
      cstr_args = new_memo ();
      inlined = new_memo ();
      inlined_lists = new_memo ();
      exts = new_memo ();
      mtys = new_memo ();
      mty_options = new_memo ();
      mds = new_memo ();
      mtds = new_memo ();
      cells = new_cell_marks ();
    }

  let nodes c = c.count

  let rec type_expr c (ty : Types.type_expr) : node =
    match Type_hash.find_opt c.nodes ty with
    | Some t -> t
    | None ->
      (* Allocate first: the descriptor may lead back to this node. *)
      let t =
        newty_in c.store ~level:(Types.get_level ty) ~id:(Types.get_id ty) Tnil
      in
      Type_hash.add c.nodes ty t;
      c.count <- c.count + 1;
      set_desc t (desc c (Types.get_desc ty));
      t

  and type_list c l = memo_list c.lists (type_expr c) l

  and desc c (d : Types.type_desc) : desc =
    match d with
    | Types.Tconstr (p, l, cell) -> tconstr c d p l cell
    | _ -> desc_memo c d

  and tconstr c d p l cell =
    let ms = c.cells in
    let k = boxed_marked !cell in
    if k >= 0 then
      let m = ms.marks.(k) in
      if m.owner == d && m.finished then m.mirror
      else
        (* The cell is shared with another descriptor, or the descriptor
           occurs inside its own subgraph (it is then converted twice). *)
        memo c.descs (fun _ -> Tconstr (p, type_list c l, m.mirror_cell)) d
    else
      let saved = !cell in
      let mirror_cell = ref Mnil in
      let k =
        push_mark ms
          {owner = d; mirror = Tnil; finished = false; cell; saved; mirror_cell}
      in
      cell := boxed_marker k;
      let l = type_list c l in
      mirror_cell := abbrev c saved;
      let m = ms.marks.(k) in
      m.mirror <- Tconstr (p, l, mirror_cell);
      m.finished <- true;
      m.mirror

  and desc_memo c d =
    memo c.descs
      (fun (d : Types.type_desc) ->
        match d with
        | Types.Tvar name -> Tvar name
        | Types.Tarrow (args, ret) ->
          let args = memo_list c.arg_lists (arg c) args in
          Tarrow (args, type_expr c ret)
        | Types.Ttuple l -> Ttuple (type_list c l)
        | Types.Tconstr (p, l, abbrev) ->
          let l = type_list c l in
          Tconstr (p, l, abbrev_ref c abbrev)
        | Types.Tobject t -> Tobject (type_expr c t)
        | Types.Tfield {name; mutability; typ; rest} ->
          let typ = type_expr c typ in
          Tfield {name; mutability; typ; rest = type_expr c rest}
        | Types.Tnil -> Tnil
        | Types.Tlink t -> Tlink (type_expr c t)
        | Types.Tsubst t -> Tsubst (type_expr c t)
        | Types.Tvariant row -> Tvariant (row_desc c row)
        | Types.Tunivar name -> Tunivar name
        | Types.Tpoly (t, l) ->
          let t = type_expr c t in
          Tpoly (t, type_list c l)
        | Types.Tpackage (p, lids, l) -> Tpackage (p, lids, type_list c l))
      d

  and arg c (a : Types.arg) : arg =
    memo c.args
      (fun (a : Types.arg) -> {lbl = a.lbl; typ = type_expr c a.typ})
      a

  and abbrev_ref c r =
    let k = boxed_marked !r in
    if k >= 0 then c.cells.marks.(k).mirror_cell
    else memo_ref c.abbrevs Mnil (abbrev c) r

  and abbrev c (m : Types.abbrev_memo) : abbrev_memo =
    memo c.abbrev_values
      (fun (m : Types.abbrev_memo) ->
        match m with
        | Types.Mnil -> Mnil
        | Types.Mcons (priv, p, t1, t2, rest) ->
          let t1 = type_expr c t1 in
          let t2 = type_expr c t2 in
          Mcons (priv, p, t1, t2, abbrev c rest)
        | Types.Mlink r -> Mlink (abbrev_ref c r))
      m

  and row_desc c (row : Types.row_desc) : row_desc =
    memo c.rows
      (fun (row : Types.row_desc) ->
        let row_fields =
          memo_list c.row_pair_lists
            (memo c.row_pairs (fun (l, f) -> (l, row_field c f)))
            row.row_fields
        in
        let row_more = type_expr c row.row_more in
        let row_name =
          memo_option c.row_names
            (memo c.row_name_pairs (fun (p, l) -> (p, type_list c l)))
            row.row_name
        in
        {
          row_fields;
          row_more;
          row_closed = row.row_closed;
          row_fixed = row.row_fixed;
          row_name;
        })
      row

  and row_field c (f : Types.row_field) : row_field =
    memo c.row_fields
      (fun (f : Types.row_field) ->
        match f with
        | Types.Rpresent o -> Rpresent (memo_option c.options (type_expr c) o)
        | Types.Reither (b1, l, b2, r) ->
          let l = type_list c l in
          Reither
            ( b1,
              l,
              b2,
              memo_ref c.row_field_refs None
                (memo_option c.row_field_options (row_field c))
                r )
        | Types.Rabsent -> Rabsent)
      f

  let type_option c o = memo_option c.options (type_expr c) o

  let rec signature c (sg : Types.signature) : Sig.signature =
    memo_list c.sig_lists (signature_item c) sg

  and signature_item c (item : Types.signature_item) : Sig.signature_item =
    memo c.sig_items
      (fun (item : Types.signature_item) ->
        match item with
        | Types.Sig_value (id, v) -> Sig.Sig_value (id, value_description c v)
        | Types.Sig_type (id, d, rs) ->
          Sig.Sig_type (id, type_declaration c d, rs)
        | Types.Sig_typext (id, e, es) ->
          Sig.Sig_typext (id, extension_constructor c e, es)
        | Types.Sig_module (id, md, rs) ->
          Sig.Sig_module (id, module_declaration c md, rs)
        | Types.Sig_modtype (id, mtd) ->
          Sig.Sig_modtype (id, modtype_declaration c mtd))
      item

  and value_description c (v : Types.value_description) =
    memo c.values
      (fun (v : Types.value_description) : Sig.value_description ->
        {
          val_type = type_expr c v.val_type;
          val_kind = v.val_kind;
          val_loc = v.val_loc;
          val_attributes = v.val_attributes;
        })
      v

  and type_declaration c (d : Types.type_declaration) =
    memo c.decls
      (fun (d : Types.type_declaration) : Sig.type_declaration ->
        let type_params = type_list c d.type_params in
        let type_kind = type_kind c d.type_kind in
        let type_manifest = type_option c d.type_manifest in
        let type_inlined_types =
          memo_list c.inlined_lists (inlined_type c) d.type_inlined_types
        in
        {
          type_params;
          type_arity = d.type_arity;
          type_kind;
          type_private = d.type_private;
          type_manifest;
          type_variance = d.type_variance;
          type_newtype_level = d.type_newtype_level;
          type_loc = d.type_loc;
          type_attributes = d.type_attributes;
          type_immediate = d.type_immediate;
          type_representation = d.type_representation;
          type_shape = d.type_shape;
          type_inlined_types;
        })
      d

  and inlined_type c (i : Types.type_inlined_type) =
    memo c.inlined
      (fun (Types.Record {type_name; labels} : Types.type_inlined_type) ->
        Sig.Record {type_name; labels = label_list c labels})
      i

  and type_kind c (k : Types.type_kind) =
    memo c.kinds
      (fun (k : Types.type_kind) : Sig.type_kind ->
        match k with
        | Types.Type_abstract -> Type_abstract
        | Types.Type_record (labels, repr) ->
          Type_record (label_list c labels, repr)
        | Types.Type_variant (cstrs, layout) ->
          Type_variant
            (memo_list c.cstr_lists (constructor_declaration c) cstrs, layout)
        | Types.Type_open -> Type_open)
      k

  and label_list c l = memo_list c.label_lists (label_declaration c) l

  and label_declaration c (l : Types.label_declaration) =
    memo c.labels
      (fun (l : Types.label_declaration) : Sig.label_declaration ->
        {
          ld_id = l.ld_id;
          ld_runtime_name = l.ld_runtime_name;
          ld_mutable = l.ld_mutable;
          ld_optional = l.ld_optional;
          ld_type = type_expr c l.ld_type;
          ld_loc = l.ld_loc;
          ld_attributes = l.ld_attributes;
        })
      l

  and constructor_arguments c (a : Types.constructor_arguments) =
    memo c.cstr_args
      (fun (a : Types.constructor_arguments) : Sig.constructor_arguments ->
        match a with
        | Types.Cstr_tuple l -> Cstr_tuple (type_list c l)
        | Types.Cstr_record l -> Cstr_record (label_list c l))
      a

  and constructor_declaration c (cd : Types.constructor_declaration) =
    memo c.cstrs
      (fun (cd : Types.constructor_declaration) : Sig.constructor_declaration ->
        let cd_args = constructor_arguments c cd.cd_args in
        {
          cd_id = cd.cd_id;
          cd_runtime_tag = cd.cd_runtime_tag;
          cd_args;
          cd_res = type_option c cd.cd_res;
          cd_loc = cd.cd_loc;
          cd_attributes = cd.cd_attributes;
        })
      cd

  and extension_constructor c (e : Types.extension_constructor) =
    memo c.exts
      (fun (e : Types.extension_constructor) : Sig.extension_constructor ->
        let ext_type_params = type_list c e.ext_type_params in
        let ext_args = constructor_arguments c e.ext_args in
        {
          ext_type_path = e.ext_type_path;
          ext_type_params;
          ext_args;
          ext_ret_type = type_option c e.ext_ret_type;
          ext_private = e.ext_private;
          ext_loc = e.ext_loc;
          ext_attributes = e.ext_attributes;
          ext_is_exception = e.ext_is_exception;
        })
      e

  and module_type c (mty : Types.module_type) =
    memo c.mtys
      (fun (mty : Types.module_type) : Sig.module_type ->
        match mty with
        | Types.Mty_ident p -> Mty_ident p
        | Types.Mty_signature sg -> Mty_signature (signature c sg)
        | Types.Mty_functor (id, arg, res) ->
          let arg = memo_option c.mty_options (module_type c) arg in
          Mty_functor (id, arg, module_type c res)
        | Types.Mty_alias (pres, p) -> Mty_alias (pres, p))
      mty

  and module_declaration c (md : Types.module_declaration) =
    memo c.mds
      (fun (md : Types.module_declaration) : Sig.module_declaration ->
        {
          md_type = module_type c md.md_type;
          md_attributes = md.md_attributes;
          md_loc = md.md_loc;
        })
      md

  and modtype_declaration c (mtd : Types.modtype_declaration) =
    memo c.mtds
      (fun (mtd : Types.modtype_declaration) : Sig.modtype_declaration ->
        {
          mtd_type = memo_option c.mty_options (module_type c) mtd.mtd_type;
          mtd_attributes = mtd.mtd_attributes;
          mtd_loc = mtd.mtd_loc;
        })
      mtd

  let check c =
    if c.cells.closed then invalid_arg "Type_arena.Import: session has ended"

  let type_expr c t =
    check c;
    type_expr c t

  let signature c sg =
    check c;
    signature c sg

  let run ?store f =
    let c = create ?store () in
    Fun.protect ~finally:(fun () -> restore_cells c.cells) (fun () -> f c)
end

module Export = struct
  (* Nodes are memoized by index in a per-store column, the other blocks by
     physical identity. *)
  type t = {
    mutable node_stores: (store * Types.type_expr option array) list;
    descs: (desc, Types.type_desc) memo;
    lists: (node list, Types.type_expr list) memo;
    args: (arg, Types.arg) memo;
    arg_lists: (arg list, Types.arg list) memo;
    options: (node option, Types.type_expr option) memo;
    abbrevs: (abbrev_memo ref, Types.abbrev_memo ref) memo;
    abbrev_values: (abbrev_memo, Types.abbrev_memo) memo;
    rows: (row_desc, Types.row_desc) memo;
    row_fields: (row_field, Types.row_field) memo;
    row_field_refs: (row_field option ref, Types.row_field option ref) memo;
    row_field_options: (row_field option, Types.row_field option) memo;
    row_pairs: (string * row_field, string * Types.row_field) memo;
    row_pair_lists:
      ((string * row_field) list, (string * Types.row_field) list) memo;
    row_names:
      ((Path.t * node list) option, (Path.t * Types.type_expr list) option) memo;
    row_name_pairs: (Path.t * node list, Path.t * Types.type_expr list) memo;
    sig_items: (Sig.signature_item, Types.signature_item) memo;
    sig_lists: (Sig.signature, Types.signature) memo;
    values: (Sig.value_description, Types.value_description) memo;
    decls: (Sig.type_declaration, Types.type_declaration) memo;
    kinds: (Sig.type_kind, Types.type_kind) memo;
    labels: (Sig.label_declaration, Types.label_declaration) memo;
    label_lists:
      (Sig.label_declaration list, Types.label_declaration list) memo;
    cstrs: (Sig.constructor_declaration, Types.constructor_declaration) memo;
    cstr_lists:
      ( Sig.constructor_declaration list,
        Types.constructor_declaration list )
      memo;
    cstr_args: (Sig.constructor_arguments, Types.constructor_arguments) memo;
    inlined: (Sig.type_inlined_type, Types.type_inlined_type) memo;
    inlined_lists:
      (Sig.type_inlined_type list, Types.type_inlined_type list) memo;
    exts: (Sig.extension_constructor, Types.extension_constructor) memo;
    mtys: (Sig.module_type, Types.module_type) memo;
    mty_options: (Sig.module_type option, Types.module_type option) memo;
    mds: (Sig.module_declaration, Types.module_declaration) memo;
    mtds: (Sig.modtype_declaration, Types.modtype_declaration) memo;
    cells: (desc, Types.type_desc, abbrev_memo, Types.abbrev_memo) cell_marks;
  }

  let create () =
    {
      node_stores = [];
      descs = new_memo ();
      lists = new_memo ();
      args = new_memo ();
      arg_lists = new_memo ();
      options = new_memo ();
      abbrevs = new_memo ();
      abbrev_values = new_memo ();
      rows = new_memo ();
      row_fields = new_memo ();
      row_field_refs = new_memo ();
      row_field_options = new_memo ();
      row_pairs = new_memo ();
      row_pair_lists = new_memo ();
      row_names = new_memo ();
      row_name_pairs = new_memo ();
      sig_items = new_memo ();
      sig_lists = new_memo ();
      values = new_memo ();
      decls = new_memo ();
      kinds = new_memo ();
      labels = new_memo ();
      label_lists = new_memo ();
      cstrs = new_memo ();
      cstr_lists = new_memo ();
      cstr_args = new_memo ();
      inlined = new_memo ();
      inlined_lists = new_memo ();
      exts = new_memo ();
      mtys = new_memo ();
      mty_options = new_memo ();
      mds = new_memo ();
      mtds = new_memo ();
      cells = new_cell_marks ();
    }

  (* The memo column for [s], grown to the store's current length. *)
  let column c s =
    let rec find = function
      | [] -> None
      | (s', col) :: _ when s' == s -> Some col
      | _ :: rest -> find rest
    in
    let fresh old =
      let col = Array.make (max 16 (2 * s.length)) None in
      Array.blit old 0 col 0 (Array.length old);
      c.node_stores <-
        (s, col) :: List.filter (fun (s', _) -> s' != s) c.node_stores;
      col
    in
    match find c.node_stores with
    | Some col when Array.length col >= s.length -> col
    | Some col -> fresh col
    | None -> fresh [||]

  let rec type_expr c (t : node) : Types.type_expr =
    let s = store_of t and i = index t in
    let col = column c s in
    match col.(i) with
    | Some ty -> ty
    | None ->
      let ty =
        Types.Transient_expr.create Types.Tnil ~level:(get_level t)
          ~id:(get_id t)
      in
      col.(i) <- Some ty;
      Types.Transient_expr.set_desc ty (desc c (get_desc t));
      ty

  and type_list c l = memo_list c.lists (type_expr c) l

  and desc c (d : desc) : Types.type_desc =
    (* [Tlink] and [Tsubst] are rebuilt by [get_desc] on every read, so they
       have no identity of their own: their boxed blocks are not shared. *)
    match d with
    | Tlink t -> Types.Tlink (type_expr c t)
    | Tsubst t -> Types.Tsubst (type_expr c t)
    | Tconstr (p, l, cell) -> tconstr c d p l cell
    | _ -> desc_memo c d

  and tconstr c d p l cell =
    let ms = c.cells in
    let k = arena_marked !cell in
    if k >= 0 then
      let m = ms.marks.(k) in
      if m.owner == d && m.finished then m.mirror
      else
        memo c.descs
          (fun _ -> Types.Tconstr (p, type_list c l, m.mirror_cell))
          d
    else
      let saved = !cell in
      let mirror_cell = ref Types.Mnil in
      let k =
        push_mark ms
          {
            owner = d;
            mirror = Types.Tnil;
            finished = false;
            cell;
            saved;
            mirror_cell;
          }
      in
      cell := arena_marker k;
      let l = type_list c l in
      mirror_cell := abbrev c saved;
      let m = ms.marks.(k) in
      m.mirror <- Types.Tconstr (p, l, mirror_cell);
      m.finished <- true;
      m.mirror

  and desc_memo c d =
    memo c.descs
      (fun (d : desc) : Types.type_desc ->
        match d with
        | Tvar name -> Types.Tvar name
        | Tarrow (args, ret) ->
          let args = memo_list c.arg_lists (arg c) args in
          Types.Tarrow (args, type_expr c ret)
        | Ttuple l -> Types.Ttuple (type_list c l)
        | Tconstr (p, l, abbrev) ->
          let l = type_list c l in
          Types.Tconstr (p, l, abbrev_ref c abbrev)
        | Tobject t -> Types.Tobject (type_expr c t)
        | Tfield {name; mutability; typ; rest} ->
          let typ = type_expr c typ in
          Types.Tfield {name; mutability; typ; rest = type_expr c rest}
        | Tnil -> Types.Tnil
        | Tlink t -> Types.Tlink (type_expr c t)
        | Tsubst t -> Types.Tsubst (type_expr c t)
        | Tvariant row -> Types.Tvariant (row_desc c row)
        | Tunivar name -> Types.Tunivar name
        | Tpoly (t, l) ->
          let t = type_expr c t in
          Types.Tpoly (t, type_list c l)
        | Tpackage (p, lids, l) -> Types.Tpackage (p, lids, type_list c l))
      d

  and arg c (a : arg) : Types.arg =
    memo c.args
      (fun (a : arg) : Types.arg -> {lbl = a.lbl; typ = type_expr c a.typ})
      a

  and abbrev_ref c r =
    let k = arena_marked !r in
    if k >= 0 then c.cells.marks.(k).mirror_cell
    else memo_ref c.abbrevs Types.Mnil (abbrev c) r

  and abbrev c (m : abbrev_memo) : Types.abbrev_memo =
    memo c.abbrev_values
      (fun (m : abbrev_memo) : Types.abbrev_memo ->
        match m with
        | Mnil -> Types.Mnil
        | Mcons (priv, p, t1, t2, rest) ->
          let t1 = type_expr c t1 in
          let t2 = type_expr c t2 in
          Types.Mcons (priv, p, t1, t2, abbrev c rest)
        | Mlink r -> Types.Mlink (abbrev_ref c r))
      m

  and row_desc c (row : row_desc) : Types.row_desc =
    memo c.rows
      (fun (row : row_desc) : Types.row_desc ->
        let row_fields =
          memo_list c.row_pair_lists
            (memo c.row_pairs (fun (l, f) -> (l, row_field c f)))
            row.row_fields
        in
        let row_more = type_expr c row.row_more in
        let row_name =
          memo_option c.row_names
            (memo c.row_name_pairs (fun (p, l) -> (p, type_list c l)))
            row.row_name
        in
        {
          row_fields;
          row_more;
          row_closed = row.row_closed;
          row_fixed = row.row_fixed;
          row_name;
        })
      row

  and row_field c (f : row_field) : Types.row_field =
    memo c.row_fields
      (fun (f : row_field) : Types.row_field ->
        match f with
        | Rpresent o -> Types.Rpresent (memo_option c.options (type_expr c) o)
        | Reither (b1, l, b2, r) ->
          let l = type_list c l in
          Types.Reither
            ( b1,
              l,
              b2,
              memo_ref c.row_field_refs None
                (memo_option c.row_field_options (row_field c))
                r )
        | Rabsent -> Types.Rabsent)
      f

  let type_option c o = memo_option c.options (type_expr c) o

  let rec signature c (sg : Sig.signature) : Types.signature =
    memo_list c.sig_lists (signature_item c) sg

  and signature_item c (item : Sig.signature_item) : Types.signature_item =
    memo c.sig_items
      (fun (item : Sig.signature_item) : Types.signature_item ->
        match item with
        | Sig.Sig_value (id, v) -> Sig_value (id, value_description c v)
        | Sig.Sig_type (id, d, rs) -> Sig_type (id, type_declaration c d, rs)
        | Sig.Sig_typext (id, e, es) ->
          Sig_typext (id, extension_constructor c e, es)
        | Sig.Sig_module (id, md, rs) ->
          Sig_module (id, module_declaration c md, rs)
        | Sig.Sig_modtype (id, mtd) ->
          Sig_modtype (id, modtype_declaration c mtd))
      item

  and value_description c (v : Sig.value_description) =
    memo c.values
      (fun (v : Sig.value_description) : Types.value_description ->
        {
          val_type = type_expr c v.val_type;
          val_kind = v.val_kind;
          val_loc = v.val_loc;
          val_attributes = v.val_attributes;
        })
      v

  and type_declaration c (d : Sig.type_declaration) =
    memo c.decls
      (fun (d : Sig.type_declaration) : Types.type_declaration ->
        let type_params = type_list c d.type_params in
        let type_kind = type_kind c d.type_kind in
        let type_manifest = type_option c d.type_manifest in
        let type_inlined_types =
          memo_list c.inlined_lists (inlined_type c) d.type_inlined_types
        in
        {
          type_params;
          type_arity = d.type_arity;
          type_kind;
          type_private = d.type_private;
          type_manifest;
          type_variance = d.type_variance;
          type_newtype_level = d.type_newtype_level;
          type_loc = d.type_loc;
          type_attributes = d.type_attributes;
          type_immediate = d.type_immediate;
          type_representation = d.type_representation;
          type_shape = d.type_shape;
          type_inlined_types;
        })
      d

  and inlined_type c (i : Sig.type_inlined_type) =
    memo c.inlined
      (fun (Sig.Record {type_name; labels} : Sig.type_inlined_type) ->
        Types.Record {type_name; labels = label_list c labels})
      i

  and type_kind c (k : Sig.type_kind) =
    memo c.kinds
      (fun (k : Sig.type_kind) : Types.type_kind ->
        match k with
        | Sig.Type_abstract -> Type_abstract
        | Sig.Type_record (labels, repr) ->
          Type_record (label_list c labels, repr)
        | Sig.Type_variant (cstrs, layout) ->
          Type_variant
            (memo_list c.cstr_lists (constructor_declaration c) cstrs, layout)
        | Sig.Type_open -> Type_open)
      k

  and label_list c l = memo_list c.label_lists (label_declaration c) l

  and label_declaration c (l : Sig.label_declaration) =
    memo c.labels
      (fun (l : Sig.label_declaration) : Types.label_declaration ->
        {
          ld_id = l.ld_id;
          ld_runtime_name = l.ld_runtime_name;
          ld_mutable = l.ld_mutable;
          ld_optional = l.ld_optional;
          ld_type = type_expr c l.ld_type;
          ld_loc = l.ld_loc;
          ld_attributes = l.ld_attributes;
        })
      l

  and constructor_arguments c (a : Sig.constructor_arguments) =
    memo c.cstr_args
      (fun (a : Sig.constructor_arguments) : Types.constructor_arguments ->
        match a with
        | Sig.Cstr_tuple l -> Cstr_tuple (type_list c l)
        | Sig.Cstr_record l -> Cstr_record (label_list c l))
      a

  and constructor_declaration c (cd : Sig.constructor_declaration) =
    memo c.cstrs
      (fun (cd : Sig.constructor_declaration) : Types.constructor_declaration ->
        let cd_args = constructor_arguments c cd.cd_args in
        {
          cd_id = cd.cd_id;
          cd_runtime_tag = cd.cd_runtime_tag;
          cd_args;
          cd_res = type_option c cd.cd_res;
          cd_loc = cd.cd_loc;
          cd_attributes = cd.cd_attributes;
        })
      cd

  and extension_constructor c (e : Sig.extension_constructor) =
    memo c.exts
      (fun (e : Sig.extension_constructor) : Types.extension_constructor ->
        let ext_type_params = type_list c e.ext_type_params in
        let ext_args = constructor_arguments c e.ext_args in
        {
          ext_type_path = e.ext_type_path;
          ext_type_params;
          ext_args;
          ext_ret_type = type_option c e.ext_ret_type;
          ext_private = e.ext_private;
          ext_loc = e.ext_loc;
          ext_attributes = e.ext_attributes;
          ext_is_exception = e.ext_is_exception;
        })
      e

  and module_type c (mty : Sig.module_type) =
    memo c.mtys
      (fun (mty : Sig.module_type) : Types.module_type ->
        match mty with
        | Sig.Mty_ident p -> Mty_ident p
        | Sig.Mty_signature sg -> Mty_signature (signature c sg)
        | Sig.Mty_functor (id, arg, res) ->
          let arg = memo_option c.mty_options (module_type c) arg in
          Mty_functor (id, arg, module_type c res)
        | Sig.Mty_alias (pres, p) -> Mty_alias (pres, p))
      mty

  and module_declaration c (md : Sig.module_declaration) =
    memo c.mds
      (fun (md : Sig.module_declaration) : Types.module_declaration ->
        {
          md_type = module_type c md.md_type;
          md_attributes = md.md_attributes;
          md_loc = md.md_loc;
        })
      md

  and modtype_declaration c (mtd : Sig.modtype_declaration) =
    memo c.mtds
      (fun (mtd : Sig.modtype_declaration) : Types.modtype_declaration ->
        {
          mtd_type = memo_option c.mty_options (module_type c) mtd.mtd_type;
          mtd_attributes = mtd.mtd_attributes;
          mtd_loc = mtd.mtd_loc;
        })
      mtd

  let check c =
    if c.cells.closed then invalid_arg "Type_arena.Export: session has ended"

  let type_expr c t =
    check c;
    type_expr c t

  let signature c sg =
    check c;
    signature c sg

  let run f =
    let c = create () in
    Fun.protect ~finally:(fun () -> restore_cells c.cells) (fun () -> f c)
end
