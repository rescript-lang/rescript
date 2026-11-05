(* Conversions between the boxed type graphs of [Types_boxed] (the format of
   [.cmi] and [.cmt] files) and the stores of [Types]. See type_arena.mli and
   rescript-perf-investigation [results/arena/DESIGN.md]. *)

open Types
module B = Types_boxed

(**** Boundaries ****)

(* Physical-identity memo tables. OCaml 5 cannot hash addresses (objects
   move), so blocks are hashed by their contents and compared with [==]:
   correct, but structurally equal distinct blocks share a bucket. The hash
   reads at most three levels and four fields per block: enough to reach the
   immutable [id] of the nodes in a list or an argument, and not far enough
   to reach the contents of an abbreviation cell below a node, which change
   while a conversion marks them (see [cell_mark]). Strings (and other blocks
   without fields) are hashed by their contents above the last level and by
   their size at the last level: a key's own names tell keys apart, the file
   name in every location below them does not. Each table has one key type,
   enforced by the phantom parameters. Immediate values ([[]], [None],
   constant constructors) have no identity and are not memoized. *)
let rec phys_hash depth (r : Obj.t) =
  if Obj.is_int r then (Obj.obj r : int)
  else
    let tag = Obj.tag r in
    if tag >= Obj.no_scan_tag then
      if depth > 0 then Hashtbl.hash r else (tag * 31) + Obj.size r
    else
      let size = Obj.size r in
      let h = ref ((tag * 31) + size) in
      if depth > 0 then
        for i = 0 to Int.min size 4 - 1 do
          h := (!h * 65599) + phys_hash (depth - 1) (Obj.field r i)
        done;
      !h land max_int

module Phys = Hashtbl.Make (struct
  type t = Obj.t
  let equal = ( == )
  let hash r = phys_hash 3 r
end)

type ('a, 'b) memo = Obj.t Phys.t

let new_memo () : ('a, 'b) memo = Phys.create 16

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

(* Memo tables with a hash of the caller's choosing, for keys that a hash of
   their own fields tells apart cheaply (label descriptions, environment
   summaries): the key is stored with its hash. *)
module Keyed = Hashtbl.Make (struct
  type t = Obj.t * int
  let equal ((a : Obj.t), _) ((b : Obj.t), _) = a == b
  let hash ((_ : Obj.t), h) = h
end)

type ('a, 'b) keyed = Obj.t Keyed.t

let new_keyed () : ('a, 'b) keyed = Keyed.create 16

let keyed_find (m : ('a, 'b) keyed) (k : 'a) h : 'b option =
  match Keyed.find_opt m (Obj.repr k, h) with
  | Some v -> Some (Obj.obj v)
  | None -> None

let keyed_add (m : ('a, 'b) keyed) (k : 'a) h (v : 'b) =
  Keyed.add m (Obj.repr k, h) (Obj.repr v)

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
    (* Filled with an immediate: [Array.make] of a large array with a young
       block forces a minor collection first. *)
    let a = Array.make (Int.max 64 (2 * n)) (Obj.magic 0) in
    Array.blit ms.marks 0 a 0 n;
    ms.marks <- a);
  ms.marks.(n) <- m;
  ms.n_marks <- n + 1;
  n

(* The marker for entry [k] ([Mlink] of an immediate), and the entry a
   cell's contents name, or -1. *)
let boxed_marker (k : int) = B.Mlink (Obj.magic k : B.abbrev_memo ref)

let boxed_marked = function
  | B.Mlink r when Obj.is_int (Obj.repr r) -> (Obj.magic r : int)
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
  type t = B.type_expr
  let equal = ( == )
  let hash (t : t) = t.B.id
end)

module Node_hash = Hashtbl.Make (struct
  type t = type_expr
  let equal = ( == )
  let hash = Store.index
end)

module Import = struct
  type t = {
    exact: bool;
    store: Store.t;
    nodes: type_expr Type_hash.t;
    mutable count: int;
    descs: (B.type_desc, type_desc) memo;
    lists: (B.type_expr list, type_expr list) memo;
    args: (B.arg, arg) memo;
    arg_lists: (B.arg list, arg list) memo;
    options: (B.type_expr option, type_expr option) memo;
    abbrevs: (B.abbrev_memo ref, abbrev_memo ref) memo;
    abbrev_values: (B.abbrev_memo, abbrev_memo) memo;
    rows: (B.row_desc, row_desc) memo;
    row_fields: (B.row_field, row_field) memo;
    row_field_refs: (B.row_field option ref, row_field option ref) memo;
    row_field_options: (B.row_field option, row_field option) memo;
    row_pairs: (string * B.row_field, string * row_field) memo;
    row_pair_lists:
      ((string * B.row_field) list, (string * row_field) list) memo;
    row_names:
      ( (Path.t * B.type_expr list) option,
        (Path.t * type_expr list) option )
      memo;
    row_name_pairs: (Path.t * B.type_expr list, Path.t * type_expr list) memo;
    (* signatures *)
    sig_items: (B.signature_item, Types.signature_item) memo;
    sig_lists: (B.signature, Types.signature) memo;
    values: (B.value_description, Types.value_description) memo;
    decls: (B.type_declaration, Types.type_declaration) memo;
    kinds: (B.type_kind, Types.type_kind) memo;
    labels: (B.label_declaration, Types.label_declaration) memo;
    label_lists: (B.label_declaration list, Types.label_declaration list) memo;
    cstrs: (B.constructor_declaration, Types.constructor_declaration) memo;
    cstr_lists:
      (B.constructor_declaration list, Types.constructor_declaration list) memo;
    cstr_args: (B.constructor_arguments, Types.constructor_arguments) memo;
    inlined: (B.type_inlined_type, Types.type_inlined_type) memo;
    inlined_lists:
      (B.type_inlined_type list, Types.type_inlined_type list) memo;
    exts: (B.extension_constructor, Types.extension_constructor) memo;
    mtys: (B.module_type, Types.module_type) memo;
    mty_options: (B.module_type option, Types.module_type option) memo;
    mds: (B.module_declaration, Types.module_declaration) memo;
    mtds: (B.modtype_declaration, Types.modtype_declaration) memo;
    cstr_descs: (B.constructor_description, constructor_description) memo;
    decl_options: (B.type_declaration option, type_declaration option) memo;
    lbl_descs_k: (B.label_description, label_description) keyed;
    lbl_arrays_k: (B.label_description array, label_description array) keyed;
    cells: (B.type_desc, type_desc, B.abbrev_memo, abbrev_memo) cell_marks;
  }

  let create ~exact ?store () =
    let store =
      match store with
      | Some s -> s
      | None -> Store.current ()
    in
    {
      exact;
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
      cstr_descs = new_memo ();
      decl_options = new_memo ();
      lbl_descs_k = new_keyed ();
      lbl_arrays_k = new_keyed ();
      cells = new_cell_marks ();
    }

  (* Outside exact sessions only what identity requires is memoized: nodes,
     label descriptions (whose [lbl_all] holds themselves), abbreviation and
     row field cells. *)
  let memo_if c tbl f x = if c.exact then memo tbl f x else f x

  let list_if c tbl f l = if c.exact then memo_list tbl f l else List.map f l

  let option_if c tbl f o =
    if c.exact then memo_option tbl f o else Option.map f o

  let label_hash (l : B.label_description) =
    (String.length l.lbl_name * 31) + l.lbl_pos + (65599 * l.lbl_res.B.id)

  let nodes c = c.count

  let rec type_expr c (ty : B.type_expr) : type_expr =
    match Type_hash.find_opt c.nodes ty with
    | Some t -> t
    | None ->
      (* Allocate first: the descriptor may lead back to this type_expr. *)
      let t = Store.newty_in c.store Tnil ~level:ty.B.level ~id:ty.B.id in
      Type_hash.add c.nodes ty t;
      c.count <- c.count + 1;
      (match ty.B.desc with
      | B.Tconstr (p, l, {contents = B.Mnil}) ->
        (* An empty abbreviation cell is a cache with nothing in it: the
           node gets its own when one is asked for. *)
        set_constr t p (type_list c l)
      | d -> Transient_expr.set_desc t (desc c d));
      t

  and type_list c l = list_if c c.lists (type_expr c) l

  and desc c (d : B.type_desc) : type_desc =
    match d with
    | B.Tconstr (p, l, cell) -> tconstr c d p l cell
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
        memo_if c c.descs (fun _ -> Tconstr (p, type_list c l, m.mirror_cell)) d
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
    memo_if c c.descs
      (fun (d : B.type_desc) ->
        match d with
        | B.Tvar name -> Tvar name
        | B.Tarrow (args, ret) ->
          let args = list_if c c.arg_lists (arg c) args in
          Tarrow (args, type_expr c ret)
        | B.Ttuple l -> Ttuple (type_list c l)
        | B.Tconstr (p, l, abbrev) ->
          let l = type_list c l in
          Tconstr (p, l, abbrev_ref c abbrev)
        | B.Tobject t -> Tobject (type_expr c t)
        | B.Tfield {name; mutability; typ; rest} ->
          let typ = type_expr c typ in
          Tfield {name; mutability; typ; rest = type_expr c rest}
        | B.Tnil -> Tnil
        | B.Tlink t -> Tlink (type_expr c t)
        | B.Tsubst t -> Tsubst (type_expr c t)
        | B.Tvariant row -> Tvariant (row_desc c row)
        | B.Tunivar name -> Tunivar name
        | B.Tpoly (t, l) ->
          let t = type_expr c t in
          Tpoly (t, type_list c l)
        | B.Tpackage (p, lids, l) -> Tpackage (p, lids, type_list c l))
      d

  and arg c (a : B.arg) : arg =
    memo_if c c.args
      (fun (a : B.arg) -> {lbl = a.lbl; typ = type_expr c a.typ})
      a

  and abbrev_ref c r =
    let k = boxed_marked !r in
    if k >= 0 then c.cells.marks.(k).mirror_cell
    else memo_ref c.abbrevs Mnil (abbrev c) r

  and abbrev c (m : B.abbrev_memo) : abbrev_memo =
    memo_if c c.abbrev_values
      (fun (m : B.abbrev_memo) ->
        match m with
        | B.Mnil -> Mnil
        | B.Mcons (priv, p, t1, t2, rest) ->
          let t1 = type_expr c t1 in
          let t2 = type_expr c t2 in
          Mcons (priv, p, t1, t2, abbrev c rest)
        | B.Mlink r -> Mlink (abbrev_ref c r))
      m

  and row_desc c (row : B.row_desc) : row_desc =
    memo_if c c.rows
      (fun (row : B.row_desc) ->
        let row_fields =
          list_if c c.row_pair_lists
            (memo_if c c.row_pairs (fun (l, f) -> (l, row_field c f)))
            row.row_fields
        in
        let row_more = type_expr c row.row_more in
        let row_name =
          option_if c c.row_names
            (memo_if c c.row_name_pairs (fun (p, l) -> (p, type_list c l)))
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

  and row_field c (f : B.row_field) : row_field =
    memo_if c c.row_fields
      (fun (f : B.row_field) ->
        match f with
        | B.Rpresent o -> Rpresent (option_if c c.options (type_expr c) o)
        | B.Reither (b1, l, b2, r) ->
          let l = type_list c l in
          Reither
            ( b1,
              l,
              b2,
              memo_ref c.row_field_refs None
                (option_if c c.row_field_options (row_field c))
                r )
        | B.Rabsent -> Rabsent)
      f

  let type_option c o = option_if c c.options (type_expr c) o

  let rec signature c (sg : B.signature) : Types.signature =
    list_if c c.sig_lists (signature_item c) sg

  and signature_item c (item : B.signature_item) : Types.signature_item =
    memo_if c c.sig_items
      (fun (item : B.signature_item) ->
        match item with
        | B.Sig_value (id, v) -> Types.Sig_value (id, value_description c v)
        | B.Sig_type (id, d, rs) -> Types.Sig_type (id, type_declaration c d, rs)
        | B.Sig_typext (id, e, es) ->
          Types.Sig_typext (id, extension_constructor c e, es)
        | B.Sig_module (id, md, rs) ->
          Types.Sig_module (id, module_declaration c md, rs)
        | B.Sig_modtype (id, mtd) ->
          Types.Sig_modtype (id, modtype_declaration c mtd))
      item

  and value_description c (v : B.value_description) =
    memo_if c c.values
      (fun (v : B.value_description) : Types.value_description ->
        {
          val_type = type_expr c v.val_type;
          val_kind = v.val_kind;
          val_loc = v.val_loc;
          val_attributes = v.val_attributes;
        })
      v

  and type_declaration c (d : B.type_declaration) =
    memo_if c c.decls
      (fun (d : B.type_declaration) : Types.type_declaration ->
        let type_params = type_list c d.type_params in
        let type_kind = type_kind c d.type_kind in
        let type_manifest = type_option c d.type_manifest in
        let type_inlined_types =
          list_if c c.inlined_lists (inlined_type c) d.type_inlined_types
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

  and inlined_type c (i : B.type_inlined_type) =
    memo_if c c.inlined
      (fun (B.Record {type_name; labels} : B.type_inlined_type) ->
        Types.Record {type_name; labels = label_list c labels})
      i

  and type_kind c (k : B.type_kind) =
    memo_if c c.kinds
      (fun (k : B.type_kind) : Types.type_kind ->
        match k with
        | B.Type_abstract -> Type_abstract
        | B.Type_record (labels, repr) -> Type_record (label_list c labels, repr)
        | B.Type_variant (cstrs, layout) ->
          Type_variant
            (list_if c c.cstr_lists (constructor_declaration c) cstrs, layout)
        | B.Type_open -> Type_open)
      k

  and label_list c l = list_if c c.label_lists (label_declaration c) l

  and label_declaration c (l : B.label_declaration) =
    memo_if c c.labels
      (fun (l : B.label_declaration) : Types.label_declaration ->
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

  and constructor_arguments c (a : B.constructor_arguments) =
    memo_if c c.cstr_args
      (fun (a : B.constructor_arguments) : Types.constructor_arguments ->
        match a with
        | B.Cstr_tuple l -> Cstr_tuple (type_list c l)
        | B.Cstr_record l -> Cstr_record (label_list c l))
      a

  and constructor_declaration c (cd : B.constructor_declaration) =
    memo_if c c.cstrs
      (fun (cd : B.constructor_declaration) : Types.constructor_declaration ->
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

  and extension_constructor c (e : B.extension_constructor) =
    memo_if c c.exts
      (fun (e : B.extension_constructor) : Types.extension_constructor ->
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

  and module_type c (mty : B.module_type) =
    memo_if c c.mtys
      (fun (mty : B.module_type) : Types.module_type ->
        match mty with
        | B.Mty_ident p -> Mty_ident p
        | B.Mty_signature sg -> Mty_signature (signature c sg)
        | B.Mty_functor (id, arg, res) ->
          let arg = option_if c c.mty_options (module_type c) arg in
          Mty_functor (id, arg, module_type c res)
        | B.Mty_alias (pres, p) -> Mty_alias (pres, p))
      mty

  and module_declaration c (md : B.module_declaration) =
    memo_if c c.mds
      (fun (md : B.module_declaration) : Types.module_declaration ->
        {
          md_type = module_type c md.md_type;
          md_attributes = md.md_attributes;
          md_loc = md.md_loc;
        })
      md

  and modtype_declaration c (mtd : B.modtype_declaration) =
    memo_if c c.mtds
      (fun (mtd : B.modtype_declaration) : Types.modtype_declaration ->
        {
          mtd_type = option_if c c.mty_options (module_type c) mtd.mtd_type;
          mtd_attributes = mtd.mtd_attributes;
          mtd_loc = mtd.mtd_loc;
        })
      mtd

  and constructor_description c (cd : B.constructor_description) =
    memo_if c c.cstr_descs
      (fun (cd : B.constructor_description) : constructor_description ->
        let cstr_res = type_expr c cd.cstr_res in
        let cstr_existentials = type_list c cd.cstr_existentials in
        let cstr_args = type_list c cd.cstr_args in
        let cstr_inlined =
          option_if c c.decl_options (type_declaration c) cd.cstr_inlined
        in
        {
          cstr_name = cd.cstr_name;
          cstr_res;
          cstr_existentials;
          cstr_args;
          cstr_arity = cd.cstr_arity;
          cstr_kind = cd.cstr_kind;
          cstr_generalized = cd.cstr_generalized;
          cstr_private = cd.cstr_private;
          cstr_loc = cd.cstr_loc;
          cstr_attributes = cd.cstr_attributes;
          cstr_inlined;
        })
      cd

  (* [lbl_all] holds the label itself: register the label, and then its
     converted [lbl_all], before converting its siblings. *)
  and label_description c (l : B.label_description) : label_description =
    let h = label_hash l in
    match keyed_find c.lbl_descs_k l h with
    | Some l' -> l'
    | None ->
      let lbl_res = type_expr c l.lbl_res in
      let lbl_arg = type_expr c l.lbl_arg in
      let l' =
        {
          lbl_name = l.lbl_name;
          lbl_runtime_name = l.lbl_runtime_name;
          lbl_res;
          lbl_arg;
          lbl_mut = l.lbl_mut;
          lbl_optional = l.lbl_optional;
          lbl_pos = l.lbl_pos;
          lbl_all = [||];
          lbl_repres = l.lbl_repres;
          lbl_private = l.lbl_private;
          lbl_loc = l.lbl_loc;
          lbl_attributes = l.lbl_attributes;
        }
      in
      keyed_add c.lbl_descs_k l h l';
      let all = l.lbl_all in
      let h_all =
        if Array.length all = 0 then 0
        else Array.length all + (65599 * label_hash all.(0))
      in
      l'.lbl_all <-
        (match keyed_find c.lbl_arrays_k all h_all with
        | Some all' -> all'
        | None ->
          (* Registered before it is filled: each sibling converted here
              finds it, instead of converting the array again. *)
          let all' = Array.make (Array.length all) (Obj.magic 0) in
          keyed_add c.lbl_arrays_k all h_all all';
          Array.iteri (fun i l -> all'.(i) <- label_description c l) all;
          all');
      l'

  let check c =
    if c.cells.closed then invalid_arg "Type_arena: session has ended"

  let checked f c x =
    check c;
    f c x

  let type_expr c = checked type_expr c
  let type_desc c = checked desc c
  let row_desc c = checked row_desc c
  let signature c = checked signature c
  let module_type c = checked module_type c
  let value_description c = checked value_description c
  let type_declaration c = checked type_declaration c
  let extension_constructor c = checked extension_constructor c
  let constructor_description c = checked constructor_description c
  let label_description c = checked label_description c
  let module_declaration c = checked module_declaration c
  let modtype_declaration c = checked modtype_declaration c

  let run ?(exact = true) ?store f =
    let c = create ~exact ?store () in
    Fun.protect ~finally:(fun () -> restore_cells c.cells) (fun () -> f c)
end

module Export = struct
  (* Nodes are memoized in a hash table by node, the other blocks by physical
     identity. *)

  type t = {
    exact: bool;
    nodes: B.type_expr Node_hash.t;
    descs: (type_desc, B.type_desc) memo;
    lists: (type_expr list, B.type_expr list) memo;
    args: (arg, B.arg) memo;
    arg_lists: (arg list, B.arg list) memo;
    options: (type_expr option, B.type_expr option) memo;
    abbrevs: (abbrev_memo ref, B.abbrev_memo ref) memo;
    abbrev_values: (abbrev_memo, B.abbrev_memo) memo;
    rows: (row_desc, B.row_desc) memo;
    row_fields: (row_field, B.row_field) memo;
    row_field_refs: (row_field option ref, B.row_field option ref) memo;
    row_field_options: (row_field option, B.row_field option) memo;
    row_pairs: (string * row_field, string * B.row_field) memo;
    row_pair_lists:
      ((string * row_field) list, (string * B.row_field) list) memo;
    row_names:
      ( (Path.t * type_expr list) option,
        (Path.t * B.type_expr list) option )
      memo;
    row_name_pairs: (Path.t * type_expr list, Path.t * B.type_expr list) memo;
    sig_items: (Types.signature_item, B.signature_item) memo;
    sig_lists: (Types.signature, B.signature) memo;
    values: (Types.value_description, B.value_description) memo;
    decls: (Types.type_declaration, B.type_declaration) memo;
    kinds: (Types.type_kind, B.type_kind) memo;
    labels: (Types.label_declaration, B.label_declaration) memo;
    label_lists: (Types.label_declaration list, B.label_declaration list) memo;
    cstrs: (Types.constructor_declaration, B.constructor_declaration) memo;
    cstr_lists:
      (Types.constructor_declaration list, B.constructor_declaration list) memo;
    cstr_args: (Types.constructor_arguments, B.constructor_arguments) memo;
    inlined: (Types.type_inlined_type, B.type_inlined_type) memo;
    inlined_lists:
      (Types.type_inlined_type list, B.type_inlined_type list) memo;
    exts: (Types.extension_constructor, B.extension_constructor) memo;
    mtys: (Types.module_type, B.module_type) memo;
    mty_options: (Types.module_type option, B.module_type option) memo;
    mds: (Types.module_declaration, B.module_declaration) memo;
    mtds: (Types.modtype_declaration, B.modtype_declaration) memo;
    cstr_descs: (constructor_description, B.constructor_description) memo;
    decl_options: (type_declaration option, B.type_declaration option) memo;
    lbl_descs_k: (label_description, B.label_description) keyed;
    lbl_arrays_k: (label_description array, B.label_description array) keyed;
    cells: (type_desc, B.type_desc, abbrev_memo, B.abbrev_memo) cell_marks;
  }

  let create ~exact =
    {
      exact;
      nodes = Node_hash.create 1024;
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
      cstr_descs = new_memo ();
      decl_options = new_memo ();
      lbl_descs_k = new_keyed ();
      lbl_arrays_k = new_keyed ();
      cells = new_cell_marks ();
    }

  (* Outside exact sessions only what identity requires is memoized: nodes,
     label descriptions (whose [lbl_all] holds themselves), abbreviation and
     row field cells. *)
  let memo_if c tbl f x = if c.exact then memo tbl f x else f x

  let list_if c tbl f l = if c.exact then memo_list tbl f l else List.map f l

  let option_if c tbl f o =
    if c.exact then memo_option tbl f o else Option.map f o

  let label_hash (l : label_description) =
    (String.length l.lbl_name * 31) + l.lbl_pos + (65599 * Store.index l.lbl_res)

  let rec type_expr c (t : type_expr) : B.type_expr =
    match Node_hash.find_opt c.nodes t with
    | Some ty -> ty
    | None ->
      let ty = {B.desc = B.Tnil; level = get_level t; id = get_id t} in
      Node_hash.add c.nodes t ty;
      ty.B.desc <- node_desc c t;
      ty

  (* [desc c (get_desc t)], reading the encoded descriptors without a view.
     Their views are fresh blocks, which no memo table finds again, so
     building the boxed descriptor directly is the same. A [Tconstr] whose
     abbreviation cell was never asked for has a cell holding [Mnil] that
     nothing else shares. *)
  and node_desc c t : B.type_desc =
    match kind t with
    | Kconstr when not (has_abbrev_cell t) ->
      B.Tconstr (constr_path t, constr_args c t 0 (constr_length t), ref B.Mnil)
    | Karrow ->
      let args = arrow_args c t 0 (arrow_arity t) in
      B.Tarrow (args, type_expr c (arrow_ret t))
    | Ktuple -> B.Ttuple (tuple_elems c t 0 (tuple_length t))
    | Knil -> B.Tnil
    | Kobject -> B.Tobject (type_expr c (object_fields t))
    | Kpoly ->
      let body = type_expr c (poly_body t) in
      B.Tpoly (body, poly_vars c t 0 (poly_length t))
    | _ -> desc c (get_desc t)

  and constr_args c t i n =
    if i = n then []
    else
      let x = type_expr c (constr_arg t i) in
      x :: constr_args c t (i + 1) n

  and tuple_elems c t i n =
    if i = n then []
    else
      let x = type_expr c (tuple_elem t i) in
      x :: tuple_elems c t (i + 1) n

  and poly_vars c t i n =
    if i = n then []
    else
      let x = type_expr c (poly_var t i) in
      x :: poly_vars c t (i + 1) n

  and arrow_args c t i n : B.arg list =
    if i = n then []
    else
      let x : B.arg =
        {lbl = arrow_label t i; typ = type_expr c (arrow_arg t i)}
      in
      x :: arrow_args c t (i + 1) n

  and type_list c l = list_if c c.lists (type_expr c) l

  and desc c (d : type_desc) : B.type_desc =
    (* [Tlink] and [Tsubst] are rebuilt by [get_desc] on every read, so they
       have no identity of their own: their boxed blocks are not shared. *)
    match d with
    | Tlink t -> B.Tlink (type_expr c t)
    | Tsubst t -> B.Tsubst (type_expr c t)
    | Tconstr (p, l, cell) -> tconstr c d p l cell
    | _ -> desc_memo c d

  and tconstr c d p l cell =
    let ms = c.cells in
    let k = arena_marked !cell in
    if k >= 0 then
      let m = ms.marks.(k) in
      if m.owner == d && m.finished then m.mirror
      else
        memo_if c c.descs
          (fun _ -> B.Tconstr (p, type_list c l, m.mirror_cell))
          d
    else
      let saved = !cell in
      let mirror_cell = ref B.Mnil in
      let k =
        push_mark ms
          {
            owner = d;
            mirror = B.Tnil;
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
      m.mirror <- B.Tconstr (p, l, mirror_cell);
      m.finished <- true;
      m.mirror

  and desc_memo c d =
    memo_if c c.descs
      (fun (d : type_desc) : B.type_desc ->
        match d with
        | Tvar name -> B.Tvar name
        | Tarrow (args, ret) ->
          let args = list_if c c.arg_lists (arg c) args in
          B.Tarrow (args, type_expr c ret)
        | Ttuple l -> B.Ttuple (type_list c l)
        | Tconstr (p, l, abbrev) ->
          let l = type_list c l in
          B.Tconstr (p, l, abbrev_ref c abbrev)
        | Tobject t -> B.Tobject (type_expr c t)
        | Tfield {name; mutability; typ; rest} ->
          let typ = type_expr c typ in
          B.Tfield {name; mutability; typ; rest = type_expr c rest}
        | Tnil -> B.Tnil
        | Tlink t -> B.Tlink (type_expr c t)
        | Tsubst t -> B.Tsubst (type_expr c t)
        | Tvariant row -> B.Tvariant (row_desc c row)
        | Tunivar name -> B.Tunivar name
        | Tpoly (t, l) ->
          let t = type_expr c t in
          B.Tpoly (t, type_list c l)
        | Tpackage (p, lids, l) -> B.Tpackage (p, lids, type_list c l))
      d

  and arg c (a : arg) : B.arg =
    memo_if c c.args
      (fun (a : arg) : B.arg -> {lbl = a.lbl; typ = type_expr c a.typ})
      a

  and abbrev_ref c r =
    let k = arena_marked !r in
    if k >= 0 then c.cells.marks.(k).mirror_cell
    else memo_ref c.abbrevs B.Mnil (abbrev c) r

  and abbrev c (m : abbrev_memo) : B.abbrev_memo =
    memo_if c c.abbrev_values
      (fun (m : abbrev_memo) : B.abbrev_memo ->
        match m with
        | Mnil -> B.Mnil
        | Mcons (priv, p, t1, t2, rest) ->
          let t1 = type_expr c t1 in
          let t2 = type_expr c t2 in
          B.Mcons (priv, p, t1, t2, abbrev c rest)
        | Mlink r -> B.Mlink (abbrev_ref c r))
      m

  and row_desc c (row : row_desc) : B.row_desc =
    memo_if c c.rows
      (fun (row : row_desc) : B.row_desc ->
        let row_fields =
          list_if c c.row_pair_lists
            (memo_if c c.row_pairs (fun (l, f) -> (l, row_field c f)))
            row.row_fields
        in
        let row_more = type_expr c row.row_more in
        let row_name =
          option_if c c.row_names
            (memo_if c c.row_name_pairs (fun (p, l) -> (p, type_list c l)))
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

  and row_field c (f : row_field) : B.row_field =
    memo_if c c.row_fields
      (fun (f : row_field) : B.row_field ->
        match f with
        | Rpresent o -> B.Rpresent (option_if c c.options (type_expr c) o)
        | Reither (b1, l, b2, r) ->
          let l = type_list c l in
          B.Reither
            ( b1,
              l,
              b2,
              memo_ref c.row_field_refs None
                (option_if c c.row_field_options (row_field c))
                r )
        | Rabsent -> B.Rabsent)
      f

  let type_option c o = option_if c c.options (type_expr c) o

  let rec signature c (sg : Types.signature) : B.signature =
    list_if c c.sig_lists (signature_item c) sg

  and signature_item c (item : Types.signature_item) : B.signature_item =
    memo_if c c.sig_items
      (fun (item : Types.signature_item) : B.signature_item ->
        match item with
        | Types.Sig_value (id, v) -> Sig_value (id, value_description c v)
        | Types.Sig_type (id, d, rs) -> Sig_type (id, type_declaration c d, rs)
        | Types.Sig_typext (id, e, es) ->
          Sig_typext (id, extension_constructor c e, es)
        | Types.Sig_module (id, md, rs) ->
          Sig_module (id, module_declaration c md, rs)
        | Types.Sig_modtype (id, mtd) ->
          Sig_modtype (id, modtype_declaration c mtd))
      item

  and value_description c (v : Types.value_description) =
    memo_if c c.values
      (fun (v : Types.value_description) : B.value_description ->
        {
          val_type = type_expr c v.val_type;
          val_kind = v.val_kind;
          val_loc = v.val_loc;
          val_attributes = v.val_attributes;
        })
      v

  and type_declaration c (d : Types.type_declaration) =
    memo_if c c.decls
      (fun (d : Types.type_declaration) : B.type_declaration ->
        let type_params = type_list c d.type_params in
        let type_kind = type_kind c d.type_kind in
        let type_manifest = type_option c d.type_manifest in
        let type_inlined_types =
          list_if c c.inlined_lists (inlined_type c) d.type_inlined_types
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
    memo_if c c.inlined
      (fun (Types.Record {type_name; labels} : Types.type_inlined_type) ->
        B.Record {type_name; labels = label_list c labels})
      i

  and type_kind c (k : Types.type_kind) =
    memo_if c c.kinds
      (fun (k : Types.type_kind) : B.type_kind ->
        match k with
        | Types.Type_abstract -> Type_abstract
        | Types.Type_record (labels, repr) ->
          Type_record (label_list c labels, repr)
        | Types.Type_variant (cstrs, layout) ->
          Type_variant
            (list_if c c.cstr_lists (constructor_declaration c) cstrs, layout)
        | Types.Type_open -> Type_open)
      k

  and label_list c l = list_if c c.label_lists (label_declaration c) l

  and label_declaration c (l : Types.label_declaration) =
    memo_if c c.labels
      (fun (l : Types.label_declaration) : B.label_declaration ->
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
    memo_if c c.cstr_args
      (fun (a : Types.constructor_arguments) : B.constructor_arguments ->
        match a with
        | Types.Cstr_tuple l -> Cstr_tuple (type_list c l)
        | Types.Cstr_record l -> Cstr_record (label_list c l))
      a

  and constructor_declaration c (cd : Types.constructor_declaration) =
    memo_if c c.cstrs
      (fun (cd : Types.constructor_declaration) : B.constructor_declaration ->
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
    memo_if c c.exts
      (fun (e : Types.extension_constructor) : B.extension_constructor ->
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
    memo_if c c.mtys
      (fun (mty : Types.module_type) : B.module_type ->
        match mty with
        | Types.Mty_ident p -> Mty_ident p
        | Types.Mty_signature sg -> Mty_signature (signature c sg)
        | Types.Mty_functor (id, arg, res) ->
          let arg = option_if c c.mty_options (module_type c) arg in
          Mty_functor (id, arg, module_type c res)
        | Types.Mty_alias (pres, p) -> Mty_alias (pres, p))
      mty

  and module_declaration c (md : Types.module_declaration) =
    memo_if c c.mds
      (fun (md : Types.module_declaration) : B.module_declaration ->
        {
          md_type = module_type c md.md_type;
          md_attributes = md.md_attributes;
          md_loc = md.md_loc;
        })
      md

  and modtype_declaration c (mtd : Types.modtype_declaration) =
    memo_if c c.mtds
      (fun (mtd : Types.modtype_declaration) : B.modtype_declaration ->
        {
          mtd_type = option_if c c.mty_options (module_type c) mtd.mtd_type;
          mtd_attributes = mtd.mtd_attributes;
          mtd_loc = mtd.mtd_loc;
        })
      mtd

  and constructor_description c (cd : constructor_description) =
    memo_if c c.cstr_descs
      (fun (cd : constructor_description) : B.constructor_description ->
        let cstr_res = type_expr c cd.cstr_res in
        let cstr_existentials = type_list c cd.cstr_existentials in
        let cstr_args = type_list c cd.cstr_args in
        let cstr_inlined =
          option_if c c.decl_options (type_declaration c) cd.cstr_inlined
        in
        {
          B.cstr_name = cd.cstr_name;
          cstr_res;
          cstr_existentials;
          cstr_args;
          cstr_arity = cd.cstr_arity;
          cstr_kind = cd.cstr_kind;
          cstr_generalized = cd.cstr_generalized;
          cstr_private = cd.cstr_private;
          cstr_loc = cd.cstr_loc;
          cstr_attributes = cd.cstr_attributes;
          cstr_inlined;
        })
      cd

  (* [lbl_all] holds the label itself: register the label, and then its
     converted [lbl_all], before converting its siblings. *)
  and label_description c (l : label_description) : B.label_description =
    let h = label_hash l in
    match keyed_find c.lbl_descs_k l h with
    | Some l' -> l'
    | None ->
      let lbl_res = type_expr c l.lbl_res in
      let lbl_arg = type_expr c l.lbl_arg in
      let l' =
        {
          B.lbl_name = l.lbl_name;
          lbl_runtime_name = l.lbl_runtime_name;
          lbl_res;
          lbl_arg;
          lbl_mut = l.lbl_mut;
          lbl_optional = l.lbl_optional;
          lbl_pos = l.lbl_pos;
          lbl_all = [||];
          lbl_repres = l.lbl_repres;
          lbl_private = l.lbl_private;
          lbl_loc = l.lbl_loc;
          lbl_attributes = l.lbl_attributes;
        }
      in
      keyed_add c.lbl_descs_k l h l';
      let all = l.lbl_all in
      let h_all =
        if Array.length all = 0 then 0
        else Array.length all + (65599 * label_hash all.(0))
      in
      l'.lbl_all <-
        (match keyed_find c.lbl_arrays_k all h_all with
        | Some all' -> all'
        | None ->
          (* Registered before it is filled: each sibling converted here
              finds it, instead of converting the array again. *)
          let all' = Array.make (Array.length all) (Obj.magic 0) in
          keyed_add c.lbl_arrays_k all h_all all';
          Array.iteri (fun i l -> all'.(i) <- label_description c l) all;
          all');
      l'

  let check c =
    if c.cells.closed then invalid_arg "Type_arena: session has ended"

  let checked f c x =
    check c;
    f c x

  let type_expr c = checked type_expr c
  let type_desc c = checked desc c
  let row_desc c = checked row_desc c
  let signature c = checked signature c
  let module_type c = checked module_type c
  let value_description c = checked value_description c
  let type_declaration c = checked type_declaration c
  let extension_constructor c = checked extension_constructor c
  let constructor_description c = checked constructor_description c
  let label_description c = checked label_description c
  let module_declaration c = checked module_declaration c
  let modtype_declaration c = checked modtype_declaration c

  let run ?(exact = true) f =
    let c = create ~exact in
    Fun.protect ~finally:(fun () -> restore_cells c.cells) (fun () -> f c)
end

module Memo = struct
  type ('a, 'b) t = {
    hash: ('a -> int) option;
    phys: ('a, 'b) memo;
    keyed: ('a, 'b) keyed;
  }

  let create ?hash () = {hash; phys = new_memo (); keyed = new_keyed ()}

  let memo m f x =
    match m.hash with
    | None -> memo m.phys f x
    | Some _ when Obj.is_int (Obj.repr x) -> f x
    | Some hash -> (
      let h = hash x in
      match keyed_find m.keyed x h with
      | Some v -> v
      | None ->
        let v = f x in
        keyed_add m.keyed x h v;
        v)
end
