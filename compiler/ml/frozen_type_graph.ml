open Types

type frozen_row_field =
  | Fpresent of int option
  | Feither of bool * int list * bool * int
  | Fabsent

type frozen_variant = {
  fields: (int * frozen_row_field) list;
  more: int;
  closed: bool;
  fixed: bool;
  name: (int * int list) option;
}

(* Common nodes use a byte tag, a scalar payload, and a contiguous slice of
   [edges]. Less common row and package data lives in indexed side tables.
   The image retains no mutable type nodes, paths, or identifier records. *)
type t = {
  roots: int array;
  requested_identifiers: int array;
  kinds: bytes;
  levels: int array;
  payloads: int array;
  edge_offsets: int array;
  edges: int array;
  arrow_labels: Asttypes.arg_label array;
  names: string array;
  ident_names: int array;
  ident_stamps: int array;
  ident_flags: int array;
  path_kinds: bytes;
  path_first: int array;
  path_second: int array;
  path_positions: int array;
  field_data: int array;
  variants: frozen_variant array;
  packages: (int * Longident.t list) array;
  mutabilities: Asttypes.mutable_flag array;
  row_references: frozen_row_field option array;
}

module Int_buffer = struct
  type t = {mutable values: int array; mutable length: int}

  let create () = {values = Array.make 16 0; length = 0}

  let reserve buffer count =
    let start = buffer.length in
    let needed = start + count in
    if needed > Array.length buffer.values then (
      let capacity = ref (Array.length buffer.values) in
      while !capacity < needed do
        capacity := 2 * !capacity
      done;
      let grown = Array.make !capacity 0 in
      Array.blit buffer.values 0 grown 0 buffer.length;
      buffer.values <- grown);
    buffer.length <- needed;
    start

  let add buffer value =
    if buffer.length = Array.length buffer.values then (
      let grown = Array.make (2 * buffer.length) 0 in
      Array.blit buffer.values 0 grown 0 buffer.length;
      buffer.values <- grown);
    buffer.values.(buffer.length) <- value;
    buffer.length <- buffer.length + 1

  let set buffer index value = buffer.values.(index) <- value

  let to_array buffer = Array.sub buffer.values 0 buffer.length
end

module Byte_buffer = struct
  type t = {mutable values: bytes; mutable length: int}

  let create () = {values = Bytes.make 16 '\000'; length = 0}

  let add buffer value =
    if buffer.length = Bytes.length buffer.values then (
      let grown = Bytes.make (2 * buffer.length) '\000' in
      Bytes.blit buffer.values 0 grown 0 buffer.length;
      buffer.values <- grown);
    Bytes.set buffer.values buffer.length value;
    buffer.length <- buffer.length + 1

  let set buffer index value = Bytes.set buffer.values index value

  let to_bytes buffer = Bytes.sub buffer.values 0 buffer.length
end

let tag_var = 0
let tag_arrow = 1
let tag_tuple = 2
let tag_constr = 3
let tag_object = 4
let tag_field = 5
let tag_nil = 6
let tag_variant = 7
let tag_univar = 8
let tag_poly = 9
let tag_package = 10

let path_ident = 0
let path_dot = 1
let path_apply = 2

module Physical_type = Hashtbl.Make (struct
  type t = type_expr
  let equal a b = a == b
  let hash ty = ty.id
end)

module Physical_ident = Hashtbl.Make (struct
  type t = Ident.t
  let equal a b = a == b
  let hash id = Hashtbl.hash (id.Ident.stamp, id.Ident.name)
end)

module Physical_mutability = Hashtbl.Make (struct
  type t = field_mutability ref
  let equal a b = a == b
  let hash = Hashtbl.hash
end)

module Physical_row_reference = Hashtbl.Make (struct
  type t = row_field option ref
  let equal a b = a == b
  let hash = Hashtbl.hash
end)

exception Unsupported of string

let freeze ?(identifiers = []) roots =
  try
    let seen_types = Physical_type.create 128 in
    let seen_idents = Physical_ident.create 32 in
    let seen_mutabilities = Physical_mutability.create 16 in
    let seen_row_references = Physical_row_reference.create 16 in
    let seen_names = Hashtbl.create 64 in
    let idents = ref [] in
    let paths = ref [] in
    let names = ref [] in
    let mutabilities = ref [] in
    let row_references = ref [] in
    let kinds = Byte_buffer.create () in
    let levels = Int_buffer.create () in
    let payloads = Int_buffer.create () in
    let edge_offsets = Int_buffer.create () in
    let edges = Int_buffer.create () in
    let arrow_labels = ref [] in
    let arrow_label_count = ref 0 in
    let field_data = Int_buffer.create () in
    let variants = ref [] in
    let packages = ref [] in
    let variant_count = ref 0 in
    let package_count = ref 0 in
    let ident_count = ref 0 in
    let path_count = ref 0 in
    let name_count = ref 0 in
    let mutability_count = ref 0 in
    let row_reference_count = ref 0 in
    let index_name name =
      match Hashtbl.find_opt seen_names name with
      | Some index -> index
      | None ->
        let index = !name_count in
        incr name_count;
        Hashtbl.add seen_names name index;
        names := (index, name) :: !names;
        index
    in
    let index_optional_name = function
      | None -> -1
      | Some name -> index_name name
    in
    let index_ident id =
      match Physical_ident.find_opt seen_idents id with
      | Some index -> index
      | None ->
        let index = !ident_count in
        incr ident_count;
        Physical_ident.add seen_idents id index;
        idents :=
          (index, (index_name id.Ident.name, id.Ident.stamp, id.Ident.flags))
          :: !idents;
        index
    in
    let rec index_path = function
      | Path.Pident id -> add_path path_ident (index_ident id) 0 0
      | Path.Pdot (parent, name, position) ->
        let parent = index_path parent in
        add_path path_dot parent (index_name name) position
      | Path.Papply (first, second) ->
        let first = index_path first in
        let second = index_path second in
        add_path path_apply first second 0
    and add_path kind first second position =
      let index = !path_count in
      incr path_count;
      paths := (index, kind, first, second, position) :: !paths;
      index
    in
    let index_mutability cell =
      let cell = Btype.mutability_ref_repr cell in
      match Physical_mutability.find_opt seen_mutabilities cell with
      | Some index -> index
      | None ->
        let index = !mutability_count in
        incr mutability_count;
        Physical_mutability.add seen_mutabilities cell index;
        mutabilities := (index, Btype.mutability_repr cell) :: !mutabilities;
        index
    in
    let rec freeze_type ty =
      match Physical_type.find_opt seen_types ty with
      | Some index -> index
      | None ->
        let index = levels.length in
        Physical_type.add seen_types ty index;
        Int_buffer.add levels ty.level;
        Byte_buffer.add kinds '\000';
        Int_buffer.add payloads 0;
        Int_buffer.add edge_offsets edges.length;
        let width =
          match ty.desc with
          | Tarrow (arguments, _) -> List.length arguments + 1
          | Ttuple types -> List.length types
          | Tconstr (_, parameters, memo) ->
            (match !memo with
            | Mnil -> ()
            | Mcons _ | Mlink _ ->
              raise (Unsupported "an active abbreviation memo"));
            List.length parameters
          | Tfield _ -> 2
          | Tpoly (_, variables) -> List.length variables
          | Tpackage (_, _, types) -> List.length types
          | Tlink _ -> raise (Unsupported "a linked type node")
          | Tsubst _ -> raise (Unsupported "an active type copy mark")
          | Tvar _ | Tobject _ | Tnil | Tvariant _ | Tunivar _ -> 0
        in
        (* Reserve a node's full edge range before visiting its children. DFS
           then assigns contiguous ranges in node-index order, even for cycles. *)
        let start = Int_buffer.reserve edges width in
        let kind, payload =
          match ty.desc with
          | Tvar name -> (tag_var, index_optional_name name)
          | Tarrow (arguments, result) ->
            let label_start = !arrow_label_count in
            let count = List.length arguments in
            arrow_label_count := label_start + count;
            List.iteri
              (fun offset argument ->
                arrow_labels :=
                  (label_start + offset, argument.lbl) :: !arrow_labels;
                Int_buffer.set edges (start + offset) (freeze_type argument.typ))
              arguments;
            Int_buffer.set edges (start + count) (freeze_type result);
            (tag_arrow, label_start)
          | Ttuple types ->
            freeze_children start types;
            (tag_tuple, 0)
          | Tconstr (path, parameters, _) ->
            let path = index_path path in
            freeze_children start parameters;
            (tag_constr, path)
          | Tobject fields -> (tag_object, freeze_type fields)
          | Tfield {name; mutability; typ; rest} ->
            let field = field_data.length / 2 in
            Int_buffer.add field_data (index_name name);
            Int_buffer.add field_data (index_mutability mutability);
            Int_buffer.set edges start (freeze_type typ);
            Int_buffer.set edges (start + 1) (freeze_type rest);
            (tag_field, field)
          | Tnil -> (tag_nil, 0)
          | Tvariant row ->
            let side = !variant_count in
            incr variant_count;
            let variant =
              {
                fields =
                  List.map
                    (fun (label, field) ->
                      (index_name label, freeze_row_field field))
                    row.row_fields;
                more = freeze_type row.row_more;
                closed = row.row_closed;
                fixed = row.row_fixed;
                name =
                  Option.map
                    (fun (path, parameters) ->
                      (index_path path, List.map freeze_type parameters))
                    row.row_name;
              }
            in
            variants := (side, variant) :: !variants;
            (tag_variant, side)
          | Tunivar name -> (tag_univar, index_optional_name name)
          | Tpoly (body, variables) ->
            let body = freeze_type body in
            freeze_children start variables;
            (tag_poly, body)
          | Tpackage (path, package_names, types) ->
            let side = !package_count in
            incr package_count;
            packages := (side, (index_path path, package_names)) :: !packages;
            freeze_children start types;
            (tag_package, side)
          | Tlink _ | Tsubst _ -> assert false
        in
        Byte_buffer.set kinds index (Char.chr kind);
        Int_buffer.set payloads index payload;
        index
    and freeze_children start types =
      List.iteri
        (fun offset typ ->
          Int_buffer.set edges (start + offset) (freeze_type typ))
        types
    and freeze_row_field = function
      | Rpresent ty -> Fpresent (Option.map freeze_type ty)
      | Reither (constant, types, matched, reference) ->
        Feither
          ( constant,
            List.map freeze_type types,
            matched,
            freeze_row_reference reference )
      | Rabsent -> Fabsent
    and freeze_row_reference reference =
      match Physical_row_reference.find_opt seen_row_references reference with
      | Some index -> index
      | None ->
        let index = !row_reference_count in
        incr row_reference_count;
        Physical_row_reference.add seen_row_references reference index;
        row_references :=
          (index, Option.map freeze_row_field !reference) :: !row_references;
        index
    in
    let requested_identifiers =
      Array.of_list (List.map index_ident identifiers)
    in
    let roots = Array.of_list (List.map freeze_type roots) in
    Int_buffer.add edge_offsets edges.length;
    let array count entries empty =
      let result = Array.make count empty in
      List.iter (fun (index, value) -> result.(index) <- value) entries;
      result
    in
    let ident_names = Array.make !ident_count 0 in
    let ident_stamps = Array.make !ident_count 0 in
    let ident_flags = Array.make !ident_count 0 in
    List.iter
      (fun (index, (name, stamp, flags)) ->
        ident_names.(index) <- name;
        ident_stamps.(index) <- stamp;
        ident_flags.(index) <- flags)
      !idents;
    let path_kinds = Bytes.create !path_count in
    let path_first = Array.make !path_count 0 in
    let path_second = Array.make !path_count 0 in
    let path_positions = Array.make !path_count 0 in
    List.iter
      (fun (index, kind, first, second, position) ->
        Bytes.set path_kinds index (Char.chr kind);
        path_first.(index) <- first;
        path_second.(index) <- second;
        path_positions.(index) <- position)
      !paths;
    Ok
      {
        roots;
        requested_identifiers;
        kinds = Byte_buffer.to_bytes kinds;
        levels = Int_buffer.to_array levels;
        payloads = Int_buffer.to_array payloads;
        edge_offsets = Int_buffer.to_array edge_offsets;
        edges = Int_buffer.to_array edges;
        arrow_labels = array !arrow_label_count !arrow_labels Asttypes.Nolabel;
        names = array !name_count !names "";
        ident_names;
        ident_stamps;
        ident_flags;
        path_kinds;
        path_first;
        path_second;
        path_positions;
        field_data = Int_buffer.to_array field_data;
        variants =
          array !variant_count !variants
            {fields = []; more = 0; closed = false; fixed = false; name = None};
        packages = array !package_count !packages (0, []);
        mutabilities = array !mutability_count !mutabilities Asttypes.Immutable;
        row_references = array !row_reference_count !row_references None;
      }
  with Unsupported reason -> Error reason

type view = {type_at: int -> type_expr; identifier_at: int -> Ident.t}

let create_view ?(map_type_path = Fun.id) ?(map_modtype_path = Fun.id) image =
  let identifiers = Array.make (Array.length image.ident_names) None in
  let mutabilities = Array.make (Array.length image.mutabilities) None in
  let row_references = Array.make (Array.length image.row_references) None in
  let nodes = Array.make (Bytes.length image.kinds) None in
  let get_name index = image.names.(index) in
  let get_optional_name index =
    if index < 0 then None else Some (get_name index)
  in
  let get_ident index =
    match identifiers.(index) with
    | Some id -> id
    | None ->
      let id =
        {
          Ident.name = get_name image.ident_names.(index);
          stamp = image.ident_stamps.(index);
          flags = image.ident_flags.(index);
        }
      in
      identifiers.(index) <- Some id;
      id
  in
  let rec thaw_path index =
    let first = image.path_first.(index) in
    match Char.code (Bytes.get image.path_kinds index) with
    | kind when kind = path_ident -> Path.Pident (get_ident first)
    | kind when kind = path_dot ->
      Path.Pdot
        ( thaw_path first,
          get_name image.path_second.(index),
          image.path_positions.(index) )
    | kind when kind = path_apply ->
      Path.Papply (thaw_path first, thaw_path image.path_second.(index))
    | _ -> assert false
  in
  let get_mutability index =
    match mutabilities.(index) with
    | Some cell -> cell
    | None ->
      let cell = ref (Mutability_value image.mutabilities.(index)) in
      mutabilities.(index) <- Some cell;
      cell
  in
  let rec get index =
    match nodes.(index) with
    | Some ty -> ty
    | None ->
      let ty = Btype.newty2 image.levels.(index) (Tvar None) in
      nodes.(index) <- Some ty;
      ty.desc <- thaw_desc index;
      ty
  and thaw_desc index =
    let payload = image.payloads.(index) in
    let start = image.edge_offsets.(index) in
    let stop = image.edge_offsets.(index + 1) in
    let edge offset = get image.edges.(start + offset) in
    let children () = List.init (stop - start) edge in
    match Char.code (Bytes.get image.kinds index) with
    | kind when kind = tag_var -> Tvar (get_optional_name payload)
    | kind when kind = tag_arrow ->
      let count = stop - start - 1 in
      let arguments =
        List.init count (fun offset ->
            Types.
              {lbl = image.arrow_labels.(payload + offset); typ = edge offset})
      in
      Tarrow (arguments, edge count)
    | kind when kind = tag_tuple -> Ttuple (children ())
    | kind when kind = tag_constr ->
      Tconstr (map_type_path (thaw_path payload), children (), ref Mnil)
    | kind when kind = tag_object -> Tobject (get payload)
    | kind when kind = tag_field ->
      Tfield
        {
          name = get_name image.field_data.(2 * payload);
          mutability = get_mutability image.field_data.((2 * payload) + 1);
          typ = edge 0;
          rest = edge 1;
        }
    | kind when kind = tag_nil -> Tnil
    | kind when kind = tag_variant ->
      let row = image.variants.(payload) in
      Tvariant
        {
          row_fields =
            List.map
              (fun (label, field) -> (get_name label, thaw_row_field field))
              row.fields;
          row_more = get row.more;
          row_closed = row.closed;
          row_fixed = row.fixed;
          row_name =
            Option.map
              (fun (path, parameters) ->
                (map_type_path (thaw_path path), List.map get parameters))
              row.name;
        }
    | kind when kind = tag_univar -> Tunivar (get_optional_name payload)
    | kind when kind = tag_poly -> Tpoly (get payload, children ())
    | kind when kind = tag_package ->
      let path, names = image.packages.(payload) in
      Tpackage (map_modtype_path (thaw_path path), names, children ())
    | _ -> assert false
  and thaw_row_field = function
    | Fpresent ty -> Rpresent (Option.map get ty)
    | Feither (constant, types, matched, reference) ->
      Reither
        (constant, List.map get types, matched, get_row_reference reference)
    | Fabsent -> Rabsent
  and get_row_reference index =
    match row_references.(index) with
    | Some reference -> reference
    | None ->
      let reference = ref None in
      row_references.(index) <- Some reference;
      reference := Option.map thaw_row_field image.row_references.(index);
      reference
  in
  {
    type_at =
      (fun root ->
        if root < 0 || root >= Array.length image.roots then
          invalid_arg "Frozen_type_graph.type_at"
        else get image.roots.(root));
    identifier_at =
      (fun requested ->
        if
          requested < 0 || requested >= Array.length image.requested_identifiers
        then invalid_arg "Frozen_type_graph.identifier_at"
        else get_ident image.requested_identifiers.(requested));
  }

let type_at view root = view.type_at root

let identifier_at view requested = view.identifier_at requested

let thaw image =
  let view = create_view image in
  List.init (Array.length image.roots) (type_at view)

let thaw_root image root = type_at (create_view image) root

let root_count image = Array.length image.roots

let node_count image = Bytes.length image.kinds
