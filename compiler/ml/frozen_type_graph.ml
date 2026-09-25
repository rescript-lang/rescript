open Types

type frozen_ident = {name: string; stamp: int; flags: int}

type frozen_path =
  | Fident of int
  | Fdot of frozen_path * string * int
  | Fapply of frozen_path * frozen_path

type frozen_row_field =
  | Fpresent of int option
  | Feither of bool * int list * bool * int
  | Fabsent

type frozen_desc =
  | Fvar of string option
  | Farrow of (Asttypes.arg_label * int) list * int
  | Ftuple of int list
  | Fconstr of frozen_path * int list
  | Fobject of int
  | Ffield of string * int * int * int
  | Fnil
  | Fvariant of
      (string * frozen_row_field) list
      * int
      * bool
      * bool
      * (frozen_path * int list) option
  | Funivar of string option
  | Fpoly of int * int list
  | Fpackage of frozen_path * Longident.t list * int list

type frozen_node = {level: int; desc: frozen_desc}

type t = {
  roots: int list;
  requested_identifiers: int list;
  nodes: frozen_node array;
  identifiers: frozen_ident array;
  mutabilities: Asttypes.mutable_flag array;
  row_references: frozen_row_field option array;
}

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
    let requested = identifiers in
    let seen_types = Physical_type.create 128 in
    let seen_idents = Physical_ident.create 32 in
    let seen_mutabilities = Physical_mutability.create 16 in
    let seen_row_references = Physical_row_reference.create 16 in
    let nodes = ref [] in
    let identifiers = ref [] in
    let mutabilities = ref [] in
    let row_references = ref [] in
    let type_count = ref 0 in
    let ident_count = ref 0 in
    let mutability_count = ref 0 in
    let row_reference_count = ref 0 in
    let index_ident id =
      match Physical_ident.find_opt seen_idents id with
      | Some index -> index
      | None ->
        let index = !ident_count in
        incr ident_count;
        Physical_ident.add seen_idents id index;
        identifiers :=
          ( index,
            {
              name = id.Ident.name;
              stamp = id.Ident.stamp;
              flags = id.Ident.flags;
            } )
          :: !identifiers;
        index
    in
    let rec freeze_path = function
      | Path.Pident id -> Fident (index_ident id)
      | Path.Pdot (path, name, position) ->
        Fdot (freeze_path path, name, position)
      | Path.Papply (first, second) ->
        Fapply (freeze_path first, freeze_path second)
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
        let index = !type_count in
        incr type_count;
        Physical_type.add seen_types ty index;
        let desc =
          match ty.desc with
          | Tvar name -> Fvar name
          | Tarrow (arguments, result) ->
            Farrow
              ( List.map
                  (fun argument -> (argument.lbl, freeze_type argument.typ))
                  arguments,
                freeze_type result )
          | Ttuple types -> Ftuple (List.map freeze_type types)
          | Tconstr (path, parameters, memo) ->
            (match !memo with
            | Mnil -> ()
            | Mcons _ | Mlink _ ->
              raise (Unsupported "an active abbreviation memo"));
            Fconstr (freeze_path path, List.map freeze_type parameters)
          | Tobject fields -> Fobject (freeze_type fields)
          | Tfield {name; mutability; typ; rest} ->
            Ffield
              ( name,
                index_mutability mutability,
                freeze_type typ,
                freeze_type rest )
          | Tnil -> Fnil
          | Tvariant row ->
            Fvariant
              ( List.map
                  (fun (label, field) -> (label, freeze_row_field field))
                  row.row_fields,
                freeze_type row.row_more,
                row.row_closed,
                row.row_fixed,
                Option.map
                  (fun (path, parameters) ->
                    (freeze_path path, List.map freeze_type parameters))
                  row.row_name )
          | Tunivar name -> Funivar name
          | Tpoly (body, variables) ->
            Fpoly (freeze_type body, List.map freeze_type variables)
          | Tpackage (path, names, types) ->
            Fpackage (freeze_path path, names, List.map freeze_type types)
          | Tlink _ -> raise (Unsupported "a linked type node")
          | Tsubst _ -> raise (Unsupported "an active type copy mark")
        in
        nodes := (index, {level = ty.level; desc}) :: !nodes;
        index
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
    let requested_identifiers = List.map index_ident requested in
    let roots = List.map freeze_type roots in
    let array count entries empty =
      let result = Array.make count empty in
      List.iter (fun (index, value) -> result.(index) <- value) entries;
      result
    in
    Ok
      {
        roots;
        requested_identifiers;
        nodes = array !type_count !nodes {level = 0; desc = Fnil};
        identifiers =
          array !ident_count !identifiers {name = ""; stamp = 0; flags = 0};
        mutabilities = array !mutability_count !mutabilities Asttypes.Immutable;
        row_references = array !row_reference_count !row_references None;
      }
  with Unsupported reason -> Error reason

type view = {type_at: int -> type_expr; identifier_at: int -> Ident.t}

let create_view ?(map_type_path = Fun.id) ?(map_modtype_path = Fun.id) image =
  let identifiers = Array.make (Array.length image.identifiers) None in
  let mutabilities = Array.make (Array.length image.mutabilities) None in
  let row_references = Array.make (Array.length image.row_references) None in
  let nodes = Array.make (Array.length image.nodes) None in
  let get_ident index =
    match identifiers.(index) with
    | Some id -> id
    | None ->
      let {name; stamp; flags} : frozen_ident = image.identifiers.(index) in
      let id = {Ident.name; stamp; flags} in
      identifiers.(index) <- Some id;
      id
  in
  let rec thaw_path = function
    | Fident index -> Path.Pident (get_ident index)
    | Fdot (path, name, position) -> Path.Pdot (thaw_path path, name, position)
    | Fapply (first, second) -> Path.Papply (thaw_path first, thaw_path second)
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
      let {level; desc} = image.nodes.(index) in
      let ty = Btype.newty2 level (Tvar None) in
      nodes.(index) <- Some ty;
      ty.desc <- thaw_desc desc;
      ty
  and thaw_desc = function
    | Fvar name -> Tvar name
    | Farrow (arguments, result) ->
      Tarrow
        ( List.map (fun (lbl, typ) -> Types.{lbl; typ = get typ}) arguments,
          get result )
    | Ftuple types -> Ttuple (List.map get types)
    | Fconstr (path, parameters) ->
      Tconstr (map_type_path (thaw_path path), List.map get parameters, ref Mnil)
    | Fobject fields -> Tobject (get fields)
    | Ffield (name, mutability, typ, rest) ->
      Tfield
        {
          name;
          mutability = get_mutability mutability;
          typ = get typ;
          rest = get rest;
        }
    | Fnil -> Tnil
    | Fvariant (fields, more, closed, fixed, name) ->
      Tvariant
        {
          row_fields =
            List.map
              (fun (label, field) -> (label, thaw_row_field field))
              fields;
          row_more = get more;
          row_closed = closed;
          row_fixed = fixed;
          row_name =
            Option.map
              (fun (path, parameters) ->
                (map_type_path (thaw_path path), List.map get parameters))
              name;
        }
    | Funivar name -> Tunivar name
    | Fpoly (body, variables) -> Tpoly (get body, List.map get variables)
    | Fpackage (path, names, types) ->
      Tpackage (map_modtype_path (thaw_path path), names, List.map get types)
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
        match List.nth_opt image.roots root with
        | Some index -> get index
        | None -> invalid_arg "Frozen_type_graph.type_at");
    identifier_at =
      (fun requested ->
        match List.nth_opt image.requested_identifiers requested with
        | Some index -> get_ident index
        | None -> invalid_arg "Frozen_type_graph.identifier_at");
  }

let type_at view root = view.type_at root

let identifier_at view requested = view.identifier_at requested

let thaw image =
  let view = create_view image in
  List.init (List.length image.roots) (type_at view)

let thaw_root image root = type_at (create_view image) root

let root_count image = List.length image.roots

let node_count image = Array.length image.nodes
