open Types

module String_map = Map.Make (String)
module String_set = Set.Make (String)
module Stamp_map = Map.Make (Int)
module Int_map = Map.Make (Int)

type binder_kind = Bound_type | Bound_module | Bound_modtype
type binder_path = (string * int) list

type value = {
  typ: int;
  kind: string option;
  loc: Location.t;
  attributes: string option;
  position: int;
}

type frozen_label = {
  name: string;
  flags: int;
  runtime_name: string option;
  mutable_flag: Asttypes.mutable_flag;
  optional: bool;
  typ: int;
  loc: Location.t;
  attributes: string option;
}

type frozen_constructor_arguments =
  | Frozen_tuple of int list
  | Frozen_record_arguments of frozen_label list

type frozen_constructor = {
  name: string;
  flags: int;
  runtime_tag: Variant_runtime.literal_tag option;
  args: frozen_constructor_arguments;
  result: int option;
  loc: Location.t;
  attributes: string option;
}

type frozen_extension = {
  name: string;
  position: int;
  type_path: string;
  type_params: int list;
  args: frozen_constructor_arguments;
  ret_type: int option;
  private_flag: Asttypes.private_flag;
  loc: Location.t;
  attributes: string option;
  is_exception: bool;
}

type constructor_source =
  | Variant_source of string
  | Extension_source of frozen_extension

type frozen_kind =
  | Frozen_abstract
  | Frozen_record of frozen_label list * frozen_record_representation
  | Frozen_variant of frozen_constructor list * int
  | Frozen_open

and frozen_record_representation =
  | Static_record of record_representation
  | Inlined_record of {name: string; layout: int; position: int}

type frozen_layout = {
  configuration: Variant_runtime.configuration;
  cases: Variant_runtime.constructor_case list;
}

type frozen_inlined_type = Frozen_inlined_record of string * frozen_label list

type frozen_type = {
  params: int list;
  arity: int;
  kind: frozen_kind;
  private_flag: Asttypes.private_flag;
  manifest: int option;
  variance: Variance.t list;
  loc: Location.t;
  attributes: string option;
  immediate: bool;
  representation: type_representation;
  inlined_types: frozen_inlined_type list;
}

type scope = {
  id: int;
  path: binder_path;
  context_id: int option;
  values: value String_map.t;
  type_names: String_set.t;
  duplicate_type_names: String_set.t;
  types: frozen_type String_map.t;
  label_sources: string list String_map.t;
  constructor_sources: constructor_source list String_map.t;
  modules: module_entry String_map.t;
  duplicate_module_names: String_set.t;
  modtypes: string String_map.t;
  duplicate_modtype_names: String_set.t;
}

and module_entry = {
  position: int;
  loc: Location.t;
  deprecated: string option;
  nested: scope option;
  alias_path: string option;
  declaration: string;
}

type t = {
  name: string;
  signature_bytes: string;
  graph: Frozen_type_graph.t;
  type_binders: binder_path Stamp_map.t;
  module_binders: binder_path Stamp_map.t;
  modtype_binders: binder_path Stamp_map.t;
  contexts: binder_context Int_map.t;
  layouts: frozen_layout array;
  root: scope;
}

and binder_context = {
  parent: int option;
  type_binders: binder_path Stamp_map.t;
  module_binders: binder_path Stamp_map.t;
  modtype_binders: binder_path Stamp_map.t;
}

type building_context = {
  id: int;
  parent: int option;
  types: binder_path Stamp_map.t ref;
  modules: binder_path Stamp_map.t ref;
  modtypes: binder_path Stamp_map.t ref;
}

type view = {
  image: t;
  graph: Frozen_type_graph.view;
  materialized_values: (int * string, value_description * int) Hashtbl.t;
  materialized_types:
    ( int * string,
      type_declaration * (constructor_description list * label_description list)
    )
    Hashtbl.t;
  materialized_extensions: (int * int, constructor_description) Hashtbl.t;
  materialized_modules: (int * string, module_declaration) Hashtbl.t;
  materialized_modtypes: (int * string, modtype_declaration) Hashtbl.t;
  substitutions: (int, Subst.t) Hashtbl.t;
  context_graphs: (int, Frozen_type_graph.view * (Path.t -> Path.t)) Hashtbl.t;
  materialized_layouts:
    (int option * int, Variant_runtime.layout_ref) Hashtbl.t;
  map_type_path: Path.t -> Path.t;
}

(* Keep imported member IDs outside the positive request-local stamp range.
   Lazy materialization must not renumber IDs saved by the compiling module. *)
let imported_member_stamp = Atomic.make (-1_000_000_000)

let freeze (cmi : Cmi_format.cmi_infos) =
  let roots = ref [] in
  let type_binders = ref Stamp_map.empty in
  let module_binders = ref Stamp_map.empty in
  let modtype_binders = ref Stamp_map.empty in
  let next_root = ref 0 in
  let next_scope = ref 0 in
  let next_context = ref 0 in
  let contexts = ref Int_map.empty in
  let modtype_definitions = Hashtbl.create 16 in
  let layout_refs = ref [] in
  let layouts = ref [] in
  let freeze_layout layout_ref =
    match
      List.find_opt (fun (saved, _) -> saved == layout_ref) !layout_refs
    with
    | Some (_, id) -> Some id
    | None -> (
      match
        try Some (Variant_runtime.get_layout layout_ref)
        with Failure _ -> None
      with
      | None -> None
      | Some layout ->
        let id = List.length !layouts in
        let cases =
          List.init
            (Variant_runtime.length layout)
            (Variant_runtime.constructor_at layout)
        in
        layout_refs := (layout_ref, id) :: !layout_refs;
        layouts :=
          {configuration = Variant_runtime.configuration layout; cases}
          :: !layouts;
        Some id)
  in
  let create_context parent =
    let id = !next_context in
    incr next_context;
    {
      id;
      parent = Option.map (fun context -> context.id) parent;
      types = ref Stamp_map.empty;
      modules = ref Stamp_map.empty;
      modtypes = ref Stamp_map.empty;
    }
  in
  let add_binder scope_path context kind id position =
    let entry = scope_path @ [(Ident.name id, position)] in
    let add table = table := Stamp_map.add id.Ident.stamp entry !table in
    match (context, kind) with
    | None, Bound_type -> add type_binders
    | None, Bound_module -> add module_binders
    | None, Bound_modtype -> add modtype_binders
    | Some context, Bound_type -> add context.types
    | Some context, Bound_module -> add context.modules
    | Some context, Bound_modtype -> add context.modtypes
  in
  let rec signature_of_modtype visited = function
    | Mty_signature signature -> Some signature
    | Mty_ident (Path.Pident id) ->
      let stamp = id.Ident.stamp in
      if List.mem stamp visited then None
      else
        Option.bind
          (Hashtbl.find_opt modtype_definitions stamp)
          (signature_of_modtype (stamp :: visited))
    | Mty_ident (Path.Pdot _ | Path.Papply _) | Mty_functor _ | Mty_alias _ ->
      None
  in
  let add_root ty =
    let root = !next_root in
    incr next_root;
    roots := ty :: !roots;
    root
  in
  let marshal_nonempty = function
    | [] -> None
    | items -> Some (Marshal.to_string items [])
  in
  let freeze_label (label : label_declaration) =
    {
      name = label.ld_id.Ident.name;
      flags = label.ld_id.Ident.flags;
      runtime_name = label.ld_runtime_name;
      mutable_flag = label.ld_mutable;
      optional = label.ld_optional;
      typ = add_root label.ld_type;
      loc = label.ld_loc;
      attributes = marshal_nonempty label.ld_attributes;
    }
  in
  let freeze_constructor (constructor : constructor_declaration) =
    {
      name = constructor.cd_id.Ident.name;
      flags = constructor.cd_id.Ident.flags;
      runtime_tag = constructor.cd_runtime_tag;
      args =
        (match constructor.cd_args with
        | Cstr_tuple types -> Frozen_tuple (List.map add_root types)
        | Cstr_record labels ->
          Frozen_record_arguments (List.map freeze_label labels));
      result = Option.map add_root constructor.cd_res;
      loc = constructor.cd_loc;
      attributes = marshal_nonempty constructor.cd_attributes;
    }
  in
  let freeze_args = function
    | Cstr_tuple types -> Frozen_tuple (List.map add_root types)
    | Cstr_record labels ->
      Frozen_record_arguments (List.map freeze_label labels)
  in
  let index_source table name source =
    let previous =
      match String_map.find_opt name !table with
      | Some sources -> sources
      | None -> []
    in
    table := String_map.add name (source :: previous) !table
  in
  let rec freeze_scope scope_path context signature =
    let id = !next_scope in
    incr next_scope;
    let values = ref String_map.empty in
    let type_names = ref String_set.empty in
    let duplicate_type_names = ref String_set.empty in
    let types = ref String_map.empty in
    let label_sources = ref String_map.empty in
    let constructor_sources = ref String_map.empty in
    let modules = ref String_map.empty in
    let duplicate_module_names = ref String_set.empty in
    let modtypes = ref String_map.empty in
    let duplicate_modtype_names = ref String_set.empty in
    let position = ref 0 in
    List.iter
      (function
        | Sig_value (id, declaration) -> (
          let typ = add_root declaration.val_type in
          let kind =
            match declaration.val_kind with
            | Val_reg -> None
            | Val_prim _ as kind -> Some (Marshal.to_string kind [])
          in
          let attributes = marshal_nonempty declaration.val_attributes in
          values :=
            String_map.add (Ident.name id)
              {
                typ;
                kind;
                loc = declaration.val_loc;
                attributes;
                position = !position;
              }
              !values;
          match declaration.val_kind with
          | Val_reg -> incr position
          | Val_prim _ -> ())
        | Sig_type (id, declaration, _) -> (
          let type_name = Ident.name id in
          if String_set.mem type_name !type_names then
            duplicate_type_names :=
              String_set.add type_name !duplicate_type_names;
          type_names := String_set.add type_name !type_names;
          add_binder scope_path context Bound_type id Path.nopos;
          let kind =
            match declaration.type_kind with
            | Type_abstract -> Some Frozen_abstract
            | Type_record (labels, representation) -> (
              List.iter
                (fun label ->
                  index_source label_sources (Ident.name label.ld_id) type_name)
                labels;
              match representation with
              | Record_inlined {name; representation = {variant; position}} ->
                Option.map
                  (fun layout ->
                    Frozen_record
                      ( List.map freeze_label labels,
                        Inlined_record {name; layout; position} ))
                  (freeze_layout variant)
              | Record_regular | Record_float_unused | Record_unboxed _
              | Record_extension ->
                Some
                  (Frozen_record
                     (List.map freeze_label labels, Static_record representation))
              )
            | Type_variant (constructors, layout_ref) ->
              List.iter
                (fun constructor ->
                  index_source constructor_sources
                    (Ident.name constructor.cd_id)
                    (Variant_source type_name))
                constructors;
              Option.map
                (fun layout ->
                  Frozen_variant
                    (List.map freeze_constructor constructors, layout))
                (freeze_layout layout_ref)
            | Type_open -> Some Frozen_open
          in
          match kind with
          | Some kind ->
            let inlined_types =
              List.map
                (function
                  | Record {type_name; labels} ->
                    Frozen_inlined_record
                      (type_name, List.map freeze_label labels))
                declaration.type_inlined_types
            in
            types :=
              String_map.add (Ident.name id)
                {
                  params = List.map add_root declaration.type_params;
                  arity = declaration.type_arity;
                  kind;
                  private_flag = declaration.type_private;
                  manifest = Option.map add_root declaration.type_manifest;
                  variance = declaration.type_variance;
                  loc = declaration.type_loc;
                  attributes = marshal_nonempty declaration.type_attributes;
                  immediate = declaration.type_immediate;
                  representation = declaration.type_representation;
                  inlined_types;
                }
                !types
          | None -> ())
        | Sig_typext (id, extension, _) ->
          add_binder scope_path context Bound_type id !position;
          let source =
            Extension_source
              {
                name = Ident.name id;
                position = !position;
                type_path = Marshal.to_string extension.ext_type_path [];
                type_params = List.map add_root extension.ext_type_params;
                args = freeze_args extension.ext_args;
                ret_type = Option.map add_root extension.ext_ret_type;
                private_flag = extension.ext_private;
                loc = extension.ext_loc;
                attributes = marshal_nonempty extension.ext_attributes;
                is_exception = extension.ext_is_exception;
              }
          in
          index_source constructor_sources (Ident.name id) source;
          incr position
        | Sig_module (id, declaration, _) ->
          let name = Ident.name id in
          if String_map.mem name !modules then
            duplicate_module_names :=
              String_set.add name !duplicate_module_names;
          add_binder scope_path context Bound_module id !position;
          let nested =
            match declaration.md_type with
            | Mty_signature signature ->
              Some
                (freeze_scope
                   (scope_path @ [(name, !position)])
                   context signature)
            | Mty_ident _ as module_type -> (
              match signature_of_modtype [] module_type with
              | None -> None
              | Some signature ->
                let instantiation = create_context context in
                let nested =
                  freeze_scope
                    (scope_path @ [(name, !position)])
                    (Some instantiation) signature
                in
                contexts :=
                  Int_map.add instantiation.id
                    {
                      parent = instantiation.parent;
                      type_binders = !(instantiation.types);
                      module_binders = !(instantiation.modules);
                      modtype_binders = !(instantiation.modtypes);
                    }
                    !contexts;
                Some nested)
            | Mty_functor _ | Mty_alias _ -> None
          in
          modules :=
            String_map.add name
              {
                position = !position;
                loc = declaration.md_loc;
                deprecated =
                  Builtin_attributes.deprecated_of_attrs
                    declaration.md_attributes;
                nested;
                alias_path =
                  (match declaration.md_type with
                  | Mty_alias (_, path) -> Some (Marshal.to_string path [])
                  | Mty_ident _ | Mty_signature _ | Mty_functor _ -> None);
                declaration = Marshal.to_string declaration [];
              }
              !modules;
          incr position
        | Sig_modtype (id, declaration) ->
          let name = Ident.name id in
          if String_map.mem name !modtypes then
            duplicate_modtype_names :=
              String_set.add name !duplicate_modtype_names;
          add_binder scope_path context Bound_modtype id Path.nopos;
          Option.iter
            (fun module_type ->
              Hashtbl.replace modtype_definitions id.Ident.stamp module_type)
            declaration.mtd_type;
          modtypes :=
            String_map.add name (Marshal.to_string declaration []) !modtypes)
      signature;
    {
      id;
      path = scope_path;
      context_id = Option.map (fun context -> context.id) context;
      values = !values;
      type_names = !type_names;
      duplicate_type_names = !duplicate_type_names;
      types = !types;
      label_sources = !label_sources;
      constructor_sources = !constructor_sources;
      modules = !modules;
      duplicate_module_names = !duplicate_module_names;
      modtypes = !modtypes;
      duplicate_modtype_names = !duplicate_modtype_names;
    }
  in
  let root = freeze_scope [] None cmi.cmi_sign in
  match Frozen_type_graph.freeze (List.rev !roots) with
  | Error reason -> Error reason
  | Ok graph ->
    Ok
      {
        name = cmi.cmi_name;
        signature_bytes = Marshal.to_string cmi.cmi_sign [];
        graph;
        type_binders = !type_binders;
        module_binders = !module_binders;
        modtype_binders = !modtype_binders;
        contexts = !contexts;
        layouts = Array.of_list (List.rev !layouts);
        root;
      }

let create_graph_view image context_id =
  let root = Path.Pident (Ident.create_persistent image.name) in
  let rec find_context_binder context_id select stamp =
    match context_id with
    | None -> None
    | Some context_id -> (
      let context = Int_map.find context_id image.contexts in
      match Stamp_map.find_opt stamp (select context) with
      | Some _ as entry -> entry
      | None -> find_context_binder context.parent select stamp)
  in
  let prefixed global select id =
    let entry =
      match find_context_binder context_id select id.Ident.stamp with
      | Some _ as entry -> entry
      | None -> Stamp_map.find_opt id.Ident.stamp global
    in
    match entry with
    | Some segments ->
      List.fold_left
        (fun path (name, position) -> Path.Pdot (path, name, position))
        root segments
    | None -> Path.Pident id
  in
  let rec map_module_path = function
    | Path.Pident id ->
      prefixed image.module_binders (fun context -> context.module_binders) id
    | Path.Pdot (path, name, position) ->
      Path.Pdot (map_module_path path, name, position)
    | Path.Papply (first, second) ->
      Path.Papply (map_module_path first, map_module_path second)
  in
  let map_type_path path =
    match Path.constructor_typath path with
    | Path.Regular (Path.Pident id) ->
      prefixed image.type_binders (fun context -> context.type_binders) id
    | Path.Regular (Path.Pdot (module_path, name, position)) ->
      Path.Pdot (map_module_path module_path, name, position)
    | Path.Regular (Path.Papply _) -> path
    | Path.Cstr (type_path, constructor) ->
      let type_path =
        match type_path with
        | Path.Pident id ->
          prefixed image.type_binders (fun context -> context.type_binders) id
        | Path.Pdot (module_path, name, position) ->
          Path.Pdot (map_module_path module_path, name, position)
        | Path.Papply _ -> type_path
      in
      Path.Pdot (type_path, constructor, Path.nopos)
    | Path.LocalExt _ -> path
    | Path.Ext (module_path, constructor) ->
      Path.Pdot (map_module_path module_path, constructor, Path.nopos)
  in
  let map_modtype_path = function
    | Path.Pident id ->
      prefixed image.modtype_binders (fun context -> context.modtype_binders) id
    | Path.Pdot (path, name, position) ->
      Path.Pdot (map_module_path path, name, position)
    | Path.Papply _ as path -> map_module_path path
  in
  let graph =
    Frozen_type_graph.create_view ~map_type_path ~map_modtype_path image.graph
  in
  (graph, map_type_path)

let create_view image =
  let graph, map_type_path = create_graph_view image None in
  {
    image;
    graph;
    materialized_values = Hashtbl.create 16;
    materialized_types = Hashtbl.create 16;
    materialized_extensions = Hashtbl.create 8;
    materialized_modules = Hashtbl.create 8;
    materialized_modtypes = Hashtbl.create 8;
    substitutions = Hashtbl.create 8;
    context_graphs = Hashtbl.create 8;
    materialized_layouts = Hashtbl.create 8;
    map_type_path;
  }

let copy_signature view : signature =
  let source = Marshal.from_string view.image.signature_bytes 0 in
  Subst.signature Subst.identity source

let source_signature view : signature =
  Marshal.from_string view.image.signature_bytes 0

let scope_graph view (scope : scope) =
  match scope.context_id with
  | None -> (view.graph, view.map_type_path)
  | Some context_id -> (
    match Hashtbl.find_opt view.context_graphs context_id with
    | Some graph -> graph
    | None ->
      let graph = create_graph_view view.image (Some context_id) in
      Hashtbl.add view.context_graphs context_id graph;
      graph)

let scope_layout view (scope : scope) id =
  let key = (scope.context_id, id) in
  match Hashtbl.find_opt view.materialized_layouts key with
  | Some layout -> layout
  | None ->
    let {configuration; cases} = view.image.layouts.(id) in
    let layout = Variant_runtime.pending_layout () in
    Variant_runtime.complete_layout layout
      (Variant_runtime.make_layout ~configuration (Array.of_list cases));
    Hashtbl.add view.materialized_layouts key layout;
    layout

let thaw_attributes = function
  | None -> []
  | Some bytes -> Marshal.from_string bytes 0

let fresh_member_id name flags =
  {Ident.name; stamp = Atomic.fetch_and_add imported_member_stamp (-1); flags}

let thaw_label graph (label : frozen_label) =
  {
    ld_id = fresh_member_id label.name label.flags;
    ld_runtime_name = label.runtime_name;
    ld_mutable = label.mutable_flag;
    ld_optional = label.optional;
    ld_type = Frozen_type_graph.type_at graph label.typ;
    ld_loc = label.loc;
    ld_attributes = thaw_attributes label.attributes;
  }

let thaw_args graph = function
  | Frozen_tuple types ->
    Cstr_tuple (List.map (Frozen_type_graph.type_at graph) types)
  | Frozen_record_arguments labels ->
    Cstr_record (List.map (thaw_label graph) labels)

let scope_path view (scope : scope) =
  List.fold_left
    (fun path (name, position) -> Path.Pdot (path, name, position))
    (Path.Pident (Ident.create_persistent view.image.name))
    scope.path

let scope_substitution view (scope : scope) =
  match Hashtbl.find_opt view.substitutions scope.id with
  | Some substitution -> substitution
  | None ->
    let rec is_prefix prefix path =
      match (prefix, path) with
      | [], _ -> true
      | first :: rest, next :: tail when first = next -> is_prefix rest tail
      | _ -> false
    in
    let rec parent_segments = function
      | [] | [_] -> []
      | first :: rest -> first :: parent_segments rest
    in
    let is_visible segments = is_prefix (parent_segments segments) scope.path in
    let path segments =
      List.fold_left
        (fun path (name, position) -> Path.Pdot (path, name, position))
        (Path.Pident (Ident.create_persistent view.image.name))
        segments
    in
    let identifier stamp segments =
      let name, _ = List.hd (List.rev segments) in
      {Ident.name; stamp; flags = 0}
    in
    let add table add substitution =
      Stamp_map.fold
        (fun stamp segments substitution ->
          if is_visible segments then
            add (identifier stamp segments) (path segments) substitution
          else substitution)
        table substitution
    in
    let substitution =
      Subst.identity
      |> add view.image.type_binders Subst.add_type
      |> add view.image.module_binders Subst.add_module
      |> add view.image.modtype_binders (fun id path substitution ->
          Subst.add_modtype id (Mty_ident path) substitution)
    in
    let rec context_chain = function
      | None -> []
      | Some context_id ->
        let context = Int_map.find context_id view.image.contexts in
        context_chain context.parent @ [context]
    in
    let substitution =
      List.fold_left
        (fun substitution (context : binder_context) ->
          substitution
          |> add context.type_binders Subst.add_type
          |> add context.module_binders Subst.add_module
          |> add context.modtype_binders (fun id path substitution ->
              Subst.add_modtype id (Mty_ident path) substitution))
        substitution
        (context_chain scope.context_id)
    in
    Hashtbl.add view.substitutions scope.id substitution;
    substitution

let find_module_declaration view (scope : scope) name =
  if String_set.mem name scope.duplicate_module_names then None
  else
    match String_map.find_opt name scope.modules with
    | None -> None
    | Some entry -> (
      let key = (scope.id, name) in
      match Hashtbl.find_opt view.materialized_modules key with
      | Some declaration -> Some (declaration, entry.position)
      | None ->
        let source = Marshal.from_string entry.declaration 0 in
        let declaration =
          Subst.module_declaration (scope_substitution view scope) source
        in
        Hashtbl.add view.materialized_modules key declaration;
        Some (declaration, entry.position))

let find_modtype_declaration view (scope : scope) name =
  if String_set.mem name scope.duplicate_modtype_names then None
  else
    match String_map.find_opt name scope.modtypes with
    | None -> None
    | Some bytes -> (
      let key = (scope.id, name) in
      match Hashtbl.find_opt view.materialized_modtypes key with
      | Some declaration -> Some declaration
      | None ->
        let source = Marshal.from_string bytes 0 in
        let declaration =
          Subst.modtype_declaration (scope_substitution view scope) source
        in
        Hashtbl.add view.materialized_modtypes key declaration;
        Some declaration)

let thaw_extension view (scope : scope) (extension : frozen_extension) =
  let key = (scope.id, extension.position) in
  match Hashtbl.find_opt view.materialized_extensions key with
  | Some descr -> descr
  | None ->
    let graph, map_type_path = scope_graph view scope in
    let path =
      Path.Pdot (scope_path view scope, extension.name, extension.position)
    in
    let ext : extension_constructor =
      {
        ext_type_path = map_type_path (Marshal.from_string extension.type_path 0);
        ext_type_params =
          List.map (Frozen_type_graph.type_at graph) extension.type_params;
        ext_args = thaw_args graph extension.args;
        ext_ret_type =
          Option.map (Frozen_type_graph.type_at graph) extension.ret_type;
        ext_private = extension.private_flag;
        ext_loc = extension.loc;
        ext_attributes = thaw_attributes extension.attributes;
        ext_is_exception = extension.is_exception;
      }
    in
    let descr = Datarepr.extension_descr path ext in
    Hashtbl.add view.materialized_extensions key descr;
    descr

let find_in_scope view (scope : scope) name =
  let key = (scope.id, name) in
  match Hashtbl.find_opt view.materialized_values key with
  | Some value -> Some value
  | None -> (
    match String_map.find_opt name scope.values with
    | None -> None
    | Some {typ; kind; loc; attributes; position} ->
      let graph, _ = scope_graph view scope in
      let val_type = Frozen_type_graph.type_at graph typ in
      let val_kind =
        match kind with
        | None -> Val_reg
        | Some bytes -> Marshal.from_string bytes 0
      in
      let val_attributes = thaw_attributes attributes in
      let value =
        ({val_type; val_kind; val_loc = loc; val_attributes}, position)
      in
      Hashtbl.add view.materialized_values key value;
      Some value)

let find_type_in_scope view (scope : scope) name =
  let key = (scope.id, name) in
  match Hashtbl.find_opt view.materialized_types key with
  | Some declaration -> Some declaration
  | None -> (
    match String_map.find_opt name scope.types with
    | None -> None
    | Some
        {
          params;
          arity;
          kind;
          private_flag;
          manifest;
          variance;
          loc;
          attributes;
          immediate;
          representation;
          inlined_types;
        } ->
      let graph, _ = scope_graph view scope in
      let thaw_constructor (constructor : frozen_constructor) =
        {
          cd_id = fresh_member_id constructor.name constructor.flags;
          cd_runtime_tag = constructor.runtime_tag;
          cd_args = thaw_args graph constructor.args;
          cd_res =
            Option.map (Frozen_type_graph.type_at graph) constructor.result;
          cd_loc = constructor.loc;
          cd_attributes = thaw_attributes constructor.attributes;
        }
      in
      let declaration =
        {
          type_params = List.map (Frozen_type_graph.type_at graph) params;
          type_arity = arity;
          type_kind =
            (match kind with
            | Frozen_abstract -> Type_abstract
            | Frozen_record (labels, representation) ->
              let representation =
                match representation with
                | Static_record representation -> representation
                | Inlined_record {name; layout; position} ->
                  Record_inlined
                    {
                      name;
                      representation =
                        {variant = scope_layout view scope layout; position};
                    }
              in
              Type_record (List.map (thaw_label graph) labels, representation)
            | Frozen_variant (constructors, layout) ->
              Type_variant
                ( List.map thaw_constructor constructors,
                  scope_layout view scope layout )
            | Frozen_open -> Type_open);
          type_private = private_flag;
          type_manifest = Option.map (Frozen_type_graph.type_at graph) manifest;
          type_variance = variance;
          type_newtype_level = None;
          type_loc = loc;
          type_attributes = thaw_attributes attributes;
          type_immediate = immediate;
          type_representation = representation;
          type_inlined_types =
            List.map
              (function
                | Frozen_inlined_record (type_name, labels) ->
                  Record
                    {type_name; labels = List.map (thaw_label graph) labels})
              inlined_types;
        }
      in
      let path = Path.Pdot (scope_path view scope, name, Path.nopos) in
      Datarepr.set_row_name declaration path;
      let descriptions =
        ( List.map snd (Datarepr.constructors_of_type path declaration),
          List.map snd (Datarepr.labels_of_type path declaration) )
      in
      let result = (declaration, descriptions) in
      Hashtbl.add view.materialized_types key result;
      Some result)

let find_labels_in_scope view (scope : scope) name =
  match String_map.find_opt name scope.label_sources with
  | None -> Some []
  | Some sources
    when List.exists
           (fun source -> String_set.mem source scope.duplicate_type_names)
           sources ->
    None
  | Some sources ->
    let rec collect acc = function
      | [] -> Some (List.rev acc)
      | source :: rest -> (
        match find_type_in_scope view scope source with
        | None -> None
        | Some (_, (_, labels)) ->
          let matching =
            List.filter (fun label -> label.lbl_name = name) labels
          in
          collect (List.rev_append matching acc) rest)
    in
    collect [] sources

let find_constructors_in_scope view (scope : scope) name =
  match String_map.find_opt name scope.constructor_sources with
  | None -> Some []
  | Some sources ->
    let rec collect acc = function
      | [] -> Some (List.rev acc)
      | Extension_source extension :: rest ->
        collect (thaw_extension view scope extension :: acc) rest
      | Variant_source source :: rest -> (
        if String_set.mem source scope.duplicate_type_names then None
        else
          match find_type_in_scope view scope source with
          | None -> None
          | Some (_, (constructors, _)) ->
            let matching =
              List.filter
                (fun constructor -> constructor.cstr_name = name)
                constructors
            in
            collect (List.rev_append matching acc) rest)
    in
    collect [] sources

let find_extension_in_scope view (scope : scope) name =
  match String_map.find_opt name scope.constructor_sources with
  | None -> None
  | Some sources -> (
    let extensions =
      List.filter_map
        (function
          | Extension_source extension -> Some extension
          | Variant_source _ -> None)
        sources
    in
    match extensions with
    | [extension] -> Some (thaw_extension view scope extension)
    | [] | _ :: _ :: _ -> None)

let root_scope view = view.image.root

let names table = List.map fst (String_map.bindings table)
let value_names (scope : scope) = names scope.values
let type_names (scope : scope) = String_set.elements scope.type_names
let label_names (scope : scope) = names scope.label_sources
let constructor_names (scope : scope) = names scope.constructor_sources
let module_names (scope : scope) = names scope.modules
let modtype_names (scope : scope) = names scope.modtypes
let has_value (scope : scope) name = String_map.mem name scope.values
let has_type (scope : scope) name = String_set.mem name scope.type_names
let has_label (scope : scope) name = String_map.mem name scope.label_sources

let has_constructor (scope : scope) name =
  String_map.mem name scope.constructor_sources

let has_module (scope : scope) name = String_map.mem name scope.modules
let has_modtype (scope : scope) name = String_map.mem name scope.modtypes

let find_module_info (scope : scope) name =
  if String_set.mem name scope.duplicate_module_names then None
  else
    match String_map.find_opt name scope.modules with
    | Some {position; loc; deprecated} -> Some (position, loc, deprecated)
    | None -> None

let find_module_alias view (scope : scope) name =
  if String_set.mem name scope.duplicate_module_names then None
  else
    match String_map.find_opt name scope.modules with
    | Some {alias_path = Some bytes} ->
      let path = Marshal.from_string bytes 0 in
      Some (Subst.module_path (scope_substitution view scope) path)
    | Some {alias_path = None} | None -> None

let find_module (scope : scope) name =
  if String_set.mem name scope.duplicate_module_names then None
  else
    match String_map.find_opt name scope.modules with
    | Some {nested = Some nested; position; loc; deprecated} ->
      Some (nested, position, loc, deprecated)
    | Some {nested = None} | None -> None

let find view name = find_in_scope view view.image.root name
let find_type view name = find_type_in_scope view view.image.root name
let find_labels view name = find_labels_in_scope view view.image.root name

let find_constructors view name =
  find_constructors_in_scope view view.image.root name

let find_extension view name = find_extension_in_scope view view.image.root name

let value_count (image : t) = String_map.cardinal image.root.values
let is_type_name_in_scope scope name = String_set.mem name scope.type_names
let is_type_name view name = is_type_name_in_scope view.image.root name
let type_count (image : t) = String_map.cardinal image.root.types
let type_node_count (image : t) = Frozen_type_graph.node_count image.graph
