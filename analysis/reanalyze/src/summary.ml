open Common
open DeadCommon

module Blake2b = Digestif.BLAKE2B

type position = {file: string; line: int; column: int; cnum: int; bol: int}

type range = {start_: position; end_: position}

type optional_args_snapshot = {
  unused: string list;
  always_used: (string * int) list;
  call_count: int;
}

type value_kind = {
  is_toplevel: bool;
  side_effects: bool;
  optional_args: optional_args_snapshot;
}

type decl_kind =
  | Value of value_kind
  | Exception
  | RecordLabel
  | VariantCase

type annotation_snapshot = {
  annotated_dead: bool;
  annotated_gen_type_or_live: bool;
  annotated_gen_type_or_dead: bool;
}

let annotation_snapshot_none =
  {
    annotated_dead = false;
    annotated_gen_type_or_live = false;
    annotated_gen_type_or_dead = false;
  }

type decl = {
  path: string list;
  module_path: string list;
  name: string;
  loc: range;
  module_loc: range option;
  decl_kind: decl_kind;
  pos_adjustment: posAdjustment option;
  annotations: annotation_snapshot;
}

type value_reference = {
  loc_from: position;
  loc_to: position;
  add_file_edge: bool;
  target_path: string list option;
}

type type_reference = {pos_from: position; pos_to: position}

type file_edge = {from_file: string; to_file: string}

type t = {
  version: int;
  source_file: string;
  digest: string;
  decls: decl list;
  value_references: value_reference list;
  type_references: type_reference list;
  file_edges: file_edge list;
}

let version = 2

exception Invalid_format of string

let column_of_pos pos =
  let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  if column < 0 then 0 else column

let position_of_lexing pos =
  {
    file = pos.Lexing.pos_fname;
    line = pos.Lexing.pos_lnum;
    column = column_of_pos pos;
    cnum = pos.Lexing.pos_cnum;
    bol = pos.Lexing.pos_bol;
  }

let range_of_positions ~start ~end_ = {start_ = position_of_lexing start; end_ = position_of_lexing end_}

let range_of_location (loc : Location.t) = range_of_positions ~start:loc.loc_start ~end_:loc.loc_end

let range_of_location_opt (loc : Location.t) =
  if loc.loc_ghost then None else Some (range_of_location loc)

let optional_args_snapshot (args : OptionalArgs.t) =
  let module SS = StringSet in
  let unused = args.unused |> SS.elements |> List.sort String.compare in
  let always_used =
    args.alwaysUsed |> SS.elements |> List.sort String.compare |> List.map (fun name -> (name, args.count))
  in
  {unused; always_used; call_count = args.count}

let lexing_position_of_summary {file; line; cnum; bol; _} =
  {
    Lexing.pos_fname = file;
    pos_lnum = line;
    pos_cnum = cnum;
    pos_bol = bol;
  }

let location_of_range {start_; end_} =
  {
    Location.loc_start = lexing_position_of_summary start_;
    loc_end = lexing_position_of_summary end_;
    loc_ghost = false;
  }

let optional_args_of_snapshot ({unused; always_used; call_count} : optional_args_snapshot) =
  let module SS = StringSet in
  {
    OptionalArgs.count = call_count;
    unused = SS.of_list unused;
    alwaysUsed = always_used |> List.map fst |> SS.of_list;
  }

let decl_kind_of_summary = function
  | Value {is_toplevel; side_effects; optional_args} ->
      DeclKind.Value
        {
          isToplevel = is_toplevel;
          sideEffects = side_effects;
          optionalArgs = optional_args_of_snapshot optional_args;
        }
  | Exception -> DeclKind.Exception
  | RecordLabel -> DeclKind.RecordLabel
  | VariantCase -> DeclKind.VariantCase

let path_of_components components =
  components |> List.rev_map (fun name -> Name.create name)

let path_components path = path |> List.rev_map Name.toString

let split_modules_and_name components =
  let rec aux acc comps =
    match comps with
    | [] -> (List.rev acc, "")
    | [name] -> (List.rev acc, name)
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] components

let pos_adjustment_to_string = function
  | FirstVariant -> "FirstVariant"
  | OtherVariant -> "OtherVariant"
  | Nothing -> "Nothing"

let pos_adjustment_of_string = function
  | "FirstVariant" -> Some FirstVariant
  | "OtherVariant" -> Some OtherVariant
  | "Nothing" -> Some Nothing
  | _ -> None

let summary_decl_of_common (decl : Common.decl) =
  let components = path_components decl.path in
  let module_path, name = split_modules_and_name components in
  let loc = range_of_positions ~start:decl.posStart ~end_:decl.posEnd in
  let module_loc = range_of_location_opt decl.moduleLoc in
  let annotations =
    {
      annotated_dead = ProcessDeadAnnotations.isAnnotatedDead decl.pos;
      annotated_gen_type_or_live = ProcessDeadAnnotations.isAnnotatedGenTypeOrLive decl.pos;
      annotated_gen_type_or_dead = ProcessDeadAnnotations.isAnnotatedGenTypeOrDead decl.pos;
    }
  in
  let decl_kind, pos_adjustment =
    match decl.declKind with
    | DeclKind.Value {isToplevel; optionalArgs; sideEffects} ->
        ( Value
            {
              is_toplevel = isToplevel;
              side_effects = sideEffects;
              optional_args = optional_args_snapshot optionalArgs;
            },
          None )
    | DeclKind.Exception -> (Exception, None)
    | DeclKind.RecordLabel -> (RecordLabel, Some decl.posAdjustment)
    | DeclKind.VariantCase -> (VariantCase, Some decl.posAdjustment)
  in
  {path = components; module_path; name; loc; module_loc; decl_kind; pos_adjustment; annotations}

let to_common_decl (decl : decl) =
  let moduleLoc =
    match decl.module_loc with
    | Some range -> location_of_range range
    | None -> Location.none
  in
  let posAdjustment = match decl.pos_adjustment with Some adj -> adj | None -> Nothing in
  let path = decl.path |> path_of_components in
  {
    declKind = decl_kind_of_summary decl.decl_kind;
    moduleLoc;
    posAdjustment;
    path;
    pos = lexing_position_of_summary decl.loc.start_;
    posEnd = lexing_position_of_summary decl.loc.end_;
    posStart = lexing_position_of_summary decl.loc.start_;
    resolvedDead = None;
    report = true;
  }

let to_common_decls (summary : t) = summary.decls |> List.map to_common_decl

let summary_value_reference (ref : Collected_types.value_reference) =
  let loc_from = position_of_lexing ref.loc_from.loc_start in
  let loc_to = position_of_lexing ref.loc_to.loc_start in
  let target_path =
    match ref.target_path with
    | None -> None
    | Some components -> Some (path_components components)
  in
  (match (target_path, Sys.getenv_opt "GRAPH_TRACE_UNKNOWN") with
  | Some path, Some ("1" | "true" | "on") ->
      Printf.eprintf "[summary] value ref target path=%s\n"
        (String.concat "." path)
  | _ -> ());
  {loc_from; loc_to; add_file_edge = ref.add_file_reference; target_path}

let summary_type_reference (ref : Collected_types.type_reference) =
  let pos_from = position_of_lexing ref.pos_from in
  let pos_to = position_of_lexing ref.pos_to in
  {pos_from; pos_to}

let dedup_file_edges refs =
  let edges =
    refs
    |> List.fold_left
         (fun acc ref ->
           if ref.add_file_edge then
             let from_file = ref.loc_from.file in
             let to_file = ref.loc_to.file in
             if from_file <> "" && to_file <> "" && not (String.equal from_file to_file) then
               (from_file, to_file) :: acc
             else acc
           else acc)
         []
    |> List.sort_uniq (fun (a1, b1) (a2, b2) ->
           match String.compare a1 a2 with
           | 0 -> String.compare b1 b2
           | c -> c)
  in
  edges |> List.map (fun (from_file, to_file) -> {from_file; to_file})

let build_from_collected ~source_file (collected : Collected_types.t) =
  let decls = collected.decls |> List.map summary_decl_of_common in
  let normalized_source_file =
    match decls with
    | decl :: _ -> decl.loc.start_.file
    | [] -> source_file
  in
  let value_references = collected.value_references |> List.map summary_value_reference in
  let type_references = collected.type_references |> List.map summary_type_reference in
  let file_edges = dedup_file_edges value_references in
  {
    version;
    source_file = normalized_source_file;
    digest = "";
    decls;
    value_references;
    type_references;
    file_edges;
  }

let json_bool = function
  | true -> Json.True
  | false -> Json.False

let json_int n = Json.Number (float_of_int n)
let json_string s = Json.String s
let json_array xs = Json.Array xs

let json_object fields =
  fields |> List.sort (fun (a, _) (b, _) -> compare a b) |> fun sorted -> Json.Object sorted

let json_option f = function
  | None -> Json.Null
  | Some v -> f v

let json_of_position {file; line; column; cnum; bol} =
  json_object
    [
      ("bol", json_int bol);
      ("column", json_int column);
      ("cnum", json_int cnum);
      ("file", json_string file);
      ("line", json_int line);
    ]

let json_of_range {start_; end_} =
  json_object [("end", json_of_position end_); ("start", json_of_position start_)]

let json_of_optional_args {unused; always_used; call_count} =
  json_object
    [
      ( "always_used",
        json_array
          (always_used
          |> List.map (fun (name, calls) ->
                 json_object [("calls", json_int calls); ("name", json_string name)])) );
      ("call_count", json_int call_count);
      ("unused", json_array (unused |> List.map json_string));
    ]

let json_of_annotations annotations =
  json_object
    [
      ("annotated_dead", json_bool annotations.annotated_dead);
      ( "annotated_gen_type_or_live",
        json_bool annotations.annotated_gen_type_or_live );
      ( "annotated_gen_type_or_dead",
        json_bool annotations.annotated_gen_type_or_dead );
    ]

let kind_to_string = function
  | Value _ -> "Value"
  | Exception -> "Exception"
  | RecordLabel -> "RecordLabel"
  | VariantCase -> "VariantCase"

let json_of_decl decl =
  let base_fields =
    [
      ("kind", json_string (kind_to_string decl.decl_kind));
      ("loc", json_of_range decl.loc);
      ("module_loc", json_option json_of_range decl.module_loc);
      ("module_path", json_array (decl.module_path |> List.map json_string));
      ("name", json_string decl.name);
      ("path", json_array (decl.path |> List.map json_string));
      ( "pos_adjustment",
        json_option (fun adj -> json_string (pos_adjustment_to_string adj)) decl.pos_adjustment );
      ("annotations", json_of_annotations decl.annotations);
    ]
  in
  let kind_fields =
    match decl.decl_kind with
    | Value {is_toplevel; side_effects; optional_args} ->
        [
          ("is_toplevel", json_bool is_toplevel);
          ("side_effects", json_bool side_effects);
          ("optional_args", json_of_optional_args optional_args);
        ]
    | _ ->
        [
          ("is_toplevel", Json.Null);
          ("side_effects", Json.Null);
          ("optional_args", Json.Null);
        ]
  in
  json_object (base_fields @ kind_fields)

let json_of_value_reference ref =
  json_object
    [
      ("add_file_edge", json_bool ref.add_file_edge);
      ("from", json_of_position ref.loc_from);
      ("to", json_of_position ref.loc_to);
      ( "target_path",
        json_option
          (fun components -> json_array (List.map json_string components))
          ref.target_path );
    ]

let json_of_type_reference ref =
  json_object
    [("from", json_of_position ref.pos_from); ("to", json_of_position ref.pos_to)]

let json_of_file_edge edge =
  json_object [("from", json_string edge.from_file); ("to", json_string edge.to_file)]

let to_json ?(include_digest = true) summary =
  let fields =
    [
      ("decls", json_array (List.map json_of_decl summary.decls));
      ("file_edges", json_array (List.map json_of_file_edge summary.file_edges));
      ("source_file", json_string summary.source_file);
      ("type_refs", json_array (List.map json_of_type_reference summary.type_references));
      ("value_refs", json_array (List.map json_of_value_reference summary.value_references));
      ("version", json_int summary.version);
    ]
  in
  let fields =
    if include_digest then ("digest", json_string summary.digest) :: fields else fields
  in
  json_object fields

let to_string ?(pretty = false) summary =
  if pretty then Json.stringifyPretty ~indent:2 (to_json summary)
  else Json.stringify (to_json summary)

let canonical_string summary = Json.stringify (to_json ~include_digest:false summary)

let compute_digest summary =
  let canonical = canonical_string summary in
  Blake2b.(digest_string canonical |> to_hex)

let verify_digest summary =
  let expected = summary.digest in
  let actual = compute_digest summary in
  if not (String.equal expected actual) then
    raise
      (Invalid_format
         (Printf.sprintf "digest mismatch: expected %s but computed %s" expected actual))

let attach_digest summary = {summary with digest = compute_digest summary}

let of_collected ~source_file collected =
  collected |> build_from_collected ~source_file |> attach_digest

let expect_object message = function
  | Json.Object fields -> fields
  | _ -> raise (Invalid_format message)

let expect_array message = function
  | Json.Array items -> items
  | _ -> raise (Invalid_format message)

let expect_string message = function
  | Json.String s -> s
  | _ -> raise (Invalid_format message)

let expect_bool message = function
  | Json.True -> true
  | Json.False -> false
  | _ -> raise (Invalid_format message)

let expect_int message = function
  | Json.Number f ->
      if Float.is_integer f then int_of_float f
      else raise (Invalid_format (message ^ " (expected integer)"))
  | _ -> raise (Invalid_format message)

let field name fields =
  match List.assoc_opt name fields with
  | Some v -> v
  | None -> raise (Invalid_format ("missing field: " ^ name))

let position_of_json json =
  let fields = expect_object "position" json in
  let file = field "file" fields |> expect_string "position.file" in
  let line = field "line" fields |> expect_int "position.line" in
  let column = field "column" fields |> expect_int "position.column" in
  let cnum = match List.assoc_opt "cnum" fields with Some v -> expect_int "position.cnum" v | None -> column in
  let bol = match List.assoc_opt "bol" fields with Some v -> expect_int "position.bol" v | None -> cnum - column in
  {file; line; column; cnum; bol}

let range_of_json json =
  let fields = expect_object "range" json in
  let start_ = field "start" fields |> position_of_json in
  let end_ = field "end" fields |> position_of_json in
  {start_; end_}

let optional_args_of_json = function
  | Json.Null -> None
  | json -> (
      let fields = expect_object "optional_args" json in
      let unused =
        field "unused" fields |> expect_array "optional_args.unused"
        |> List.map (expect_string "optional_args.unused entry")
      in
      let call_count = field "call_count" fields |> expect_int "optional_args.call_count" in
      let always_used =
        field "always_used" fields |> expect_array "optional_args.always_used"
        |> List.map (fun entry ->
               let entry_fields = expect_object "optional_args.always_used entry" entry in
               let name = field "name" entry_fields |> expect_string "optional_args.always_used.name" in
               let calls = field "calls" entry_fields |> expect_int "optional_args.always_used.calls" in
               (name, calls))
      in
      Some {unused; always_used; call_count})

let annotations_of_json = function
  | Json.Null -> annotation_snapshot_none
  | json ->
      let fields = expect_object "annotations" json in
      let annotated_dead =
        match List.assoc_opt "annotated_dead" fields with
        | Some value -> expect_bool "annotations.annotated_dead" value
        | None -> false
      in
      let annotated_gen_type_or_live =
        match List.assoc_opt "annotated_gen_type_or_live" fields with
        | Some value -> expect_bool "annotations.annotated_gen_type_or_live" value
        | None -> false
      in
      let annotated_gen_type_or_dead =
        match List.assoc_opt "annotated_gen_type_or_dead" fields with
        | Some value -> expect_bool "annotations.annotated_gen_type_or_dead" value
        | None -> false
      in
      {annotated_dead; annotated_gen_type_or_live; annotated_gen_type_or_dead}

let decl_of_json json =
  let fields = expect_object "decl" json in
  let path =
    field "path" fields |> expect_array "decl.path"
    |> List.map (expect_string "decl.path entry")
  in
  let module_path =
    field "module_path" fields |> expect_array "decl.module_path"
    |> List.map (expect_string "decl.module_path entry")
  in
  let name = field "name" fields |> expect_string "decl.name" in
  let loc = field "loc" fields |> range_of_json in
  let module_loc =
    match List.assoc_opt "module_loc" fields with
    | Some Json.Null | None -> None
    | Some json -> Some (range_of_json json)
  in
  let kind =
    match field "kind" fields |> expect_string "decl.kind" with
    | "Value" -> `Value
    | "Exception" -> `Exception
    | "RecordLabel" -> `RecordLabel
    | "VariantCase" -> `VariantCase
    | other -> raise (Invalid_format ("unknown decl kind: " ^ other))
  in
  let pos_adjustment =
    match List.assoc_opt "pos_adjustment" fields with
    | Some Json.Null | None -> None
    | Some (Json.String s) -> (
        match pos_adjustment_of_string s with
        | Some adj -> Some adj
        | None -> raise (Invalid_format ("unknown pos_adjustment: " ^ s)))
    | Some _ -> raise (Invalid_format "decl.pos_adjustment")
  in
  let annotations =
    match List.assoc_opt "annotations" fields with
    | Some json -> annotations_of_json json
    | None -> annotation_snapshot_none
  in
  let decl_kind =
    match kind with
    | `Value ->
        let is_toplevel =
          field "is_toplevel" fields |> expect_bool "decl.is_toplevel"
        in
        let side_effects =
          field "side_effects" fields |> expect_bool "decl.side_effects"
        in
        let optional_args =
          match optional_args_of_json (field "optional_args" fields) with
          | Some snapshot -> snapshot
          | None -> raise (Invalid_format "decl.optional_args is required for value decls")
        in
        Value {is_toplevel; side_effects; optional_args}
    | `Exception -> Exception
    | `RecordLabel -> RecordLabel
    | `VariantCase -> VariantCase
  in
  {path; module_path; name; loc; module_loc; decl_kind; pos_adjustment; annotations}

let value_reference_of_json json =
  let fields = expect_object "value_ref" json in
  let add_file_edge = field "add_file_edge" fields |> expect_bool "value_ref.add_file_edge" in
  let loc_from = field "from" fields |> position_of_json in
  let loc_to = field "to" fields |> position_of_json in
  let target_path =
    match List.assoc_opt "target_path" fields with
    | None -> None
    | Some Json.Null -> None
    | Some (Json.Array items) ->
        Some (List.map (expect_string "value_ref.path entry") items)
    | Some _ -> raise (Invalid_format "value_ref.path")
  in
  {loc_from; loc_to; add_file_edge; target_path}

let type_reference_of_json json =
  let fields = expect_object "type_ref" json in
  let pos_from = field "from" fields |> position_of_json in
  let pos_to = field "to" fields |> position_of_json in
  {pos_from; pos_to}

let file_edge_of_json json =
  let fields = expect_object "file_edge" json in
  let from_file = field "from" fields |> expect_string "file_edge.from" in
  let to_file = field "to" fields |> expect_string "file_edge.to" in
  {from_file; to_file}

let of_json json =
  let fields = expect_object "summary" json in
  let version_value = field "version" fields |> expect_int "version" in
  if version_value <> version && version_value <> 1 then
    raise
      (Invalid_format
         (Printf.sprintf "unsupported summary version %d (expected %d)" version_value version));
  let source_file = field "source_file" fields |> expect_string "source_file" in
  let digest = field "digest" fields |> expect_string "digest" in
  let decls =
    field "decls" fields |> expect_array "decls"
    |> List.map decl_of_json
  in
  let value_references =
    field "value_refs" fields |> expect_array "value_refs"
    |> List.map value_reference_of_json
  in
  let type_references =
    field "type_refs" fields |> expect_array "type_refs"
    |> List.map type_reference_of_json
  in
  let file_edges =
    field "file_edges" fields |> expect_array "file_edges"
    |> List.map file_edge_of_json
  in
  let summary =
    {version; source_file; digest; decls; value_references; type_references; file_edges}
  in
  verify_digest summary;
  summary

let of_string str =
  match Json.parse str with
  | Some json -> of_json json
  | None -> raise (Invalid_format "invalid summary JSON")

let () = ()

