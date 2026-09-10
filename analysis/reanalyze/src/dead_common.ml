module File_context = struct
  type t = {source_path: string; module_name: string; is_interface: bool}

  (** Get module name as Name.t tagged with interface/implementation info *)
  let module_name_tagged file =
    file.module_name |> Name.create ~is_interface:file.is_interface

  let is_interface (file : t) = file.is_interface
end

(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

module Config = struct
  (* Turn on type analysis *)
  let analyze_types = ref true
  let analyze_externals = ref false
  let report_underscore = false
  let report_types_dead_only_in_interface = false
  let warn_on_circular_dependencies = false
end

let rec check_sub s1 s2 n =
  n <= 0
  || (try s1.[n] = s2.[n] with Invalid_argument _ -> false)
     && check_sub s1 s2 (n - 1)

let file_is_implementation_of s1 s2 =
  let n1 = String.length s1 and n2 = String.length s2 in
  n2 = n1 + 1 && check_sub s1 s2 (n1 - 1)

let live_annotation = "live"

type decls = Decl.t Pos_hash.t
(** type alias for declaration hashtables *)

(* NOTE: Global decls removed - now using Declarations.builder/t pattern *)

(* NOTE: Global ValueReferences removed - now using References.builder/t pattern *)

(* Local reporting context used only while emitting dead-code warnings.
   It tracks, per file, the end position of the last value we reported on,
   so nested values inside that range don't get duplicate warnings. *)
module Reporting_context = struct
  type t = Lexing.position ref

  let create () : t = ref Lexing.dummy_pos
  let get_max_end (ctx : t) = !ctx
  let set_max_end (ctx : t) (pos : Lexing.position) = ctx := pos
end

(* NOTE: Global TypeReferences removed - now using References.builder/t pattern *)

let decl_get_loc decl =
  let loc_start =
    let offset =
      match decl.Decl.pos_adjustment with
      | FirstVariant | Nothing -> 0
      | OtherVariant -> 2
    in
    let cnum_with_offset = decl.pos_start.pos_cnum + offset in
    if cnum_with_offset < decl.pos_end.pos_cnum then
      {decl.pos_start with pos_cnum = cnum_with_offset}
    else decl.pos_start
  in
  {Location.loc_start; loc_end = decl.pos_end; loc_ghost = false}

let add_value_reference ~config ~refs ~file_deps ~(binding : Location.t)
    ~add_file_reference ~(loc_from : Location.t) ~(loc_to : Location.t) : unit =
  let effective_from = if binding = Location.none then loc_from else binding in
  if not effective_from.loc_ghost then (
    if config.Dce_config.cli.debug then
      Log_.item "addValueReference %s --> %s@."
        (effective_from.loc_start |> Pos.to_string)
        (loc_to.loc_start |> Pos.to_string);
    References.add_value_ref refs ~pos_to:loc_to.loc_start
      ~pos_from:effective_from.loc_start;
    if
      add_file_reference && (not loc_to.loc_ghost)
      && (not effective_from.loc_ghost)
      && effective_from.loc_start.pos_fname <> loc_to.loc_start.pos_fname
    then
      File_deps.add_dep file_deps ~from_file:effective_from.loc_start.pos_fname
        ~to_file:loc_to.loc_start.pos_fname)

let addDeclaration_ ~config ~decls ~(file : File_context.t) ?pos_end ?pos_start
    ~decl_kind ~path ~(loc : Location.t) ?(pos_adjustment = Decl.Nothing)
    ?manifest_type_path ~module_loc (name : Name.t) =
  let pos = loc.loc_start in
  let pos_start =
    match pos_start with
    | Some pos_start -> pos_start
    | None -> pos
  in
  let pos_end =
    match pos_end with
    | Some pos_end -> pos_end
    | None -> loc.loc_end
  in
  (* a .cmi file can contain locations from other files.
     For instance:
         module M : Set.S with type elt = int
     will create value definitions whose location is in set.mli
  *)
  if (not loc.loc_ghost) && pos.pos_fname = file.source_path then (
    if config.Dce_config.cli.debug then
      Log_.item "add%sDeclaration %s %s path:%s@."
        (decl_kind |> Decl.Kind.to_string)
        (name |> Name.to_string) (pos |> Pos.to_string)
        (path |> Dce_path.to_string);
    let decl =
      {
        Decl.decl_kind;
        module_loc;
        pos_adjustment;
        path = name :: path;
        manifest_type_path;
        pos;
        pos_end;
        pos_start;
        resolved_dead = None;
        report = true;
      }
    in
    Declarations.add decls pos decl)

let add_value_declaration ~config ~decls ~file ?(is_toplevel = true)
    ?(reports_optional_args = false) ~(loc : Location.t) ~module_loc
    ?(optional_args = Optional_args.empty) ~path ~side_effects name =
  name
  |> addDeclaration_ ~config ~decls ~file
       ~decl_kind:
         (Value
            {is_toplevel; reports_optional_args; optional_args; side_effects})
       ~loc ~module_loc ~path

(** Create a dead code issue. Pure - no side effects. *)
let make_dead_issue ~decl ~message dead_warning : Issue.t =
  let loc = decl |> decl_get_loc in
  Analysis_result.make_dead_issue ~loc ~dead_warning
    ~path:(Dce_path.without_head decl.path)
    ~message

let is_inside_reported_value (ctx : Reporting_context.t) decl =
  let max_end = Reporting_context.get_max_end ctx in
  let file_has_changed = max_end.pos_fname <> decl.Decl.pos.pos_fname in
  let inside_reported_value =
    decl |> Decl.is_value && (not file_has_changed)
    && max_end.pos_cnum > decl.pos.pos_cnum
  in
  if not inside_reported_value then
    if decl |> Decl.is_value then
      if file_has_changed || decl.pos_end.pos_cnum > max_end.pos_cnum then
        Reporting_context.set_max_end ctx decl.pos_end;
  inside_reported_value

(** Check if a reference position is "below" the declaration.
    A ref is below if it's in a different file, or comes after the declaration
    (but not inside it, e.g. not a callback). *)
let ref_is_below (decl : Decl.t) (pos_from : Lexing.position) =
  decl.pos.pos_fname <> pos_from.pos_fname
  || decl.pos.pos_cnum < pos_from.pos_cnum
     &&
     (* not a function defined inside a function, e.g. not a callback *)
     decl.pos_end.pos_cnum < pos_from.pos_cnum

(** Create hasRefBelow function using on-demand per-decl search.
    [iter_value_refs_from] iterates over (posFrom, posToSet) pairs.
    O(total_refs) per dead decl, but dead decls should be few. *)
let make_hasRefBelow ~transitive ~iter_value_refs_from =
  if transitive then fun _ -> false
  else fun decl ->
    let found = ref false in
    iter_value_refs_from (fun pos_from pos_to_set ->
        if (not !found) && Pos_set.mem decl.Decl.pos pos_to_set then
          if ref_is_below decl pos_from then found := true);
    !found

(** Report a dead declaration. Returns list of issues (dead module first, then dead value).
    [hasRefBelow] checks if there are references from "below" the declaration.
    Only used when [config.run.transitive] is false.
    [?checkModuleDead] optional callback for checking dead modules. Defaults to DeadModules.checkModuleDead.
    [?shouldReport] optional callback to check if a decl should be reported. Defaults to checking decl.report. *)
let report_declaration ~config ~has_ref_below ?check_module_dead ?should_report
    (ctx : Reporting_context.t) decl : Issue.t list =
  let inside_reported_value = decl |> is_inside_reported_value ctx in
  let should_report =
    match should_report with
    | Some f -> f decl
    | None -> decl.report
  in
  (* For type re-exports (type y = x = {...}), the re-exported record/variant
     labels are restated but not independently actionable. Avoid duplicate/noisy
     warnings by suppressing reporting for the re-exported copy. *)
  let should_report =
    should_report
    &&
    match (decl.decl_kind, decl.manifest_type_path) with
    | (RecordLabel | VariantCase), Some _ -> false
    | _ -> true
  in
  if not should_report then []
  else
    let dead_warning, message =
      match decl.decl_kind with
      | Exception ->
        (Issue.WarningDeadException, "is never raised or passed as value")
      | Value {side_effects} -> (
        let no_side_effects_or_underscore =
          (not side_effects)
          ||
          match decl.path with
          | hd :: _ -> hd |> Name.starts_with_underscore
          | [] -> false
        in
        ( (match not no_side_effects_or_underscore with
          | true -> WarningDeadValueWithSideEffects
          | false -> WarningDeadValue),
          match decl.path with
          | name :: _ when name |> Name.is_underscore ->
            "has no side effects and can be removed"
          | _ -> (
            "is never used"
            ^
            match not no_side_effects_or_underscore with
            | true -> " and could have side effects"
            | false -> "") ))
      | RecordLabel ->
        (WarningDeadType, "is a record label never used to read a value")
      | VariantCase ->
        (WarningDeadType, "is a variant case which is never constructed")
    in
    let should_emit_warning =
      (not inside_reported_value)
      && (match decl.path with
        | name :: _ when name |> Name.is_underscore -> Config.report_underscore
        | _ -> true)
      && (config.Dce_config.run.transitive || not (has_ref_below decl))
    in
    if should_emit_warning then
      let module_name =
        decl.path
        |> Dce_path.to_module_name ~is_type:(decl.decl_kind |> Decl.Kind.is_type)
      in
      let dead_module_issue =
        match check_module_dead with
        | Some f -> f ~file_name:decl.pos.pos_fname module_name
        | None ->
          Dead_modules.check_module_dead ~config ~file_name:decl.pos.pos_fname
            module_name
      in
      let dead_value_issue = make_dead_issue ~decl ~message dead_warning in
      (* Return in order: dead module first (if any), then dead value *)
      match dead_module_issue with
      | Some mi -> [mi; dead_value_issue]
      | None -> [dead_value_issue]
    else []

let do_report_dead ~ann_store pos =
  not (Annotation_store.is_annotated_gentype_or_dead ann_store pos)
