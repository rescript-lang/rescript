(** Reactive merge of per-file DCE data into global collections.

    Given a reactive collection of (path, file_data), this creates derived
    reactive collections that automatically update when source files change. *)

(** {1 Types} *)

type t = {
  decls: (Lexing.position, Decl.t) Reactive.t;
  annotations: (Lexing.position, File_annotations.annotated_as) Reactive.t;
  value_refs_from: (Lexing.position, Pos_set.t) Reactive.t;
  type_refs_from: (Lexing.position, Pos_set.t) Reactive.t;
  cross_file_items: (string, Cross_file_items.t) Reactive.t;
  file_deps_map: (string, File_set.t) Reactive.t;
  files: (string, unit) Reactive.t;
  (* Reactive type/exception dependencies *)
  type_deps: Reactive_type_deps.t;
  exception_refs: Reactive_exception_refs.t;
}
(** All derived reactive collections from per-file data *)

(** {1 Creation} *)

let create (source : (string, Dce_file_processing.file_data option) Reactive.t)
    : t =
  (* Declarations: (pos, Decl.t) with last-write-wins *)
  let decls =
    Reactive.flat_map ~name:"decls" source
      ~f:(fun _path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          Declarations.builder_to_list file_data.Dce_file_processing.decls)
      ()
  in

  (* Annotations: (pos, annotated_as) with last-write-wins *)
  let annotations =
    Reactive.flat_map ~name:"annotations" source
      ~f:(fun _path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          File_annotations.builder_to_list
            file_data.Dce_file_processing.annotations)
      ()
  in

  (* Value refs_from: (posFrom, PosSet of targets) with PosSet.union merge *)
  let value_refs_from =
    Reactive.flat_map ~name:"value_refs_from" source
      ~f:(fun _path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          References.builder_value_refs_from_list
            file_data.Dce_file_processing.refs)
      ~merge:Pos_set.union ()
  in

  (* Type refs_from: (posFrom, PosSet of targets) with PosSet.union merge *)
  let type_refs_from =
    Reactive.flat_map ~name:"type_refs_from" source
      ~f:(fun _path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          References.builder_type_refs_from_list
            file_data.Dce_file_processing.refs)
      ~merge:Pos_set.union ()
  in

  (* Cross-file items: (path, CrossFileItems.t) with merge by concatenation *)
  let cross_file_items =
    Reactive.flat_map ~name:"cross_file_items" source
      ~f:(fun path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          let items =
            Cross_file_items.builder_to_t
              file_data.Dce_file_processing.cross_file
          in
          [(path, items)])
      ~merge:(fun a b ->
        Cross_file_items.
          {
            exception_refs = a.exception_refs @ b.exception_refs;
            optional_arg_calls = a.optional_arg_calls @ b.optional_arg_calls;
            function_refs = a.function_refs @ b.function_refs;
          })
      ()
  in

  (* File deps map: (from_file, FileSet of to_files) with FileSet.union merge *)
  let file_deps_map =
    Reactive.flat_map ~name:"file_deps_map" source
      ~f:(fun _path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          File_deps.builder_deps_to_list file_data.Dce_file_processing.file_deps)
      ~merge:File_set.union ()
  in

  (* Files set: (source_path, ()) - just track which source files exist *)
  let files =
    Reactive.flat_map ~name:"files" source
      ~f:(fun _cmt_path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          (* Include all source files from file_deps (NOT the CMT path) *)
          let file_set =
            File_deps.builder_files file_data.Dce_file_processing.file_deps
          in
          File_set.fold (fun f acc -> (f, ()) :: acc) file_set [])
      ()
  in

  (* Extract exception_refs from cross_file_items for ReactiveExceptionRefs *)
  let exception_refs_collection =
    Reactive.flat_map ~name:"exception_refs_collection" cross_file_items
      ~f:(fun _path items ->
        items.Cross_file_items.exception_refs
        |> List.map (fun (r : Cross_file_items.exception_ref) ->
               (r.exception_path, r.loc_from)))
      ()
  in

  (* Create reactive type-label dependencies *)
  let type_deps =
    Reactive_type_deps.create ~decls
      ~report_types_dead_only_in_interface:
        Dead_common.Config.report_types_dead_only_in_interface
  in

  (* Create reactive exception refs resolution *)
  let exception_refs =
    Reactive_exception_refs.create ~decls
      ~exception_refs:exception_refs_collection
  in

  {
    decls;
    annotations;
    value_refs_from;
    type_refs_from;
    cross_file_items;
    file_deps_map;
    files;
    type_deps;
    exception_refs;
  }

(** {1 Conversion to solver-ready format} *)

(** Convert reactive decls to Declarations.t for solver *)
let freeze_decls (t : t) : Declarations.t =
  let result = Pos_hash.create 256 in
  Reactive.iter (fun pos decl -> Pos_hash.replace result pos decl) t.decls;
  Declarations.create_from_hashtbl result

(** Convert reactive annotations to FileAnnotations.t for solver *)
let freeze_annotations (t : t) : File_annotations.t =
  let result = Pos_hash.create 256 in
  Reactive.iter (fun pos ann -> Pos_hash.replace result pos ann) t.annotations;
  File_annotations.create_from_hashtbl result

(** Convert reactive refs to References.t for solver.
    Includes type-label deps and exception refs from reactive computations. *)
let freeze_refs (t : t) : References.t =
  let value_refs_from = Pos_hash.create 256 in
  let type_refs_from = Pos_hash.create 256 in

  (* Helper to add to refs_from hashtable *)
  let add_to_from tbl pos_from pos_to =
    let existing =
      match Pos_hash.find_opt tbl pos_from with
      | Some s -> s
      | None -> Pos_set.empty
    in
    Pos_hash.replace tbl pos_from (Pos_set.add pos_to existing)
  in

  (* Merge per-file value refs_from *)
  Reactive.iter
    (fun pos_from pos_to_set ->
      Pos_set.iter
        (fun pos_to -> add_to_from value_refs_from pos_from pos_to)
        pos_to_set)
    t.value_refs_from;

  (* Merge per-file type refs_from *)
  Reactive.iter
    (fun pos_from pos_to_set ->
      Pos_set.iter
        (fun pos_to -> add_to_from type_refs_from pos_from pos_to)
        pos_to_set)
    t.type_refs_from;

  (* Add type-label dependency refs from all sources *)
  let add_type_refs_from reactive =
    Reactive.iter
      (fun pos_from pos_to_set ->
        Pos_set.iter
          (fun pos_to -> add_to_from type_refs_from pos_from pos_to)
          pos_to_set)
      reactive
  in
  add_type_refs_from t.type_deps.all_type_refs_from;

  (* Add exception refs (to value refs_from) *)
  Reactive.iter
    (fun pos_from pos_to_set ->
      Pos_set.iter
        (fun pos_to -> add_to_from value_refs_from pos_from pos_to)
        pos_to_set)
    t.exception_refs.resolved_refs_from;

  References.create ~value_refs_from ~type_refs_from

(** Collect all cross-file items *)
let collect_cross_file_items (t : t) : Cross_file_items.t =
  let exception_refs = ref [] in
  let optional_arg_calls = ref [] in
  let function_refs = ref [] in
  Reactive.iter
    (fun _path items ->
      exception_refs := items.Cross_file_items.exception_refs @ !exception_refs;
      optional_arg_calls :=
        items.Cross_file_items.optional_arg_calls @ !optional_arg_calls;
      function_refs := items.Cross_file_items.function_refs @ !function_refs)
    t.cross_file_items;
  {
    Cross_file_items.exception_refs = !exception_refs;
    optional_arg_calls = !optional_arg_calls;
    function_refs = !function_refs;
  }

(** Convert reactive file deps to FileDeps.t for solver.
    Includes file deps from exception refs. *)
let freeze_file_deps (t : t) : File_deps.t =
  let files =
    let result = ref File_set.empty in
    Reactive.iter (fun path () -> result := File_set.add path !result) t.files;
    !result
  in
  let deps = File_deps.File_hash.create 256 in
  Reactive.iter
    (fun from_file to_files ->
      File_deps.File_hash.replace deps from_file to_files)
    t.file_deps_map;
  (* Add file deps from exception refs - iterate value_refs_from *)
  Reactive.iter
    (fun pos_from pos_to_set ->
      Pos_set.iter
        (fun pos_to ->
          let from_file = pos_from.Lexing.pos_fname in
          let to_file = pos_to.Lexing.pos_fname in
          if from_file <> to_file then
            let existing =
              match File_deps.File_hash.find_opt deps from_file with
              | Some s -> s
              | None -> File_set.empty
            in
            File_deps.File_hash.replace deps from_file
              (File_set.add to_file existing))
        pos_to_set)
    t.exception_refs.resolved_refs_from;
  File_deps.create ~files ~deps
