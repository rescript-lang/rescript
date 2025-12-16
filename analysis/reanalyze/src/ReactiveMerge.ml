(** Reactive merge of per-file DCE data into global collections.

    Given a reactive collection of (path, file_data), this creates derived
    reactive collections that automatically update when source files change. *)

(** {1 Types} *)

type t = {
  decls: (Lexing.position, Decl.t) Reactive.t;
  annotations: (Lexing.position, FileAnnotations.annotated_as) Reactive.t;
  value_refs: (Lexing.position, PosSet.t) Reactive.t;
  type_refs: (Lexing.position, PosSet.t) Reactive.t;
  cross_file_items: (string, CrossFileItems.t) Reactive.t;
  file_deps_map: (string, FileSet.t) Reactive.t;
  files: (string, unit) Reactive.t;
  (* Reactive type/exception dependencies *)
  type_deps: ReactiveTypeDeps.t;
  exception_refs: ReactiveExceptionRefs.t;
}
(** All derived reactive collections from per-file data *)

(** {1 Creation} *)

let create (source : (string, DceFileProcessing.file_data option) Reactive.t) :
    t =
  (* Declarations: (pos, Decl.t) with last-write-wins *)
  let decls =
    Reactive.flatMap source
      ~f:(fun _path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          Declarations.builder_to_list file_data.DceFileProcessing.decls)
      ()
  in

  (* Annotations: (pos, annotated_as) with last-write-wins *)
  let annotations =
    Reactive.flatMap source
      ~f:(fun _path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          FileAnnotations.builder_to_list
            file_data.DceFileProcessing.annotations)
      ()
  in

  (* Value refs: (posTo, PosSet) with PosSet.union merge *)
  let value_refs =
    Reactive.flatMap source
      ~f:(fun _path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          References.builder_value_refs_to_list file_data.DceFileProcessing.refs)
      ~merge:PosSet.union ()
  in

  (* Type refs: (posTo, PosSet) with PosSet.union merge *)
  let type_refs =
    Reactive.flatMap source
      ~f:(fun _path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          References.builder_type_refs_to_list file_data.DceFileProcessing.refs)
      ~merge:PosSet.union ()
  in

  (* Cross-file items: (path, CrossFileItems.t) with merge by concatenation *)
  let cross_file_items =
    Reactive.flatMap source
      ~f:(fun path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          let items =
            CrossFileItems.builder_to_t file_data.DceFileProcessing.cross_file
          in
          [(path, items)])
      ~merge:(fun a b ->
        CrossFileItems.
          {
            exception_refs = a.exception_refs @ b.exception_refs;
            optional_arg_calls = a.optional_arg_calls @ b.optional_arg_calls;
            function_refs = a.function_refs @ b.function_refs;
          })
      ()
  in

  (* File deps map: (from_file, FileSet of to_files) with FileSet.union merge *)
  let file_deps_map =
    Reactive.flatMap source
      ~f:(fun _path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          FileDeps.builder_deps_to_list file_data.DceFileProcessing.file_deps)
      ~merge:FileSet.union ()
  in

  (* Files set: (source_path, ()) - just track which source files exist *)
  let files =
    Reactive.flatMap source
      ~f:(fun _cmt_path file_data_opt ->
        match file_data_opt with
        | None -> []
        | Some file_data ->
          (* Include all source files from file_deps (NOT the CMT path) *)
          let file_set =
            FileDeps.builder_files file_data.DceFileProcessing.file_deps
          in
          FileSet.fold (fun f acc -> (f, ()) :: acc) file_set [])
      ()
  in

  (* Extract exception_refs from cross_file_items for ReactiveExceptionRefs *)
  let exception_refs_collection =
    Reactive.flatMap cross_file_items
      ~f:(fun _path items ->
        items.CrossFileItems.exception_refs
        |> List.map (fun (r : CrossFileItems.exception_ref) ->
               (r.exception_path, r.loc_from)))
      ()
  in

  (* Create reactive type-label dependencies *)
  let type_deps =
    ReactiveTypeDeps.create ~decls
      ~report_types_dead_only_in_interface:
        DeadCommon.Config.reportTypesDeadOnlyInInterface
  in

  (* Create reactive exception refs resolution *)
  let exception_refs =
    ReactiveExceptionRefs.create ~decls
      ~exception_refs:exception_refs_collection
  in

  {
    decls;
    annotations;
    value_refs;
    type_refs;
    cross_file_items;
    file_deps_map;
    files;
    type_deps;
    exception_refs;
  }

(** {1 Conversion to solver-ready format} *)

(** Convert reactive decls to Declarations.t for solver *)
let freeze_decls (t : t) : Declarations.t =
  let result = PosHash.create 256 in
  Reactive.iter (fun pos decl -> PosHash.replace result pos decl) t.decls;
  Declarations.create_from_hashtbl result

(** Convert reactive annotations to FileAnnotations.t for solver *)
let freeze_annotations (t : t) : FileAnnotations.t =
  let result = PosHash.create 256 in
  Reactive.iter (fun pos ann -> PosHash.replace result pos ann) t.annotations;
  FileAnnotations.create_from_hashtbl result

(** Convert reactive refs to References.t for solver.
    Includes type-label deps and exception refs from reactive computations. *)
let freeze_refs (t : t) : References.t =
  let value_refs = PosHash.create 256 in
  let type_refs = PosHash.create 256 in
  (* Helper to merge refs into a hashtable *)
  let merge_into tbl posTo posFromSet =
    let existing =
      match PosHash.find_opt tbl posTo with
      | Some s -> s
      | None -> PosSet.empty
    in
    PosHash.replace tbl posTo (PosSet.union existing posFromSet)
  in
  (* Merge per-file value refs *)
  Reactive.iter (fun pos refs -> merge_into value_refs pos refs) t.value_refs;
  (* Merge per-file type refs *)
  Reactive.iter (fun pos refs -> merge_into type_refs pos refs) t.type_refs;
  (* Add type-label dependency refs from all sources *)
  Reactive.iter
    (fun pos refs -> merge_into type_refs pos refs)
    t.type_deps.same_path_refs;
  Reactive.iter
    (fun pos refs -> merge_into type_refs pos refs)
    t.type_deps.cross_file_refs;
  Reactive.iter
    (fun pos refs -> merge_into type_refs pos refs)
    t.type_deps.impl_to_intf_refs_path2;
  Reactive.iter
    (fun pos refs -> merge_into type_refs pos refs)
    t.type_deps.intf_to_impl_refs;
  (* Add exception refs (to value refs) *)
  Reactive.iter
    (fun pos refs -> merge_into value_refs pos refs)
    t.exception_refs.resolved_refs;
  References.create ~value_refs ~type_refs

(** Collect all cross-file items *)
let collect_cross_file_items (t : t) : CrossFileItems.t =
  let exception_refs = ref [] in
  let optional_arg_calls = ref [] in
  let function_refs = ref [] in
  Reactive.iter
    (fun _path items ->
      exception_refs := items.CrossFileItems.exception_refs @ !exception_refs;
      optional_arg_calls :=
        items.CrossFileItems.optional_arg_calls @ !optional_arg_calls;
      function_refs := items.CrossFileItems.function_refs @ !function_refs)
    t.cross_file_items;
  {
    CrossFileItems.exception_refs = !exception_refs;
    optional_arg_calls = !optional_arg_calls;
    function_refs = !function_refs;
  }

(** Convert reactive file deps to FileDeps.t for solver.
    Includes file deps from exception refs. *)
let freeze_file_deps (t : t) : FileDeps.t =
  let files =
    let result = ref FileSet.empty in
    Reactive.iter (fun path () -> result := FileSet.add path !result) t.files;
    !result
  in
  let deps = FileDeps.FileHash.create 256 in
  Reactive.iter
    (fun from_file to_files ->
      FileDeps.FileHash.replace deps from_file to_files)
    t.file_deps_map;
  (* Add file deps from exception refs *)
  Reactive.iter
    (fun posTo posFromSet ->
      PosSet.iter
        (fun posFrom ->
          let from_file = posFrom.Lexing.pos_fname in
          let to_file = posTo.Lexing.pos_fname in
          if from_file <> to_file then
            let existing =
              match FileDeps.FileHash.find_opt deps from_file with
              | Some s -> s
              | None -> FileSet.empty
            in
            FileDeps.FileHash.replace deps from_file
              (FileSet.add to_file existing))
        posFromSet)
    t.exception_refs.resolved_refs;
  FileDeps.create ~files ~deps
