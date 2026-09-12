type change_kind = Added | Removed | Modified
type change = {path: string; kind: change_kind}

type file = {modified: float; size: int; digest: string}
type dependency = {modified: float; size: int}

type state =
  | File of file
  | Dependency_candidate of dependency
  | Missing_dependency_candidate
  | Unreadable

type entry = {path: string; state: state}

let entry_equal first second =
  first.path = second.path
  &&
  match (first.state, second.state) with
  | File first, File second ->
    first.modified = second.modified
    && first.size = second.size
    && first.digest = second.digest
  | Dependency_candidate first, Dependency_candidate second ->
    first.modified = second.modified && first.size = second.size
  | Missing_dependency_candidate, Missing_dependency_candidate -> true
  | Unreadable, Unreadable -> true
  | File _, (Dependency_candidate _ | Missing_dependency_candidate | Unreadable)
  | Dependency_candidate _, (File _ | Missing_dependency_candidate | Unreadable)
  | Missing_dependency_candidate, (File _ | Dependency_candidate _ | Unreadable)
  | Unreadable, (File _ | Dependency_candidate _ | Missing_dependency_candidate)
    ->
    false

let equal = List.equal entry_equal

let rec nearest_existing_ancestor path =
  if File_util.is_directory path then Some (Platform.canonicalize_path path)
  else
    let parent = Filename.dirname path in
    if parent = path then None else nearest_existing_ancestor parent

let create ?(on_source_symlink = fun _ -> ()) digest_cache
    (scope : Watch_scope.t) =
  let visited_directories = Hashtbl.create 64 in
  let seen_files = Hashtbl.create 256 in
  let unreadable path acc =
    Hashtbl.replace seen_files path ();
    Hashtbl.remove digest_cache path;
    {path; state = Unreadable} :: acc
  in
  let digest path stat =
    Hashtbl.replace seen_files path ();
    (* Native events are only wakeups; a content snapshot decides whether to
       rebuild. Reuse hashes while all cheap identity fields are unchanged. *)
    match Hashtbl.find_opt digest_cache path with
    | Some (mtime, ctime, size, digest)
      when mtime = stat.Unix.st_mtime && ctime = stat.Unix.st_ctime
           && size = stat.Unix.st_size ->
      digest
    | _ ->
      let digest = File_util.digest_file path |> Digest.to_hex in
      Hashtbl.replace digest_cache path
        (stat.Unix.st_mtime, stat.Unix.st_ctime, stat.Unix.st_size, digest);
      digest
  in
  let add_file path stat acc =
    try
      if Hashtbl.mem seen_files path then acc
      else
        let digest = digest path stat in
        {
          path;
          state =
            File
              {modified = stat.Unix.st_mtime; size = stat.Unix.st_size; digest};
        }
        :: acc
    with
    | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
      Hashtbl.remove seen_files path;
      Hashtbl.remove digest_cache path;
      acc
    | Unix.Unix_error _ | Sys_error _ -> unreadable path acc
  in
  let matches_source (source : Watch_scope.source_root) path =
    Option.fold ~none:true
      ~some:(fun filter -> Source_filter.matches_basename filter path)
      source.filter
  in
  let rec walk (source : Watch_scope.source_root) recursive dir acc =
    match Platform.canonicalize_path dir with
    | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> acc
    | exception (Unix.Unix_error _ | Sys_error _) -> unreadable dir acc
    | canonical -> (
      let admission =
        Traversal_coverage.admit visited_directories canonical ~recursive
      in
      match admission with
      | Traversal_coverage.Skip -> acc
      | Traversal_coverage.Visit_current
      | Traversal_coverage.Visit_current_and_descendants
      | Traversal_coverage.Visit_descendants ->
        let visit_current = Traversal_coverage.visits_current admission in
        let visit_descendants =
          Traversal_coverage.visits_descendants admission
        in
        let entries =
          try Some (File_util.directory_entries dir) with
          | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Some []
          | Unix.Unix_error _ | Sys_error _ -> None
        in
        Option.fold ~none:(unreadable dir acc)
          ~some:
            (List.fold_left
               (fun acc name ->
                 let path = Filename.concat dir name in
                 match Unix.lstat path with
                 | stat -> (
                   match stat.Unix.st_kind with
                   | Unix.S_DIR ->
                     if
                       (not visit_descendants)
                       || Native_watcher.is_compiler_artifact_directory path
                     then acc
                     else walk source true path acc
                   | Unix.S_LNK -> (
                     try
                       let is_source_name =
                         Option.is_some (Source.source_kind path)
                         && matches_source source path
                       in
                       (if visit_current && is_source_name then
                          match Unix.readlink path with
                          | target ->
                            let target =
                              if Filename.is_relative target then
                                Filename.concat (Filename.dirname path) target
                              else target
                            in
                            let target =
                              match Platform.canonicalize_path target with
                              | canonical -> canonical
                              | exception
                                  Unix.Unix_error
                                    ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
                                target
                            in
                            on_source_symlink target
                          | exception
                              Unix.Unix_error
                                ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
                            ());
                       match Unix.stat path with
                       | target -> (
                         match target.Unix.st_kind with
                         | Unix.S_DIR when visit_descendants ->
                           walk source true path acc
                         | Unix.S_REG when visit_current && is_source_name ->
                           add_file path target acc
                         | _ -> acc)
                       | exception
                           Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _)
                         ->
                         acc
                       | exception (Unix.Unix_error _ | Sys_error _) ->
                         unreadable path acc
                     with Unix.Unix_error _ | Sys_error _ ->
                       unreadable path acc)
                   | Unix.S_REG
                     when visit_current
                          && Option.is_some (Source.source_kind path)
                          && matches_source source path ->
                     add_file path stat acc
                   | _ -> acc)
                 | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _)
                   ->
                   acc
                 | exception (Unix.Unix_error _ | Sys_error _) ->
                   unreadable path acc)
               acc)
          entries
      | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> acc)
  in
  let add_control_files acc root =
    Watch_scope.control_file_names
    |> List.fold_left
         (fun acc name ->
           let path = Filename.concat root name in
           match Unix.stat path with
           | stat ->
             if stat.Unix.st_kind = Unix.S_REG then add_file path stat acc
             else acc
           | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
             acc
           | exception (Unix.Unix_error _ | Sys_error _) -> unreadable path acc)
         acc
  in
  let result =
    List.fold_left add_control_files [] scope.roots |> fun acc ->
    List.fold_left
      (fun acc source -> walk source source.recursive source.directory acc)
      acc scope.sources
    |> fun acc ->
    List.fold_left
      (fun acc path ->
        match Unix.lstat path with
        | stat ->
          Hashtbl.replace seen_files path ();
          {
            path;
            state =
              Dependency_candidate
                {modified = stat.Unix.st_mtime; size = stat.Unix.st_size};
          }
          :: acc
        | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
          {path; state = Missing_dependency_candidate} :: acc
        | exception (Unix.Unix_error _ | Sys_error _) -> unreadable path acc)
      acc scope.unresolved
    |> List.sort compare
  in
  Hashtbl.filter_map_inplace
    (fun path value -> if Hashtbl.mem seen_files path then Some value else None)
    digest_cache;
  result

let create_with_symlink_paths digest_cache scope =
  let targets = ref [] in
  let snapshot =
    create
      ~on_source_symlink:(fun target -> targets := target :: !targets)
      digest_cache scope
  in
  let paths =
    !targets
    |> List.sort_uniq String.compare
    |> List.filter_map (fun target ->
        Filename.dirname target |> nearest_existing_ancestor)
    |> List.concat_map (fun directory ->
        let parent = Filename.dirname directory in
        let directories =
          if parent = directory then [directory] else [directory; parent]
        in
        List.map
          (fun directory -> Native_watcher.{directory; recursive = false})
          directories)
  in
  (snapshot, paths, List.sort_uniq String.compare !targets)

let changes_between before after =
  (* A content snapshot deliberately treats native events only as wakeups. The
     before/after membership is still enough to distinguish an in-place edit,
     which can reuse the build graph, from a structural change that requires
     rediscovery. *)
  let before_by_path = Hashtbl.create (List.length before) in
  let after_by_path = Hashtbl.create (List.length after) in
  List.iter
    (fun entry -> Hashtbl.replace before_by_path entry.path entry)
    before;
  List.iter (fun entry -> Hashtbl.replace after_by_path entry.path entry) after;
  let changes = ref [] in
  Hashtbl.iter
    (fun path before_entry ->
      match Hashtbl.find_opt after_by_path path with
      | None -> changes := {path; kind = Removed} :: !changes
      | Some after_entry when not (entry_equal before_entry after_entry) ->
        changes := {path; kind = Modified} :: !changes
      | Some _ -> ())
    before_by_path;
  Hashtbl.iter
    (fun path _ ->
      if not (Hashtbl.mem before_by_path path) then
        changes := {path; kind = Added} :: !changes)
    after_by_path;
  List.sort
    (fun (first : change) second -> String.compare first.path second.path)
    !changes

let update_entries digest_cache previous changes =
  let changed = Hashtbl.create (List.length changes) in
  List.iter
    (fun (change : change) -> Hashtbl.replace changed change.path change.kind)
    changes;
  let updated =
    previous
    |> List.filter_map (fun entry ->
        match Hashtbl.find_opt changed entry.path with
        | None -> Some entry
        | Some Removed ->
          Hashtbl.remove changed entry.path;
          Hashtbl.remove digest_cache entry.path;
          None
        | Some (Added | Modified) -> (
          Hashtbl.remove changed entry.path;
          try
            let stat = Unix.stat entry.path in
            let digest = File_util.digest_file entry.path |> Digest.to_hex in
            Hashtbl.replace digest_cache entry.path
              (stat.Unix.st_mtime, stat.Unix.st_ctime, stat.Unix.st_size, digest);
            Some
              {
                path = entry.path;
                state =
                  File
                    {
                      modified = stat.Unix.st_mtime;
                      size = stat.Unix.st_size;
                      digest;
                    };
              }
          with
          | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
            Hashtbl.remove digest_cache entry.path;
            None
          | Unix.Unix_error _ | Sys_error _ ->
            (* Preserve the old baseline so the caller still runs the build.
               Source diagnostics then remain inside the watcher's recoverable
               build boundary instead of terminating the watch loop here. *)
            Some entry))
  in
  if Hashtbl.length changed = 0 then Some updated else None

let polling_build_changes ~previous ~trigger ~before_build =
  if equal trigger previous then [] else changes_between previous before_build

let changes_are_incremental changes =
  changes <> []
  && List.for_all
       (fun change ->
         change.kind = Modified
         && Option.is_some (Source.source_kind change.path))
       changes

let reconciliation_baseline ~(old_scope : Watch_scope.t) ~new_scope before after
    =
  (* A configuration edit can add or remove files from the watch scope. Those
     membership changes are already covered by the build that read the new
     configuration; only changes to files shared by both scopes require
     another build. *)
  let shared path =
    Watch_scope.path_in_scope old_scope path
    && Watch_scope.path_in_scope new_scope path
  in
  List.filter (fun entry -> not (shared entry.path)) after
  @ List.filter (fun entry -> shared entry.path) before
  |> List.sort compare
