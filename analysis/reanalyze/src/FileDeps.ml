(** File dependencies collected during AST processing.
    
    Tracks which files reference which other files. *)

(* File-keyed hashtable *)
module FileHash = Hashtbl.Make (struct
  type t = string

  let hash (x : t) = Hashtbl.hash x
  let equal (x : t) y = x = y
end)

(** {2 Types} *)

type t = {
  files: FileSet.t;
  deps: FileSet.t FileHash.t; (* from_file -> set of to_files *)
}

type builder = {mutable files: FileSet.t; deps: FileSet.t FileHash.t}

(** {2 Builder API} *)

let create_builder () : builder =
  {files = FileSet.empty; deps = FileHash.create 256}

let add_file (b : builder) file =
  b.files <- FileSet.add file b.files;
  (* Ensure file has an entry even if no deps *)
  if not (FileHash.mem b.deps file) then
    FileHash.replace b.deps file FileSet.empty

let add_dep (b : builder) ~from_file ~to_file =
  let set =
    match FileHash.find_opt b.deps from_file with
    | Some s -> s
    | None -> FileSet.empty
  in
  FileHash.replace b.deps from_file (FileSet.add to_file set)

(** {2 Merge API} *)

let merge_into_builder ~(from : builder) ~(into : builder) =
  into.files <- FileSet.union into.files from.files;
  FileHash.iter
    (fun from_file to_files ->
      let existing =
        match FileHash.find_opt into.deps from_file with
        | Some s -> s
        | None -> FileSet.empty
      in
      FileHash.replace into.deps from_file (FileSet.union existing to_files))
    from.deps

let freeze_builder (b : builder) : t =
  (* This is a zero-copy operation, so it's "unsafe" if the builder is
     subsequently mutated. However, the calling discipline is that the
     builder is no longer used after freezing. *)
  {files = b.files; deps = b.deps}

let merge_all (builders : builder list) : t =
  let merged_builder = create_builder () in
  builders
  |> List.iter (fun b -> merge_into_builder ~from:b ~into:merged_builder);
  freeze_builder merged_builder

(** {2 Builder extraction for reactive merge} *)

let builder_files (builder : builder) : FileSet.t = builder.files

let builder_deps_to_list (builder : builder) : (string * FileSet.t) list =
  FileHash.fold
    (fun from_file to_files acc -> (from_file, to_files) :: acc)
    builder.deps []

let create ~files ~deps : t = {files; deps}

(** {2 Read-only API} *)

let get_files (t : t) = t.files

let get_deps (t : t) file =
  match FileHash.find_opt t.deps file with
  | Some s -> s
  | None -> FileSet.empty

let iter_deps (t : t) f = FileHash.iter f t.deps

let file_exists (t : t) file = FileHash.mem t.deps file

let files_count (t : t) = FileSet.cardinal t.files

let deps_count (t : t) = FileHash.length t.deps

(** {2 Topological ordering} *)

let iter_files_from_roots_to_leaves (t : t) iter_fun =
  (* For each file, the number of incoming references *)
  let inverse_references = (Hashtbl.create 256 : (string, int) Hashtbl.t) in
  (* For each number of incoming references, the files *)
  let references_by_number = (Hashtbl.create 256 : (int, FileSet.t) Hashtbl.t) in
  let get_num file_name =
    try Hashtbl.find inverse_references file_name with Not_found -> 0
  in
  let get_set num =
    try Hashtbl.find references_by_number num with Not_found -> FileSet.empty
  in
  let add_incoming_edge file_name =
    let old_num = get_num file_name in
    let new_num = old_num + 1 in
    let old_set_at_num = get_set old_num in
    let new_set_at_num = FileSet.remove file_name old_set_at_num in
    let old_set_at_new_num = get_set new_num in
    let new_set_at_new_num = FileSet.add file_name old_set_at_new_num in
    Hashtbl.replace inverse_references file_name new_num;
    Hashtbl.replace references_by_number old_num new_set_at_num;
    Hashtbl.replace references_by_number new_num new_set_at_new_num
  in
  let remove_incoming_edge file_name =
    let old_num = get_num file_name in
    let new_num = old_num - 1 in
    let old_set_at_num = get_set old_num in
    let new_set_at_num = FileSet.remove file_name old_set_at_num in
    let old_set_at_new_num = get_set new_num in
    let new_set_at_new_num = FileSet.add file_name old_set_at_new_num in
    Hashtbl.replace inverse_references file_name new_num;
    Hashtbl.replace references_by_number old_num new_set_at_num;
    Hashtbl.replace references_by_number new_num new_set_at_new_num
  in
  let add_edge from_file to_file =
    if file_exists t from_file then add_incoming_edge to_file
  in
  let remove_edge from_file to_file =
    if file_exists t from_file then remove_incoming_edge to_file
  in
  iter_deps t (fun from_file set ->
      if get_num from_file = 0 then
        Hashtbl.replace references_by_number 0 (FileSet.add from_file (get_set 0));
      set |> FileSet.iter (fun to_file -> add_edge from_file to_file));
  while get_set 0 <> FileSet.empty do
    let files_with_no_incoming_references = get_set 0 in
    Hashtbl.remove references_by_number 0;
    files_with_no_incoming_references
    |> FileSet.iter (fun file_name ->
           iter_fun file_name;
           let references = get_deps t file_name in
           references |> FileSet.iter (fun to_file -> remove_edge file_name to_file))
  done;
  (* Process any remaining items in case of circular references *)
  references_by_number
  |> Hashtbl.iter (fun _num set ->
         if FileSet.is_empty set then ()
         else set |> FileSet.iter (fun file_name -> iter_fun file_name))
