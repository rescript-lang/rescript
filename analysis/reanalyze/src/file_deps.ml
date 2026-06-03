(** File dependencies collected during AST processing.
    
    Tracks which files reference which other files. *)

(* File-keyed hashtable *)
module File_hash = Hashtbl.Make (struct
  type t = string

  let hash (x : t) = Hashtbl.hash x
  let equal (x : t) y = x = y
end)

(** {2 Types} *)

type t = {
  files: File_set.t;
  deps: File_set.t File_hash.t; (* from_file -> set of to_files *)
}

type builder = {mutable files: File_set.t; deps: File_set.t File_hash.t}

(** {2 Builder API} *)

let create_builder () : builder =
  {files = File_set.empty; deps = File_hash.create 256}

let add_file (b : builder) file =
  b.files <- File_set.add file b.files;
  (* Ensure file has an entry even if no deps *)
  if not (File_hash.mem b.deps file) then
    File_hash.replace b.deps file File_set.empty

let add_dep (b : builder) ~from_file ~to_file =
  let set =
    match File_hash.find_opt b.deps from_file with
    | Some s -> s
    | None -> File_set.empty
  in
  File_hash.replace b.deps from_file (File_set.add to_file set)

(** {2 Merge API} *)

let merge_into_builder ~(from : builder) ~(into : builder) =
  into.files <- File_set.union into.files from.files;
  File_hash.iter
    (fun from_file to_files ->
      let existing =
        match File_hash.find_opt into.deps from_file with
        | Some s -> s
        | None -> File_set.empty
      in
      File_hash.replace into.deps from_file (File_set.union existing to_files))
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

let builder_files (builder : builder) : File_set.t = builder.files

let builder_deps_to_list (builder : builder) : (string * File_set.t) list =
  File_hash.fold
    (fun from_file to_files acc -> (from_file, to_files) :: acc)
    builder.deps []

let create ~files ~deps : t = {files; deps}

(** {2 Read-only API} *)

let get_files (t : t) = t.files

let get_deps (t : t) file =
  match File_hash.find_opt t.deps file with
  | Some s -> s
  | None -> File_set.empty

let iter_deps (t : t) f = File_hash.iter f t.deps

let file_exists (t : t) file = File_hash.mem t.deps file

let files_count (t : t) = File_set.cardinal t.files

let deps_count (t : t) = File_hash.length t.deps

(** {2 Topological ordering} *)

let iter_files_from_roots_to_leaves (t : t) iter_fun =
  (* For each file, the number of incoming references *)
  let inverse_references = (Hashtbl.create 256 : (string, int) Hashtbl.t) in
  (* For each number of incoming references, the files *)
  let references_by_number =
    (Hashtbl.create 256 : (int, File_set.t) Hashtbl.t)
  in
  let get_num file_name =
    try Hashtbl.find inverse_references file_name with Not_found -> 0
  in
  let get_set num =
    try Hashtbl.find references_by_number num with Not_found -> File_set.empty
  in
  let add_incoming_edge file_name =
    let old_num = get_num file_name in
    let new_num = old_num + 1 in
    let old_set_at_num = get_set old_num in
    let new_set_at_num = File_set.remove file_name old_set_at_num in
    let old_set_at_new_num = get_set new_num in
    let new_set_at_new_num = File_set.add file_name old_set_at_new_num in
    Hashtbl.replace inverse_references file_name new_num;
    Hashtbl.replace references_by_number old_num new_set_at_num;
    Hashtbl.replace references_by_number new_num new_set_at_new_num
  in
  let remove_incoming_edge file_name =
    let old_num = get_num file_name in
    let new_num = old_num - 1 in
    let old_set_at_num = get_set old_num in
    let new_set_at_num = File_set.remove file_name old_set_at_num in
    let old_set_at_new_num = get_set new_num in
    let new_set_at_new_num = File_set.add file_name old_set_at_new_num in
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
        Hashtbl.replace references_by_number 0
          (File_set.add from_file (get_set 0));
      set |> File_set.iter (fun to_file -> add_edge from_file to_file));
  while get_set 0 <> File_set.empty do
    let files_with_no_incoming_references = get_set 0 in
    Hashtbl.remove references_by_number 0;
    files_with_no_incoming_references
    |> File_set.iter (fun file_name ->
           iter_fun file_name;
           let references = get_deps t file_name in
           references
           |> File_set.iter (fun to_file -> remove_edge file_name to_file))
  done;
  (* Process any remaining items in case of circular references *)
  references_by_number
  |> Hashtbl.iter (fun _num set ->
         if File_set.is_empty set then ()
         else set |> File_set.iter (fun file_name -> iter_fun file_name))
