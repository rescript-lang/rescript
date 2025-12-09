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

(** {2 Read-only API} *)

let get_files (t : t) = t.files

let get_deps (t : t) file =
  match FileHash.find_opt t.deps file with
  | Some s -> s
  | None -> FileSet.empty

let iter_deps (t : t) f = FileHash.iter f t.deps

let file_exists (t : t) file = FileHash.mem t.deps file

(** {2 Topological ordering} *)

let iter_files_from_roots_to_leaves (t : t) iterFun =
  (* For each file, the number of incoming references *)
  let inverseReferences = (Hashtbl.create 256 : (string, int) Hashtbl.t) in
  (* For each number of incoming references, the files *)
  let referencesByNumber = (Hashtbl.create 256 : (int, FileSet.t) Hashtbl.t) in
  let getNum fileName =
    try Hashtbl.find inverseReferences fileName with Not_found -> 0
  in
  let getSet num =
    try Hashtbl.find referencesByNumber num with Not_found -> FileSet.empty
  in
  let addIncomingEdge fileName =
    let oldNum = getNum fileName in
    let newNum = oldNum + 1 in
    let oldSetAtNum = getSet oldNum in
    let newSetAtNum = FileSet.remove fileName oldSetAtNum in
    let oldSetAtNewNum = getSet newNum in
    let newSetAtNewNum = FileSet.add fileName oldSetAtNewNum in
    Hashtbl.replace inverseReferences fileName newNum;
    Hashtbl.replace referencesByNumber oldNum newSetAtNum;
    Hashtbl.replace referencesByNumber newNum newSetAtNewNum
  in
  let removeIncomingEdge fileName =
    let oldNum = getNum fileName in
    let newNum = oldNum - 1 in
    let oldSetAtNum = getSet oldNum in
    let newSetAtNum = FileSet.remove fileName oldSetAtNum in
    let oldSetAtNewNum = getSet newNum in
    let newSetAtNewNum = FileSet.add fileName oldSetAtNewNum in
    Hashtbl.replace inverseReferences fileName newNum;
    Hashtbl.replace referencesByNumber oldNum newSetAtNum;
    Hashtbl.replace referencesByNumber newNum newSetAtNewNum
  in
  let addEdge fromFile toFile =
    if file_exists t fromFile then addIncomingEdge toFile
  in
  let removeEdge fromFile toFile =
    if file_exists t fromFile then removeIncomingEdge toFile
  in
  iter_deps t (fun fromFile set ->
      if getNum fromFile = 0 then
        Hashtbl.replace referencesByNumber 0 (FileSet.add fromFile (getSet 0));
      set |> FileSet.iter (fun toFile -> addEdge fromFile toFile));
  while getSet 0 <> FileSet.empty do
    let filesWithNoIncomingReferences = getSet 0 in
    Hashtbl.remove referencesByNumber 0;
    filesWithNoIncomingReferences
    |> FileSet.iter (fun fileName ->
           iterFun fileName;
           let references = get_deps t fileName in
           references |> FileSet.iter (fun toFile -> removeEdge fileName toFile))
  done;
  (* Process any remaining items in case of circular references *)
  referencesByNumber
  |> Hashtbl.iter (fun _num set ->
         if FileSet.is_empty set then ()
         else set |> FileSet.iter (fun fileName -> iterFun fileName))
