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
  files: String_set.t;
  deps: String_set.t File_hash.t; (* from_file -> set of to_files *)
}

type builder = {mutable files: String_set.t; deps: String_set.t File_hash.t}

(** {2 Builder API} *)

let create_builder () : builder =
  {files = String_set.empty; deps = File_hash.create 256}

let add_file (b : builder) file =
  b.files <- String_set.add file b.files;
  (* Ensure file has an entry even if no deps *)
  if not (File_hash.mem b.deps file) then
    File_hash.replace b.deps file String_set.empty

let add_dep (b : builder) ~from_file ~to_file =
  let set =
    match File_hash.find_opt b.deps from_file with
    | Some s -> s
    | None -> String_set.empty
  in
  File_hash.replace b.deps from_file (String_set.add to_file set)

(** {2 Builder extraction for reactive merge} *)

let builder_files (builder : builder) : String_set.t = builder.files

let builder_deps_to_list (builder : builder) : (string * String_set.t) list =
  File_hash.fold
    (fun from_file to_files acc -> (from_file, to_files) :: acc)
    builder.deps []
