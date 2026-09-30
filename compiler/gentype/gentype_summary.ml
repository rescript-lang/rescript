(* Declaration-only inputs for imported GenType modules, with an optional full
   interface input when editor annotations are disabled. The version and
   provenance fields keep stale summaries from replacing their CMT fallback. *)

let magic = "ReScript genType summary 3\n"
let version = 3

type dependency = {path: string; digest: Digest.t}

type t = {
  version: int;
  compiler_identity: string;
  project_root: string;
  config_digest: Digest.t;
  config_source: dependency option;
  source_path: string;
  source_digest: Digest.t;
  dependencies: dependency list;
  annots: Cmt_format.binary_annots;
  interface_annots: Cmt_format.binary_annots option;
}

let summary_file cmt_file = cmt_file ^ ".gts"

let remove ~cmt_file =
  try Sys.remove (summary_file cmt_file)
  with Sys_error _ | Unix.Unix_error _ -> ()

let compiler_identity_key = Domain.DLS.new_key (fun () -> None)

let with_compiler_identity identity action =
  let previous = Domain.DLS.get compiler_identity_key in
  Domain.DLS.set compiler_identity_key (Some identity);
  Fun.protect action ~finally:(fun () ->
      Domain.DLS.set compiler_identity_key previous)

let absolute ~cwd path =
  if Filename.is_relative path then Filename.concat cwd path else path

let config_digest ~namespace =
  Digest.string
    (Marshal.to_string (Gentype_config.snapshot_flags (), namespace) [])

let config_source ~project_root =
  ["rescript.json"; "bsconfig.json"]
  |> List.find_map (fun name ->
      let path = Filename.concat project_root name in
      if Sys.file_exists path then Some {path; digest = Digest.file path}
      else None)

let declaration_annots = function
  | Cmt_format.Implementation structure ->
    Cmt_format.Implementation
      {
        structure with
        str_items =
          List.filter
            (fun (item : Typedtree.structure_item) ->
              match item.str_desc with
              | Tstr_type _ | Tstr_modtype _ | Tstr_module _ -> true
              | _ -> false)
            structure.str_items;
      }
  | Interface signature ->
    Interface
      {
        signature with
        sig_items =
          List.filter
            (fun (item : Typedtree.signature_item) ->
              match item.sig_desc with
              | Tsig_type _ | Tsig_modtype _ -> true
              | _ -> false)
            signature.sig_items;
      }
  | (Packed _ | Partial_implementation _ | Partial_interface _) as annots ->
    annots

let dependency_paths ~cmt_file (cmt : Cmt_format.cmt_infos) =
  let directories =
    Filename.dirname (absolute ~cwd:cmt.cmt_builddir cmt_file)
    :: List.map (absolute ~cwd:cmt.cmt_builddir) cmt.cmt_loadpath
  in
  cmt.cmt_imports
  |> List.filter_map (function
    | _, None -> Some None
    | name, Some _ ->
      let filename = name ^ ".cmi" in
      directories
      |> List.find_map (fun directory ->
          let path = Filename.concat directory filename in
          if Sys.file_exists path then Some path else None)
      |> Option.map (fun path -> Some path))
  |> fun paths ->
  if List.length paths <> List.length cmt.cmt_imports then None
  else Some (paths |> List.filter_map Fun.id |> List.sort_uniq String.compare)

let save ~cmt_file (cmt : Cmt_format.cmt_infos) =
  let written =
    try
      match (cmt.cmt_sourcefile, Domain.DLS.get compiler_identity_key) with
      | Some source_file, Some compiler_identity -> (
        let source_path = absolute ~cwd:cmt.cmt_builddir source_file in
        let source_digest = Digest.file source_path in
        if cmt.cmt_source_digest <> Some source_digest then false
        else
          match dependency_paths ~cmt_file cmt with
          | None -> false
          | Some paths ->
            let dependencies =
              List.map (fun path -> {path; digest = Digest.file path}) paths
            in
            let annots = declaration_annots cmt.cmt_annots in
            let interface_annots =
              match cmt.cmt_annots with
              | Interface _ when not !((Clflags.current ()).binary_annotations)
                ->
                Some cmt.cmt_annots
              | Interface _ | Implementation _ | Packed _
              | Partial_implementation _ | Partial_interface _ ->
                None
            in
            let summary =
              let project_root =
                absolute
                  ~cwd:(Compiler_request_state.cwd ())
                  !(Gentype_config.project_root ())
              in
              {
                version;
                compiler_identity;
                project_root;
                config_digest =
                  config_digest ~namespace:(Paths.find_name_space cmt_file);
                config_source = config_source ~project_root;
                source_path;
                source_digest;
                dependencies;
                annots;
                interface_annots;
              }
            in
            Misc.output_to_bin_file_directly (summary_file cmt_file)
              (fun _ channel ->
                output_string channel magic;
                output_value channel summary);
            true)
      | None, _ | _, None -> false
    with Sys_error _ | Unix.Unix_error _ | Invalid_argument _ -> false
  in
  if not written then remove ~cmt_file

let load_validated ~cmt_file =
  try
    let channel = open_in_bin (summary_file cmt_file) in
    let summary =
      Fun.protect
        (fun () ->
          if really_input_string channel (String.length magic) <> magic then
            None
          else Some (input_value channel : t))
        ~finally:(fun () -> close_in_noerr channel)
    in
    Option.bind summary (fun summary ->
        if
          summary.version = version
          && Some summary.compiler_identity
             = Domain.DLS.get compiler_identity_key
          && (match summary.config_source with
            | Some {path; digest} -> Digest.file path = digest
            | None -> true)
          && (if
                absolute
                  ~cwd:(Compiler_request_state.cwd ())
                  !(Gentype_config.project_root ())
                = summary.project_root
              then
                summary.config_digest
                = config_digest ~namespace:(Paths.find_name_space cmt_file)
              else Option.is_some summary.config_source)
          && Digest.file summary.source_path = summary.source_digest
          && List.for_all
               (fun {path; digest} -> Digest.file path = digest)
               summary.dependencies
        then Some summary
        else None)
  with
  | Sys_error _ | Unix.Unix_error _ | End_of_file | Failure _
  | Invalid_argument _
  ->
    None

let read ~cmt_file =
  Option.map (fun summary -> summary.annots) (load_validated ~cmt_file)

let read_interface ~cmt_file =
  Option.bind (load_validated ~cmt_file) (fun summary ->
      summary.interface_annots)
