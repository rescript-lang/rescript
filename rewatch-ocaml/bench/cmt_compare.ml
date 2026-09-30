let summary value =
  try
    let bytes = Marshal.to_string value [] in
    Printf.sprintf "%d %s" (String.length bytes)
      (Digest.to_hex (Digest.string bytes))
  with Invalid_argument reason -> "unmarshalable: " ^ reason

let compare_field label first second =
  let first = summary first in
  let second = summary second in
  if first <> second then Printf.printf "%s\t%s\t%s\n" label first second

let annotation_text = function
  | Cmt_format.Interface signature ->
    Format.asprintf "%a" Printtyped.interface signature
  | Cmt_format.Implementation structure ->
    Format.asprintf "%a" Printtyped.implementation structure
  | Cmt_format.Packed _ | Cmt_format.Partial_implementation _
  | Cmt_format.Partial_interface _ ->
    "unsupported annotation kind"

let type_text typ =
  Printtyp.reset_and_mark_loops typ;
  Format.asprintf "%a" Printtyp.type_sch typ

let value_description_equal first second =
  summary first.Types.val_kind = summary second.Types.val_kind
  && summary first.val_loc = summary second.val_loc
  && summary first.val_attributes = summary second.val_attributes
  && type_text first.val_type = type_text second.val_type

let value_dependencies_equal first second =
  List.length first = List.length second
  && List.for_all2
       (fun (first_from, first_to) (second_from, second_to) ->
         value_description_equal first_from second_from
         && value_description_equal first_to second_to)
       first second

let report_value_dependency_differences first second =
  Printf.printf "value dependencies semantically equal: %b\n"
    (value_dependencies_equal first second);
  List.iteri
    (fun index ((first_from, first_to), (second_from, second_to)) ->
      if
        not
          (value_description_equal first_from second_from
          && value_description_equal first_to second_to)
      then
        Printf.printf "value dependency %d: %s -> %s | %s -> %s\n" index
          (type_text first_from.val_type)
          (type_text first_to.val_type)
          (type_text second_from.val_type)
          (type_text second_to.val_type))
    (List.combine first second)

let report_text_difference first second =
  let rec first_difference index first second =
    match (first, second) with
    | left :: left_tail, right :: right_tail when left = right ->
      first_difference (index + 1) left_tail right_tail
    | left :: _, right :: _ ->
      Printf.printf
        "printed first difference at line %d\nclassic: %s\nfrozen: %s\n" index
        left right
    | [], [] -> ()
    | [], _ | _, [] ->
      Printf.printf "printed annotations have different lengths\n"
  in
  first_difference 1
    (String.split_on_char '\n' first)
    (String.split_on_char '\n' second)

let report_item_differences label first second summarize =
  let differences =
    List.combine first second
    |> List.mapi (fun index (left, right) ->
        if summary (summarize left) = summary (summarize right) then None
        else Some index)
    |> List.filter_map Fun.id
  in
  match differences with
  | [] -> ()
  | _ ->
    Printf.printf "%s: %d differing items; first: %s\n" label
      (List.length differences)
      (String.concat ", "
         (List.map string_of_int
            (List.filteri (fun index _ -> index < 5) differences)))

let compare_annotations first second =
  match (first, second) with
  | Cmt_format.Interface first, Cmt_format.Interface second ->
    compare_field "sig_type" first.sig_type second.sig_type;
    compare_field "sig_final_env" first.sig_final_env second.sig_final_env;
    report_item_differences "sig_desc" first.sig_items second.sig_items
      (fun item -> item.sig_desc);
    report_item_differences "sig_env" first.sig_items second.sig_items
      (fun item -> item.sig_env);
    let describe = function
      | Types.Sig_value (id, value) ->
        Printf.sprintf "value %s stamp=%d type_id=%d" id.Ident.name
          id.Ident.stamp value.val_type.id
      | Types.Sig_type (id, _, _) ->
        Printf.sprintf "type %s stamp=%d" id.Ident.name id.Ident.stamp
      | Types.Sig_module (id, _, _) ->
        Printf.sprintf "module %s stamp=%d" id.Ident.name id.Ident.stamp
      | Types.Sig_modtype (id, _) ->
        Printf.sprintf "modtype %s stamp=%d" id.Ident.name id.Ident.stamp
      | Types.Sig_typext (id, _, _) ->
        Printf.sprintf "extension %s stamp=%d" id.Ident.name id.Ident.stamp
    in
    List.iteri
      (fun index (left, right) ->
        if index < 3 then
          Printf.printf "sig_type[%d]\t%s\t%s\n" index (describe left)
            (describe right))
      (List.combine first.sig_type second.sig_type)
  | Cmt_format.Implementation first, Cmt_format.Implementation second -> (
    compare_field "str_type" first.str_type second.str_type;
    compare_field "str_final_env" first.str_final_env second.str_final_env;
    report_item_differences "str_desc" first.str_items second.str_items
      (fun item -> item.str_desc);
    report_item_differences "str_env" first.str_items second.str_items
      (fun item -> item.str_env);
    match (first.str_type, second.str_type) with
    | ( Types.Sig_value (left_id, left) :: _,
        Types.Sig_value (right_id, right) :: _ ) ->
      Printf.printf "first str_type value\t%s/%d type=%d\t%s/%d type=%d\n"
        left_id.Ident.name left_id.Ident.stamp left.val_type.id
        right_id.Ident.name right_id.Ident.stamp right.val_type.id
    | _ -> ())
  | _ -> ()

let semantic_equal first second =
  let same left right = summary left = summary right in
  let supported =
    match (first.Cmt_format.cmt_annots, second.Cmt_format.cmt_annots) with
    | Cmt_format.Interface _, Cmt_format.Interface _
    | Cmt_format.Implementation _, Cmt_format.Implementation _ ->
      true
    | _ -> false
  in
  supported
  && annotation_text first.cmt_annots = annotation_text second.cmt_annots
  && same first.cmt_modname second.cmt_modname
  && value_dependencies_equal first.cmt_value_dependencies
       second.cmt_value_dependencies
  && same first.cmt_comments second.cmt_comments
  && same first.cmt_args second.cmt_args
  && same first.cmt_sourcefile second.cmt_sourcefile
  && same first.cmt_builddir second.cmt_builddir
  && same first.cmt_loadpath second.cmt_loadpath
  && same first.cmt_source_digest second.cmt_source_digest
  && same first.cmt_initial_env second.cmt_initial_env
  && same first.cmt_imports second.cmt_imports
  && same first.cmt_interface_digest second.cmt_interface_digest
  && same first.cmt_use_summaries second.cmt_use_summaries
  && same first.cmt_extra_info second.cmt_extra_info

let () =
  if Array.length Sys.argv = 5 && Sys.argv.(1) = "--semantic-tree" then (
    let first_root = Sys.argv.(2) in
    let second_root = Sys.argv.(3) in
    let paths = open_in Sys.argv.(4) in
    Fun.protect
      (fun () ->
        try
          while true do
            let path = input_line paths in
            let first = Cmt_format.read_cmt (Filename.concat first_root path) in
            let second =
              Cmt_format.read_cmt (Filename.concat second_root path)
            in
            if not (semantic_equal first second) then (
              Printf.eprintf "annotation semantics differ: %s\n" path;
              exit 1)
          done
        with End_of_file -> ())
      ~finally:(fun () -> close_in paths);
    exit 0);
  let semantic, first_path, second_path =
    match Array.to_list Sys.argv with
    | [_; first; second] -> (false, first, second)
    | [_; "--semantic"; first; second] -> (true, first, second)
    | _ ->
      prerr_endline "Usage: cmt_compare [--semantic] FIRST.cmt SECOND.cmt";
      exit 2
  in
  let first = Cmt_format.read_cmt first_path in
  let second = Cmt_format.read_cmt second_path in
  if semantic then (
    if not (semantic_equal first second) then (
      Printf.eprintf "annotation semantics differ: %s %s\n" first_path
        second_path;
      exit 1);
    exit 0);
  compare_field "modname" first.cmt_modname second.cmt_modname;
  compare_field "annots" first.cmt_annots second.cmt_annots;
  compare_annotations first.cmt_annots second.cmt_annots;
  report_text_difference
    (annotation_text first.cmt_annots)
    (annotation_text second.cmt_annots);
  compare_field "value_dependencies" first.cmt_value_dependencies
    second.cmt_value_dependencies;
  report_value_dependency_differences first.cmt_value_dependencies
    second.cmt_value_dependencies;
  compare_field "comments" first.cmt_comments second.cmt_comments;
  compare_field "args" first.cmt_args second.cmt_args;
  compare_field "sourcefile" first.cmt_sourcefile second.cmt_sourcefile;
  compare_field "builddir" first.cmt_builddir second.cmt_builddir;
  compare_field "loadpath" first.cmt_loadpath second.cmt_loadpath;
  compare_field "source_digest" first.cmt_source_digest second.cmt_source_digest;
  compare_field "initial_env" first.cmt_initial_env second.cmt_initial_env;
  compare_field "imports" first.cmt_imports second.cmt_imports;
  compare_field "interface_digest" first.cmt_interface_digest
    second.cmt_interface_digest;
  compare_field "use_summaries" first.cmt_use_summaries second.cmt_use_summaries;
  compare_field "extra_info" first.cmt_extra_info second.cmt_extra_info
