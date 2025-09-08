module StringSet = Set.Make (String)

let cmt_check_annotations ~check_annotation input_cmt =
  match input_cmt.Cmt_format.cmt_annots with
  | Implementation structure ->
    structure |> Annotation.structure_check_annotation ~check_annotation
  | Interface signature ->
    signature |> Annotation.signature_check_annotation ~check_annotation
  | _ -> false

let cmt_has_type_errors input_cmt =
  match input_cmt.Cmt_format.cmt_annots with
  | Partial_implementation _ | Partial_interface _ -> true
  | _ -> false

let structure_item_is_declaration struct_item =
  match struct_item.Typedtree.str_desc with
  | Typedtree.Tstr_type _ | Tstr_modtype _ | Tstr_module _ -> true
  | _ -> false

let signature_item_is_declaration signature_item =
  match signature_item.Typedtree.sig_desc with
  | Typedtree.Tsig_type _ | Tsig_modtype _ -> true
  | _ -> false

let input_cmt_translate_type_declarations ~config ~output_file_relative
    ~resolver input_cmt : CodeItem.translation =
  let {Cmt_format.cmt_annots} = input_cmt in
  let type_env = TypeEnv.root () in
  let translations =
    match cmt_annots with
    | Implementation structure ->
      {
        structure with
        str_items =
          structure.str_items |> List.filter structure_item_is_declaration;
      }
      |> TranslateStructure.translate_structure ~config ~output_file_relative
           ~resolver ~type_env
    | Interface signature ->
      {
        signature with
        sig_items =
          signature.sig_items |> List.filter signature_item_is_declaration;
      }
      |> TranslateSignature.translate_signature ~config ~output_file_relative
           ~resolver ~type_env
    | Packed _ | Partial_implementation _ | Partial_interface _ -> []
  in
  translations |> Translation.combine
  |> Translation.add_type_declarations_from_module_equations ~type_env

let translate_c_m_t ~config ~output_file_relative ~resolver input_cmt :
    Translation.t =
  let {Cmt_format.cmt_annots} = input_cmt in
  let type_env = TypeEnv.root () in
  let translations =
    match cmt_annots with
    | Implementation structure ->
      structure
      |> TranslateStructure.translate_structure ~config ~output_file_relative
           ~resolver ~type_env
    | Interface signature ->
      signature
      |> TranslateSignature.translate_signature ~config ~output_file_relative
           ~resolver ~type_env
    | _ -> []
  in
  translations |> Translation.combine
  |> Translation.add_type_declarations_from_module_equations ~type_env

let emit_translation ~config ~file_name ~output_file ~output_file_relative
    ~resolver ~source_file translation =
  let code_text =
    translation
    |> EmitJs.emit_translation_as_string ~config ~file_name
         ~output_file_relative ~resolver ~input_cmt_translate_type_declarations
  in
  let file_contents =
    EmitType.file_header ~source_file:(Filename.basename source_file)
    ^ "\n" ^ code_text ^ "\n"
  in
  GeneratedFiles.write_file_if_required ~output_file ~file_contents

let read_cmt cmt_file =
  try Cmt_format.read_cmt cmt_file
  with Cmi_format.Error _ ->
    Log_.item "Error loading %s\n\n" cmt_file;
    Log_.item "It looks like you might have stale compilation artifacts.\n";
    Log_.item "Try to clean and rebuild.\n\n";
    assert false

let read_input_cmt is_interface cmt_file =
  let input_cmt = read_cmt cmt_file in
  let ignore_interface = ref false in
  let check_annotation ~loc:_ attributes =
    if
      attributes
      |> Annotation.get_attribute_payload
           Annotation.tag_is_gentype_ignore_interface
      <> None
    then ignore_interface := true;
    attributes
    |> Annotation.get_attribute_payload
         Annotation.tag_is_one_of_the_gentype_annotations
    <> None
  in
  let has_gentype_annotations =
    input_cmt |> cmt_check_annotations ~check_annotation
  in
  if is_interface then
    let cmt_file_impl =
      (cmt_file |> (Filename.chop_extension [@doesNotRaise])) ^ ".cmt"
    in
    let input_cmt_impl = read_cmt cmt_file_impl in
    let has_gentype_annotations_impl =
      input_cmt_impl
      |> cmt_check_annotations ~check_annotation:(fun ~loc attributes ->
             if attributes |> check_annotation ~loc then (
               if not !ignore_interface then (
                 Log_.Color.setup ();
                 Log_.info ~loc ~name:"Warning genType" (fun ppf () ->
                     Format.fprintf ppf
                       "Annotation is ignored as there's a .resi file"));
               true)
             else false)
    in
    ( (match !ignore_interface with
      | true -> input_cmt_impl
      | false -> input_cmt),
      match !ignore_interface with
      | true -> has_gentype_annotations_impl
      | false -> has_gentype_annotations )
  else (input_cmt, has_gentype_annotations)

let process_cmt_file cmt =
  let config = Paths.read_config ~namespace:(cmt |> Paths.find_name_space) in
  if !Debug.basic then Log_.item "Cmt %s\n" cmt;
  let cmt_file = cmt |> Paths.get_cmt_file in
  if cmt_file <> "" then
    let file_name = cmt |> Paths.get_module_name in
    let is_interface = Filename.check_suffix cmt_file ".cmti" in
    let input_cmt, has_gentype_annotations =
      read_input_cmt is_interface cmt_file
    in
    let source_file =
      match input_cmt.cmt_annots |> FindSourceFile.cmt with
      | Some source_file -> source_file
      | None -> (
        (file_name |> ModuleName.to_string)
        ^
        match is_interface with
        | true -> ".resi"
        | false -> ".res")
    in
    let output_file = source_file |> Paths.get_output_file ~config in
    let output_file_relative =
      source_file |> Paths.get_output_file_relative ~config
    in
    let resolver =
      ModuleResolver.create_lazy_resolver ~config
        ~extensions:[".res"; ".shim.ts"] ~exclude_file:(fun fname ->
          fname = "React.res" || fname = "ReasonReact.res")
    in
    if has_gentype_annotations then
      input_cmt
      |> translate_c_m_t ~config ~output_file_relative ~resolver
      |> emit_translation ~config ~file_name ~output_file ~output_file_relative
           ~resolver ~source_file
    else if input_cmt |> cmt_has_type_errors then
      output_file |> GeneratedFiles.log_file_action TypeError
    else (
      output_file |> GeneratedFiles.log_file_action NoMatch;
      if Sys.file_exists output_file then Sys.remove output_file)
[@@live]
