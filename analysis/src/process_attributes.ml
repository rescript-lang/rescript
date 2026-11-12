open Shared_types

(* TODO should I hang on to location? *)
let rec find_doc_attribute attributes =
  match attributes with
  | [] -> None
  | ({Asttypes.txt = "res.doc"}, payload) :: rest -> (
    match Ast_payload.semantic_string_of_payload payload with
    | Some doc -> Some doc
    | None -> find_doc_attribute rest)
  | _ :: rest -> find_doc_attribute rest

(* The message of a [@deprecated] attribute, read as the compiler reads it
   for its warning *)
let find_deprecated_attribute attributes =
  Builtin_attributes.deprecated_of_attrs_with_migrate attributes
  |> Option.map (fun (reason, _, _) -> reason)

let new_declared ~item ~extent ~name ~stamp ~module_path is_exported attributes
    =
  {
    Declared.name;
    stamp;
    extent_loc = extent;
    is_exported;
    module_path;
    deprecated = find_deprecated_attribute (List.rev attributes);
    docstring =
      (match find_doc_attribute attributes with
      | None -> []
      | Some d -> [d]);
    item;
  }

let rec find_editor_complete_from_attribute ?(module_paths = []) attributes =
  let open Parsetree in
  match attributes with
  | [] -> module_paths
  | ( {Asttypes.txt = "editor.completeFrom"},
      PStr [{pstr_desc = Pstr_eval (payload_expr, _)}] )
    :: rest ->
    let items =
      match Ast_payload.unwrap_braces payload_expr with
      | {pexp_desc = Pexp_array items} -> items
      | p -> [p]
    in
    let module_paths_from_array =
      items
      |> List.filter_map (fun item ->
          match (Ast_payload.unwrap_braces item).Parsetree.pexp_desc with
          | Pexp_construct ({txt = path}, {txt = []}) ->
            Some (Utils.flatten_long_ident path)
          | _ -> None)
    in
    find_editor_complete_from_attribute
      ~module_paths:(module_paths_from_array @ module_paths)
      rest
  | _ :: rest -> find_editor_complete_from_attribute ~module_paths rest
