open Shared_types

(* TODO should I hang on to location? *)
let rec find_doc_attribute attributes =
  let open Parsetree in
  match attributes with
  | [] -> None
  | ( {Asttypes.txt = "ocaml.doc" | "ocaml.text" | "ns.doc" | "res.doc"},
      PStr
        [
          {
            pstr_desc =
              Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (doc, _))}, _);
          };
        ] )
    :: _ ->
    Some doc
  | _ :: rest -> find_doc_attribute rest

let rec find_deprecated_attribute attributes =
  let open Parsetree in
  match attributes with
  | [] -> None
  | ( {Asttypes.txt = "deprecated"},
      PStr [{pstr_desc = Pstr_eval ({pexp_desc = expr}, _)}] )
    :: _ -> (
    match expr with
    (* Simple deprecated attr @deprecated("message") *)
    | Pexp_constant (Pconst_string (_msg, _)) -> Some _msg
    (* deprecated attr with record *)
    | Pexp_record (fields, _) ->
      let reason = ref "" in

      fields
      |> List.iter (fun {lid = {txt}; x} ->
             match (txt, x) with
             | ( Lident "reason",
                 {pexp_desc = Pexp_constant (Pconst_string (msg, _))} ) ->
               reason := msg
             | _ -> ());

      Some !reason
    | _ -> None)
  | ({Asttypes.txt = "deprecated"}, _) :: _ -> Some ""
  | _ :: rest -> find_deprecated_attribute rest

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
      match payload_expr with
      | {pexp_desc = Pexp_array items} -> items
      | p -> [p]
    in
    let module_paths_from_array =
      items
      |> List.filter_map (fun item ->
             match item.Parsetree.pexp_desc with
             | Pexp_construct ({txt = path}, None) ->
               Some (Utils.flatten_long_ident path)
             | _ -> None)
    in
    find_editor_complete_from_attribute
      ~module_paths:(module_paths_from_array @ module_paths)
      rest
  | _ :: rest -> find_editor_complete_from_attribute ~module_paths rest
