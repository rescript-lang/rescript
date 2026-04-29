open GenTypeCommon

type export_module_item = (string, export_module_value) Hashtbl.t

and export_module_value =
  | S of {
      name: string;
      path: string list;
      type_: type_;
      doc_string: DocString.t;
    }
  | M of {export_module_item: export_module_item}

type export_module_items = (string, export_module_item) Hashtbl.t

type types = {
  type_for_value: type_;
  type_for_type: type_;
  doc_string: DocString.t;
}

type field_info = {field_for_value: field; field_for_type: field}

let rec export_module_value_to_type ~config export_module_value =
  match export_module_value with
  | S {name; type_; doc_string; _} ->
    {type_for_value = ident name; type_for_type = type_; doc_string}
  | M {export_module_item} ->
    let fields_info =
      export_module_item |> export_module_item_to_fields ~config
    in
    let fields_for_value =
      fields_info |> List.map (fun {field_for_value} -> field_for_value)
    in
    let fields_for_type =
      fields_info |> List.map (fun {field_for_type} -> field_for_type)
    in
    {
      type_for_value = Object (Open, fields_for_value);
      type_for_type = Object (Open, fields_for_type);
      doc_string = DocString.empty;
    }

and export_module_item_to_fields =
  (fun ~config export_module_item ->
     Hashtbl.fold
       (fun field_name export_module_value fields ->
         let {type_for_value; type_for_type; doc_string} =
           export_module_value |> export_module_value_to_type ~config
         in
         let field_for_type =
           {
             mutable_ = Mutable;
             name_js = field_name;
             optional = Mandatory;
             type_ = type_for_type;
             doc_string;
           }
         in
         let field_for_value = {field_for_type with type_ = type_for_value} in
         {field_for_value; field_for_type} :: fields)
       export_module_item []
    : config:Config.t -> export_module_item -> field_info list)

let rec extend_export_module_item ~doc_string ~resolved_name x
    ~(export_module_item : export_module_item) ~type_ ~value_name =
  match x with
  | [] -> ()
  | [field_name] ->
    Hashtbl.replace export_module_item field_name
      (S {name = value_name; path = resolved_name; type_; doc_string})
  | field_name :: rest ->
    let inner_export_module_item =
      match Hashtbl.find export_module_item field_name with
      | M {export_module_item = inner_export_module_item} ->
        inner_export_module_item
      | S _ -> assert false
      | exception Not_found ->
        let inner_export_module_item = Hashtbl.create 1 in
        Hashtbl.replace export_module_item field_name
          (M {export_module_item = inner_export_module_item});
        inner_export_module_item
    in
    rest
    |> extend_export_module_item ~doc_string ~resolved_name
         ~export_module_item:inner_export_module_item ~value_name ~type_

let extend_export_module_items x ~doc_string
    ~(export_module_items : export_module_items) ~type_ ~value_name =
  match x with
  | [] -> assert false
  | [_valueName] -> ()
  | module_name :: rest ->
    let export_module_item =
      match Hashtbl.find export_module_items module_name with
      | export_module_item -> export_module_item
      | exception Not_found ->
        let export_module_item = Hashtbl.create 1 in
        Hashtbl.replace export_module_items module_name export_module_item;
        export_module_item
    in
    rest
    |> extend_export_module_item ~doc_string ~resolved_name:x
         ~export_module_item ~type_ ~value_name

let create_module_items_emitter =
  (fun () -> Hashtbl.create 1 : unit -> export_module_items)

let rev_fold f tbl base =
  let list = Hashtbl.fold (fun k v l -> (k, v) :: l) tbl [] in
  List.fold_left (fun x (k, v) -> f k v x) base list

let emit_all_module_items ~config ~emitters ~file_name
    (export_module_items : export_module_items) =
  let is_react_component_type type_ =
    match type_ with
    | Function {arg_types = [{a_type = Object (_, fields)}]; ret_type; _} ->
      ret_type |> EmitType.is_type_function_component ~fields
    | _ -> false
  in
  let hidden_export_access resolved_name =
    ModuleName.nested_make_hidden_export_access ~file_name resolved_name
  in
  let single_make_component_export export_module_item =
    match Hashtbl.length export_module_item with
    | 1 -> (
      match Hashtbl.find_opt export_module_item "make" with
      | Some (S {path; type_; doc_string; _}) when is_react_component_type type_
        -> (
        match hidden_export_access path with
        | Some access -> Some (access, type_, doc_string)
        | None -> None)
      | Some (S _) | Some (M _) | None -> None)
    | _ -> None
  in
  emitters
  |> rev_fold
       (fun module_name export_module_item emitters ->
         if !Debug.code_items then Log_.item "EmitModule %s @." module_name;
         let emitted_module_item, type_for_type, doc_string =
           match single_make_component_export export_module_item with
           | Some (component_access, type_, doc_string) ->
             (component_access, type_, doc_string)
           | None ->
             let {type_for_type; doc_string} =
               M {export_module_item} |> export_module_value_to_type ~config
             in
             ( ModuleName.for_inner_module ~file_name
                 ~inner_module_name:module_name
               |> ModuleName.to_string,
               type_for_type,
               doc_string )
         in
         emitted_module_item
         |> EmitType.emit_export_const ~doc_string ~early:false ~config
              ~emitters ~name:module_name ~type_:type_for_type
              ~type_name_is_interface:(fun _ -> false))
       export_module_items

let extend_export_modules ~(module_items_emitter : export_module_items)
    ~doc_string ~type_ resolved_name =
  resolved_name |> ResolvedName.to_list
  |> extend_export_module_items ~export_module_items:module_items_emitter ~type_
       ~doc_string
       ~value_name:(resolved_name |> ResolvedName.to_string)
