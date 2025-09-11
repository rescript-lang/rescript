open GenTypeCommon

(* Utilities to build TS import path expressions from a list of segments. *)
let ts_import_path (segments : string list) : string =
  match segments with
  | [] -> "import(\"\")." (* should not happen, but keep a valid token *)
  | pkg :: rest -> (
    match rest with
    | [] -> "import(\"" ^ pkg ^ "\")"
    | _ -> "import(\"" ^ pkg ^ "\")." ^ String.concat "." rest)

type type_assertion = {
  name: string;
  type_vars: string list;
  type_: type_;
  import_segments: string list;
}

type value_assertion = {
  name: string; (* original name of the value *)
  type_: type_;
  import_segments: string list;
}

type assertions = {
  type_assertions: type_assertion list;
  value_assertions: value_assertion list;
  gentype_type_defs: (string * string list * type_) list;
  gentype_value_defs: (string * type_) list;
}

let empty_assertions =
  {
    type_assertions = [];
    value_assertions = [];
    gentype_type_defs = [];
    gentype_value_defs = [];
  }

let collect_from_structure ~(config : GenTypeConfig.t) ~output_file_relative
    ~resolver ~type_env (structure : Typedtree.structure) : assertions =
  let rec process_items (items : Typedtree.structure_item list)
      (acc : assertions) : assertions =
    match items with
    | [] -> acc
    | item :: rest ->
      let acc1 =
        match item.str_desc with
        | Tstr_type (rec_flag, type_declarations) ->
          (* Filter those with @gentype.satisfies and translate the whole group. *)
          let wanted =
            type_declarations
            |> List.filter (fun (td : Typedtree.type_declaration) ->
                   Annotation.get_satisfies_path td.typ_attributes <> None)
          in
          let has_regular_gentype attrs =
            Annotation.has_attribute Annotation.tag_is_gentype attrs
            || Annotation.has_attribute Annotation.tag_is_gentype_as attrs
            || Annotation.has_attribute Annotation.tag_is_gentype_opaque attrs
          in
          let wanted_gentype =
            type_declarations
            |> List.filter (fun (td : Typedtree.type_declaration) ->
                   has_regular_gentype td.typ_attributes)
          in
          if wanted = [] && wanted_gentype = [] then acc
          else
            let trans =
              TranslateTypeDeclarations.translate_type_declarations ~config
                ~output_file_relative ~recursive:(rec_flag = Recursive)
                ~resolver ~type_env type_declarations
            in
            (* Build a map from base type name -> (type_, type_vars) *)
            let by_name =
              trans
              |> List.fold_left
                   (fun m (td : CodeItem.type_declaration) ->
                     let et = td.export_from_type_declaration.export_type in
                     let base_name =
                       match ResolvedName.to_list et.resolved_type_name with
                       | [] -> ""
                       | xs -> List.hd (List.rev xs)
                     in
                     StringMap.add base_name (et.type_, et.type_vars) m)
                   StringMap.empty
            in
            let add_one_satisfies acc (td : Typedtree.type_declaration) =
              let name = Ident.name td.typ_id in
              match Annotation.get_satisfies_path td.typ_attributes with
              | None -> acc
              | Some import_segments -> (
                match StringMap.find name by_name with
                | exception Not_found -> acc
                | type_, type_vars ->
                  {
                    acc with
                    type_assertions =
                      {name; type_vars; type_; import_segments}
                      :: acc.type_assertions;
                  })
            in
            let acc = wanted |> List.fold_left add_one_satisfies acc in
            (* Also include regular @gentype type declarations (non-exported). *)
            let add_one_gentype acc (td : Typedtree.type_declaration) =
              let name = Ident.name td.typ_id in
              if Annotation.get_satisfies_path td.typ_attributes <> None then
                acc
              else
                match StringMap.find name by_name with
                | exception Not_found -> acc
                | type_, type_vars ->
                  {
                    acc with
                    gentype_type_defs =
                      (name, type_vars, type_) :: acc.gentype_type_defs;
                  }
            in
            wanted_gentype |> List.fold_left add_one_gentype acc
        | Tstr_value (_loc, value_bindings) ->
          let add_binding acc (vb : Typedtree.value_binding) =
            match Annotation.get_satisfies_path vb.vb_attributes with
            | None -> acc
            | Some import_segments -> (
              match vb.vb_pat.pat_desc with
              | Tpat_var (id, _) | Tpat_alias ({pat_desc = Tpat_any}, id, _) ->
                let name = Ident.name id in
                let tt =
                  vb.vb_pat.pat_type
                  |> TranslateTypeExprFromTypes.translate_type_expr_from_types
                       ~config ~type_env
                in
                let type_vars = tt.type_ |> TypeVars.free in
                let type_ =
                  Translation.abstract_the_type_parameters ~type_vars tt.type_
                in
                {
                  acc with
                  value_assertions =
                    {name; type_; import_segments} :: acc.value_assertions;
                }
              | _ -> acc)
          in
          let acc = value_bindings |> List.fold_left add_binding acc in
          (* Also include regular @gentype values (non-exported). *)
          let add_binding_gentype acc (vb : Typedtree.value_binding) =
            let is_gentype =
              Annotation.has_attribute Annotation.tag_is_gentype
                vb.vb_attributes
              || Annotation.has_attribute Annotation.tag_is_gentype_as
                   vb.vb_attributes
              || Annotation.has_attribute Annotation.tag_is_gentype_opaque
                   vb.vb_attributes
            in
            if not is_gentype then acc
            else if Annotation.get_satisfies_path vb.vb_attributes <> None then
              acc
            else
              match vb.vb_pat.pat_desc with
              | Tpat_var (id, _) | Tpat_alias ({pat_desc = Tpat_any}, id, _) ->
                let name = Ident.name id in
                let tt =
                  vb.vb_pat.pat_type
                  |> TranslateTypeExprFromTypes.translate_type_expr_from_types
                       ~config ~type_env
                in
                let type_vars = tt.type_ |> TypeVars.free in
                let type_ =
                  Translation.abstract_the_type_parameters ~type_vars tt.type_
                in
                {
                  acc with
                  gentype_value_defs = (name, type_) :: acc.gentype_value_defs;
                }
              | _ -> acc
          in
          value_bindings |> List.fold_left add_binding_gentype acc
        | Tstr_primitive vd -> (
          let
          (* external *)
          open
            Typedtree in
          match Annotation.get_satisfies_path vd.val_attributes with
          | None ->
            if
              (* Also include regular @gentype externals (non-exported). *)
              Annotation.has_attribute Annotation.tag_is_gentype
                vd.val_attributes
              || Annotation.has_attribute Annotation.tag_is_gentype_as
                   vd.val_attributes
              || Annotation.has_attribute Annotation.tag_is_gentype_opaque
                   vd.val_attributes
            then
              let name =
                match vd.val_prim with
                | "" :: _ | [] -> Ident.name vd.val_id
                | name_of_extern :: _ -> name_of_extern
              in
              let tt =
                vd.val_desc
                |> TranslateCoreType.translate_core_type ~config ~type_env
              in
              let type_vars = tt.type_ |> TypeVars.free in
              let type_ =
                Translation.abstract_the_type_parameters ~type_vars tt.type_
              in
              {
                acc with
                gentype_value_defs = (name, type_) :: acc.gentype_value_defs;
              }
            else acc
          | Some import_segments ->
            let name =
              match vd.val_prim with
              | "" :: _ | [] -> Ident.name vd.val_id
              | name_of_extern :: _ -> name_of_extern
            in
            let tt =
              vd.val_desc
              |> TranslateCoreType.translate_core_type ~config ~type_env
            in
            let type_vars = tt.type_ |> TypeVars.free in
            let type_ =
              Translation.abstract_the_type_parameters ~type_vars tt.type_
            in
            {
              acc with
              value_assertions =
                {name; type_; import_segments} :: acc.value_assertions;
            })
        | _ -> acc
      in
      process_items rest acc1
  in
  process_items structure.str_items empty_assertions

let collect_from_signature ~(config : GenTypeConfig.t) ~output_file_relative
    ~resolver ~type_env (signature : Typedtree.signature) : assertions =
  let rec process_items (items : Typedtree.signature_item list)
      (acc : assertions) : assertions =
    match items with
    | [] -> acc
    | item :: rest ->
      let acc1 =
        match item.sig_desc with
        | Tsig_type (_rec, type_declarations) ->
          let wanted =
            type_declarations
            |> List.filter (fun (td : Typedtree.type_declaration) ->
                   Annotation.get_satisfies_path td.typ_attributes <> None)
          in
          let has_regular_gentype attrs =
            Annotation.has_attribute Annotation.tag_is_gentype attrs
            || Annotation.has_attribute Annotation.tag_is_gentype_as attrs
            || Annotation.has_attribute Annotation.tag_is_gentype_opaque attrs
          in
          let wanted_gentype =
            type_declarations
            |> List.filter (fun (td : Typedtree.type_declaration) ->
                   has_regular_gentype td.typ_attributes)
          in
          if wanted = [] && wanted_gentype = [] then acc
          else
            let trans =
              TranslateTypeDeclarations.translate_type_declarations ~config
                ~output_file_relative ~recursive:false ~resolver ~type_env
                type_declarations
            in
            let by_name =
              trans
              |> List.fold_left
                   (fun m (td : CodeItem.type_declaration) ->
                     let et = td.export_from_type_declaration.export_type in
                     let base_name =
                       match ResolvedName.to_list et.resolved_type_name with
                       | [] -> ""
                       | xs -> List.hd (List.rev xs)
                     in
                     StringMap.add base_name (et.type_, et.type_vars) m)
                   StringMap.empty
            in
            let add_one acc (td : Typedtree.type_declaration) =
              let name = Ident.name td.typ_id in
              match Annotation.get_satisfies_path td.typ_attributes with
              | None -> acc
              | Some import_segments -> (
                match StringMap.find name by_name with
                | exception Not_found -> acc
                | type_, type_vars ->
                  {
                    acc with
                    type_assertions =
                      {name; type_vars; type_; import_segments}
                      :: acc.type_assertions;
                  })
            in
            let acc = wanted |> List.fold_left add_one acc in
            let add_one_gentype acc (td : Typedtree.type_declaration) =
              let name = Ident.name td.typ_id in
              if Annotation.get_satisfies_path td.typ_attributes <> None then
                acc
              else
                match StringMap.find name by_name with
                | exception Not_found -> acc
                | type_, type_vars ->
                  {
                    acc with
                    gentype_type_defs =
                      (name, type_vars, type_) :: acc.gentype_type_defs;
                  }
            in
            wanted_gentype |> List.fold_left add_one_gentype acc
        | Tsig_value vd -> (
          match Annotation.get_satisfies_path vd.val_attributes with
          | None -> acc
          | Some import_segments ->
            let name = Ident.name vd.val_id in
            let tt =
              vd.val_desc
              |> TranslateCoreType.translate_core_type ~config ~type_env
            in
            let type_vars = tt.type_ |> TypeVars.free in
            let type_ =
              Translation.abstract_the_type_parameters ~type_vars tt.type_
            in
            {
              acc with
              value_assertions =
                {name; type_; import_segments} :: acc.value_assertions;
            })
        | _ -> acc
      in
      process_items rest acc1
  in
  process_items signature.sig_items empty_assertions

let render ~(config : GenTypeConfig.t) (a : assertions) : string option =
  let type_name_is_interface _ = false in
  let buf = Buffer.create 256 in
  (* Emit the helper type if there's at least one type assertion *)
  (match a.type_assertions with
  | [] -> ()
  | _ ->
    Buffer.add_string buf
      "type $RescriptTypeSatisfiesTypeScriptType<RescriptType, TypeScriptType \
       extends RescriptType> = TypeScriptType;\n\n");
  let emit_type (t : type_assertion) =
    let generics =
      match t.type_vars with
      | [] -> ""
      | vs -> "<" ^ String.concat ", " vs ^ ">"
    in
    let res_type =
      EmitType.type_to_string ~config ~type_name_is_interface t.type_
    in
    let import_ts = ts_import_path t.import_segments in
    Buffer.add_string buf
      ("type " ^ sanitize_type_name t.name ^ generics
     ^ " = $RescriptTypeSatisfiesTypeScriptType<" ^ res_type ^ ", " ^ import_ts
     ^ ">;\n")
  in
  let emit_value (v : value_assertion) =
    let res_type =
      EmitType.type_to_string ~config ~type_name_is_interface v.type_
    in
    let import_ts = ts_import_path v.import_segments in
    let name = "_" ^ v.name in
    Buffer.add_string buf
      ("const " ^ name ^ " = null as unknown as " ^ res_type
     ^ " satisfies typeof " ^ import_ts ^ ";\n")
  in
  a.type_assertions |> List.rev |> List.iter emit_type;
  (match a.type_assertions with
  | [] -> ()
  | _ -> Buffer.add_char buf '\n');
  a.value_assertions |> List.rev |> List.iter emit_value;
  (* Also emit definitions for @gentype types and values (non-exported). *)
  let emit_gentype_type (name, type_vars, type_) =
    let generics =
      match type_vars with
      | [] -> ""
      | vs -> "<" ^ String.concat ", " vs ^ ">"
    in
    Buffer.add_string buf
      ("type " ^ sanitize_type_name name ^ generics ^ " = "
      ^ EmitType.type_to_string ~config ~type_name_is_interface type_
      ^ ";\n")
  in
  (match a.gentype_type_defs with
  | [] -> ()
  | _ -> Buffer.add_char buf '\n');
  a.gentype_type_defs |> List.rev |> List.iter emit_gentype_type;
  (* Also emit definitions for @gentype values (non-exported). *)
  let emit_gentype_value (name, type_) =
    Buffer.add_string buf
      ("const _" ^ name ^ " = null as unknown as "
      ^ EmitType.type_to_string ~config ~type_name_is_interface type_
      ^ ";\n")
  in
  (match a.gentype_value_defs with
  | [] -> ()
  | _ -> Buffer.add_char buf '\n');
  a.gentype_value_defs |> List.rev |> List.iter emit_gentype_value;
  if Buffer.length buf = 0 then None else Some (Buffer.contents buf)

let emit_for_cmt ~(config : GenTypeConfig.t) ~(output_file_relative : string)
    ~(resolver : ModuleResolver.resolver) (input_cmt : Cmt_format.cmt_infos) :
    string option =
  let type_env = TypeEnv.root () in
  let assertions =
    match input_cmt.Cmt_format.cmt_annots with
    | Cmt_format.Implementation structure ->
      collect_from_structure ~config ~output_file_relative ~resolver ~type_env
        structure
    | Cmt_format.Interface signature ->
      collect_from_signature ~config ~output_file_relative ~resolver ~type_env
        signature
    | _ -> empty_assertions
  in
  render ~config assertions
