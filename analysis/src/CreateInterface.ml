module SourceFileExtractor = struct
  let create ~path =
    match Files.read_file path with
    | None -> [||]
    | Some text -> text |> String.split_on_char '\n' |> Array.of_list

  let extract lines ~pos_start ~pos_end =
    let line_start, col_start = pos_start in
    let line_end, col_end = pos_end in
    let res = ref [] in
    if line_start < 0 || line_start > line_end || line_end >= Array.length lines
    then []
    else (
      for n = line_end downto line_start do
        let line = lines.(n) in
        let len = String.length line in
        if n = line_start && n = line_end then (
          if col_start >= 0 && col_start < col_end && col_end <= len then
            let indent = String.make col_start ' ' in
            res :=
              (indent ^ String.sub line col_start (col_end - col_start)) :: !res)
        else if n = line_start then (
          if col_start >= 0 && col_start < len then
            let indent = String.make col_start ' ' in
            res := (indent ^ String.sub line col_start (len - col_start)) :: !res)
        else if n = line_end then (
          if col_end > 0 && col_end <= len then
            res := String.sub line 0 col_end :: !res)
        else res := line :: !res
      done;
      !res)
end

module AttributesUtils : sig
  type t

  val make : string list -> t

  val contains : string -> t -> bool

  val to_string : t -> string
end = struct
  type attribute = {line: int; offset: int; name: string}
  type t = attribute list
  type parse_state = Search | Collect of int

  let make lines =
    let make_attr line_idx attr_offset_start attr_offset_end line =
      {
        line = line_idx;
        offset = attr_offset_start;
        name = String.sub line attr_offset_start (attr_offset_end - attr_offset_start);
      }
    in
    let res = ref [] in
    lines
    |> List.iteri (fun line_idx line ->
           let state = ref Search in
           for i = 0 to String.length line - 1 do
             let ch = line.[i] in
             match (!state, ch) with
             | Search, '@' -> state := Collect i
             | Collect attr_offset, ' ' ->
               res := make_attr line_idx attr_offset i line :: !res;
               state := Search
             | Search, _ | Collect _, _ -> ()
           done;

           match !state with
           | Collect attr_offset ->
             res :=
               make_attr line_idx attr_offset (String.length line) line :: !res
           | _ -> ());
    !res |> List.rev

  let contains attribute_for_search t =
    t |> List.exists (fun {name} -> name = attribute_for_search)

  let to_string t =
    match t with
    | [] -> ""
    | {line} :: _ ->
      let prev_line = ref line in
      let buffer = ref "" in
      let res = ref [] in
      t
      |> List.iter (fun attr ->
             let {line; offset; name} = attr in

             if line <> !prev_line then (
               res := !buffer :: !res;
               buffer := "";
               prev_line := line);

             let indent = String.make (offset - String.length !buffer) ' ' in
             buffer := !buffer ^ indent ^ name);
      res := !buffer :: !res;
      !res |> List.rev |> String.concat "\n"
end

let print_signature ~extractor ~signature =
  Printtyp.reset_names ();
  let sig_item_to_string (item : Outcometree.out_sig_item) =
    item |> Res_outcome_printer.print_out_sig_item_doc
    |> Res_doc.to_string ~width:Res_printer.default_print_width
  in

  let gen_sig_str_for_inline_attr lines attributes id vd =
    let divider = if List.length lines > 1 then "\n" else " " in

    let sig_str =
      sig_item_to_string
        (Printtyp.tree_of_value_description id {vd with val_kind = Val_reg})
    in

    (attributes |> AttributesUtils.to_string) ^ divider ^ sig_str ^ "\n"
  in

  let buf = Buffer.create 10 in

  let get_component_type (typ : Types.type_expr) =
    let react_element =
      Ctype.newconstr (Pdot (Pident (Ident.create "React"), "element", 0)) []
    in
    match typ.desc with
    | Tarrow
        ( {typ = {desc = Tconstr (Path.Pident props_id, type_args, _)}},
          ret_type,
          _,
          _ )
      when Ident.name props_id = "props" ->
      Some (type_args, ret_type)
    | Tconstr
        ( Pdot (Pident {name = "React"}, "component", _),
          [{desc = Tconstr (Path.Pident props_id, type_args, _)}],
          _ )
      when Ident.name props_id = "props" ->
      Some (type_args, react_element)
    | Tconstr
        ( Pdot (Pident {name = "React"}, "componentLike", _),
          [{desc = Tconstr (Path.Pident props_id, type_args, _)}; ret_type],
          _ )
      when Ident.name props_id = "props" ->
      Some (type_args, ret_type)
    | _ -> None
  in

  let rec process_signature ~indent (signature : Types.signature) : unit =
    match signature with
    | Sig_type
        (props_id, {type_params; type_kind = Type_record (label_decls, _)}, _)
      :: Sig_value (make_id (* make *), make_value_desc)
      :: rest
      when Ident.name props_id = "props"
           && get_component_type make_value_desc.val_type <> None ->
      (* PPX V4 component declaration:
         type props = {...}
         let v = ...
      *)
      let new_item_str =
        let type_args, ret_type =
          match get_component_type make_value_desc.val_type with
          | Some x -> x
          | None -> assert false
        in
        let rec mk_fun_type (label_decls : Types.label_declaration list) =
          match label_decls with
          | [] -> ret_type
          | label_decl :: rest ->
            let prop_type =
              TypeUtils.instantiate_type ~type_params:type_params ~type_args
                label_decl.ld_type
            in
            let lbl_name = label_decl.ld_id |> Ident.name in
            let lbl =
              if label_decl.ld_optional then
                Asttypes.Optional {txt = lbl_name; loc = Location.none}
              else Asttypes.Labelled {txt = lbl_name; loc = Location.none}
            in
            {
              ret_type with
              desc = Tarrow ({lbl; typ = prop_type}, mk_fun_type rest, Cok, None);
            }
        in
        let fun_type =
          if List.length label_decls = 0 (* No props *) then
            let t_unit =
              Ctype.newconstr (Path.Pident (Ident.create "unit")) []
            in
            {
              ret_type with
              desc = Tarrow ({lbl = Nolabel; typ = t_unit}, ret_type, Cok, None);
            }
          else mk_fun_type label_decls
        in
        sig_item_to_string
          (Printtyp.tree_of_value_description make_id
             {make_value_desc with val_type = fun_type})
      in
      Buffer.add_string buf (indent ^ "@react.component\n");
      Buffer.add_string buf (indent ^ new_item_str ^ "\n");
      process_signature ~indent rest
    | Sig_module (id, mod_decl, rec_status) :: rest ->
      let colon_or_equals =
        match mod_decl.md_type with
        | Mty_alias _ -> " = "
        | _ -> ": "
      in
      Buffer.add_string buf
        (indent
        ^ (match rec_status with
          | Trec_not -> "module "
          | Trec_first -> "module rec "
          | Trec_next -> "and ")
        ^ Ident.name id ^ colon_or_equals);
      process_module_type ~indent mod_decl.md_type;
      Buffer.add_string buf "\n";
      process_signature ~indent rest
    | Sig_modtype (id, mtd) :: rest ->
      let () =
        match mtd.mtd_type with
        | None ->
          Buffer.add_string buf (indent ^ "module type " ^ Ident.name id ^ "\n")
        | Some mt ->
          Buffer.add_string buf (indent ^ "module type " ^ Ident.name id ^ " = ");
          process_module_type ~indent mt;
          Buffer.add_string buf "\n"
      in
      process_signature ~indent rest
    | Sig_value (id, ({val_kind = Val_prim prim; val_loc} as vd)) :: items
      when prim.prim_native_name <> "" && prim.prim_native_name.[0] = '\132' ->
      (* Rescript primitive name, e.g. @val external ... *)
      let lines =
        let pos_start, pos_end = Loc.range val_loc in
        extractor |> SourceFileExtractor.extract ~pos_start ~pos_end
      in
      let attributes = AttributesUtils.make lines in

      if AttributesUtils.contains "@inline" attributes then
        (* Generate type signature for @inline declaration *)
        Buffer.add_string buf (gen_sig_str_for_inline_attr lines attributes id vd)
      else
        (* Copy the external declaration verbatim from the implementation file *)
        Buffer.add_string buf ((lines |> String.concat "\n") ^ "\n");

      process_signature ~indent items
    | Sig_value (id, vd) :: items ->
      let new_item_str =
        sig_item_to_string (Printtyp.tree_of_value_description id vd)
      in
      Buffer.add_string buf (indent ^ new_item_str ^ "\n");
      process_signature ~indent items
    | Sig_type (_id, type_decl, _recStatus) :: items ->
      let lines =
        let pos_start, pos_end = Loc.range type_decl.type_loc in
        extractor |> SourceFileExtractor.extract ~pos_start ~pos_end
      in
      (* Copy the type declaration verbatim to preserve attributes *)
      Buffer.add_string buf ((lines |> String.concat "\n") ^ "\n");
      process_signature ~indent items
    | Sig_typext (id, ext_constr, ext_status) :: items ->
      let new_item_str =
        sig_item_to_string
          (Printtyp.tree_of_extension_constructor id ext_constr ext_status)
      in
      Buffer.add_string buf (indent ^ new_item_str ^ "\n");
      process_signature ~indent items
    | Sig_class _ :: items ->
      (* not needed *)
      process_signature ~indent items
    | Sig_class_type _ :: items ->
      (* not needed *)
      process_signature ~indent items
    | [] -> ()
  and process_module_type ~indent (mt : Types.module_type) =
    match mt with
    | Mty_signature signature ->
      Buffer.add_string buf "{\n";
      process_signature ~indent:(indent ^ "  ") signature;
      Buffer.add_string buf (indent ^ "}")
    | Mty_functor _ ->
      let rec collect_functor_args ~args (mt : Types.module_type) =
        match mt with
        | Mty_functor (id, None, mt) when Ident.name id = "*" ->
          (* AST encoding of functor with no arguments *)
          collect_functor_args ~args mt
        | Mty_functor (id, mto, mt) ->
          collect_functor_args ~args:((id, mto) :: args) mt
        | mt -> (List.rev args, mt)
      in
      let args, ret_mt = collect_functor_args ~args:[] mt in
      Buffer.add_string buf "(";
      args
      |> List.iter (fun (id, mto) ->
             Buffer.add_string buf ("\n" ^ indent ^ "  ");
             (match mto with
             | None -> Buffer.add_string buf (Ident.name id)
             | Some mt ->
               Buffer.add_string buf (Ident.name id ^ ": ");
               process_module_type ~indent:(indent ^ "  ") mt);
             Buffer.add_string buf ",");
      if args <> [] then Buffer.add_string buf ("\n" ^ indent);
      Buffer.add_string buf (") =>\n" ^ indent);
      process_module_type ~indent ret_mt
    | Mty_ident path | Mty_alias (_, path) ->
      let rec out_ident_to_string (ident : Outcometree.out_ident) =
        match ident with
        | Oide_ident s -> s
        | Oide_dot (ident, s) -> out_ident_to_string ident ^ "." ^ s
        | Oide_apply (call, arg) ->
          out_ident_to_string call ^ "(" ^ out_ident_to_string arg ^ ")"
      in
      Buffer.add_string buf (out_ident_to_string (Printtyp.tree_of_path path))
  in

  process_signature ~indent:"" signature;
  Buffer.contents buf

let command ~path ~cmi_file =
  match Shared.try_read_cmi cmi_file with
  | Some cmi_info ->
    (* For reading the config *)
    let _ = Cmt.load_full_cmt_from_path ~path in
    let extractor = SourceFileExtractor.create ~path in
    print_signature ~extractor ~signature:cmi_info.cmi_sign
  | None -> ""
