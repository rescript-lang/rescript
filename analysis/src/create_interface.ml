module Source_file_extractor = struct
  let leading_spaces line =
    let len = String.length line in
    let rec go i = if i < len && line.[i] = ' ' then go (i + 1) else i in
    go 0

  (* For each line from [line_start] to [line_end], whether it begins inside
     a string or template literal that starts on an earlier line. Such lines
     are part of the literal's value, so reindenting them would change it. *)
  let literal_lines lines ~line_start ~col_start ~line_end =
    let first = lines.(line_start) in
    let col_start = max 0 (min col_start (String.length first)) in
    let src =
      String.concat "\n"
        (String.sub first col_start (String.length first - col_start)
        :: Array.to_list
             (Array.sub lines (line_start + 1) (line_end - line_start)))
    in
    let inside = Array.make (line_end - line_start + 1) false in
    let mark (start_pos : Lexing.position) (end_pos : Lexing.position) =
      for n = start_pos.pos_lnum to end_pos.pos_lnum - 1 do
        inside.(n) <- true
      done
    in
    let scanner = Res_scanner.make ~filename:"" src in
    (* Scan the rest of a template literal, whose backtick is at [start_pos],
       including the expressions in its interpolations. *)
    let rec template start_pos =
      let _, end_pos, token = Res_scanner.scan_template_literal_token scanner in
      match token with
      | Res_token.TemplatePart _ ->
        if interpolation 0 then template start_pos else mark start_pos end_pos
      | _ -> mark start_pos end_pos
    (* Scan up to the [}] closing an interpolation; false at the end of the
       text. *)
    and interpolation depth =
      match Res_scanner.scan scanner with
      | _, _, Res_token.Eof -> false
      | _, _, Rbrace when depth = 0 -> true
      | _, _, Rbrace -> interpolation (depth - 1)
      | _, _, Lbrace -> interpolation (depth + 1)
      | start_pos, _, Backtick ->
        template start_pos;
        interpolation depth
      | _ -> interpolation depth
    in
    let rec go () =
      match Res_scanner.scan scanner with
      | _, _, Res_token.Eof -> ()
      | start_pos, end_pos, String _ ->
        mark start_pos end_pos;
        go ()
      | start_pos, _, Backtick ->
        template start_pos;
        go ()
      | _ -> go ()
    in
    go ();
    inside

  (* The source text from [pos_start] to [pos_end], indented by [indent]:
     the lines after the first keep their indentation relative to the line
     the text starts on, except lines inside a string or template literal,
     which are copied verbatim. *)
  let extract lines ~indent ~pos_start ~pos_end =
    let line_start, col_start = pos_start in
    let line_end, col_end = pos_end in
    let res = ref [] in
    if line_start < 0 || line_start > line_end || line_end >= Array.length lines
    then []
    else
      let base = min col_start (leading_spaces lines.(line_start)) in
      let in_literal = literal_lines lines ~line_start ~col_start ~line_end in
      let reindent n line =
        let drop = min base (leading_spaces line) in
        if in_literal.(n - line_start) then line
        else if drop = String.length line then ""
        else indent ^ String.sub line drop (String.length line - drop)
      in
      for n = line_end downto line_start do
        let line = lines.(n) in
        let len = String.length line in
        if n = line_start && n = line_end then (
          if col_start >= 0 && col_start < col_end && col_end <= len then
            res :=
              (indent ^ String.sub line col_start (col_end - col_start)) :: !res)
        else if n = line_start then (
          if col_start >= 0 && col_start < len then
            res :=
              (indent ^ String.sub line col_start (len - col_start)) :: !res)
        else if n = line_end then (
          if col_end > 0 && col_end <= len then
            res := reindent n (String.sub line 0 col_end) :: !res)
        else res := reindent n line :: !res
      done;
      !res
end

let loc_within ~(outer : Location.t) (loc : Location.t) =
  outer.loc_start.pos_cnum <= loc.loc_start.pos_cnum
  && loc.loc_end.pos_cnum <= outer.loc_end.pos_cnum

(* The locations of the functors in a signature, nested ones included. *)
let rec functor_locs (signature : Types.signature) =
  signature
  |> List.concat_map (function
    | Types.Sig_module (_, {md_type; md_loc}, _) -> (
      match md_type with
      | Mty_functor _ -> [md_loc]
      | Mty_signature signature -> functor_locs signature
      | Mty_ident _ | Mty_alias _ -> [])
    | _ -> [])

(* The source text of a type declaration can be copied when it says what the
   type is at this point of the interface: when the type is declared in this
   file, and either inside the module being printed or outside any functor.
   A type of an applied functor is located in the functor's body, where its
   definition mentions the functor's parameters, and a type included from
   another file is located there; both are printed from the signature. *)
let declared_in ~path ~functors ~(scope : Location.t option) (loc : Location.t)
    =
  Filename.basename loc.loc_start.pos_fname = Filename.basename path
  &&
  match scope with
  | Some scope when loc_within ~outer:scope loc -> true
  | _ -> not (List.exists (fun outer -> loc_within ~outer loc) functors)

(* A declaration's location starts at [type], after the attributes written
   in front of it, such as [@tag("kind")] or a doc comment: the position of
   the first of those, when it comes before [pos_start]. An attribute's
   location may start at its name, after the [@]. *)
let attributes_start ~extractor (type_decl : Types.type_declaration) pos_start =
  type_decl.type_attributes
  |> List.fold_left
       (fun pos_start (({loc}, _) : Parsetree.attribute) ->
         if loc.loc_ghost then pos_start
         else
           let line, col = Loc.start loc in
           let col =
             if
               col > 0
               && line < Array.length extractor
               && col <= String.length extractor.(line)
               && extractor.(line).[col - 1] = '@'
             then col - 1
             else col
           in
           min pos_start (line, col))
       pos_start

(* The attributes of a [let] binding that an interface keeps in front of the
   values it binds: doc comments, [@deprecated], which warns at uses from
   other modules, and the annotations that genType and reanalyze read from
   an interface, among them [@throws], which reanalyze reads at calls from
   other modules. Other attributes, such as [@warning] or [@inline], are about
   the implementation. *)
let interface_attribute (({txt}, _) : Parsetree.attribute) =
  match txt with
  | "res.doc" | "deprecated" | "throws" | "live" | "dead" | "genType"
  | "gentype" ->
    true
  | _ ->
    String.starts_with ~prefix:"genType." txt
    || String.starts_with ~prefix:"gentype." txt

type binding = {
  pattern: Location.t;
  attributes: Parsetree.attributes;  (** The interface attributes *)
  inline: bool;
}

(* The [let] bindings in the source that have interface attributes or an
   [@inline] attribute. *)
let bindings ~source =
  let {Res_driver.parsetree = structure} =
    Res_driver.parsing_engine.parse_implementation_from_source ~source
  in
  let bindings = ref [] in
  let iterator =
    {
      Ast_iterator.default_iterator with
      value_binding =
        (fun iterator (vb : Parsetree.value_binding) ->
          let attributes = List.filter interface_attribute vb.pvb_attributes in
          let inline =
            List.exists
              (fun (({txt}, _) : Parsetree.attribute) -> txt = "inline")
              vb.pvb_attributes
          in
          if attributes <> [] || inline then
            bindings :=
              {pattern = vb.pvb_pat.ppat_loc; attributes; inline} :: !bindings;
          Ast_iterator.default_iterator.value_binding iterator vb);
    }
  in
  iterator.structure iterator structure;
  !bindings

let print_signature ~path ~source ~extractor ~signature =
  Printtyp.reset_names ();
  let functors = functor_locs signature in
  let sig_item_to_string (item : Outcometree.out_sig_item) =
    item |> Res_outcome_printer.print_out_sig_item_doc
    |> Res_doc.to_string ~width:Res_printer.default_print_width
  in

  let bindings = bindings ~source in
  (* The binding of a value in this file. A value's location is the variable
     in the binding's pattern, or for an [@inline] constant the whole
     binding. *)
  let binding_of (val_loc : Location.t) =
    if Filename.basename val_loc.loc_start.pos_fname <> Filename.basename path
    then None
    else
      List.find_opt
        (fun {pattern} ->
          loc_within ~outer:pattern val_loc || loc_within ~outer:val_loc pattern)
        bindings
  in
  (* The interface attributes of the binding of a value, one per line. *)
  let value_attributes ~indent val_loc =
    match binding_of val_loc with
    | None -> ""
    | Some {attributes} ->
      attributes
      |> List.map (fun attr ->
          indent
          ^ Res_doc.to_string ~width:Res_printer.default_print_width
              (Res_printer.print_attribute attr (Res_comments_table.make ()))
          ^ "\n")
      |> String.concat ""
  in

  let buf = Buffer.create 10 in

  let get_component_type (typ : Types.type_expr) =
    let react_element =
      Ctype.newconstr (Pdot (Pident (Ident.create "React"), "element", 0)) []
    in
    match Types.get_desc typ with
    | Tarrow ({typ = props_type} :: _, ret_type) -> (
      match Types.get_desc props_type with
      | Tconstr (Path.Pident props_id, type_args, _)
        when Ident.name props_id = "props" ->
        Some (type_args, ret_type)
      | _ -> None)
    | Tconstr (Pdot (Pident {name = "React"}, "component", _), [props_type], _)
      -> (
      match Types.get_desc props_type with
      | Tconstr (Path.Pident props_id, type_args, _)
        when Ident.name props_id = "props" ->
        Some (type_args, react_element)
      | _ -> None)
    | Tconstr
        ( Pdot (Pident {name = "React"}, "componentLike", _),
          [props_type; ret_type],
          _ ) -> (
      match Types.get_desc props_type with
      | Tconstr (Path.Pident props_id, type_args, _)
        when Ident.name props_id = "props" ->
        Some (type_args, ret_type)
      | _ -> None)
    | _ -> None
  in

  let rec process_signature ~scope ~indent (signature : Types.signature) : unit
      =
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
        let mk_fun_type (label_decls : Types.label_declaration list) =
          let params =
            label_decls
            |> List.map (fun (label_decl : Types.label_declaration) ->
                let prop_type =
                  Type_utils.instantiate_type ~type_params ~type_args
                    label_decl.ld_type
                in
                let lbl_name = label_decl.ld_id |> Ident.name in
                let lbl =
                  if label_decl.ld_optional then
                    Asttypes.Optional {txt = lbl_name; loc = Location.none}
                  else Asttypes.Labelled {txt = lbl_name; loc = Location.none}
                in
                {Types.lbl; typ = prop_type})
          in
          Types.Transient_expr.create
            (Tarrow (params, ret_type))
            ~level:(Types.get_level ret_type) ~id:(Types.get_id ret_type)
        in
        let fun_type =
          if List.length label_decls = 0 (* No props *) then
            let t_unit =
              Ctype.newconstr (Path.Pident (Ident.create "unit")) []
            in
            Types.Transient_expr.create
              (Tarrow ([{Types.lbl = Nolabel; typ = t_unit}], ret_type))
              ~level:(Types.get_level ret_type) ~id:(Types.get_id ret_type)
          else mk_fun_type label_decls
        in
        sig_item_to_string
          (Printtyp.tree_of_value_description make_id
             {make_value_desc with val_type = fun_type})
      in
      Buffer.add_string buf (value_attributes ~indent make_value_desc.val_loc);
      Buffer.add_string buf (indent ^ "@react.component\n");
      Buffer.add_string buf (indent ^ new_item_str ^ "\n");
      process_signature ~scope ~indent rest
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
      process_module_type ~scope:(Some mod_decl.md_loc) ~indent mod_decl.md_type;
      Buffer.add_string buf "\n";
      process_signature ~scope ~indent rest
    | Sig_modtype (id, mtd) :: rest ->
      let () =
        match mtd.mtd_type with
        | None ->
          Buffer.add_string buf (indent ^ "module type " ^ Ident.name id ^ "\n")
        | Some mt ->
          Buffer.add_string buf (indent ^ "module type " ^ Ident.name id ^ " = ");
          process_module_type ~scope:(Some mtd.mtd_loc) ~indent mt;
          Buffer.add_string buf "\n"
      in
      process_signature ~scope ~indent rest
    | Sig_value (id, ({val_kind = Val_prim prim; val_loc} as vd)) :: items
      when prim.prim_kind <> Primitive.Kind_intrinsic ->
      (match binding_of val_loc with
      | Some {inline = true} ->
        (* A [let] binding an [@inline] constant, which is a primitive *)
        Buffer.add_string buf (value_attributes ~indent val_loc);
        Buffer.add_string buf
          (indent ^ "@inline "
          ^ sig_item_to_string
              (Printtyp.tree_of_value_description id
                 {vd with val_kind = Val_reg})
          ^ "\n")
      | _ ->
        (* Copy the external declaration verbatim from the implementation
           file *)
        let lines =
          let pos_start, pos_end = Loc.range val_loc in
          extractor |> Source_file_extractor.extract ~indent ~pos_start ~pos_end
        in
        Buffer.add_string buf ((lines |> String.concat "\n") ^ "\n"));
      process_signature ~scope ~indent items
    | Sig_value (id, vd) :: items ->
      let new_item_str =
        sig_item_to_string (Printtyp.tree_of_value_description id vd)
      in
      Buffer.add_string buf (value_attributes ~indent vd.val_loc);
      Buffer.add_string buf (indent ^ new_item_str ^ "\n");
      process_signature ~scope ~indent items
    | Sig_type (id, type_decl, rec_status) :: items ->
      (if declared_in ~path ~functors ~scope type_decl.type_loc then
         let lines =
           let pos_start, pos_end = Loc.range type_decl.type_loc in
           let pos_start = attributes_start ~extractor type_decl pos_start in
           extractor
           |> Source_file_extractor.extract ~indent ~pos_start ~pos_end
         in
         (* Copy the type declaration verbatim to preserve attributes *)
         Buffer.add_string buf ((lines |> String.concat "\n") ^ "\n")
       else
         let new_item_str =
           sig_item_to_string
             (Printtyp.tree_of_type_declaration id type_decl rec_status)
         in
         Buffer.add_string buf (indent ^ new_item_str ^ "\n"));
      process_signature ~scope ~indent items
    | Sig_typext (id, ext_constr, ext_status) :: items ->
      let new_item_str =
        sig_item_to_string
          (Printtyp.tree_of_extension_constructor id ext_constr ext_status)
      in
      Buffer.add_string buf (indent ^ new_item_str ^ "\n");
      process_signature ~scope ~indent items
    | [] -> ()
  and process_module_type ~scope ~indent (mt : Types.module_type) =
    match mt with
    | Mty_signature signature ->
      Buffer.add_string buf "{\n";
      process_signature ~scope ~indent:(indent ^ "  ") signature;
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
            process_module_type ~scope ~indent:(indent ^ "  ") mt);
          Buffer.add_string buf ",");
      if args <> [] then Buffer.add_string buf ("\n" ^ indent);
      Buffer.add_string buf (") =>\n" ^ indent);
      process_module_type ~scope ~indent ret_mt
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

  process_signature ~scope:None ~indent:"" signature;
  Buffer.contents buf

let command ~path ~source ~cmi_file =
  match Shared.try_read_cmi cmi_file with
  | Some cmi_info ->
    let extractor = source |> String.split_on_char '\n' |> Array.of_list in
    Ok (print_signature ~path ~source ~extractor ~signature:cmi_info.cmi_sign)
  | None -> Error ("Failed to read cmi file " ^ cmi_file)
