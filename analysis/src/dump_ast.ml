open Shared_types
(* This is intended to be a debug tool. It's by no means complete. Rather, you're encouraged to extend this with printing whatever types you need printing. *)

let empty_loc_denom = "<x>"
let has_cursor_denom = "<*>"
let no_cursor_denom = ""

let print_loc_denominator loc ~pos =
  match loc |> Cursor_position.classify_loc ~pos with
  | EmptyLoc -> empty_loc_denom
  | HasCursor -> has_cursor_denom
  | NoCursor -> no_cursor_denom

let print_loc_denominator_loc loc ~pos =
  match loc |> Cursor_position.classify_location_loc ~pos with
  | Cursor_position.EmptyLoc -> empty_loc_denom
  | HasCursor -> has_cursor_denom
  | NoCursor -> no_cursor_denom

let print_loc_denominator_pos pos ~pos_start ~pos_end =
  match Cursor_position.classify_positions pos ~pos_start ~pos_end with
  | Cursor_position.EmptyLoc -> empty_loc_denom
  | HasCursor -> has_cursor_denom
  | NoCursor -> no_cursor_denom

let add_indentation indentation =
  let rec indent str indentation =
    if indentation < 1 then str else indent (str ^ "  ") (indentation - 1)
  in
  indent "" indentation

let print_attributes attributes =
  match List.length attributes with
  | 0 -> ""
  | _ ->
    "["
    ^ (attributes
      |> List.map (fun ({Location.txt}, _payload) -> "@" ^ txt)
      |> String.concat ",")
    ^ "]"

let print_constant const =
  match const with
  | Parsetree.Pconst_integer (s, _) -> "Pconst_integer(" ^ s ^ ")"
  | Pconst_char c -> "Pconst_char(" ^ String.make 1 (Char.chr c) ^ ")"
  | Pconst_string (s, delim) ->
    let delim =
      match delim with
      | None -> ""
      | Some delim -> delim ^ " "
    in
    "Pconst_string(" ^ delim ^ s ^ delim ^ ")"
  | Pconst_float (s, _) -> "Pconst_float(" ^ s ^ ")"

let print_core_type typ ~pos =
  print_attributes typ.Parsetree.ptyp_attributes
  ^ (typ.ptyp_loc |> print_loc_denominator ~pos)
  ^
  match typ.ptyp_desc with
  | Ptyp_any -> "Ptyp_any"
  | Ptyp_var name -> "Ptyp_var(" ^ str name ^ ")"
  | Ptyp_constr (lid, _types) ->
    "Ptyp_constr("
    ^ (lid |> print_loc_denominator_loc ~pos)
    ^ (Utils.flatten_long_ident lid.txt |> ident |> str)
    ^ ")"
  | Ptyp_variant _ -> "Ptyp_variant(<unimplemented>)"
  | _ -> "<unimplemented_ptyp_desc>"

let rec print_pattern pattern ~pos ~indentation =
  print_attributes pattern.Parsetree.ppat_attributes
  ^ (pattern.ppat_loc |> print_loc_denominator ~pos)
  ^
  match pattern.Parsetree.ppat_desc with
  | Ppat_or (pat1, pat2) ->
    "Ppat_or(\n"
    ^ add_indentation (indentation + 1)
    ^ print_pattern pat1 ~pos ~indentation:(indentation + 2)
    ^ ",\n"
    ^ add_indentation (indentation + 1)
    ^ print_pattern pat2 ~pos ~indentation:(indentation + 2)
    ^ "\n"
    ^ add_indentation indentation
    ^ ")"
  | Ppat_extension (({txt} as loc), _) ->
    "Ppat_extension(%" ^ (loc |> print_loc_denominator_loc ~pos) ^ txt ^ ")"
  | Ppat_var ({txt} as loc) ->
    "Ppat_var(" ^ (loc |> print_loc_denominator_loc ~pos) ^ txt ^ ")"
  | Ppat_constant const -> "Ppat_constant(" ^ print_constant const ^ ")"
  | Ppat_construct (({txt} as loc), maybe_pat) ->
    "Ppat_construct("
    ^ (loc |> print_loc_denominator_loc ~pos)
    ^ (Utils.flatten_long_ident txt |> ident |> str)
    ^ (match maybe_pat with
      | None -> ""
      | Some pat -> "," ^ print_pattern pat ~pos ~indentation)
    ^ ")"
  | Ppat_variant (label, maybe_pat) ->
    "Ppat_variant(" ^ str label
    ^ (match maybe_pat with
      | None -> ""
      | Some pat -> "," ^ print_pattern pat ~pos ~indentation)
    ^ ")"
  | Ppat_record (fields, _) ->
    "Ppat_record(\n"
    ^ add_indentation (indentation + 1)
    ^ "fields:\n"
    ^ (Ext_list.map fields (fun {lid; x = pat} ->
           add_indentation (indentation + 2)
           ^ (lid |> print_loc_denominator_loc ~pos)
           ^ (Utils.flatten_long_ident lid.txt |> ident |> str)
           ^ ": "
           ^ print_pattern pat ~pos ~indentation:(indentation + 2))
      |> String.concat "\n")
    ^ "\n"
    ^ add_indentation indentation
    ^ ")"
  | Ppat_tuple patterns ->
    "Ppat_tuple(\n"
    ^ (patterns
      |> List.map (fun pattern ->
             add_indentation (indentation + 2)
             ^ (pattern |> print_pattern ~pos ~indentation:(indentation + 2)))
      |> String.concat ",\n")
    ^ "\n"
    ^ add_indentation indentation
    ^ ")"
  | Ppat_any -> "Ppat_any"
  | Ppat_constraint (pattern, typ) ->
    "Ppat_constraint(\n"
    ^ add_indentation (indentation + 1)
    ^ print_core_type typ ~pos ^ ",\n"
    ^ add_indentation (indentation + 1)
    ^ (pattern |> print_pattern ~pos ~indentation:(indentation + 1))
    ^ "\n"
    ^ add_indentation indentation
    ^ ")"
  | v -> Printf.sprintf "<unimplemented_ppat_desc: %s>" (Utils.identify_ppat v)

and print_case case ~pos ~indentation ~case_num =
  add_indentation indentation
  ^ Printf.sprintf "case %i:\n" case_num
  ^ add_indentation (indentation + 1)
  ^ "pattern"
  ^ (case.Parsetree.pc_lhs.ppat_loc |> print_loc_denominator ~pos)
  ^ ":\n"
  ^ add_indentation (indentation + 2)
  ^ print_pattern case.Parsetree.pc_lhs ~pos ~indentation
  ^ "\n"
  ^ add_indentation (indentation + 1)
  ^ "expr"
  ^ (case.Parsetree.pc_rhs.pexp_loc |> print_loc_denominator ~pos)
  ^ ":\n"
  ^ add_indentation (indentation + 2)
  ^ print_expr_item case.pc_rhs ~pos ~indentation:(indentation + 2)

and print_expr_item expr ~pos ~indentation =
  print_attributes expr.Parsetree.pexp_attributes
  ^ (expr.pexp_loc |> print_loc_denominator ~pos)
  ^
  match expr.Parsetree.pexp_desc with
  | Pexp_array exprs ->
    "Pexp_array(\n"
    ^ add_indentation (indentation + 1)
    ^ (exprs
      |> List.map (fun expr ->
             expr |> print_expr_item ~pos ~indentation:(indentation + 1))
      |> String.concat ("\n" ^ add_indentation (indentation + 1)))
    ^ "\n"
    ^ add_indentation indentation
    ^ ")"
  | Pexp_match (match_expr, cases) ->
    "Pexp_match("
    ^ print_expr_item match_expr ~pos ~indentation:0
    ^ ")\n"
    ^ (cases
      |> List.mapi (fun case_num case ->
             print_case case ~pos ~case_num:(case_num + 1)
               ~indentation:(indentation + 1))
      |> String.concat "\n")
  | Pexp_ident {txt} ->
    "Pexp_ident:" ^ (Utils.flatten_long_ident txt |> Shared_types.ident)
  | Pexp_break -> "Pexp_break"
  | Pexp_continue -> "Pexp_continue"
  | Pexp_apply {funct = expr; args} ->
    let print_label labelled ~pos =
      match labelled with
      | None -> "<unlabelled>"
      | Some labelled ->
        print_loc_denominator_pos pos ~pos_start:labelled.pos_start
          ~pos_end:labelled.pos_end
        ^ "~"
        ^ if labelled.opt then "?" else "" ^ labelled.name
    in
    let args = extract_exp_apply_args ~args in
    "Pexp_apply(\n"
    ^ add_indentation (indentation + 1)
    ^ "expr:\n"
    ^ add_indentation (indentation + 2)
    ^ print_expr_item expr ~pos ~indentation:(indentation + 2)
    ^ "\n"
    ^ add_indentation (indentation + 1)
    ^ "args:\n"
    ^ (args
      |> List.map (fun arg ->
             add_indentation (indentation + 2)
             ^ print_label arg.label ~pos ^ "=\n"
             ^ add_indentation (indentation + 3)
             ^ print_expr_item arg.exp ~pos ~indentation:(indentation + 3))
      |> String.concat ",\n")
    ^ "\n"
    ^ add_indentation indentation
    ^ ")"
  | Pexp_constant constant -> "Pexp_constant(" ^ print_constant constant ^ ")"
  | Pexp_construct (({txt} as loc), maybe_expr) ->
    "Pexp_construct("
    ^ (loc |> print_loc_denominator_loc ~pos)
    ^ (Utils.flatten_long_ident txt |> ident |> str)
    ^ (match maybe_expr with
      | None -> ""
      | Some expr -> ", " ^ print_expr_item expr ~pos ~indentation)
    ^ ")"
  | Pexp_variant (label, maybe_expr) ->
    "Pexp_variant(" ^ str label
    ^ (match maybe_expr with
      | None -> ""
      | Some expr -> "," ^ print_expr_item expr ~pos ~indentation)
    ^ ")"
  | Pexp_fun {arg_label = arg; lhs = pattern; rhs = next_expr} ->
    "Pexp_fun(\n"
    ^ add_indentation (indentation + 1)
    ^ "arg: "
    ^ (match arg with
      | Nolabel -> "Nolabel"
      | Labelled {txt = name} -> "Labelled(" ^ name ^ ")"
      | Optional {txt = name} -> "Optional(" ^ name ^ ")")
    ^ ",\n"
    ^ add_indentation (indentation + 2)
    ^ "pattern: "
    ^ print_pattern pattern ~pos ~indentation:(indentation + 2)
    ^ ",\n"
    ^ add_indentation (indentation + 1)
    ^ "next expr:\n"
    ^ add_indentation (indentation + 2)
    ^ print_expr_item next_expr ~pos ~indentation:(indentation + 2)
    ^ "\n"
    ^ add_indentation indentation
    ^ ")"
  | Pexp_extension (({txt} as loc), _) ->
    "Pexp_extension(%" ^ (loc |> print_loc_denominator_loc ~pos) ^ txt ^ ")"
  | Pexp_assert expr ->
    "Pexp_assert(" ^ print_expr_item expr ~pos ~indentation ^ ")"
  | Pexp_field (exp, loc) ->
    "Pexp_field("
    ^ (loc |> print_loc_denominator_loc ~pos)
    ^ print_expr_item exp ~pos ~indentation
    ^ ")"
  | Pexp_record (fields, _) ->
    "Pexp_record(\n"
    ^ add_indentation (indentation + 1)
    ^ "fields:\n"
    ^ (Ext_list.map fields (fun {lid; x = expr} ->
           add_indentation (indentation + 2)
           ^ (lid |> print_loc_denominator_loc ~pos)
           ^ (Utils.flatten_long_ident lid.txt |> ident |> str)
           ^ ": "
           ^ print_expr_item expr ~pos ~indentation:(indentation + 2))
      |> String.concat "\n")
    ^ "\n"
    ^ add_indentation indentation
    ^ ")"
  | Pexp_tuple exprs ->
    "Pexp_tuple(\n"
    ^ (exprs
      |> List.map (fun expr ->
             add_indentation (indentation + 2)
             ^ (expr |> print_expr_item ~pos ~indentation:(indentation + 2)))
      |> String.concat ",\n")
    ^ "\n"
    ^ add_indentation indentation
    ^ ")"
  | v -> Printf.sprintf "<unimplemented_pexp_desc: %s>" (Utils.identify_pexp v)

let print_value_binding value ~pos ~indentation =
  print_attributes value.Parsetree.pvb_attributes
  ^ "value" ^ ":\n"
  ^ add_indentation (indentation + 1)
  ^ (value.pvb_pat |> print_pattern ~pos ~indentation:(indentation + 1))
  ^ "\n"
  ^ add_indentation indentation
  ^ "expr:\n"
  ^ add_indentation (indentation + 1)
  ^ print_expr_item value.pvb_expr ~pos ~indentation:(indentation + 1)

let print_struct_item struct_item ~pos ~source =
  match struct_item.Parsetree.pstr_loc |> Cursor_position.classify_loc ~pos with
  | HasCursor -> (
    let start_offset =
      match
        Pos.position_to_offset source (struct_item.pstr_loc |> Loc.start)
      with
      | None -> 0
      | Some offset -> offset
    in
    let end_offset =
      (* Include the next line of the source since that will hold the ast comment pointing to the position.
         Caveat: this only works for single line sources with a comment on the next line. Will need to be
         adapted if that's not the only use case.*)
      let line, _col = struct_item.pstr_loc |> Loc.end_ in
      match Pos.position_to_offset source (line + 2, 0) with
      | None -> 0
      | Some offset -> offset
    in

    ("\nSource:\n// "
    ^ String.sub source start_offset (end_offset - start_offset)
    ^ "\n")
    ^ print_loc_denominator struct_item.pstr_loc ~pos
    ^
    match struct_item.pstr_desc with
    | Pstr_eval (expr, _attributes) ->
      "Pstr_eval(\n" ^ print_expr_item expr ~pos ~indentation:1 ^ "\n)"
    | Pstr_value (rec_flag, values) ->
      "Pstr_value(\n"
      ^ (match rec_flag with
        | Recursive -> "  rec,\n"
        | Nonrecursive -> "")
      ^ (values
        |> List.map (fun value ->
               add_indentation 1 ^ print_value_binding value ~pos ~indentation:1)
        |> String.concat ",\n")
      ^ "\n)"
    | _ -> "<structure_item_not_implemented>")
  | _ -> ""

let dump ~current_file ~pos =
  let {Res_driver.parsetree = structure; source} =
    Res_driver.parsing_engine.parse_implementation ~for_printer:true
      ~filename:current_file
  in

  print_endline
    (structure
    |> List.map (fun struct_item -> print_struct_item struct_item ~pos ~source)
    |> String.concat "")
