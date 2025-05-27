open Types
open Typedtree
open Parsetree

let mkpat desc = Ast_helper.Pat.mk desc

let untype typed =
  let rec loop pat =
    match pat.pat_desc with
    | Tpat_or (p1, {pat_desc = Tpat_or (p2, p3, r_i)}, r_o) ->
      (* Turn A | (B | C) into (A | B) | C for pretty printing without parens *)
      let new_inner = {pat with pat_desc = Tpat_or (p1, p2, r_i)} in
      let new_outer = {pat with pat_desc = Tpat_or (new_inner, p3, r_o)} in
      loop new_outer
    | Tpat_or (pa, pb, _) -> mkpat (Ppat_or (loop pa, loop pb))
    | Tpat_any | Tpat_var _ -> mkpat Ppat_any
    | Tpat_constant c -> mkpat (Ppat_constant (Untypeast.constant c))
    | Tpat_alias (p, _, _) -> loop p
    | Tpat_tuple lst -> mkpat (Ppat_tuple (List.map loop lst))
    | Tpat_construct (cstr_lid, cstr, lst) ->
      let lid = {cstr_lid with txt = Longident.Lident cstr.cstr_name} in
      let arg =
        match List.map loop lst with
        | [] -> None
        | [p] -> Some p
        | lst -> Some (mkpat (Ppat_tuple lst))
      in
      mkpat (Ppat_construct (lid, arg))
    | Tpat_variant (label, p_opt, _row_desc) ->
      let arg = Option.map loop p_opt in
      mkpat (Ppat_variant (label, arg))
    | Tpat_record (subpatterns, closed_flag) ->
      let fields =
        List.map
          (fun (_, lbl, p, opt) ->
            {lid = mknoloc (Longident.Lident lbl.lbl_name); x = loop p; opt})
          subpatterns
      in
      mkpat (Ppat_record (fields, closed_flag))
    | Tpat_array lst -> mkpat (Ppat_array (List.map loop lst))
  in
  loop typed

let print_pattern typed =
  let pat = untype typed in
  let doc = Res_printer.print_pattern pat Res_comments_table.empty in
  Res_doc.to_string ~width:80 doc
