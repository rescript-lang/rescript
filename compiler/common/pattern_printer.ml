open Types
open Typedtree
open Parsetree
open Asttypes

let mkpat desc = Ast_helper.Pat.mk desc

let[@warning "-4"] is_generated_optional_constructor
    (lid : Longident.t Location.loc) =
  match lid.txt with
  | Longident.Lident name ->
    String.length name >= 2 && name.[0] = '#' && name.[1] = '$'
  | _ -> false

(* Optional fields become “option-of-option” internally: the outer layer is
   added by the compiler to track presence, while the inner layer is the user’s
   payload. When printing counterexamples we only need to know which of these
   situations we saw. *)
type optional_field_state =
  | Field_normal (* Regular user patterns: `{b: Some(_)}`, `{b}`, `_`, etc. *)
  | Field_missing
    (* The outer constructor was the synthetic `#$None…`, i.e. the field was
         not provided at all. This is what should print as `{b: ?None}`. *)
  | Field_present_none
(* The outer constructor was the synthetic `#$Some…` but its payload was `None`.
         This means the optional field exists with value `None`, so we should
         print `{b: None}`. *)

(* Optional record fields are lowered into an extra option layer; we re-infer
   whether we’re looking at a missing field vs. a present-but-`None` value so
   we can render useful surface syntax in error messages. *)
let[@warning "-4"] rec classify_optional_field_state pat =
  match pat.pat_desc with
  | Tpat_construct (lid, cstr, [])
    when is_generated_optional_constructor lid && cstr.cstr_name = "None" ->
    Field_missing
  | Tpat_construct (lid, cstr, [inner])
    when is_generated_optional_constructor lid && cstr.cstr_name = "Some" -> (
    match classify_optional_field_state inner with
    | Field_missing | Field_present_none -> Field_present_none
    | Field_normal -> Field_normal)
  | _ -> Field_normal

let none_pattern =
  mkpat (Ppat_construct (mknoloc (Longident.Lident "None"), None))

let[@warning "-4"] strip_synthetic_some pat =
  match pat.pat_desc with
  | Tpat_construct (lid, cstr, [inner])
    when is_generated_optional_constructor lid && cstr.cstr_name = "Some" ->
    inner
  | _ -> pat

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
      let fields, saw_optional_rewrite =
        List.fold_right
          (fun (_, lbl, p, opt) (fields, saw_optional_rewrite) ->
            let state =
              if lbl.lbl_optional then classify_optional_field_state p
              else Field_normal
            in
            let opt, par_pat, rewrote_optional =
              match state with
              | Field_missing -> (true, none_pattern, true)
              | Field_present_none -> (opt, loop (strip_synthetic_some p), true)
              | Field_normal -> (opt, loop p, false)
            in
            let field =
              {lid = mknoloc (Longident.Lident lbl.lbl_name); x = par_pat; opt}
            in
            (field :: fields, saw_optional_rewrite || rewrote_optional))
          subpatterns ([], false)
      in
      let closed_flag = if saw_optional_rewrite then Closed else closed_flag in
      mkpat (Ppat_record (fields, closed_flag))
    | Tpat_array lst -> mkpat (Ppat_array (List.map loop lst))
  in
  loop typed

let print_pattern typed =
  let pat = untype typed in
  let doc = Res_printer.print_pattern pat Res_comments_table.empty in
  Res_doc.to_string ~width:80 doc
