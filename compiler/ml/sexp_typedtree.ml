(* Sexp printer for Typedtree - used for parity testing between OCaml and Rust compilers *)

open Asttypes
open Typedtree

(* Sexp module - simple string-based pretty printing *)
module Sexp = struct
  type t = Atom of string | List of t list

  let atom s = Atom s
  let list l = List l

  (* Simple pretty printer without Res_doc dependency *)
  let rec to_string_indent indent t =
    let spaces = String.make (indent * 2) ' ' in
    match t with
    | Atom s -> s
    | List [] -> "()"
    | List [Atom s] -> "(" ^ s ^ ")"
    | List [sexpr] -> "(" ^ to_string_indent indent sexpr ^ ")"
    | List items ->
      let inner = String.concat ("\n" ^ spaces ^ "  ")
        (List.map (to_string_indent (indent + 1)) items) in
      "(" ^ inner ^ ")"

  let to_string sexpr = to_string_indent 0 sexpr
end

(* Helper functions *)
let string txt = Sexp.atom ("\"" ^ txt ^ "\"")

let opt_string = function
  | None -> Sexp.atom "None"
  | Some s -> Sexp.list [Sexp.atom "Some"; string s]

let opt f = function
  | None -> Sexp.atom "None"
  | Some x -> Sexp.list [Sexp.atom "Some"; f x]

let location ~with_locs loc =
  if with_locs then
    Sexp.list [
      Sexp.atom "loc";
      Sexp.atom (string_of_int loc.Location.loc_start.Lexing.pos_lnum);
      Sexp.atom (string_of_int (loc.Location.loc_start.Lexing.pos_cnum - loc.Location.loc_start.Lexing.pos_bol));
      Sexp.atom (string_of_int loc.Location.loc_end.Lexing.pos_lnum);
      Sexp.atom (string_of_int (loc.Location.loc_end.Lexing.pos_cnum - loc.Location.loc_end.Lexing.pos_bol));
    ]
  else
    Sexp.list []

(* Path printer *)
let rec path p =
  match p with
  | Path.Pident id -> Sexp.list [Sexp.atom "Pident"; string (Ident.name id)]
  | Path.Pdot (p, s, _) -> Sexp.list [Sexp.atom "Pdot"; path p; string s]
  | Path.Papply (p1, p2) -> Sexp.list [Sexp.atom "Papply"; path p1; path p2]

(* Longident printer *)
let rec longident l =
  match l with
  | Longident.Lident s -> Sexp.list [Sexp.atom "Lident"; string s]
  | Longident.Ldot (l, s) -> Sexp.list [Sexp.atom "Ldot"; longident l; string s]
  | Longident.Lapply (l1, l2) -> Sexp.list [Sexp.atom "Lapply"; longident l1; longident l2]

let longident_loc li = longident li.txt

(* Ident printer - just the name, skip stamp for determinism *)
let ident id = string (Ident.name id)

(* Constant printer *)
let constant c =
  match c with
  | Const_int i -> Sexp.list [Sexp.atom "Const_int"; Sexp.atom (string_of_int i)]
  | Const_char c -> Sexp.list [Sexp.atom "Const_char"; Sexp.atom (Printf.sprintf "%d" c)]
  | Const_string (s, delim) ->
    Sexp.list [Sexp.atom "Const_string"; string s; opt string delim]
  | Const_float s -> Sexp.list [Sexp.atom "Const_float"; string s]
  | Const_int32 i -> Sexp.list [Sexp.atom "Const_int32"; Sexp.atom (Int32.to_string i)]
  | Const_int64 i -> Sexp.list [Sexp.atom "Const_int64"; Sexp.atom (Int64.to_string i)]
  | Const_bigint (positive, s) ->
    let sign_str = if positive then "+" else "-" in
    Sexp.list [Sexp.atom "Const_bigint"; Sexp.atom sign_str; string s]

(* Flags *)
let rec_flag = function
  | Nonrecursive -> Sexp.atom "Nonrecursive"
  | Recursive -> Sexp.atom "Recursive"

let direction_flag = function
  | Upto -> Sexp.atom "Upto"
  | Downto -> Sexp.atom "Downto"

let private_flag = function
  | Public -> Sexp.atom "Public"
  | Private -> Sexp.atom "Private"

let mutable_flag = function
  | Immutable -> Sexp.atom "Immutable"
  | Mutable -> Sexp.atom "Mutable"

let override_flag = function
  | Override -> Sexp.atom "Override"
  | Fresh -> Sexp.atom "Fresh"

let closed_flag = function
  | Closed -> Sexp.atom "Closed"
  | Open -> Sexp.atom "Open"

let sexp_arg_label lbl =
  match lbl with
  | Nolabel -> Sexp.atom "Nolabel"
  | Labelled {txt} -> Sexp.list [Sexp.atom "Labelled"; string txt]
  | Optional {txt} -> Sexp.list [Sexp.atom "Optional"; string txt]

let variance = function
  | Covariant -> Sexp.atom "Covariant"
  | Contravariant -> Sexp.atom "Contravariant"
  | Invariant -> Sexp.atom "Invariant"

let partial = function
  | Partial -> Sexp.atom "Partial"
  | Total -> Sexp.atom "Total"

(* Type expression printer - follows Tlink chains via Btype.repr *)
let rec type_expr te =
  let te = Btype.repr te in  (* Follow Tlink chains *)
  match te.Types.desc with
  | Tvar name -> Sexp.list [Sexp.atom "Tvar"; opt_string name]
  | Tarrow (arg, ret, _, arity) ->
    Sexp.list [
      Sexp.atom "Tarrow";
      sexp_arg_label arg.Types.lbl;
      type_expr arg.Types.typ;
      type_expr ret;
      opt (fun a -> Sexp.atom (string_of_int a)) arity;
    ]
  | Ttuple l -> Sexp.list (Sexp.atom "Ttuple" :: List.map type_expr l)
  | Tconstr (p, args, _) ->
    Sexp.list [Sexp.atom "Tconstr"; path p; Sexp.list (List.map type_expr args)]
  | Tobject (ty, _) ->
    Sexp.list [Sexp.atom "Tobject"; type_expr ty]
  | Tfield (name, kind, ty1, ty2) ->
    let kind_str = match Btype.field_kind_repr kind with
      | Fpresent -> "Fpresent"
      | Fabsent -> "Fabsent"
      | Fvar _ -> "Fvar"
    in
    Sexp.list [Sexp.atom "Tfield"; string name; Sexp.atom kind_str; type_expr ty1; type_expr ty2]
  | Tnil -> Sexp.atom "Tnil"
  | Tlink _ -> Sexp.atom "Tlink"  (* Should not happen after Btype.repr *)
  | Tsubst ty -> Sexp.list [Sexp.atom "Tsubst"; type_expr ty]
  | Tvariant row -> row_desc row
  | Tunivar name -> Sexp.list [Sexp.atom "Tunivar"; opt_string name]
  | Tpoly (ty, tyl) ->
    Sexp.list [Sexp.atom "Tpoly"; type_expr ty; Sexp.list (List.map type_expr tyl)]
  | Tpackage (p, lids, tyl) ->
    Sexp.list [
      Sexp.atom "Tpackage";
      path p;
      Sexp.list (List.map (fun l -> longident l) lids);
      Sexp.list (List.map type_expr tyl);
    ]

and row_desc row =
  let row = Btype.row_repr row in
  Sexp.list [
    Sexp.atom "Tvariant";
    Sexp.list (List.map (fun (l, rf) ->
      Sexp.list [string l; row_field rf]
    ) row.Types.row_fields);
    Sexp.atom (if row.Types.row_closed then "closed" else "open");
    Sexp.atom (if row.Types.row_fixed then "fixed" else "not_fixed");
  ]

and row_field rf =
  match Btype.row_field_repr rf with
  | Rpresent None -> Sexp.atom "Rpresent_none"
  | Rpresent (Some ty) -> Sexp.list [Sexp.atom "Rpresent"; type_expr ty]
  | Reither (c, tyl, _, _) ->
    Sexp.list [
      Sexp.atom "Reither";
      Sexp.atom (if c then "const" else "non_const");
      Sexp.list (List.map type_expr tyl);
    ]
  | Rabsent -> Sexp.atom "Rabsent"

(* Pattern *)
let rec pattern ~with_locs p =
  let loc = if with_locs then [location ~with_locs p.pat_loc] else [] in
  let extras = List.map (fun (extra, loc, attrs) ->
    pat_extra ~with_locs extra loc attrs
  ) p.pat_extra in
  let desc = pattern_desc ~with_locs p.pat_desc in
  let ty = type_expr p.pat_type in
  Sexp.list ([Sexp.atom "pattern"] @ loc @ extras @ [desc; Sexp.list [Sexp.atom "type"; ty]])

and pat_extra ~with_locs extra loc attrs =
  let loc_sexp = if with_locs then [location ~with_locs loc] else [] in
  let attrs_sexp = attributes ~with_locs attrs in
  match extra with
  | Tpat_constraint ct ->
    Sexp.list ([Sexp.atom "Tpat_constraint"; core_type ~with_locs ct] @ loc_sexp @ [attrs_sexp])
  | Tpat_type (p, _) ->
    Sexp.list ([Sexp.atom "Tpat_type"; path p] @ loc_sexp @ [attrs_sexp])
  | Tpat_open (p, _, _) ->
    Sexp.list ([Sexp.atom "Tpat_open"; path p] @ loc_sexp @ [attrs_sexp])
  | Tpat_unpack ->
    Sexp.list ([Sexp.atom "Tpat_unpack"] @ loc_sexp @ [attrs_sexp])

and pattern_desc ~with_locs desc =
  match desc with
  | Tpat_any -> Sexp.atom "Tpat_any"
  | Tpat_var (id, _) -> Sexp.list [Sexp.atom "Tpat_var"; ident id]
  | Tpat_alias (p, id, _) ->
    Sexp.list [Sexp.atom "Tpat_alias"; pattern ~with_locs p; ident id]
  | Tpat_constant c -> Sexp.list [Sexp.atom "Tpat_constant"; constant c]
  | Tpat_tuple l ->
    Sexp.list (Sexp.atom "Tpat_tuple" :: List.map (pattern ~with_locs) l)
  | Tpat_construct (li, _, args) ->
    Sexp.list [
      Sexp.atom "Tpat_construct";
      longident_loc li;
      Sexp.list (List.map (pattern ~with_locs) args);
    ]
  | Tpat_variant (label, arg, _) ->
    Sexp.list [Sexp.atom "Tpat_variant"; string label; opt (pattern ~with_locs) arg]
  | Tpat_record (fields, flag) ->
    Sexp.list [
      Sexp.atom "Tpat_record";
      Sexp.list (List.map (fun (li, _, p, opt) ->
        Sexp.list [longident_loc li; pattern ~with_locs p; Sexp.atom (if opt then "optional" else "required")]
      ) fields);
      closed_flag flag;
    ]
  | Tpat_array l ->
    Sexp.list (Sexp.atom "Tpat_array" :: List.map (pattern ~with_locs) l)
  | Tpat_or (p1, p2, _) ->
    Sexp.list [Sexp.atom "Tpat_or"; pattern ~with_locs p1; pattern ~with_locs p2]

(* Expression *)
and expression ~with_locs e =
  let loc = if with_locs then [location ~with_locs e.exp_loc] else [] in
  let extras = List.map (fun (extra, loc, attrs) ->
    exp_extra ~with_locs extra loc attrs
  ) e.exp_extra in
  let desc = expression_desc ~with_locs e.exp_desc in
  let ty = type_expr e.exp_type in
  let attrs = attributes ~with_locs e.exp_attributes in
  Sexp.list ([Sexp.atom "expression"] @ loc @ extras @ [desc; Sexp.list [Sexp.atom "type"; ty]; attrs])

and exp_extra ~with_locs extra loc attrs =
  let loc_sexp = if with_locs then [location ~with_locs loc] else [] in
  let attrs_sexp = attributes ~with_locs attrs in
  match extra with
  | Texp_constraint ct ->
    Sexp.list ([Sexp.atom "Texp_constraint"; core_type ~with_locs ct] @ loc_sexp @ [attrs_sexp])
  | Texp_coerce ct ->
    Sexp.list ([Sexp.atom "Texp_coerce"; core_type ~with_locs ct] @ loc_sexp @ [attrs_sexp])
  | Texp_open (flag, p, _, _) ->
    Sexp.list ([Sexp.atom "Texp_open"; override_flag flag; path p] @ loc_sexp @ [attrs_sexp])
  | Texp_newtype s ->
    Sexp.list ([Sexp.atom "Texp_newtype"; string s] @ loc_sexp @ [attrs_sexp])

and expression_desc ~with_locs desc =
  match desc with
  | Texp_ident (p, li, _) ->
    Sexp.list [Sexp.atom "Texp_ident"; path p; longident_loc li]
  | Texp_constant c -> Sexp.list [Sexp.atom "Texp_constant"; constant c]
  | Texp_let (flag, vbs, e) ->
    Sexp.list [
      Sexp.atom "Texp_let";
      rec_flag flag;
      Sexp.list (List.map (value_binding ~with_locs) vbs);
      expression ~with_locs e;
    ]
  | Texp_function { arg_label; arity; param; case = c; partial = p; async } ->
    Sexp.list [
      Sexp.atom "Texp_function";
      sexp_arg_label arg_label;
      opt (fun a -> Sexp.atom (string_of_int a)) arity;
      ident param;
      case ~with_locs c;
      partial p;
      Sexp.atom (if async then "async" else "sync");
    ]
  | Texp_apply { funct; args; partial = p; transformed_jsx } ->
    Sexp.list [
      Sexp.atom "Texp_apply";
      expression ~with_locs funct;
      Sexp.list (List.map (fun (lbl, arg) ->
        Sexp.list [sexp_arg_label lbl; opt (expression ~with_locs) arg]
      ) args);
      Sexp.atom (if p then "partial" else "total");
      Sexp.atom (if transformed_jsx then "jsx" else "not_jsx");
    ]
  | Texp_match (e, cases, exn_cases, p) ->
    Sexp.list [
      Sexp.atom "Texp_match";
      expression ~with_locs e;
      Sexp.list (List.map (case ~with_locs) cases);
      Sexp.list (List.map (case ~with_locs) exn_cases);
      partial p;
    ]
  | Texp_try (e, cases) ->
    Sexp.list [
      Sexp.atom "Texp_try";
      expression ~with_locs e;
      Sexp.list (List.map (case ~with_locs) cases);
    ]
  | Texp_tuple l ->
    Sexp.list (Sexp.atom "Texp_tuple" :: List.map (expression ~with_locs) l)
  | Texp_construct (li, _, args) ->
    Sexp.list [
      Sexp.atom "Texp_construct";
      longident_loc li;
      Sexp.list (List.map (expression ~with_locs) args);
    ]
  | Texp_variant (label, arg) ->
    Sexp.list [Sexp.atom "Texp_variant"; string label; opt (expression ~with_locs) arg]
  | Texp_record { fields; representation = _; extended_expression } ->
    Sexp.list [
      Sexp.atom "Texp_record";
      Sexp.list (Array.to_list (Array.map (fun (ld, def, opt) ->
        let def_sexp = match def with
          | Kept ty -> Sexp.list [Sexp.atom "Kept"; type_expr ty]
          | Overridden (li, e) ->
            Sexp.list [Sexp.atom "Overridden"; longident_loc li; expression ~with_locs e]
        in
        Sexp.list [string ld.Types.lbl_name; def_sexp; Sexp.atom (if opt then "optional" else "required")]
      ) fields));
      opt (expression ~with_locs) extended_expression;
    ]
  | Texp_field (e, li, _) ->
    Sexp.list [Sexp.atom "Texp_field"; expression ~with_locs e; longident_loc li]
  | Texp_setfield (e1, li, _, e2) ->
    Sexp.list [Sexp.atom "Texp_setfield"; expression ~with_locs e1; longident_loc li; expression ~with_locs e2]
  | Texp_array l ->
    Sexp.list (Sexp.atom "Texp_array" :: List.map (expression ~with_locs) l)
  | Texp_ifthenelse (e1, e2, e3) ->
    Sexp.list [Sexp.atom "Texp_ifthenelse"; expression ~with_locs e1; expression ~with_locs e2; opt (expression ~with_locs) e3]
  | Texp_sequence (e1, e2) ->
    Sexp.list [Sexp.atom "Texp_sequence"; expression ~with_locs e1; expression ~with_locs e2]
  | Texp_while (e1, e2) ->
    Sexp.list [Sexp.atom "Texp_while"; expression ~with_locs e1; expression ~with_locs e2]
  | Texp_for (id, _, e1, e2, flag, e3) ->
    Sexp.list [Sexp.atom "Texp_for"; ident id; expression ~with_locs e1; expression ~with_locs e2; direction_flag flag; expression ~with_locs e3]
  | Texp_send (e, Tmeth_name name, arg) ->
    Sexp.list [Sexp.atom "Texp_send"; expression ~with_locs e; string name; opt (expression ~with_locs) arg]
  | Texp_letmodule (id, _, me, e) ->
    Sexp.list [Sexp.atom "Texp_letmodule"; ident id; module_expr ~with_locs me; expression ~with_locs e]
  | Texp_letexception (ec, e) ->
    Sexp.list [Sexp.atom "Texp_letexception"; extension_constructor ~with_locs ec; expression ~with_locs e]
  | Texp_assert e ->
    Sexp.list [Sexp.atom "Texp_assert"; expression ~with_locs e]
  | Texp_pack me ->
    Sexp.list [Sexp.atom "Texp_pack"; module_expr ~with_locs me]
  | Texp_extension_constructor (li, p) ->
    Sexp.list [Sexp.atom "Texp_extension_constructor"; longident_loc li; path p]

and case ~with_locs c =
  Sexp.list [
    Sexp.atom "case";
    pattern ~with_locs c.c_lhs;
    opt (expression ~with_locs) c.c_guard;
    expression ~with_locs c.c_rhs;
  ]

and value_binding ~with_locs vb =
  let loc = if with_locs then [location ~with_locs vb.vb_loc] else [] in
  Sexp.list ([
    Sexp.atom "value_binding";
  ] @ loc @ [
    pattern ~with_locs vb.vb_pat;
    expression ~with_locs vb.vb_expr;
    attributes ~with_locs vb.vb_attributes;
  ])

(* Core type *)
and core_type ~with_locs ct =
  let loc = if with_locs then [location ~with_locs ct.ctyp_loc] else [] in
  let desc = core_type_desc ~with_locs ct.ctyp_desc in
  let ty = type_expr ct.ctyp_type in
  let attrs = attributes ~with_locs ct.ctyp_attributes in
  Sexp.list ([Sexp.atom "core_type"] @ loc @ [desc; Sexp.list [Sexp.atom "type"; ty]; attrs])

and core_type_desc ~with_locs desc =
  match desc with
  | Ttyp_any -> Sexp.atom "Ttyp_any"
  | Ttyp_var s -> Sexp.list [Sexp.atom "Ttyp_var"; string s]
  | Ttyp_arrow (arg, ret, arity) ->
    Sexp.list [
      Sexp.atom "Ttyp_arrow";
      sexp_arg_label arg.lbl;
      attributes ~with_locs arg.attrs;
      core_type ~with_locs arg.typ;
      core_type ~with_locs ret;
      opt (fun a -> Sexp.atom (string_of_int a)) arity;
    ]
  | Ttyp_tuple l ->
    Sexp.list (Sexp.atom "Ttyp_tuple" :: List.map (core_type ~with_locs) l)
  | Ttyp_constr (p, _, args) ->
    Sexp.list [Sexp.atom "Ttyp_constr"; path p; Sexp.list (List.map (core_type ~with_locs) args)]
  | Ttyp_object (fields, flag) ->
    Sexp.list [
      Sexp.atom "Ttyp_object";
      Sexp.list (List.map (object_field ~with_locs) fields);
      closed_flag flag;
    ]
  | Ttyp_alias (ct, s) ->
    Sexp.list [Sexp.atom "Ttyp_alias"; core_type ~with_locs ct; string s]
  | Ttyp_variant (fields, flag, labels) ->
    Sexp.list [
      Sexp.atom "Ttyp_variant";
      Sexp.list (List.map (row_field_decl ~with_locs) fields);
      closed_flag flag;
      opt (fun l -> Sexp.list (List.map string l)) labels;
    ]
  | Ttyp_poly (vars, ct) ->
    Sexp.list [Sexp.atom "Ttyp_poly"; Sexp.list (List.map string vars); core_type ~with_locs ct]
  | Ttyp_package pkg ->
    Sexp.list [
      Sexp.atom "Ttyp_package";
      path pkg.pack_path;
      Sexp.list (List.map (fun (li, ct) ->
        Sexp.list [longident_loc li; core_type ~with_locs ct]
      ) pkg.pack_fields);
    ]

and object_field ~with_locs field =
  match field with
  | OTtag (name, attrs, ct) ->
    Sexp.list [Sexp.atom "OTtag"; string name.txt; attributes ~with_locs attrs; core_type ~with_locs ct]
  | OTinherit ct ->
    Sexp.list [Sexp.atom "OTinherit"; core_type ~with_locs ct]

and row_field_decl ~with_locs field =
  match field with
  | Ttag (name, attrs, const, args) ->
    Sexp.list [
      Sexp.atom "Ttag";
      string name.txt;
      attributes ~with_locs attrs;
      Sexp.atom (if const then "const" else "non_const");
      Sexp.list (List.map (core_type ~with_locs) args);
    ]
  | Tinherit ct ->
    Sexp.list [Sexp.atom "Tinherit"; core_type ~with_locs ct]

(* Attributes *)
and attribute ~with_locs (name, payload) =
  Sexp.list [Sexp.atom "attribute"; string name.txt; payload_sexp ~with_locs payload]

and attributes ~with_locs attrs =
  Sexp.list (Sexp.atom "attributes" :: List.map (attribute ~with_locs) attrs)

and payload_sexp ~with_locs:_ p =
  match p with
  | Parsetree.PStr _ -> Sexp.list [Sexp.atom "PStr"; Sexp.atom "<structure>"]
  | Parsetree.PSig _ -> Sexp.list [Sexp.atom "PSig"; Sexp.atom "<signature>"]
  | Parsetree.PTyp _ -> Sexp.list [Sexp.atom "PTyp"; Sexp.atom "<core_type>"]
  | Parsetree.PPat _ -> Sexp.list [Sexp.atom "PPat"; Sexp.atom "<pattern>"]

(* Type declarations *)
and type_declaration ~with_locs td =
  let loc = if with_locs then [location ~with_locs td.typ_loc] else [] in
  Sexp.list ([
    Sexp.atom "type_declaration";
    ident td.typ_id;
    string td.typ_name.txt;
  ] @ loc @ [
    Sexp.list [Sexp.atom "params"; Sexp.list (List.map (fun (ct, v) ->
      Sexp.list [core_type ~with_locs ct; variance v]
    ) td.typ_params)];
    Sexp.list [Sexp.atom "cstrs"; Sexp.list (List.map (fun (ct1, ct2, _loc) ->
      Sexp.list [core_type ~with_locs ct1; core_type ~with_locs ct2]
    ) td.typ_cstrs)];
    Sexp.list [Sexp.atom "kind"; type_kind ~with_locs td.typ_kind];
    Sexp.list [Sexp.atom "private"; private_flag td.typ_private];
    Sexp.list [Sexp.atom "manifest"; opt (core_type ~with_locs) td.typ_manifest];
    attributes ~with_locs td.typ_attributes;
  ])

and type_kind ~with_locs kind =
  match kind with
  | Ttype_abstract -> Sexp.atom "Ttype_abstract"
  | Ttype_variant cds ->
    Sexp.list (Sexp.atom "Ttype_variant" :: List.map (constructor_declaration ~with_locs) cds)
  | Ttype_record lds ->
    Sexp.list (Sexp.atom "Ttype_record" :: List.map (label_declaration ~with_locs) lds)
  | Ttype_open -> Sexp.atom "Ttype_open"

and constructor_declaration ~with_locs cd =
  let loc = if with_locs then [location ~with_locs cd.cd_loc] else [] in
  Sexp.list ([
    Sexp.atom "constructor_declaration";
    ident cd.cd_id;
    string cd.cd_name.txt;
  ] @ loc @ [
    constructor_arguments ~with_locs cd.cd_args;
    opt (core_type ~with_locs) cd.cd_res;
    attributes ~with_locs cd.cd_attributes;
  ])

and constructor_arguments ~with_locs args =
  match args with
  | Cstr_tuple cts ->
    Sexp.list (Sexp.atom "Cstr_tuple" :: List.map (core_type ~with_locs) cts)
  | Cstr_record lds ->
    Sexp.list (Sexp.atom "Cstr_record" :: List.map (label_declaration ~with_locs) lds)

and label_declaration ~with_locs ld =
  let loc = if with_locs then [location ~with_locs ld.ld_loc] else [] in
  Sexp.list ([
    Sexp.atom "label_declaration";
    ident ld.ld_id;
    string ld.ld_name.txt;
  ] @ loc @ [
    mutable_flag ld.ld_mutable;
    Sexp.atom (if ld.ld_optional then "optional" else "required");
    core_type ~with_locs ld.ld_type;
    attributes ~with_locs ld.ld_attributes;
  ])

(* Type extension *)
and type_extension ~with_locs te =
  Sexp.list [
    Sexp.atom "type_extension";
    path te.tyext_path;
    longident_loc te.tyext_txt;
    Sexp.list [Sexp.atom "params"; Sexp.list (List.map (fun (ct, v) ->
      Sexp.list [core_type ~with_locs ct; variance v]
    ) te.tyext_params)];
    Sexp.list [Sexp.atom "constructors"; Sexp.list (List.map (extension_constructor ~with_locs) te.tyext_constructors)];
    private_flag te.tyext_private;
    attributes ~with_locs te.tyext_attributes;
  ]

and extension_constructor ~with_locs ec =
  let loc = if with_locs then [location ~with_locs ec.ext_loc] else [] in
  Sexp.list ([
    Sexp.atom "extension_constructor";
    ident ec.ext_id;
    string ec.ext_name.txt;
  ] @ loc @ [
    extension_constructor_kind ~with_locs ec.ext_kind;
    attributes ~with_locs ec.ext_attributes;
  ])

and extension_constructor_kind ~with_locs kind =
  match kind with
  | Text_decl (args, ret) ->
    Sexp.list [Sexp.atom "Text_decl"; constructor_arguments ~with_locs args; opt (core_type ~with_locs) ret]
  | Text_rebind (p, _) ->
    Sexp.list [Sexp.atom "Text_rebind"; path p]

(* Value description *)
and value_description ~with_locs vd =
  let loc = if with_locs then [location ~with_locs vd.val_loc] else [] in
  Sexp.list ([
    Sexp.atom "value_description";
    ident vd.val_id;
    string vd.val_name.txt;
  ] @ loc @ [
    core_type ~with_locs vd.val_desc;
    Sexp.list (Sexp.atom "prim" :: List.map string vd.val_prim);
    attributes ~with_locs vd.val_attributes;
  ])

(* Module types *)
and module_type ~with_locs mt =
  let loc = if with_locs then [location ~with_locs mt.mty_loc] else [] in
  let desc = module_type_desc ~with_locs mt.mty_desc in
  let attrs = attributes ~with_locs mt.mty_attributes in
  Sexp.list ([Sexp.atom "module_type"] @ loc @ [desc; attrs])

and module_type_desc ~with_locs desc =
  match desc with
  | Tmty_ident (p, _) -> Sexp.list [Sexp.atom "Tmty_ident"; path p]
  | Tmty_signature sg -> Sexp.list [Sexp.atom "Tmty_signature"; signature ~with_locs sg]
  | Tmty_functor (id, _, mt_arg, mt_res) ->
    Sexp.list [Sexp.atom "Tmty_functor"; ident id; opt (module_type ~with_locs) mt_arg; module_type ~with_locs mt_res]
  | Tmty_with (mt, constraints) ->
    Sexp.list [
      Sexp.atom "Tmty_with";
      module_type ~with_locs mt;
      Sexp.list (List.map (fun (p, _, wc) ->
        Sexp.list [path p; with_constraint ~with_locs wc]
      ) constraints);
    ]
  | Tmty_typeof me -> Sexp.list [Sexp.atom "Tmty_typeof"; module_expr ~with_locs me]
  | Tmty_alias (p, _) -> Sexp.list [Sexp.atom "Tmty_alias"; path p]

and with_constraint ~with_locs wc =
  match wc with
  | Twith_type td -> Sexp.list [Sexp.atom "Twith_type"; type_declaration ~with_locs td]
  | Twith_module (p, _) -> Sexp.list [Sexp.atom "Twith_module"; path p]
  | Twith_typesubst td -> Sexp.list [Sexp.atom "Twith_typesubst"; type_declaration ~with_locs td]
  | Twith_modsubst (p, _) -> Sexp.list [Sexp.atom "Twith_modsubst"; path p]

(* Signature *)
and signature ~with_locs sg =
  Sexp.list (Sexp.atom "signature" :: List.map (signature_item ~with_locs) sg.sig_items)

and signature_item ~with_locs si =
  let loc = if with_locs then [location ~with_locs si.sig_loc] else [] in
  let desc = signature_item_desc ~with_locs si.sig_desc in
  Sexp.list ([Sexp.atom "signature_item"] @ loc @ [desc])

and signature_item_desc ~with_locs desc =
  match desc with
  | Tsig_value vd -> Sexp.list [Sexp.atom "Tsig_value"; value_description ~with_locs vd]
  | Tsig_type (flag, tds) ->
    Sexp.list [Sexp.atom "Tsig_type"; rec_flag flag; Sexp.list (List.map (type_declaration ~with_locs) tds)]
  | Tsig_typext te -> Sexp.list [Sexp.atom "Tsig_typext"; type_extension ~with_locs te]
  | Tsig_exception ec -> Sexp.list [Sexp.atom "Tsig_exception"; extension_constructor ~with_locs ec]
  | Tsig_module md -> Sexp.list [Sexp.atom "Tsig_module"; module_declaration ~with_locs md]
  | Tsig_recmodule mds ->
    Sexp.list [Sexp.atom "Tsig_recmodule"; Sexp.list (List.map (module_declaration ~with_locs) mds)]
  | Tsig_modtype mtd -> Sexp.list [Sexp.atom "Tsig_modtype"; module_type_declaration ~with_locs mtd]
  | Tsig_open od -> Sexp.list [Sexp.atom "Tsig_open"; open_description ~with_locs od]
  | Tsig_include incl -> Sexp.list [Sexp.atom "Tsig_include"; include_description ~with_locs incl]
  | Tsig_attribute attr -> Sexp.list [Sexp.atom "Tsig_attribute"; attribute ~with_locs attr]

and module_declaration ~with_locs md =
  let loc = if with_locs then [location ~with_locs md.md_loc] else [] in
  Sexp.list ([
    Sexp.atom "module_declaration";
    ident md.md_id;
    string md.md_name.txt;
  ] @ loc @ [
    module_type ~with_locs md.md_type;
    attributes ~with_locs md.md_attributes;
  ])

and module_type_declaration ~with_locs mtd =
  let loc = if with_locs then [location ~with_locs mtd.mtd_loc] else [] in
  Sexp.list ([
    Sexp.atom "module_type_declaration";
    ident mtd.mtd_id;
    string mtd.mtd_name.txt;
  ] @ loc @ [
    opt (module_type ~with_locs) mtd.mtd_type;
    attributes ~with_locs mtd.mtd_attributes;
  ])

and open_description ~with_locs od =
  let loc = if with_locs then [location ~with_locs od.open_loc] else [] in
  Sexp.list ([
    Sexp.atom "open_description";
    path od.open_path;
    longident_loc od.open_txt;
  ] @ loc @ [
    override_flag od.open_override;
    attributes ~with_locs od.open_attributes;
  ])

and include_description ~with_locs incl =
  let loc = if with_locs then [location ~with_locs incl.incl_loc] else [] in
  Sexp.list ([
    Sexp.atom "include_description";
  ] @ loc @ [
    module_type ~with_locs incl.incl_mod;
    attributes ~with_locs incl.incl_attributes;
  ])

(* Module expression *)
and module_expr ~with_locs me =
  let loc = if with_locs then [location ~with_locs me.mod_loc] else [] in
  let desc = module_expr_desc ~with_locs me.mod_desc in
  let attrs = attributes ~with_locs me.mod_attributes in
  Sexp.list ([Sexp.atom "module_expr"] @ loc @ [desc; attrs])

and module_expr_desc ~with_locs desc =
  match desc with
  | Tmod_ident (p, _) -> Sexp.list [Sexp.atom "Tmod_ident"; path p]
  | Tmod_structure str -> Sexp.list [Sexp.atom "Tmod_structure"; structure ~with_locs str]
  | Tmod_functor (id, _, mt_arg, me) ->
    Sexp.list [Sexp.atom "Tmod_functor"; ident id; opt (module_type ~with_locs) mt_arg; module_expr ~with_locs me]
  | Tmod_apply (me1, me2, _) ->
    Sexp.list [Sexp.atom "Tmod_apply"; module_expr ~with_locs me1; module_expr ~with_locs me2]
  | Tmod_constraint (me, _, constraint_, _) ->
    let constraint_sexp = match constraint_ with
      | Tmodtype_implicit -> Sexp.atom "implicit"
      | Tmodtype_explicit mt -> Sexp.list [Sexp.atom "explicit"; module_type ~with_locs mt]
    in
    Sexp.list [Sexp.atom "Tmod_constraint"; module_expr ~with_locs me; constraint_sexp]
  | Tmod_unpack (e, _) -> Sexp.list [Sexp.atom "Tmod_unpack"; expression ~with_locs e]

(* Structure *)
and structure ~with_locs str =
  Sexp.list (Sexp.atom "structure" :: List.map (structure_item ~with_locs) str.str_items)

and structure_item ~with_locs si =
  let loc = if with_locs then [location ~with_locs si.str_loc] else [] in
  let desc = structure_item_desc ~with_locs si.str_desc in
  Sexp.list ([Sexp.atom "structure_item"] @ loc @ [desc])

and structure_item_desc ~with_locs desc =
  match desc with
  | Tstr_eval (e, attrs) ->
    Sexp.list [Sexp.atom "Tstr_eval"; expression ~with_locs e; attributes ~with_locs attrs]
  | Tstr_value (flag, vbs) ->
    Sexp.list [Sexp.atom "Tstr_value"; rec_flag flag; Sexp.list (List.map (value_binding ~with_locs) vbs)]
  | Tstr_primitive vd -> Sexp.list [Sexp.atom "Tstr_primitive"; value_description ~with_locs vd]
  | Tstr_type (flag, tds) ->
    Sexp.list [Sexp.atom "Tstr_type"; rec_flag flag; Sexp.list (List.map (type_declaration ~with_locs) tds)]
  | Tstr_typext te -> Sexp.list [Sexp.atom "Tstr_typext"; type_extension ~with_locs te]
  | Tstr_exception ec -> Sexp.list [Sexp.atom "Tstr_exception"; extension_constructor ~with_locs ec]
  | Tstr_module mb -> Sexp.list [Sexp.atom "Tstr_module"; module_binding ~with_locs mb]
  | Tstr_recmodule mbs ->
    Sexp.list [Sexp.atom "Tstr_recmodule"; Sexp.list (List.map (module_binding ~with_locs) mbs)]
  | Tstr_modtype mtd -> Sexp.list [Sexp.atom "Tstr_modtype"; module_type_declaration ~with_locs mtd]
  | Tstr_open od -> Sexp.list [Sexp.atom "Tstr_open"; open_description ~with_locs od]
  | Tstr_include incl -> Sexp.list [Sexp.atom "Tstr_include"; include_declaration ~with_locs incl]
  | Tstr_attribute attr -> Sexp.list [Sexp.atom "Tstr_attribute"; attribute ~with_locs attr]

and module_binding ~with_locs mb =
  let loc = if with_locs then [location ~with_locs mb.mb_loc] else [] in
  Sexp.list ([
    Sexp.atom "module_binding";
    ident mb.mb_id;
    string mb.mb_name.txt;
  ] @ loc @ [
    module_expr ~with_locs mb.mb_expr;
    attributes ~with_locs mb.mb_attributes;
  ])

and include_declaration ~with_locs incl =
  let loc = if with_locs then [location ~with_locs incl.incl_loc] else [] in
  Sexp.list ([
    Sexp.atom "include_declaration";
  ] @ loc @ [
    module_expr ~with_locs incl.incl_mod;
    attributes ~with_locs incl.incl_attributes;
  ])

(* Public API *)
let print_typed_structure ppf str =
  let sexp = structure ~with_locs:false str in
  Format.fprintf ppf "%s" (Sexp.to_string sexp)

let print_typed_structure_with_locs ppf str =
  let sexp = structure ~with_locs:true str in
  Format.fprintf ppf "%s" (Sexp.to_string sexp)

let print_typed_signature ppf sg =
  let sexp = signature ~with_locs:false sg in
  Format.fprintf ppf "%s" (Sexp.to_string sexp)

let print_typed_signature_with_locs ppf sg =
  let sexp = signature ~with_locs:true sg in
  Format.fprintf ppf "%s" (Sexp.to_string sexp)
