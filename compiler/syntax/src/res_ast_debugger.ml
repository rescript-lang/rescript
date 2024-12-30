module Doc = Res_doc
module CommentTable = Res_comments_table

let print_engine =
  Res_driver.
    {
      print_implementation =
        (fun ~width:_ ~filename:_ ~comments:_ structure ->
          Printast.implementation Format.std_formatter structure);
      print_interface =
        (fun ~width:_ ~filename:_ ~comments:_ signature ->
          Printast.interface Format.std_formatter signature);
    }

module Sexp : sig
  type t

  val atom : string -> t
  val list : t list -> t
  val to_string : t -> string
end = struct
  type t = Atom of string | List of t list

  let atom s = Atom s
  let list l = List l

  let rec to_doc t =
    match t with
    | Atom s -> Doc.text s
    | List [] -> Doc.text "()"
    | List [sexpr] -> Doc.concat [Doc.lparen; to_doc sexpr; Doc.rparen]
    | List (hd :: tail) ->
      Doc.group
        (Doc.concat
           [
             Doc.lparen;
             to_doc hd;
             Doc.indent
               (Doc.concat
                  [Doc.line; Doc.join ~sep:Doc.line (List.map to_doc tail)]);
             Doc.rparen;
           ])

  let to_string sexpr =
    let doc = to_doc sexpr in
    Doc.to_string ~width:80 doc
end

module SexpAst = struct
  open Parsetree

  let map_empty ~f items =
    match items with
    | [] -> [Sexp.list []]
    | items -> List.map f items

  let string txt =
    Sexp.atom ("\"" ^ Ext_ident.unwrap_uppercase_exotic txt ^ "\"")

  let char c = Sexp.atom ("'" ^ Char.escaped c ^ "'")

  let opt_char oc =
    match oc with
    | None -> Sexp.atom "None"
    | Some c -> Sexp.list [Sexp.atom "Some"; char c]

  let longident l =
    let rec loop l =
      match l with
      | Longident.Lident ident -> Sexp.list [Sexp.atom "Lident"; string ident]
      | Longident.Ldot (lident, txt) ->
        Sexp.list [Sexp.atom "Ldot"; loop lident; string txt]
      | Longident.Lapply (l1, l2) ->
        Sexp.list [Sexp.atom "Lapply"; loop l1; loop l2]
    in
    Sexp.list [Sexp.atom "longident"; loop l]

  let closed_flag flag =
    match flag with
    | Asttypes.Closed -> Sexp.atom "Closed"
    | Open -> Sexp.atom "Open"

  let direction_flag flag =
    match flag with
    | Asttypes.Upto -> Sexp.atom "Upto"
    | Downto -> Sexp.atom "Downto"

  let rec_flag flag =
    match flag with
    | Asttypes.Recursive -> Sexp.atom "Recursive"
    | Nonrecursive -> Sexp.atom "Nonrecursive"

  let override_flag flag =
    match flag with
    | Asttypes.Override -> Sexp.atom "Override"
    | Fresh -> Sexp.atom "Fresh"

  let private_flag flag =
    match flag with
    | Asttypes.Public -> Sexp.atom "Public"
    | Private -> Sexp.atom "Private"

  let mutable_flag flag =
    match flag with
    | Asttypes.Immutable -> Sexp.atom "Immutable"
    | Mutable -> Sexp.atom "Mutable"

  let variance v =
    match v with
    | Asttypes.Covariant -> Sexp.atom "Covariant"
    | Contravariant -> Sexp.atom "Contravariant"
    | Invariant -> Sexp.atom "Invariant"

  let arg_label lbl =
    match lbl with
    | Asttypes.Nolabel -> Sexp.atom "Nolabel"
    | Labelled txt -> Sexp.list [Sexp.atom "Labelled"; string txt]
    | Optional txt -> Sexp.list [Sexp.atom "Optional"; string txt]

  let constant c =
    let sexpr =
      match c with
      | Pconst_integer (txt, tag) ->
        Sexp.list [Sexp.atom "Pconst_integer"; string txt; opt_char tag]
      | Pconst_char _ -> Sexp.list [Sexp.atom "Pconst_char"]
      | Pconst_string (_, Some "INTERNAL_RES_CHAR_CONTENTS") ->
        Sexp.list [Sexp.atom "Pconst_char"]
      | Pconst_string (txt, tag) ->
        Sexp.list
          [
            Sexp.atom "Pconst_string";
            string txt;
            (match tag with
            | Some txt -> Sexp.list [Sexp.atom "Some"; string txt]
            | None -> Sexp.atom "None");
          ]
      | Pconst_float (txt, tag) ->
        Sexp.list [Sexp.atom "Pconst_float"; string txt; opt_char tag]
    in
    Sexp.list [Sexp.atom "constant"; sexpr]

  let rec structure s =
    Sexp.list (Sexp.atom "structure" :: List.map structure_item s)

  and structure_item si =
    let desc =
      match si.pstr_desc with
      | Pstr_eval (expr, attrs) ->
        Sexp.list [Sexp.atom "Pstr_eval"; expression expr; attributes attrs]
      | Pstr_value (flag, vbs) ->
        Sexp.list
          [
            Sexp.atom "Pstr_value";
            rec_flag flag;
            Sexp.list (map_empty ~f:value_binding vbs);
          ]
      | Pstr_primitive vd ->
        Sexp.list [Sexp.atom "Pstr_primitive"; value_description vd]
      | Pstr_type (flag, tds) ->
        Sexp.list
          [
            Sexp.atom "Pstr_type";
            rec_flag flag;
            Sexp.list (map_empty ~f:type_declaration tds);
          ]
      | Pstr_typext typext ->
        Sexp.list [Sexp.atom "Pstr_type"; type_extension typext]
      | Pstr_exception ec ->
        Sexp.list [Sexp.atom "Pstr_exception"; extension_constructor ec]
      | Pstr_module mb -> Sexp.list [Sexp.atom "Pstr_module"; module_binding mb]
      | Pstr_recmodule mbs ->
        Sexp.list
          [
            Sexp.atom "Pstr_recmodule";
            Sexp.list (map_empty ~f:module_binding mbs);
          ]
      | Pstr_modtype mod_typ_decl ->
        Sexp.list
          [Sexp.atom "Pstr_modtype"; module_type_declaration mod_typ_decl]
      | Pstr_open open_desc ->
        Sexp.list [Sexp.atom "Pstr_open"; open_description open_desc]
      | Pstr_class _ -> Sexp.atom "Pstr_class"
      | Pstr_class_type _ -> Sexp.atom "Pstr_class_type"
      | Pstr_include id ->
        Sexp.list [Sexp.atom "Pstr_include"; include_declaration id]
      | Pstr_attribute attr ->
        Sexp.list [Sexp.atom "Pstr_attribute"; attribute attr]
      | Pstr_extension (ext, attrs) ->
        Sexp.list [Sexp.atom "Pstr_extension"; extension ext; attributes attrs]
    in
    Sexp.list [Sexp.atom "structure_item"; desc]

  and include_declaration id =
    Sexp.list
      [
        Sexp.atom "include_declaration";
        module_expression id.pincl_mod;
        attributes id.pincl_attributes;
      ]

  and open_description od =
    Sexp.list
      [
        Sexp.atom "open_description";
        longident od.popen_lid.Asttypes.txt;
        attributes od.popen_attributes;
      ]

  and module_type_declaration mtd =
    Sexp.list
      [
        Sexp.atom "module_type_declaration";
        string mtd.pmtd_name.Asttypes.txt;
        (match mtd.pmtd_type with
        | None -> Sexp.atom "None"
        | Some mod_type -> Sexp.list [Sexp.atom "Some"; module_type mod_type]);
        attributes mtd.pmtd_attributes;
      ]

  and module_binding mb =
    Sexp.list
      [
        Sexp.atom "module_binding";
        string mb.pmb_name.Asttypes.txt;
        module_expression mb.pmb_expr;
        attributes mb.pmb_attributes;
      ]

  and module_expression me =
    let desc =
      match me.pmod_desc with
      | Pmod_ident mod_name ->
        Sexp.list [Sexp.atom "Pmod_ident"; longident mod_name.Asttypes.txt]
      | Pmod_structure s -> Sexp.list [Sexp.atom "Pmod_structure"; structure s]
      | Pmod_functor (lbl, opt_mod_type, mod_expr) ->
        Sexp.list
          [
            Sexp.atom "Pmod_functor";
            string lbl.Asttypes.txt;
            (match opt_mod_type with
            | None -> Sexp.atom "None"
            | Some mod_type ->
              Sexp.list [Sexp.atom "Some"; module_type mod_type]);
            module_expression mod_expr;
          ]
      | Pmod_apply (call_mod_expr, mod_expr_arg) ->
        Sexp.list
          [
            Sexp.atom "Pmod_apply";
            module_expression call_mod_expr;
            module_expression mod_expr_arg;
          ]
      | Pmod_constraint (mod_expr, mod_type) ->
        Sexp.list
          [
            Sexp.atom "Pmod_constraint";
            module_expression mod_expr;
            module_type mod_type;
          ]
      | Pmod_unpack expr -> Sexp.list [Sexp.atom "Pmod_unpack"; expression expr]
      | Pmod_extension ext ->
        Sexp.list [Sexp.atom "Pmod_extension"; extension ext]
    in
    Sexp.list [Sexp.atom "module_expr"; desc; attributes me.pmod_attributes]

  and module_type mt =
    let desc =
      match mt.pmty_desc with
      | Pmty_ident longident_loc ->
        Sexp.list [Sexp.atom "Pmty_ident"; longident longident_loc.Asttypes.txt]
      | Pmty_signature s -> Sexp.list [Sexp.atom "Pmty_signature"; signature s]
      | Pmty_functor (lbl, opt_mod_type, mod_type) ->
        Sexp.list
          [
            Sexp.atom "Pmty_functor";
            string lbl.Asttypes.txt;
            (match opt_mod_type with
            | None -> Sexp.atom "None"
            | Some mod_type ->
              Sexp.list [Sexp.atom "Some"; module_type mod_type]);
            module_type mod_type;
          ]
      | Pmty_alias longident_loc ->
        Sexp.list [Sexp.atom "Pmty_alias"; longident longident_loc.Asttypes.txt]
      | Pmty_extension ext ->
        Sexp.list [Sexp.atom "Pmty_extension"; extension ext]
      | Pmty_typeof mod_expr ->
        Sexp.list [Sexp.atom "Pmty_typeof"; module_expression mod_expr]
      | Pmty_with (mod_type, with_constraints) ->
        Sexp.list
          [
            Sexp.atom "Pmty_with";
            module_type mod_type;
            Sexp.list (map_empty ~f:with_constraint with_constraints);
          ]
    in
    Sexp.list [Sexp.atom "module_type"; desc; attributes mt.pmty_attributes]

  and with_constraint wc =
    match wc with
    | Pwith_type (longident_loc, td) ->
      Sexp.list
        [
          Sexp.atom "Pmty_with";
          longident longident_loc.Asttypes.txt;
          type_declaration td;
        ]
    | Pwith_module (l1, l2) ->
      Sexp.list
        [
          Sexp.atom "Pwith_module";
          longident l1.Asttypes.txt;
          longident l2.Asttypes.txt;
        ]
    | Pwith_typesubst (longident_loc, td) ->
      Sexp.list
        [
          Sexp.atom "Pwith_typesubst";
          longident longident_loc.Asttypes.txt;
          type_declaration td;
        ]
    | Pwith_modsubst (l1, l2) ->
      Sexp.list
        [
          Sexp.atom "Pwith_modsubst";
          longident l1.Asttypes.txt;
          longident l2.Asttypes.txt;
        ]

  and signature s =
    Sexp.list (Sexp.atom "signature" :: List.map signature_item s)

  and signature_item si =
    let descr =
      match si.psig_desc with
      | Psig_value vd ->
        Sexp.list [Sexp.atom "Psig_value"; value_description vd]
      | Psig_type (flag, type_declarations) ->
        Sexp.list
          [
            Sexp.atom "Psig_type";
            rec_flag flag;
            Sexp.list (map_empty ~f:type_declaration type_declarations);
          ]
      | Psig_typext typ_ext ->
        Sexp.list [Sexp.atom "Psig_typext"; type_extension typ_ext]
      | Psig_exception ext_constr ->
        Sexp.list [Sexp.atom "Psig_exception"; extension_constructor ext_constr]
      | Psig_module mod_decl ->
        Sexp.list [Sexp.atom "Psig_module"; module_declaration mod_decl]
      | Psig_recmodule mod_decls ->
        Sexp.list
          [
            Sexp.atom "Psig_recmodule";
            Sexp.list (map_empty ~f:module_declaration mod_decls);
          ]
      | Psig_modtype mod_typ_decl ->
        Sexp.list
          [Sexp.atom "Psig_modtype"; module_type_declaration mod_typ_decl]
      | Psig_open open_desc ->
        Sexp.list [Sexp.atom "Psig_open"; open_description open_desc]
      | Psig_include incl_decl ->
        Sexp.list [Sexp.atom "Psig_include"; include_description incl_decl]
      | Psig_class _ -> Sexp.list [Sexp.atom "Psig_class"]
      | Psig_class_type _ -> Sexp.list [Sexp.atom "Psig_class_type"]
      | Psig_attribute attr ->
        Sexp.list [Sexp.atom "Psig_attribute"; attribute attr]
      | Psig_extension (ext, attrs) ->
        Sexp.list [Sexp.atom "Psig_extension"; extension ext; attributes attrs]
    in
    Sexp.list [Sexp.atom "signature_item"; descr]

  and include_description id =
    Sexp.list
      [
        Sexp.atom "include_description";
        module_type id.pincl_mod;
        attributes id.pincl_attributes;
      ]

  and module_declaration md =
    Sexp.list
      [
        Sexp.atom "module_declaration";
        string md.pmd_name.Asttypes.txt;
        module_type md.pmd_type;
        attributes md.pmd_attributes;
      ]

  and value_binding vb =
    Sexp.list
      [
        Sexp.atom "value_binding";
        pattern vb.pvb_pat;
        expression vb.pvb_expr;
        attributes vb.pvb_attributes;
      ]

  and value_description vd =
    Sexp.list
      [
        Sexp.atom "value_description";
        string vd.pval_name.Asttypes.txt;
        core_type vd.pval_type;
        Sexp.list (map_empty ~f:string vd.pval_prim);
        attributes vd.pval_attributes;
      ]

  and type_declaration td =
    Sexp.list
      [
        Sexp.atom "type_declaration";
        string td.ptype_name.Asttypes.txt;
        Sexp.list
          [
            Sexp.atom "ptype_params";
            Sexp.list
              (map_empty
                 ~f:(fun (typexpr, var) ->
                   Sexp.list [core_type typexpr; variance var])
                 td.ptype_params);
          ];
        Sexp.list
          [
            Sexp.atom "ptype_cstrs";
            Sexp.list
              (map_empty
                 ~f:(fun (typ1, typ2, _loc) ->
                   Sexp.list [core_type typ1; core_type typ2])
                 td.ptype_cstrs);
          ];
        Sexp.list [Sexp.atom "ptype_kind"; type_kind td.ptype_kind];
        Sexp.list
          [
            Sexp.atom "ptype_manifest";
            (match td.ptype_manifest with
            | None -> Sexp.atom "None"
            | Some typ -> Sexp.list [Sexp.atom "Some"; core_type typ]);
          ];
        Sexp.list [Sexp.atom "ptype_private"; private_flag td.ptype_private];
        attributes td.ptype_attributes;
      ]

  and extension_constructor ec =
    Sexp.list
      [
        Sexp.atom "extension_constructor";
        string ec.pext_name.Asttypes.txt;
        extension_constructor_kind ec.pext_kind;
        attributes ec.pext_attributes;
      ]

  and extension_constructor_kind kind =
    match kind with
    | Pext_decl (args, opt_typ_expr) ->
      Sexp.list
        [
          Sexp.atom "Pext_decl";
          constructor_arguments args;
          (match opt_typ_expr with
          | None -> Sexp.atom "None"
          | Some typ -> Sexp.list [Sexp.atom "Some"; core_type typ]);
        ]
    | Pext_rebind longident_loc ->
      Sexp.list [Sexp.atom "Pext_rebind"; longident longident_loc.Asttypes.txt]

  and type_extension te =
    Sexp.list
      [
        Sexp.atom "type_extension";
        Sexp.list
          [Sexp.atom "ptyext_path"; longident te.ptyext_path.Asttypes.txt];
        Sexp.list
          [
            Sexp.atom "ptyext_parms";
            Sexp.list
              (map_empty
                 ~f:(fun (typexpr, var) ->
                   Sexp.list [core_type typexpr; variance var])
                 te.ptyext_params);
          ];
        Sexp.list
          [
            Sexp.atom "ptyext_constructors";
            Sexp.list
              (map_empty ~f:extension_constructor te.ptyext_constructors);
          ];
        Sexp.list [Sexp.atom "ptyext_private"; private_flag te.ptyext_private];
        attributes te.ptyext_attributes;
      ]

  and type_kind kind =
    match kind with
    | Ptype_abstract -> Sexp.atom "Ptype_abstract"
    | Ptype_variant constr_decls ->
      Sexp.list
        [
          Sexp.atom "Ptype_variant";
          Sexp.list (map_empty ~f:constructor_declaration constr_decls);
        ]
    | Ptype_record lbl_decls ->
      Sexp.list
        [
          Sexp.atom "Ptype_record";
          Sexp.list (map_empty ~f:label_declaration lbl_decls);
        ]
    | Ptype_open -> Sexp.atom "Ptype_open"

  and constructor_declaration cd =
    Sexp.list
      [
        Sexp.atom "constructor_declaration";
        string cd.pcd_name.Asttypes.txt;
        Sexp.list [Sexp.atom "pcd_args"; constructor_arguments cd.pcd_args];
        Sexp.list
          [
            Sexp.atom "pcd_res";
            (match cd.pcd_res with
            | None -> Sexp.atom "None"
            | Some typ -> Sexp.list [Sexp.atom "Some"; core_type typ]);
          ];
        attributes cd.pcd_attributes;
      ]

  and constructor_arguments args =
    match args with
    | Pcstr_tuple types ->
      Sexp.list
        [Sexp.atom "Pcstr_tuple"; Sexp.list (map_empty ~f:core_type types)]
    | Pcstr_record lds ->
      Sexp.list
        [
          Sexp.atom "Pcstr_record";
          Sexp.list (map_empty ~f:label_declaration lds);
        ]

  and label_declaration ld =
    Sexp.list
      [
        Sexp.atom "label_declaration";
        string ld.pld_name.Asttypes.txt;
        mutable_flag ld.pld_mutable;
        core_type ld.pld_type;
        attributes ld.pld_attributes;
      ]

  and expression expr =
    let desc =
      match expr.pexp_desc with
      | Pexp_ident longident_loc ->
        Sexp.list [Sexp.atom "Pexp_ident"; longident longident_loc.Asttypes.txt]
      | Pexp_constant c -> Sexp.list [Sexp.atom "Pexp_constant"; constant c]
      | Pexp_let (flag, vbs, expr) ->
        Sexp.list
          [
            Sexp.atom "Pexp_let";
            rec_flag flag;
            Sexp.list (map_empty ~f:value_binding vbs);
            expression expr;
          ]
      | Pexp_fun
          {arg_label = arg_lbl; default = expr_opt; lhs = pat; rhs = expr} ->
        Sexp.list
          [
            Sexp.atom "Pexp_fun";
            arg_label arg_lbl;
            (match expr_opt with
            | None -> Sexp.atom "None"
            | Some expr -> Sexp.list [Sexp.atom "Some"; expression expr]);
            pattern pat;
            expression expr;
          ]
      | Pexp_apply (expr, args) ->
        Sexp.list
          [
            Sexp.atom "Pexp_apply";
            expression expr;
            Sexp.list
              (map_empty
                 ~f:(fun (arg_lbl, expr) ->
                   Sexp.list [arg_label arg_lbl; expression expr])
                 args);
          ]
      | Pexp_match (expr, cases) ->
        Sexp.list
          [
            Sexp.atom "Pexp_match";
            expression expr;
            Sexp.list (map_empty ~f:case cases);
          ]
      | Pexp_try (expr, cases) ->
        Sexp.list
          [
            Sexp.atom "Pexp_try";
            expression expr;
            Sexp.list (map_empty ~f:case cases);
          ]
      | Pexp_tuple exprs ->
        Sexp.list
          [Sexp.atom "Pexp_tuple"; Sexp.list (map_empty ~f:expression exprs)]
      | Pexp_construct (longident_loc, expr_opt) ->
        Sexp.list
          [
            Sexp.atom "Pexp_construct";
            longident longident_loc.Asttypes.txt;
            (match expr_opt with
            | None -> Sexp.atom "None"
            | Some expr -> Sexp.list [Sexp.atom "Some"; expression expr]);
          ]
      | Pexp_variant (lbl, expr_opt) ->
        Sexp.list
          [
            Sexp.atom "Pexp_variant";
            string lbl;
            (match expr_opt with
            | None -> Sexp.atom "None"
            | Some expr -> Sexp.list [Sexp.atom "Some"; expression expr]);
          ]
      | Pexp_record (rows, opt_expr) ->
        Sexp.list
          [
            Sexp.atom "Pexp_record";
            Sexp.list
              (map_empty
                 ~f:(fun (longident_loc, expr, _) ->
                   Sexp.list
                     [longident longident_loc.Asttypes.txt; expression expr])
                 rows);
            (match opt_expr with
            | None -> Sexp.atom "None"
            | Some expr -> Sexp.list [Sexp.atom "Some"; expression expr]);
          ]
      | Pexp_field (expr, longident_loc) ->
        Sexp.list
          [
            Sexp.atom "Pexp_field";
            expression expr;
            longident longident_loc.Asttypes.txt;
          ]
      | Pexp_setfield (expr1, longident_loc, expr2) ->
        Sexp.list
          [
            Sexp.atom "Pexp_setfield";
            expression expr1;
            longident longident_loc.Asttypes.txt;
            expression expr2;
          ]
      | Pexp_array exprs ->
        Sexp.list
          [Sexp.atom "Pexp_array"; Sexp.list (map_empty ~f:expression exprs)]
      | Pexp_ifthenelse (expr1, expr2, opt_expr) ->
        Sexp.list
          [
            Sexp.atom "Pexp_ifthenelse";
            expression expr1;
            expression expr2;
            (match opt_expr with
            | None -> Sexp.atom "None"
            | Some expr -> Sexp.list [Sexp.atom "Some"; expression expr]);
          ]
      | Pexp_sequence (expr1, expr2) ->
        Sexp.list
          [Sexp.atom "Pexp_sequence"; expression expr1; expression expr2]
      | Pexp_while (expr1, expr2) ->
        Sexp.list [Sexp.atom "Pexp_while"; expression expr1; expression expr2]
      | Pexp_for (pat, e1, e2, flag, e3) ->
        Sexp.list
          [
            Sexp.atom "Pexp_for";
            pattern pat;
            expression e1;
            expression e2;
            direction_flag flag;
            expression e3;
          ]
      | Pexp_constraint (expr, typexpr) ->
        Sexp.list
          [Sexp.atom "Pexp_constraint"; expression expr; core_type typexpr]
      | Pexp_coerce (expr, (), typexpr) ->
        Sexp.list [Sexp.atom "Pexp_coerce"; expression expr; core_type typexpr]
      | Pexp_send _ -> Sexp.list [Sexp.atom "Pexp_send"]
      | Pexp_new _ -> Sexp.list [Sexp.atom "Pexp_new"]
      | Pexp_setinstvar _ -> Sexp.list [Sexp.atom "Pexp_setinstvar"]
      | Pexp_override _ -> Sexp.list [Sexp.atom "Pexp_override"]
      | Pexp_letmodule (mod_name, mod_expr, expr) ->
        Sexp.list
          [
            Sexp.atom "Pexp_letmodule";
            string mod_name.Asttypes.txt;
            module_expression mod_expr;
            expression expr;
          ]
      | Pexp_letexception (ext_constr, expr) ->
        Sexp.list
          [
            Sexp.atom "Pexp_letexception";
            extension_constructor ext_constr;
            expression expr;
          ]
      | Pexp_assert expr -> Sexp.list [Sexp.atom "Pexp_assert"; expression expr]
      | Pexp_lazy expr -> Sexp.list [Sexp.atom "Pexp_lazy"; expression expr]
      | Pexp_poly _ -> Sexp.list [Sexp.atom "Pexp_poly"]
      | Pexp_newtype (lbl, expr) ->
        Sexp.list
          [Sexp.atom "Pexp_newtype"; string lbl.Asttypes.txt; expression expr]
      | Pexp_pack mod_expr ->
        Sexp.list [Sexp.atom "Pexp_pack"; module_expression mod_expr]
      | Pexp_open (flag, longident_loc, expr) ->
        Sexp.list
          [
            Sexp.atom "Pexp_open";
            override_flag flag;
            longident longident_loc.Asttypes.txt;
            expression expr;
          ]
      | Pexp_extension ext ->
        Sexp.list [Sexp.atom "Pexp_extension"; extension ext]
    in
    Sexp.list [Sexp.atom "expression"; desc]

  and case c =
    Sexp.list
      [
        Sexp.atom "case";
        Sexp.list [Sexp.atom "pc_lhs"; pattern c.pc_lhs];
        Sexp.list
          [
            Sexp.atom "pc_guard";
            (match c.pc_guard with
            | None -> Sexp.atom "None"
            | Some expr -> Sexp.list [Sexp.atom "Some"; expression expr]);
          ];
        Sexp.list [Sexp.atom "pc_rhs"; expression c.pc_rhs];
      ]

  and pattern p =
    let descr =
      match p.ppat_desc with
      | Ppat_any -> Sexp.atom "Ppat_any"
      | Ppat_var var ->
        Sexp.list [Sexp.atom "Ppat_var"; string var.Location.txt]
      | Ppat_alias (p, alias) ->
        Sexp.list [Sexp.atom "Ppat_alias"; pattern p; string alias.txt]
      | Ppat_constant c -> Sexp.list [Sexp.atom "Ppat_constant"; constant c]
      | Ppat_interval (lo, hi) ->
        Sexp.list [Sexp.atom "Ppat_interval"; constant lo; constant hi]
      | Ppat_tuple patterns ->
        Sexp.list
          [Sexp.atom "Ppat_tuple"; Sexp.list (map_empty ~f:pattern patterns)]
      | Ppat_construct (longident_loc, opt_pattern) ->
        Sexp.list
          [
            Sexp.atom "Ppat_construct";
            longident longident_loc.Location.txt;
            (match opt_pattern with
            | None -> Sexp.atom "None"
            | Some p -> Sexp.list [Sexp.atom "some"; pattern p]);
          ]
      | Ppat_variant (lbl, opt_pattern) ->
        Sexp.list
          [
            Sexp.atom "Ppat_variant";
            string lbl;
            (match opt_pattern with
            | None -> Sexp.atom "None"
            | Some p -> Sexp.list [Sexp.atom "Some"; pattern p]);
          ]
      | Ppat_record (rows, flag) ->
        Sexp.list
          [
            Sexp.atom "Ppat_record";
            closed_flag flag;
            Sexp.list
              (map_empty
                 ~f:(fun (longident_loc, p, _) ->
                   Sexp.list [longident longident_loc.Location.txt; pattern p])
                 rows);
          ]
      | Ppat_array patterns ->
        Sexp.list
          [Sexp.atom "Ppat_array"; Sexp.list (map_empty ~f:pattern patterns)]
      | Ppat_or (p1, p2) ->
        Sexp.list [Sexp.atom "Ppat_or"; pattern p1; pattern p2]
      | Ppat_constraint (p, typexpr) ->
        Sexp.list [Sexp.atom "Ppat_constraint"; pattern p; core_type typexpr]
      | Ppat_type longident_loc ->
        Sexp.list [Sexp.atom "Ppat_type"; longident longident_loc.Location.txt]
      | Ppat_lazy p -> Sexp.list [Sexp.atom "Ppat_lazy"; pattern p]
      | Ppat_unpack string_loc ->
        Sexp.list [Sexp.atom "Ppat_unpack"; string string_loc.Location.txt]
      | Ppat_exception p -> Sexp.list [Sexp.atom "Ppat_exception"; pattern p]
      | Ppat_extension ext ->
        Sexp.list [Sexp.atom "Ppat_extension"; extension ext]
      | Ppat_open (longident_loc, p) ->
        Sexp.list
          [
            Sexp.atom "Ppat_open";
            longident longident_loc.Location.txt;
            pattern p;
          ]
    in
    Sexp.list [Sexp.atom "pattern"; descr]

  and object_field field =
    match field with
    | Otag (lbl_loc, attrs, typexpr) ->
      Sexp.list
        [
          Sexp.atom "Otag";
          string lbl_loc.txt;
          attributes attrs;
          core_type typexpr;
        ]
    | Oinherit typexpr -> Sexp.list [Sexp.atom "Oinherit"; core_type typexpr]

  and row_field field =
    match field with
    | Rtag (label_loc, attrs, truth, types) ->
      Sexp.list
        [
          Sexp.atom "Rtag";
          string label_loc.txt;
          attributes attrs;
          Sexp.atom (if truth then "true" else "false");
          Sexp.list (map_empty ~f:core_type types);
        ]
    | Rinherit typexpr -> Sexp.list [Sexp.atom "Rinherit"; core_type typexpr]

  and package_type (mod_name_loc, package_constraints) =
    Sexp.list
      [
        Sexp.atom "package_type";
        longident mod_name_loc.Asttypes.txt;
        Sexp.list
          (map_empty
             ~f:(fun (mod_name_loc, typexpr) ->
               Sexp.list
                 [longident mod_name_loc.Asttypes.txt; core_type typexpr])
             package_constraints);
      ]

  and core_type typexpr =
    let desc =
      match typexpr.ptyp_desc with
      | Ptyp_any -> Sexp.atom "Ptyp_any"
      | Ptyp_var var -> Sexp.list [Sexp.atom "Ptyp_var"; string var]
      | Ptyp_arrow (arg_lbl, typ1, typ2, _) ->
        Sexp.list
          [
            Sexp.atom "Ptyp_arrow";
            arg_label arg_lbl;
            core_type typ1;
            core_type typ2;
          ]
      | Ptyp_tuple types ->
        Sexp.list
          [Sexp.atom "Ptyp_tuple"; Sexp.list (map_empty ~f:core_type types)]
      | Ptyp_constr (longident_loc, types) ->
        Sexp.list
          [
            Sexp.atom "Ptyp_constr";
            longident longident_loc.txt;
            Sexp.list (map_empty ~f:core_type types);
          ]
      | Ptyp_alias (typexpr, alias) ->
        Sexp.list [Sexp.atom "Ptyp_alias"; core_type typexpr; string alias]
      | Ptyp_object (fields, flag) ->
        Sexp.list
          [
            Sexp.atom "Ptyp_object";
            closed_flag flag;
            Sexp.list (map_empty ~f:object_field fields);
          ]
      | Ptyp_variant (fields, flag, opt_labels) ->
        Sexp.list
          [
            Sexp.atom "Ptyp_variant";
            Sexp.list (map_empty ~f:row_field fields);
            closed_flag flag;
            (match opt_labels with
            | None -> Sexp.atom "None"
            | Some lbls -> Sexp.list (map_empty ~f:string lbls));
          ]
      | Ptyp_poly (lbls, typexpr) ->
        Sexp.list
          [
            Sexp.atom "Ptyp_poly";
            Sexp.list (map_empty ~f:(fun lbl -> string lbl.Asttypes.txt) lbls);
            core_type typexpr;
          ]
      | Ptyp_package package ->
        Sexp.list [Sexp.atom "Ptyp_package"; package_type package]
      | Ptyp_extension ext ->
        Sexp.list [Sexp.atom "Ptyp_extension"; extension ext]
    in
    Sexp.list [Sexp.atom "core_type"; desc]

  and payload p =
    match p with
    | PStr s -> Sexp.list (Sexp.atom "PStr" :: map_empty ~f:structure_item s)
    | PSig s -> Sexp.list [Sexp.atom "PSig"; signature s]
    | PTyp ct -> Sexp.list [Sexp.atom "PTyp"; core_type ct]
    | PPat (pat, opt_expr) ->
      Sexp.list
        [
          Sexp.atom "PPat";
          pattern pat;
          (match opt_expr with
          | Some expr -> Sexp.list [Sexp.atom "Some"; expression expr]
          | None -> Sexp.atom "None");
        ]

  and attribute (string_loc, p) =
    Sexp.list
      [Sexp.atom "attribute"; Sexp.atom string_loc.Asttypes.txt; payload p]

  and extension (string_loc, p) =
    Sexp.list
      [Sexp.atom "extension"; Sexp.atom string_loc.Asttypes.txt; payload p]

  and attributes attrs =
    let sexprs = map_empty ~f:attribute attrs in
    Sexp.list (Sexp.atom "attributes" :: sexprs)

  let print_engine =
    Res_driver.
      {
        print_implementation =
          (fun ~width:_ ~filename:_ ~comments:_ parsetree ->
            parsetree |> structure |> Sexp.to_string |> print_string);
        print_interface =
          (fun ~width:_ ~filename:_ ~comments:_ parsetree ->
            parsetree |> signature |> Sexp.to_string |> print_string);
      }
end

let sexp_print_engine = SexpAst.print_engine

let comments_print_engine =
  {
    Res_driver.print_implementation =
      (fun ~width:_ ~filename:_ ~comments s ->
        let cmt_tbl = CommentTable.make () in
        CommentTable.walk_structure s cmt_tbl comments;
        CommentTable.log cmt_tbl);
    print_interface =
      (fun ~width:_ ~filename:_ ~comments s ->
        let cmt_tbl = CommentTable.make () in
        CommentTable.walk_signature s cmt_tbl comments;
        CommentTable.log cmt_tbl);
  }
