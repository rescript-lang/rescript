(* For the curious: the outcome printer is a printer to print data
 * from the outcometree.mli file in the ocaml compiler.
 * The outcome tree is used by:
 *  - ocaml's toplevel/repl, print results/errors
 *  - super errors, print nice errors
 *  - editor tooling, e.g. show type on hover
 *
 * In general it represent messages to show results or errors to the user. *)

module Doc = Res_doc
module Printer = Res_printer

(* ReScript doesn't have parenthesized identifiers.
 * We don't support custom operators. *)
let parenthesized_ident _name = true

(* TODO: better allocation strategy for the buffer *)
let escape_string_contents s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    let c = (String.get [@doesNotRaise]) s i in
    if c = '\008' then (
      Buffer.add_char b '\\';
      Buffer.add_char b 'b')
    else if c = '\009' then (
      Buffer.add_char b '\\';
      Buffer.add_char b 't')
    else if c = '\010' then (
      Buffer.add_char b '\\';
      Buffer.add_char b 'n')
    else if c = '\013' then (
      Buffer.add_char b '\\';
      Buffer.add_char b 'r')
    else if c = '\034' then (
      Buffer.add_char b '\\';
      Buffer.add_char b '"')
    else if c = '\092' then (
      Buffer.add_char b '\\';
      Buffer.add_char b '\\')
    else Buffer.add_char b c
  done;
  Buffer.contents b

(* let rec print_ident fmt ident = match ident with
   | Outcometree.Oide_ident s -> Format.pp_print_string fmt s
   | Oide_dot (id, s) ->
     print_ident fmt id;
     Format.pp_print_char fmt '.';
     Format.pp_print_string fmt s
   | Oide_apply (id1, id2) ->
     print_ident fmt id1;
     Format.pp_print_char fmt '(';
     print_ident fmt id2;
     Format.pp_print_char fmt ')' *)

let rec print_out_ident_doc ?(allow_uident = true)
    (ident : Outcometree.out_ident) =
  match ident with
  | Oide_ident s -> Printer.print_ident_like ~allow_uident s
  | Oide_dot (ident, s) ->
    Doc.concat [print_out_ident_doc ident; Doc.dot; Doc.text s]
  | Oide_apply (call, arg) ->
    Doc.concat
      [
        print_out_ident_doc call; Doc.lparen; print_out_ident_doc arg; Doc.rparen;
      ]

let print_out_attribute_doc (out_attribute : Outcometree.out_attribute) =
  Doc.concat [Doc.text "@"; Doc.text out_attribute.oattr_name]

let print_out_attributes_doc (attrs : Outcometree.out_attribute list) =
  match attrs with
  | [] -> Doc.nil
  | attrs ->
    Doc.concat
      [
        Doc.group
          (Doc.join ~sep:Doc.line (List.map print_out_attribute_doc attrs));
        Doc.line;
      ]

let rec collect_arrow_args (out_type : Outcometree.out_type) args =
  match out_type with
  | Otyp_arrow (label, arg_type, return_type, arity)
    when arity = None || args = [] ->
    let arg = (label, arg_type) in
    collect_arrow_args return_type (arg :: args)
  | _ as return_type -> (List.rev args, return_type)

let rec collect_functor_args (out_module_type : Outcometree.out_module_type)
    args =
  match out_module_type with
  | Omty_functor (lbl, opt_mod_type, return_mod_type) ->
    let arg = (lbl, opt_mod_type) in
    collect_functor_args return_mod_type (arg :: args)
  | _ -> (List.rev args, out_module_type)

let rec print_out_type_doc (out_type : Outcometree.out_type) =
  match out_type with
  | Otyp_abstract | Otyp_open -> Doc.nil
  | Otyp_variant (non_gen, out_variant, closed, labels) ->
    (* bool * out_variant * bool * (string list) option *)
    let opening =
      match (closed, labels) with
      | true, None -> (* [#A | #B] *) Doc.soft_line
      | false, None ->
        (* [> #A | #B] *)
        Doc.concat [Doc.greater_than; Doc.line]
      | true, Some [] ->
        (* [< #A | #B] *)
        Doc.concat [Doc.less_than; Doc.line]
      | true, Some _ ->
        (* [< #A | #B > #X #Y ] *)
        Doc.concat [Doc.less_than; Doc.line]
      | false, Some _ ->
        (* impossible!? ocaml seems to print ?, see oprint.ml in 4.06 *)
        Doc.concat [Doc.text "?"; Doc.line]
    in
    Doc.group
      (Doc.concat
         [
           (if non_gen then Doc.text "_" else Doc.nil);
           Doc.lbracket;
           Doc.indent (Doc.concat [opening; print_out_variant out_variant]);
           Doc.soft_line;
           Doc.rbracket;
         ])
  | Otyp_alias (typ, alias_txt) ->
    Doc.concat
      [
        Doc.lparen;
        print_out_type_doc typ;
        Doc.text " as '";
        Doc.text alias_txt;
        Doc.rparen;
      ]
  | Otyp_constr (out_ident, []) ->
    print_out_ident_doc ~allow_uident:false out_ident
  | Otyp_manifest (typ1, typ2) ->
    Doc.concat
      [print_out_type_doc typ1; Doc.text " = "; print_out_type_doc typ2]
  | Otyp_record record -> print_record_declaration_doc ~inline:true record
  | Otyp_stuff txt -> Doc.text txt
  | Otyp_var (ng, s) ->
    Doc.concat [Doc.text ("'" ^ if ng then "_" else ""); Doc.text s]
  | Otyp_object (fields, rest) -> print_object_fields fields rest
  | Otyp_class _ -> Doc.nil
  | Otyp_attribute (typ, attribute) ->
    Doc.group
      (Doc.concat
         [print_out_attribute_doc attribute; Doc.line; print_out_type_doc typ])
  (* example: Red | Blue | Green | CustomColour(float, float, float) *)
  | Otyp_sum constructors -> print_out_constructors_doc constructors
  (* example: {"name": string, "age": int} *)
  | Otyp_constr (Oide_dot (Oide_ident "Js", "t"), [Otyp_object (fields, rest)])
    ->
    print_object_fields fields rest
  (* example: node<root, 'value> *)
  | Otyp_constr (out_ident, args) ->
    let args_doc =
      match args with
      | [] -> Doc.nil
      | args ->
        Doc.concat
          [
            Doc.less_than;
            Doc.indent
              (Doc.concat
                 [
                   Doc.soft_line;
                   Doc.join
                     ~sep:(Doc.concat [Doc.comma; Doc.line])
                     (List.map print_out_type_doc args);
                 ]);
            Doc.trailing_comma;
            Doc.soft_line;
            Doc.greater_than;
          ]
    in
    Doc.group (Doc.concat [print_out_ident_doc out_ident; args_doc])
  | Otyp_tuple tuple_args ->
    Doc.group
      (Doc.concat
         [
           Doc.lparen;
           Doc.indent
             (Doc.concat
                [
                  Doc.soft_line;
                  Doc.join
                    ~sep:(Doc.concat [Doc.comma; Doc.line])
                    (List.map print_out_type_doc tuple_args);
                ]);
           Doc.trailing_comma;
           Doc.soft_line;
           Doc.rparen;
         ])
  | Otyp_poly (vars, out_type) ->
    Doc.group
      (Doc.concat
         [
           Doc.join ~sep:Doc.space
             (List.map (fun var -> Doc.text ("'" ^ var)) vars);
           Doc.dot;
           Doc.space;
           print_out_type_doc out_type;
         ])
  | Otyp_arrow _ as typ -> print_out_arrow_type typ
  | Otyp_module (mod_name, string_list, out_types) ->
    let package_type_doc =
      match (string_list, out_types) with
      | [], [] -> Doc.nil
      | labels, types ->
        let i = ref 0 in
        let package =
          Doc.join ~sep:Doc.line
            ((List.map2 [@doesNotRaise])
               (fun lbl typ ->
                 Doc.concat
                   [
                     Doc.text
                       (if i.contents > 0 then "and type " else "with type ");
                     Doc.text lbl;
                     Doc.text " = ";
                     print_out_type_doc typ;
                   ])
               labels types)
        in
        Doc.indent (Doc.concat [Doc.line; package])
    in
    Doc.concat
      [
        Doc.text "module";
        Doc.lparen;
        Doc.text mod_name;
        package_type_doc;
        Doc.rparen;
      ]

and print_out_arrow_type typ =
  let typ_args, typ = collect_arrow_args typ [] in
  let args =
    Doc.join
      ~sep:(Doc.concat [Doc.comma; Doc.line])
      (List.map
         (fun (lbl, typ) ->
           let lbl_len = String.length lbl in
           if lbl_len = 0 then print_out_type_doc typ
           else
             let lbl, optional_indicator =
               (* the ocaml compiler hardcodes the optional label inside the string of the label in printtyp.ml *)
               match String.unsafe_get lbl 0 with
               | '?' ->
                 ( (String.sub [@doesNotRaise]) lbl 1 (lbl_len - 1),
                   Doc.text "=?" )
               | _ -> (lbl, Doc.nil)
             in
             Doc.group
               (Doc.concat
                  [
                    Doc.text ("~" ^ lbl ^ ": ");
                    print_out_type_doc typ;
                    optional_indicator;
                  ]))
         typ_args)
  in
  let args_doc =
    let needs_parens =
      match typ_args with
      | [(_, (Otyp_tuple _ | Otyp_arrow _))] -> true
      (* single argument should not be wrapped *)
      | [("", _)] -> false
      | _ -> true
    in
    if needs_parens then
      Doc.group
        (Doc.concat
           [
             Doc.lparen;
             Doc.indent (Doc.concat [Doc.soft_line; args]);
             Doc.trailing_comma;
             Doc.soft_line;
             Doc.rparen;
           ])
    else args
  in
  Doc.concat [args_doc; Doc.text " => "; print_out_type_doc typ]

and print_out_variant variant =
  match variant with
  | Ovar_fields fields ->
    (* (string * bool * out_type list) list *)
    Doc.join ~sep:Doc.line
      ((*
        * [< | #T([< u2]) & ([< u2]) & ([< u1])]  --> no ampersand
        * [< | #S & ([< u2]) & ([< u2]) & ([< u1])] --> ampersand
        *)
       List.mapi
         (fun i (name, ampersand, types) ->
           let needs_parens =
             match types with
             | [Outcometree.Otyp_tuple _] -> false
             | _ -> true
           in
           Doc.concat
             [
               (if i > 0 then Doc.text "| "
                else Doc.if_breaks (Doc.text "| ") Doc.nil);
               Doc.group
                 (Doc.concat
                    [
                      Doc.text "#";
                      Printer.print_poly_var_ident name;
                      (match types with
                      | [] -> Doc.nil
                      | types ->
                        Doc.concat
                          [
                            (if ampersand then Doc.text " & " else Doc.nil);
                            Doc.indent
                              (Doc.concat
                                 [
                                   Doc.join
                                     ~sep:(Doc.concat [Doc.text " &"; Doc.line])
                                     (List.map
                                        (fun typ ->
                                          let out_type_doc =
                                            print_out_type_doc typ
                                          in
                                          if needs_parens then
                                            Doc.concat
                                              [
                                                Doc.lparen;
                                                out_type_doc;
                                                Doc.rparen;
                                              ]
                                          else out_type_doc)
                                        types);
                                 ]);
                          ]);
                    ]);
             ])
         fields)
  | Ovar_typ typ -> print_out_type_doc typ

and print_object_fields fields rest =
  let dots =
    match rest with
    | Some non_gen -> Doc.text ((if non_gen then "_" else "") ^ "..")
    | None -> if fields = [] then Doc.dot else Doc.nil
  in
  Doc.group
    (Doc.concat
       [
         Doc.lbrace;
         dots;
         Doc.indent
           (Doc.concat
              [
                Doc.soft_line;
                Doc.join
                  ~sep:(Doc.concat [Doc.comma; Doc.line])
                  (List.map
                     (fun (lbl, out_type) ->
                       Doc.group
                         (Doc.concat
                            [
                              Doc.text ("\"" ^ lbl ^ "\": ");
                              print_out_type_doc out_type;
                            ]))
                     fields);
              ]);
         Doc.trailing_comma;
         Doc.soft_line;
         Doc.rbrace;
       ])

and print_out_constructors_doc constructors =
  Doc.group
    (Doc.indent
       (Doc.concat
          [
            Doc.soft_line;
            Doc.join ~sep:Doc.line
              (List.mapi
                 (fun i constructor ->
                   Doc.concat
                     [
                       (if i > 0 then Doc.text "| "
                        else Doc.if_breaks (Doc.text "| ") Doc.nil);
                       print_out_constructor_doc constructor;
                     ])
                 constructors);
          ]))

and print_out_constructor_doc (name, args, gadt, repr) =
  let repr_doc =
    match repr with
    | None -> Doc.nil
    | Some s -> Doc.text (s ^ " ")
  in
  let gadt_doc =
    match gadt with
    | Some out_type -> Doc.concat [Doc.text ": "; print_out_type_doc out_type]
    | None -> Doc.nil
  in
  let args_doc =
    match args with
    | [] -> Doc.nil
    | [Otyp_record record] ->
      (* inline records
       *   | Root({
       *      mutable value: 'value,
       *      mutable updatedTime: float,
       *    })
       *)
      Doc.concat
        [
          Doc.lparen;
          Doc.indent (print_record_declaration_doc ~inline:true record);
          Doc.rparen;
        ]
    | _types ->
      Doc.indent
        (Doc.concat
           [
             Doc.lparen;
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    Doc.join
                      ~sep:(Doc.concat [Doc.comma; Doc.line])
                      (List.map print_out_type_doc args);
                  ]);
             Doc.trailing_comma;
             Doc.soft_line;
             Doc.rparen;
           ])
  in
  Doc.group (Doc.concat [repr_doc; Doc.text name; args_doc; gadt_doc])

and print_record_decl_row_doc (name, mut, opt, arg) =
  Doc.group
    (Doc.concat
       [
         (if mut then Doc.text "mutable " else Doc.nil);
         Printer.print_ident_like ~allow_uident:false name;
         (if opt then Doc.text "?" else Doc.nil);
         Doc.text ": ";
         print_out_type_doc arg;
       ])

and print_record_declaration_doc ~inline rows =
  let content =
    Doc.concat
      [
        Doc.lbrace;
        Doc.indent
          (Doc.concat
             [
               Doc.soft_line;
               Doc.join
                 ~sep:(Doc.concat [Doc.comma; Doc.line])
                 (List.map print_record_decl_row_doc rows);
             ]);
        Doc.trailing_comma;
        Doc.soft_line;
        Doc.rbrace;
      ]
  in
  if not inline then Doc.group content else content

let print_out_type fmt out_type =
  Format.pp_print_string fmt
    (Doc.to_string ~width:80 (print_out_type_doc out_type))

let print_type_parameter_doc (typ, (co, cn)) =
  Doc.concat
    [
      (if not cn then Doc.text "+" else if not co then Doc.text "-" else Doc.nil);
      (if typ = "_" then Doc.text "_" else Doc.text ("'" ^ typ));
    ]

let rec print_out_sig_item_doc ?(print_name_as_is = false)
    (out_sig_item : Outcometree.out_sig_item) =
  match out_sig_item with
  | Osig_class _ | Osig_class_type _ -> Doc.nil
  | Osig_ellipsis -> Doc.dotdotdot
  | Osig_value value_decl ->
    Doc.group
      (Doc.concat
         [
           print_out_attributes_doc value_decl.oval_attributes;
           Doc.text
             (match value_decl.oval_prims with
             | [] -> "let "
             | _ -> "external ");
           Doc.text value_decl.oval_name;
           Doc.text ":";
           Doc.space;
           print_out_type_doc value_decl.oval_type;
           (match value_decl.oval_prims with
           | [] -> Doc.nil
           | primitives ->
             Doc.indent
               (Doc.concat
                  [
                    Doc.text " =";
                    Doc.line;
                    Doc.group
                      (Doc.join ~sep:Doc.line
                         (List.map
                            (fun prim ->
                              let prim =
                                if
                                  prim <> ""
                                  && (prim.[0] [@doesNotRaise]) = '\132'
                                then "#rescript-external"
                                else prim
                              in
                              (* not display those garbage '\132' is a magic number for marshal *)
                              Doc.text ("\"" ^ prim ^ "\""))
                            primitives));
                  ]));
         ])
  | Osig_typext (out_extension_constructor, _outExtStatus) ->
    print_out_extension_constructor_doc out_extension_constructor
  | Osig_modtype (mod_name, Omty_signature []) ->
    Doc.concat [Doc.text "module type "; Doc.text mod_name]
  | Osig_modtype (mod_name, out_module_type) ->
    Doc.group
      (Doc.concat
         [
           Doc.text "module type ";
           Doc.text mod_name;
           Doc.text " = ";
           print_out_module_type_doc out_module_type;
         ])
  | Osig_module (mod_name, Omty_alias ident, _) ->
    Doc.group
      (Doc.concat
         [
           Doc.text "module ";
           Doc.text mod_name;
           Doc.text " =";
           Doc.line;
           print_out_ident_doc ident;
         ])
  | Osig_module (mod_name, out_mod_type, out_rec_status) ->
    Doc.group
      (Doc.concat
         [
           Doc.text
             (match out_rec_status with
             | Orec_not -> "module "
             | Orec_first -> "module rec "
             | Orec_next -> "and ");
           Doc.text mod_name;
           Doc.text ": ";
           print_out_module_type_doc out_mod_type;
         ])
  | Osig_type (out_type_decl, out_rec_status) ->
    (* TODO: manifest ? *)
    let attrs =
      match (out_type_decl.otype_immediate, out_type_decl.otype_unboxed) with
      | false, false -> Doc.nil
      | true, false -> Doc.concat [Doc.text "@immediate"; Doc.line]
      | false, true -> Doc.concat [Doc.text "@unboxed"; Doc.line]
      | true, true -> Doc.concat [Doc.text "@immediate @unboxed"; Doc.line]
    in
    let kw =
      Doc.text
        (match out_rec_status with
        | Orec_not -> "type "
        | Orec_first -> "type rec "
        | Orec_next -> "and ")
    in
    let type_params =
      match out_type_decl.otype_params with
      | [] -> Doc.nil
      | _params ->
        Doc.group
          (Doc.concat
             [
               Doc.less_than;
               Doc.indent
                 (Doc.concat
                    [
                      Doc.soft_line;
                      Doc.join
                        ~sep:(Doc.concat [Doc.comma; Doc.line])
                        (List.map print_type_parameter_doc
                           out_type_decl.otype_params);
                    ]);
               Doc.trailing_comma;
               Doc.soft_line;
               Doc.greater_than;
             ])
    in
    let private_doc =
      match out_type_decl.otype_private with
      | Asttypes.Private -> Doc.text "private "
      | Public -> Doc.nil
    in
    let kind =
      match out_type_decl.otype_type with
      | Otyp_open -> Doc.concat [Doc.text " = "; private_doc; Doc.text ".."]
      | Otyp_abstract -> Doc.nil
      | Otyp_record record ->
        Doc.concat
          [
            Doc.text " = ";
            private_doc;
            print_record_declaration_doc ~inline:false record;
          ]
      | typ -> Doc.concat [Doc.text " = "; print_out_type_doc typ]
    in
    let constraints =
      match out_type_decl.otype_cstrs with
      | [] -> Doc.nil
      | _ ->
        Doc.group
          (Doc.indent
             (Doc.concat
                [
                  Doc.hard_line;
                  Doc.join ~sep:Doc.line
                    (List.map
                       (fun (typ1, typ2) ->
                         Doc.group
                           (Doc.concat
                              [
                                Doc.text "constraint ";
                                print_out_type_doc typ1;
                                Doc.text " =";
                                Doc.space;
                                print_out_type_doc typ2;
                              ]))
                       out_type_decl.otype_cstrs);
                ]))
    in
    Doc.group
      (Doc.concat
         [
           attrs;
           Doc.group
             (Doc.concat
                [
                  kw;
                  (if print_name_as_is then Doc.text out_type_decl.otype_name
                   else
                     Printer.print_ident_like ~allow_uident:false
                       out_type_decl.otype_name);
                  type_params;
                  kind;
                ]);
           constraints;
         ])

and print_out_module_type_doc (out_mod_type : Outcometree.out_module_type) =
  match out_mod_type with
  | Omty_abstract -> Doc.nil
  | Omty_ident ident -> print_out_ident_doc ident
  (* example: module Increment = (M: X_int) => X_int *)
  | Omty_functor _ ->
    let args, return_mod_type = collect_functor_args out_mod_type [] in
    let args_doc =
      match args with
      | [(_, None)] -> Doc.text "()"
      | args ->
        Doc.group
          (Doc.concat
             [
               Doc.lparen;
               Doc.indent
                 (Doc.concat
                    [
                      Doc.soft_line;
                      Doc.join
                        ~sep:(Doc.concat [Doc.comma; Doc.line])
                        (List.map
                           (fun (lbl, opt_mod_type) ->
                             Doc.group
                               (Doc.concat
                                  [
                                    Doc.text lbl;
                                    (match opt_mod_type with
                                    | None -> Doc.nil
                                    | Some mod_type ->
                                      Doc.concat
                                        [
                                          Doc.text ": ";
                                          print_out_module_type_doc mod_type;
                                        ]);
                                  ]))
                           args);
                    ]);
               Doc.trailing_comma;
               Doc.soft_line;
               Doc.rparen;
             ])
    in
    Doc.group
      (Doc.concat
         [args_doc; Doc.text " => "; print_out_module_type_doc return_mod_type])
  | Omty_signature [] -> Doc.nil
  | Omty_signature signature ->
    Doc.breakable_group ~force_break:true
      (Doc.concat
         [
           Doc.lbrace;
           Doc.indent (Doc.concat [Doc.line; print_out_signature_doc signature]);
           Doc.soft_line;
           Doc.rbrace;
         ])
  | Omty_alias _ident -> Doc.nil

and print_out_signature_doc (signature : Outcometree.out_sig_item list) =
  let rec loop signature acc =
    match signature with
    | [] -> List.rev acc
    | Outcometree.Osig_typext (ext, Oext_first) :: items ->
      (* Gather together the extension constructors *)
      let rec gather_extensions acc items =
        match items with
        | Outcometree.Osig_typext (ext, Oext_next) :: items ->
          gather_extensions
            ((ext.oext_name, ext.oext_args, ext.oext_ret_type, ext.oext_repr)
            :: acc)
            items
        | _ -> (List.rev acc, items)
      in
      let exts, items =
        gather_extensions
          [(ext.oext_name, ext.oext_args, ext.oext_ret_type, ext.oext_repr)]
          items
      in
      let te =
        {
          Outcometree.otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private;
        }
      in
      let doc = print_out_type_extension_doc te in
      loop items (doc :: acc)
    | item :: items ->
      let doc = print_out_sig_item_doc ~print_name_as_is:false item in
      loop items (doc :: acc)
  in
  match loop signature [] with
  | [doc] -> doc
  | docs -> Doc.breakable_group ~force_break:true (Doc.join ~sep:Doc.line docs)

and print_out_extension_constructor_doc
    (out_ext : Outcometree.out_extension_constructor) =
  let type_params =
    match out_ext.oext_type_params with
    | [] -> Doc.nil
    | params ->
      Doc.group
        (Doc.concat
           [
             Doc.less_than;
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    Doc.join
                      ~sep:(Doc.concat [Doc.comma; Doc.line])
                      (List.map
                         (fun ty ->
                           Doc.text (if ty = "_" then ty else "'" ^ ty))
                         params);
                  ]);
             Doc.soft_line;
             Doc.greater_than;
           ])
  in

  Doc.group
    (Doc.concat
       [
         Doc.text "type ";
         Printer.print_ident_like ~allow_uident:false out_ext.oext_type_name;
         type_params;
         Doc.text " += ";
         Doc.line;
         (if out_ext.oext_private = Asttypes.Private then Doc.text "private "
          else Doc.nil);
         print_out_constructor_doc
           ( out_ext.oext_name,
             out_ext.oext_args,
             out_ext.oext_ret_type,
             out_ext.oext_repr );
       ])

and print_out_type_extension_doc
    (type_extension : Outcometree.out_type_extension) =
  let type_params =
    match type_extension.otyext_params with
    | [] -> Doc.nil
    | params ->
      Doc.group
        (Doc.concat
           [
             Doc.less_than;
             Doc.indent
               (Doc.concat
                  [
                    Doc.soft_line;
                    Doc.join
                      ~sep:(Doc.concat [Doc.comma; Doc.line])
                      (List.map
                         (fun ty ->
                           Doc.text (if ty = "_" then ty else "'" ^ ty))
                         params);
                  ]);
             Doc.soft_line;
             Doc.greater_than;
           ])
  in

  Doc.group
    (Doc.concat
       [
         Doc.text "type ";
         Printer.print_ident_like ~allow_uident:false type_extension.otyext_name;
         type_params;
         Doc.text " += ";
         (if type_extension.otyext_private = Asttypes.Private then
            Doc.text "private "
          else Doc.nil);
         print_out_constructors_doc type_extension.otyext_constructors;
       ])

let print_out_sig_item fmt out_sig_item =
  Format.pp_print_string fmt
    (Doc.to_string ~width:80 (print_out_sig_item_doc out_sig_item))

let print_out_signature fmt signature =
  Format.pp_print_string fmt
    (Doc.to_string ~width:80 (print_out_signature_doc signature))

let valid_float_lexeme s =
  let l = String.length s in
  let rec loop i =
    if i >= l then s ^ "."
    else
      match s.[i] [@doesNotRaise] with
      | '0' .. '9' | '-' -> loop (i + 1)
      | _ -> s
  in
  loop 0

let float_repres f =
  match classify_float f with
  | FP_nan -> "nan"
  | FP_infinite -> if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
    let float_val =
      let s1 = Printf.sprintf "%.12g" f in
      if f = (float_of_string [@doesNotRaise]) s1 then s1
      else
        let s2 = Printf.sprintf "%.15g" f in
        if f = (float_of_string [@doesNotRaise]) s2 then s2
        else Printf.sprintf "%.18g" f
    in
    valid_float_lexeme float_val

let rec print_out_value_doc (out_value : Outcometree.out_value) =
  match out_value with
  | Oval_array out_values ->
    Doc.group
      (Doc.concat
         [
           Doc.lbracket;
           Doc.indent
             (Doc.concat
                [
                  Doc.soft_line;
                  Doc.join
                    ~sep:(Doc.concat [Doc.comma; Doc.line])
                    (List.map print_out_value_doc out_values);
                ]);
           Doc.trailing_comma;
           Doc.soft_line;
           Doc.rbracket;
         ])
  | Oval_char c -> Doc.text ("'" ^ Char.escaped c ^ "'")
  | Oval_constr (out_ident, out_values) ->
    Doc.group
      (Doc.concat
         [
           print_out_ident_doc out_ident;
           Doc.lparen;
           Doc.indent
             (Doc.concat
                [
                  Doc.soft_line;
                  Doc.join
                    ~sep:(Doc.concat [Doc.comma; Doc.line])
                    (List.map print_out_value_doc out_values);
                ]);
           Doc.trailing_comma;
           Doc.soft_line;
           Doc.rparen;
         ])
  | Oval_ellipsis -> Doc.text "..."
  | Oval_int i -> Doc.text (Format.sprintf "%i" i)
  | Oval_int32 i -> Doc.text (Format.sprintf "%lil" i)
  | Oval_int64 i -> Doc.text (Format.sprintf "%LiL" i)
  | Oval_nativeint i -> Doc.text (Format.sprintf "%nin" i)
  | Oval_float f -> Doc.text (float_repres f)
  | Oval_list out_values ->
    Doc.group
      (Doc.concat
         [
           Doc.text "list[";
           Doc.indent
             (Doc.concat
                [
                  Doc.soft_line;
                  Doc.join
                    ~sep:(Doc.concat [Doc.comma; Doc.line])
                    (List.map print_out_value_doc out_values);
                ]);
           Doc.trailing_comma;
           Doc.soft_line;
           Doc.rbracket;
         ])
  | Oval_printer fn ->
    let fmt = Format.str_formatter in
    fn fmt;
    let str = Format.flush_str_formatter () in
    Doc.text str
  | Oval_record rows ->
    Doc.group
      (Doc.concat
         [
           Doc.lparen;
           Doc.indent
             (Doc.concat
                [
                  Doc.soft_line;
                  Doc.join
                    ~sep:(Doc.concat [Doc.comma; Doc.line])
                    (List.map
                       (fun (out_ident, out_value) ->
                         Doc.group
                           (Doc.concat
                              [
                                print_out_ident_doc out_ident;
                                Doc.text ": ";
                                print_out_value_doc out_value;
                              ]))
                       rows);
                ]);
           Doc.trailing_comma;
           Doc.soft_line;
           Doc.rparen;
         ])
  | Oval_string (txt, _sizeToPrint, _kind) ->
    Doc.text (escape_string_contents txt)
  | Oval_stuff txt -> Doc.text txt
  | Oval_tuple out_values ->
    Doc.group
      (Doc.concat
         [
           Doc.lparen;
           Doc.indent
             (Doc.concat
                [
                  Doc.soft_line;
                  Doc.join
                    ~sep:(Doc.concat [Doc.comma; Doc.line])
                    (List.map print_out_value_doc out_values);
                ]);
           Doc.trailing_comma;
           Doc.soft_line;
           Doc.rparen;
         ])
  (* Not supported by ReScript *)
  | Oval_variant _ -> Doc.nil

let print_out_exception_doc exc out_value =
  match exc with
  | Sys.Break -> Doc.text "Interrupted."
  | Out_of_memory -> Doc.text "Out of memory during evaluation."
  | Stack_overflow ->
    Doc.text "Stack overflow during evaluation (looping recursion?)."
  | _ ->
    Doc.group
      (Doc.indent
         (Doc.concat
            [Doc.text "Exception:"; Doc.line; print_out_value_doc out_value]))

let print_out_phrase_signature signature =
  let rec loop signature acc =
    match signature with
    | [] -> List.rev acc
    | (Outcometree.Osig_typext (ext, Oext_first), None) :: signature ->
      (* Gather together extension constructors *)
      let rec gather_extensions acc items =
        match items with
        | (Outcometree.Osig_typext (ext, Oext_next), None) :: items ->
          gather_extensions
            ((ext.oext_name, ext.oext_args, ext.oext_ret_type, ext.oext_repr)
            :: acc)
            items
        | _ -> (List.rev acc, items)
      in
      let exts, signature =
        gather_extensions
          [(ext.oext_name, ext.oext_args, ext.oext_ret_type, ext.oext_repr)]
          signature
      in
      let te =
        {
          Outcometree.otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private;
        }
      in
      let doc = print_out_type_extension_doc te in
      loop signature (doc :: acc)
    | (sig_item, opt_out_value) :: signature ->
      let doc =
        match opt_out_value with
        | None -> print_out_sig_item_doc sig_item
        | Some out_value ->
          Doc.group
            (Doc.concat
               [
                 print_out_sig_item_doc sig_item;
                 Doc.text " = ";
                 print_out_value_doc out_value;
               ])
      in
      loop signature (doc :: acc)
  in
  Doc.breakable_group ~force_break:true
    (Doc.join ~sep:Doc.line (loop signature []))

let print_out_phrase_doc (out_phrase : Outcometree.out_phrase) =
  match out_phrase with
  | Ophr_eval (out_value, out_type) ->
    Doc.group
      (Doc.concat
         [
           Doc.text "- : ";
           print_out_type_doc out_type;
           Doc.text " =";
           Doc.indent (Doc.concat [Doc.line; print_out_value_doc out_value]);
         ])
  | Ophr_signature [] -> Doc.nil
  | Ophr_signature signature -> print_out_phrase_signature signature
  | Ophr_exception (exc, out_value) -> print_out_exception_doc exc out_value

let print_out_phrase fmt out_phrase =
  Format.pp_print_string fmt
    (Doc.to_string ~width:80 (print_out_phrase_doc out_phrase))

let print_out_module_type fmt out_module_type =
  Format.pp_print_string fmt
    (Doc.to_string ~width:80 (print_out_module_type_doc out_module_type))

let print_out_type_extension fmt type_extension =
  Format.pp_print_string fmt
    (Doc.to_string ~width:80 (print_out_type_extension_doc type_extension))

let print_out_value fmt out_value =
  Format.pp_print_string fmt
    (Doc.to_string ~width:80 (print_out_value_doc out_value))

(* Not supported in ReScript *)
(* Oprint.out_class_type *)
let setup =
  lazy
    (Oprint.out_value := print_out_value;
     Oprint.out_type := print_out_type;
     Oprint.out_module_type := print_out_module_type;
     Oprint.out_sig_item := print_out_sig_item;
     Oprint.out_signature := print_out_signature;
     Oprint.out_type_extension := print_out_type_extension;
     Oprint.out_phrase := print_out_phrase)
