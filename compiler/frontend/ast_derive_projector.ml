open Ast_helper

let invalid_config (config : Parsetree.expression) =
  Location.raise_errorf ~loc:config.pexp_loc
    "such configuration is not supported"

let raise_unsupported_vaiant_record_arg loc =
  Location.raise_errorf ~loc
    "@deriving(accessors) from a variant record argument is unsupported. \
     Either define the record type separately from the variant type or use a \
     positional argument."

type tdcls = Parsetree.type_declaration list

let deriving_name = "accessors"

let init () =
  Ast_derive.register deriving_name (fun (x : Parsetree.expression option) ->
      Ext_option.iter x invalid_config;
      {
        structure_gen =
          (fun (tdcls : tdcls) _explict_nonrec ->
            let handle_tdcl tdcl =
              let core_type =
                Ast_derive_util.core_type_of_type_declaration tdcl
              in
              let gentype_attrs =
                match
                  Ext_list.exists core_type.ptyp_attributes
                    Ast_attributes.is_gentype
                with
                | true -> Some [Ast_attributes.gentype]
                | false -> None
              in
              match tdcl.ptype_kind with
              | Ptype_record label_declarations ->
                Ext_list.map label_declarations
                  (fun
                    ({pld_name = {loc; txt = pld_label} as pld_name} :
                      Parsetree.label_declaration)
                  ->
                    let txt = "param" in
                    Ast_comb.single_non_rec_value ?attrs:gentype_attrs pld_name
                      (* arity will always be 1 since these are single param functions *)
                      (Exp.fun_
                         [
                           Exp.fun_param Nolabel
                             (Pat.constraint_ (Pat.var {txt; loc}) core_type);
                         ]
                         (Exp.field
                            (Exp.ident {txt = Lident txt; loc})
                            {txt = Longident.Lident pld_label; loc})))
              | Ptype_variant constructor_declarations ->
                Ext_list.map constructor_declarations
                  (fun
                    {
                      pcd_name = {loc; txt = con_name};
                      pcd_args;
                      pcd_loc;
                      pcd_res;
                    }
                  ->
                    (* TODO: add type annotations *)
                    let pcd_args =
                      match pcd_args with
                      | Pcstr_tuple pcd_args -> pcd_args
                      | Pcstr_record _ ->
                        raise_unsupported_vaiant_record_arg pcd_loc
                    in
                    let little_con_name =
                      Ext_string.uncapitalize_ascii con_name
                    in
                    let arity = List.length pcd_args in
                    let annotate_type =
                      match pcd_res with
                      | None -> core_type
                      | Some x -> x
                    in
                    Ast_comb.single_non_rec_value ?attrs:gentype_attrs
                      {loc; txt = little_con_name}
                      (if arity = 0 then
                         (*TODO: add a prefix, better inter-op with FFI *)
                         Exp.constraint_
                           (Exp.construct
                              {loc; txt = Longident.Lident con_name}
                              {txt = []; loc})
                           annotate_type
                       else
                         let vars =
                           Ext_list.init arity (fun x ->
                               "param_" ^ string_of_int x)
                         in
                         let exp =
                           Exp.constraint_
                             (Exp.construct
                                {loc; txt = Longident.Lident con_name}
                                {
                                  txt =
                                    Ext_list.map vars (fun x ->
                                        Exp.ident {loc; txt = Lident x});
                                  loc;
                                })
                             annotate_type
                         in
                         Ast_helper.Exp.fun_
                           (Ext_list.map vars (fun var ->
                                Ast_helper.Exp.fun_param Nolabel
                                  (Pat.var {loc; txt = var})))
                           exp))
              | Ptype_abstract | Ptype_open ->
                Ast_derive_util.not_applicable tdcl.ptype_loc deriving_name;
                []
              (* Location.raise_errorf "projector only works with record" *)
            in
            Ext_list.flat_map tdcls handle_tdcl);
        signature_gen =
          (fun (tdcls : Parsetree.type_declaration list) _explict_nonrec ->
            let handle_tdcl tdcl =
              let core_type =
                Ast_derive_util.core_type_of_type_declaration tdcl
              in
              let gentype_attrs =
                match
                  Ext_list.exists core_type.ptyp_attributes
                    Ast_attributes.is_gentype
                with
                | true -> Some [Ast_attributes.gentype]
                | false -> None
              in
              match tdcl.ptype_kind with
              | Ptype_record label_declarations ->
                Ext_list.map label_declarations (fun {pld_name; pld_type} ->
                    Ast_comb.single_non_rec_val ?attrs:gentype_attrs pld_name
                      (Ast_helper.Typ.arrow
                         [{attrs = []; lbl = Nolabel; typ = core_type}]
                         pld_type
                         (*arity will alwys be 1 since these are single param functions*)))
              | Ptype_variant constructor_declarations ->
                Ext_list.map constructor_declarations
                  (fun
                    {
                      pcd_name = {loc; txt = con_name};
                      pcd_args;
                      pcd_loc;
                      pcd_res;
                    }
                  ->
                    let pcd_args =
                      match pcd_args with
                      | Pcstr_tuple pcd_args -> pcd_args
                      | Pcstr_record _ ->
                        raise_unsupported_vaiant_record_arg pcd_loc
                    in
                    let annotate_type =
                      match pcd_res with
                      | Some x -> x
                      | None -> core_type
                    in
                    Ast_comb.single_non_rec_val ?attrs:gentype_attrs
                      {loc; txt = Ext_string.uncapitalize_ascii con_name}
                      (match
                         Ext_list.map pcd_args (fun x ->
                             ({attrs = []; lbl = Nolabel; typ = x}
                               : Parsetree.arg))
                       with
                      | [] ->
                        (* zero-argument constructor: the accessor is a value *)
                        annotate_type
                      | args -> Ast_helper.Typ.arrow ~loc args annotate_type))
              | Ptype_open | Ptype_abstract ->
                Ast_derive_util.not_applicable tdcl.ptype_loc deriving_name;
                []
            in
            Ext_list.flat_map tdcls handle_tdcl);
        expression_gen = None;
      })
