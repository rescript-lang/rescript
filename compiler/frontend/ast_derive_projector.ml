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
            let handle_uncurried_accessor_tranform ~arity accessor =
              (* Accessors with no params (arity of 0) are simply values and not functions *)
              if arity > 0 then Ast_uncurried.uncurried_fun ~arity accessor
              else accessor
            in
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
                      (Ast_compatible.fun_ ~arity:(Some 1)
                         (Pat.constraint_ (Pat.var {txt; loc}) core_type)
                         (Exp.field
                            (Exp.ident {txt = Lident txt; loc})
                            {txt = Longident.Lident pld_label; loc})
                      (*arity will alwys be 1 since these are single param functions*)
                      |> handle_uncurried_accessor_tranform ~arity:1))
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
                              None)
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
                             @@ Some
                                  (if arity = 1 then
                                     Exp.ident
                                       {loc; txt = Lident (List.hd vars)}
                                   else
                                     Exp.tuple
                                       (Ext_list.map vars (fun x ->
                                            Exp.ident {loc; txt = Lident x}))))
                             annotate_type
                         in
                         Ext_list.fold_right vars exp (fun var b ->
                             Ast_compatible.fun_ ~arity:None
                               (Pat.var {loc; txt = var})
                               b)
                         |> handle_uncurried_accessor_tranform ~arity))
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
                      (Ast_compatible.arrow ~arity:(Some 1) core_type
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
                    let arity = pcd_args |> List.length in
                    let annotate_type =
                      match pcd_res with
                      | Some x -> x
                      | None -> core_type
                    in
                    let add_arity ~arity t =
                      if arity > 0 then Ast_uncurried.uncurried_type ~arity t
                      else t
                    in
                    Ast_comb.single_non_rec_val ?attrs:gentype_attrs
                      {loc; txt = Ext_string.uncapitalize_ascii con_name}
                      (Ext_list.fold_right pcd_args annotate_type (fun x acc ->
                           Ast_compatible.arrow ~arity:None x acc)
                      |> add_arity ~arity))
              | Ptype_open | Ptype_abstract ->
                Ast_derive_util.not_applicable tdcl.ptype_loc deriving_name;
                []
            in
            Ext_list.flat_map tdcls handle_tdcl);
        expression_gen = None;
      })
