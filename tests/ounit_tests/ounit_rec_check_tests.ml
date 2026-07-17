let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let loc = Location.none
let typ = Predef.type_unit

let val_desc =
  {Types.val_type = typ; val_kind = Val_reg; val_loc = loc; val_attributes = []}

let ident_expr id =
  {
    Typedtree.exp_desc =
      Texp_ident
        ( Path.Pident id,
          Location.mknoloc (Longident.Lident (Ident.name id)),
          val_desc );
    exp_loc = loc;
    exp_extra = [];
    exp_type = typ;
    exp_env = Env.empty;
    exp_attributes = [];
  }

let record_rest_pat rest_ident =
  {
    Typedtree.pat_desc =
      Tpat_record
        ( [],
          Asttypes.Closed,
          Some
            {
              rest_ident;
              rest_name = Location.mknoloc (Ident.name rest_ident);
              rest_type = typ;
              excluded_runtime_labels = [];
            } );
    pat_loc = loc;
    pat_extra = [];
    pat_type = typ;
    pat_env = Env.empty;
    pat_attributes = [];
  }

let value_binding ~pat ~expr =
  {Typedtree.vb_pat = pat; vb_expr = expr; vb_attributes = []; vb_loc = loc}

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           let rest = Ident.create "rest" in
           let binding =
             value_binding ~pat:(record_rest_pat rest) ~expr:(ident_expr rest)
           in
           let raised =
             try
               Rec_check.check_recursive_bindings [binding];
               false
             with _ -> true
           in
           OUnit.assert_bool __LOC__ raised );
       ]
