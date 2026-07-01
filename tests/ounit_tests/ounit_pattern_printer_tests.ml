let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let loc = Location.none

let rest_type =
  Btype.newgenty
    (Types.Tconstr (Path.Pident (Ident.create "restConfig"), [], ref Types.Mnil))

let record_rest_pattern =
  let rest = Ident.create "_rest" in
  {
    Typedtree.pat_desc =
      Tpat_record
        ( [],
          Asttypes.Closed,
          Some
            {
              rest_ident = rest;
              rest_name = Location.mknoloc (Ident.name rest);
              rest_type;
              excluded_runtime_labels = [];
            } );
    pat_loc = loc;
    pat_extra = [];
    pat_type = rest_type;
    pat_env = Env.empty;
    pat_attributes = [];
  }

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             ~printer:(fun x -> x)
             "{..._rest}"
             (Pattern_printer.print_pattern record_rest_pattern) );
       ]
