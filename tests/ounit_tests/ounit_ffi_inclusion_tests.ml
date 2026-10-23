let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let obj_params (fields : (string * bool option) list) :
    External_arg_spec.obj_params =
  Ext_list.map fields (fun (name, opt) ->
      {
        External_arg_spec.obj_arg_type = Nothing;
        obj_arg_label =
          (match opt with
          | None -> External_arg_spec.obj_label name
          | Some for_sure -> External_arg_spec.optional for_sure name);
      })

let obj fields = External_ffi_types.ffi_obj_create (obj_params fields)

let compatible = External_ffi_types.inclusion_compatible

let suites =
  __FILE__
  >::: [
         (* [for_sure_no_nested_option] is per-module codegen conservatism
            derived from each side's own view of the field's type; it plays
            no role in declaration compatibility, in either direction. *)
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool "impl false, intf true"
             (compatible (obj [("x", Some false)]) (obj [("x", Some true)])) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool "impl true, intf false"
             (compatible (obj [("x", Some true)]) (obj [("x", Some false)])) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool "field names must match"
             (not
                (compatible (obj [("x", Some true)]) (obj [("y", Some true)])))
         );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool "optional vs required must match"
             (not (compatible (obj [("x", Some true)]) (obj [("x", None)]))) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool "field count must match"
             (not
                (compatible
                   (obj [("x", None)])
                   (obj [("x", None); ("y", None)]))) );
       ]
