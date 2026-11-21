let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

(* An [@as] whose payload is a template string with the given source. *)
let as_template source : Parsetree.attribute =
  ( Location.mknoloc "as",
    Parsetree.PStr
      [
        Ast_helper.Str.eval
          (Ast_helper.Exp.template [Location.mknoloc source] []);
      ] )

let attribute_names attrs =
  List.map (fun (({txt}, _) : Parsetree.attribute) -> txt) attrs

(* The compiler reports what Ast_helper leaves in place, for example in PPX
   output, which the parser has not checked. *)
let assert_invalid_escape_error f =
  match f () with
  | _ -> OUnit.assert_failure "expected an invalid escape error"
  | exception Loc_report.Error {msg} ->
    OUnit.assert_equal ~printer:Fun.id "Invalid string escape sequence" msg

let suites =
  __FILE__
  >::: [
         ( "field with an invalid @as escape keeps the attribute" >:: fun _ ->
           (* Ast_helper builds nodes for the parser, which reports the escape
              itself, and for PPX output, which the type checker reports. It
              must not raise. *)
           let field =
             Ast_helper.Type.field
               ~attrs:[as_template "\\xZZ"]
               (Location.mknoloc "x") (Ast_helper.Typ.any ())
           in
           OUnit.assert_bool "no runtime name" (field.pld_runtime_name = None);
           OUnit.assert_equal ~printer:(String.concat ",") ["as"]
             (attribute_names field.pld_attributes) );
         ( "constructor with an invalid @as escape keeps the attribute"
         >:: fun _ ->
           let constructor =
             Ast_helper.Type.constructor
               ~attrs:[as_template "\\u{110000}"]
               (Location.mknoloc "A")
           in
           OUnit.assert_bool "no runtime tag"
             (constructor.pcd_runtime_tag = None);
           OUnit.assert_equal ~printer:(String.concat ",") ["as"]
             (attribute_names constructor.pcd_attributes) );
         ( "a valid @as template still names the field" >:: fun _ ->
           let field =
             Ast_helper.Type.field
               ~attrs:[as_template "a"]
               (Location.mknoloc "x") (Ast_helper.Typ.any ())
           in
           OUnit.assert_equal ~printer:(String.concat ",") []
             (attribute_names field.pld_attributes);
           OUnit.assert_bool "renamed" (field.pld_runtime_name <> None) );
         ( "the compiler reports an invalid @as escape on a constructor"
         >:: fun _ ->
           let constructor =
             Ast_helper.Type.constructor
               ~attrs:[as_template "\\u{110000}"]
               (Location.mknoloc "A")
           in
           assert_invalid_escape_error (fun () ->
               Ast_untagged_variants.process_constructor_tag constructor) );
         ( "the compiler reports an invalid @as escape on a field" >:: fun _ ->
           let field =
             Ast_helper.Type.field
               ~attrs:[as_template "\\xZZ"]
               (Location.mknoloc "x") (Ast_helper.Typ.any ())
           in
           assert_invalid_escape_error (fun () ->
               Record_runtime.extra_as_attribute field) );
       ]
