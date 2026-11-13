open OUnit
open Analysis

let var name = Btype.newgenty (Types.Tvar (Some name))

(* instantiate_type2 doesn't read the environment *)
let env =
  Shared_types.Query_env.from_file
    {
      Shared_types.File.uri = Uri.from_path "/Test.res";
      stamps = Shared_types.Stamps.init ();
      module_name = "Test";
      structure =
        {
          name = "Test";
          docstring = [];
          exported = Shared_types.Exported.init ();
          items = [];
          deprecated = None;
        };
    }

let is_int (t : Types.type_expr) =
  match (Btype.repr t).desc with
  | Tconstr (path, [], _) -> Path.same path Predef.path_int
  | _ -> false

let var_name (t : Types.type_expr) =
  match (Btype.repr t).desc with
  | Tvar name -> name
  | _ -> None

let deprecated payload =
  Process_attributes.find_deprecated_attribute
    [
      ( {Location.txt = "deprecated"; loc = Location.none},
        Parsetree.PStr [Ast_helper.Str.eval payload] );
    ]

let string_exp s = Ast_helper.Exp.constant (Ast_helper.Const.string s)

let record fields =
  Ast_helper.Exp.record
    (List.map
       (fun (name, x) ->
         {
           Parsetree.lid = {txt = Longident.Lident name; loc = Location.none};
           x;
           opt = false;
         })
       fields)
    None

let show = function
  | Some s -> "Some " ^ s
  | None -> "None"

let suites =
  __FILE__
  >::: [
         ( "instantiating by name keeps a variable that isn't a parameter"
         >:: fun _ ->
           let type_arg_context =
             {
               Shared_types.env;
               type_params = [var "a"];
               type_args = [Predef.type_int ()];
             }
           in
           let t = Btype.newgenty (Ttuple [var "a"; var "b"]) in
           match (Type_utils.instantiate_type2 ~type_arg_context t).desc with
           | Ttuple [a; b] ->
             assert_bool "the parameter is substituted" (is_int a);
             assert_equal ~printer:show (Some "b") (var_name b)
           | _ -> assert_failure "not a tuple" );
         ( "@deprecated reads as the compiler reads it" >:: fun _ ->
           assert_equal ~printer:show (Some "use b")
             (deprecated (string_exp "use b"));
           assert_equal ~printer:show (Some "use b")
             (deprecated (record [("reason", string_exp "use b")]));
           (* the compiler doesn't warn for a record without a reason *)
           assert_equal ~printer:show None
             (deprecated (record [("migrate", string_exp "b")])) );
       ]
